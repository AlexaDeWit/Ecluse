-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Chunk boundaries, source size, cancellation and shared texts for incremental registry reads.
module Ecluse.Core.Registry.JsonStreamSpec (spec) where

import Data.Aeson (Value (Array, Bool, Null, Number, Object, String), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import UnliftIO.Async (cancel, waitCatch, withAsync)
import UnliftIO.Exception (finally)

import Ecluse.Core.Registry.JsonStream
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge))
import Ecluse.Core.Text (textStorageBytes)
import Ecluse.Test.Json (fieldAt, genValue)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks, sharesKey, sharesString)
import Ecluse.Test.Support (expectRight)

-- | Verify retained-depth boundaries, source size, response cancellation and the intern table.
spec :: Spec
spec = do
    readSpec
    internSpec

readSpec :: Spec
readSpec = describe "readJsonStream" $ do
    forM_ [("ASCII", "plain", "value"), ("Unicode", "clé😀", "été𝄞"), ("escaped", "key\\\"\n", "value\t\\\""), ("long", T.replicate 40000 "k", T.replicate 40000 "v")] $ \(label, key, value) ->
        it ("preserves " <> label <> " keys and values across source chunks") $ do
            let expected = object [Key.fromText key .= value]
                body = toStrict (encode expected)
            forM_ [1, 7, 32768] $ \size -> do
                let chunks = unfoldr (\rest -> if BS.null rest then Nothing else Just (BS.splitAt size rest)) body
                result <- expectRight (decode (retainedValue 2) chunks)
                streamValue result `shouldBe` Right (Just expected)
                streamBytes result `shouldBe` BS.length body

    it "equates escaped Unicode keys and preserves their first value at every boundary" $ do
        let body = encodeUtf8 ("{\"cl\\u00e9\\ud83d\\ude00\":\"\\ud834\\udd1e\",\"clé😀\":\"later\"}" :: Text)
            expected = object ["clé😀" .= ("𝄞" :: Text)]
        forM_ [1 .. BS.length body - 1] $ \position -> do
            result <- expectRight (decode (retainedValue 2) [BS.take position body, BS.drop position body])
            streamValue result `shouldBe` Right (Just expected)

    forM_ [("object", \fallback -> retainedObjectWith fallback (everyMember (retainedValue 3)), object ["keep" .= (1 :: Int)]), ("array", \fallback -> retainedArrayWith fallback (retainedValue 3), Array (fromList [Number 1]))] $ \(label, choose, value) ->
        it ("commits to the " <> label <> " before reading its next chunk") $ do
            let fallback = J.mapWithFailure (const (Left "unselected fallback")) (J.objectFound () () mempty <> J.arrayFound () () mempty)
                body = toStrict (encode value)
            result <- expectRight (decode (choose fallback) [BS.take 1 body, BS.drop 1 body])
            streamValue result `shouldBe` Right (Just value)

    forM_ [Null, Bool False, Number 2, String "text", object [], Array mempty] $ \value ->
        it ("preserves generic shape selection for " <> show value) $ do
            result <- expectRight (decode (J.objectWithKey "value" (retainedValue 1)) [toStrict (encode (object ["value" .= value]))])
            streamValue result `shouldBe` Right (Just value)

    it "preserves a scalar fallback and an invalid-container witness" $ do
        let parser = retainedObjectWith (retainedScalar <|> pure (Array mempty)) (everyMember (retainedValue 1))
        forM_ [("true", Bool True), ("null", Null), ("[1,2]", Array mempty)] $ \(body, expected) -> do
            result <- expectRight (decode (J.objectWithKey "value" parser) ["{\"value\":" <> body <> "}"])
            streamValue result `shouldBe` Right (Just expected)

    it "keeps first duplicate keys and nested array order across both container alternatives" $ do
        let body = "[{\"key\":[1,2],\"key\":[3]},[false,null]]"
            expected = fromList [object ["key" .= [Number 1, Number 2]], Array (fromList [Bool False, Null])]
        forM_ [1 .. BS.length body - 1] $ \position -> do
            result <- expectRight (decode (retainedValue 4) [BS.take position body, BS.drop position body])
            streamValue result `shouldBe` Right (Just (Array expected))

    forM_ [object [], Array mempty, String "leaf"] $ \value -> do
        it ("accepts one retained level for " <> show value) $ do
            result <- expectRight (decode (retainedValue 1) [toStrict (encode value)])
            streamValue result `shouldBe` Right (Just value)
        it ("refuses an exhausted retained level for " <> show value) $ do
            result <- expectRight (decode (withinRetainedDepth 0 (retainedValue 1)) [toStrict (encode value)])
            streamValue result `shouldSatisfy` isLeft

    it "preserves nested values and split escapes at every source boundary" $ do
        let body = "{\"keep\":{\"list\":[1,true,null,\"a\\\\b\\u00e9\"]},\"ignored\":{\"blob\":[1,2,3]}}"
            parser = retainedObjectWith mempty (namedMembers [("keep", retainedValue 10)])
            baseline = decode parser [body]
        forM_ [1 .. BS.length body - 1] $ \position ->
            decode parser [BS.take position body, BS.drop position body] `shouldBe` baseline
        result <- expectRight baseline
        streamValue result `shouldBe` Right (Just (object ["keep" .= object ["list" .= [Number 1, Bool True, Null, String "a\\b\xE9"]]]))
        streamBytes result `shouldBe` BS.length body

    it "skips an unrecognised object before constructing retained values" $ do
        result <-
            expectRight
                ( decode
                    (retainedObjectWith mempty (namedMembers [("keep", retainedValue 3)]))
                    ["{\"ignored\":{\"deep\":[[[[[1]]]]]},\"keep\":\"yes\"}"]
                )
        streamValue result `shouldBe` Right (Just (object ["keep" .= ("yes" :: Text)]))

    forM_ [("a named member", namedMembers [("keep", retainedValue 1)], True), ("a known member", knownMembers ["keep"] (retainedValue 1), True), ("an unlisted member", everyMember (retainedValue 1), False)] $ \(label, members, shared) ->
        it ("holds the key of " <> label <> " as one object across objects: " <> show shared) $ do
            result <- expectRight (decode (retainedArrayWith mempty (retainedObjectWith mempty members)) ["[{\"keep\":1},{\"keep\":2}]"])
            objects <- either (fail . show) (maybe (fail "no array") pure) (streamValue result)
            sharesKey "keep" (arrayItems objects) `shouldReturn` shared

    it "keeps the first of duplicate named members" $ do
        result <- expectRight (decode (retainedObjectWith mempty (namedMembers [("keep", retainedValue 1)])) ["{\"keep\":1,\"keep\":2}"])
        streamValue result `shouldBe` Right (Just (object ["keep" .= (1 :: Int)]))

    it "drains trailing chunks after the selected JSON object ends" $ do
        let chunks = ["{\"name\":\"thing\"}", " trailing", " bytes"]
        result <- expectRight (decode (J.objectWithKey "name" (retainedValue 1)) chunks)
        streamValue result `shouldBe` Right (Just (String "thing"))
        streamBytes result `shouldBe` BS.length (BS.concat chunks)

    it "applies the decompressed body ceiling to ignored trailing bytes" $
        parseJsonChunks (MetadataBodyLimit 2) (retainedValue 3) (\_ value -> Right (Just value)) Nothing ["{}", "x"]
            `shouldBe` Left (BodyTooLarge (MetadataBodyLimit 2))

    it "reports a truncated required object as a parse error" $ do
        result <- expectRight (decode (retainedValue 3) ["{\"keep\":"])
        streamValue result `shouldSatisfy` isLeft

    it "propagates cancellation while waiting for the next body chunk" $ do
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        released <- newEmptyMVar
        let next = (putMVar entered () >> takeMVar blocked) `finally` putMVar released ()
        withAsync (readJsonStream (MetadataBodyLimit 1024) (retainedValue 3) (\_ value -> Right (Just value)) Nothing next) $ \worker -> do
            takeMVar entered
            cancel worker
            waitCatch worker >>= (`shouldSatisfy` isLeft)
            takeMVar released

internSpec :: Spec
internSpec = describe "internValue" $ do
    it "holds every occurrence of a repeated key and string as one copy" $ do
        items <- repeatedItems
        sharesKey "dep" items `shouldReturn` False
        let held = internAll emptyInternTable items
        sharesKey "dep" held `shouldReturn` True
        sharesString "^2" (mapMaybe (fieldAt "dep") held) `shouldReturn` True

    it "keeps the first of duplicate keys as the reader kept it" $ do
        result <- expectRight (decode (retainedValue 2) ["{\"dep\":\"^2\",\"dep\":\"^3\"}"])
        item <- either (fail . show) (maybe (fail "no object") pure) (streamValue result)
        snd (internValue emptyInternTable item) `shouldBe` object ["dep" .= ("^2" :: Text)]

    it "shares nothing between the tables of two documents" $ do
        items <- repeatedItems
        let held = concatMap (internAll emptyInternTable . one) items
        sharesKey "dep" held `shouldReturn` False
        sharesString "^2" (mapMaybe (fieldAt "dep") held) `shouldReturn` False

    it "holds a key and a string with the same text as one copy" $ do
        items <- expectRight (decode (retainedValue 2) ["{\"dep\":\"dep\"}"])
        item <- either (fail . show) (maybe (fail "no object") pure) (streamValue items)
        let texts value = case value of
                Object fields -> map (String . Key.toText) (KeyMap.keys fields) <> KeyMap.elems fields
                _ -> []
        sharesString "dep" (texts item) `shouldReturn` False
        sharesString "dep" (texts (snd (internValue emptyInternTable item))) `shouldReturn` True

    it "stores a compact copy of a text cut from a larger one" $ do
        let slice = T.drop 1 "xvalue"
        textStorageBytes slice `shouldBe` 6
        textStorageBytes (snd (internText emptyInternTable slice)) `shouldBe` 5

    it "changes no value and no encoding" $
        hedgehog $ do
            values <- forAll (Gen.list (Range.linear 1 6) (genValue ["dep", "name", "version"]))
            let held = internAll emptyInternTable values
            held === values
            map encode held === map encode values

internAll :: InternTable -> [Value] -> [Value]
internAll table = snd . mapAccumL internValue table

-- Two equal objects the reader decoded separately, so they share no key or string yet.
repeatedItems :: IO [Value]
repeatedItems = do
    result <- expectRight (decode (retainedValue 3) ["[{\"dep\":\"^2\"},{\"dep\":\"^2\"}]"])
    either (fail . show) (maybe (fail "no array") (pure . arrayItems)) (streamValue result)

arrayItems :: Value -> [Value]
arrayItems = \case
    Array items -> toList items
    _ -> []

-- Keep the test fold independent of an accumulated list of parser events.
decode :: J.Parser a -> [ByteString] -> Either LimitError (StreamResult (Maybe a))
decode parser = parseJsonChunks (MetadataBodyLimit (1024 * 1024)) parser (\_ value -> Right (Just value)) Nothing
