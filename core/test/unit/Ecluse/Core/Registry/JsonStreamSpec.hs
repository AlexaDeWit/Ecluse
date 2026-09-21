-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Chunk boundaries, source identity and cancellation for incremental registry reads.
module Ecluse.Core.Registry.JsonStreamSpec (spec) where

import Data.Aeson (Value (Array, Bool, Null, Number, String), encode, object, (.=))
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Test.Hspec
import UnliftIO.Async (cancel, waitCatch, withAsync)
import UnliftIO.Exception (finally)

import Ecluse.Core.Registry.JsonStream
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge))
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

-- | Verify retained-depth boundaries, source identity and response cancellation.
spec :: Spec
spec = describe "readJsonStream" $ do
    forM_ [("object", \fallback -> retainedObjectWith fallback (const (retainedValue 3)), object ["keep" .= (1 :: Int)]), ("array", \fallback -> retainedArrayWith fallback (retainedValue 3), Array (fromList [Number 1]))] $ \(label, choose, value) ->
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
        let parser = retainedObjectWith (retainedScalar <|> pure (Array mempty)) (const (retainedValue 1))
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
            parser = retainedObjectWith mempty (\key -> if key == "keep" then retainedValue 10 else mempty)
            baseline = decode parser [body]
        forM_ [1 .. BS.length body - 1] $ \position ->
            decode parser [BS.take position body, BS.drop position body] `shouldBe` baseline
        result <- expectRight baseline
        streamValue result `shouldBe` Right (Just (object ["keep" .= object ["list" .= [Number 1, Bool True, Null, String "a\\b\xE9"]]]))
        streamDigest result `shouldBe` digestOf body
        streamBytes result `shouldBe` BS.length body

    it "skips an unrecognised object before constructing retained values" $ do
        result <-
            expectRight
                ( decode
                    (retainedObjectWith mempty (\key -> if key == "keep" then retainedValue 3 else mempty))
                    ["{\"ignored\":{\"deep\":[[[[[1]]]]]},\"keep\":\"yes\"}"]
                )
        streamValue result `shouldBe` Right (Just (object ["keep" .= ("yes" :: Text)]))

    it "drains and hashes trailing chunks after the selected JSON object ends" $ do
        let chunks = ["{\"name\":\"thing\"}", " trailing", " bytes"]
            body = BS.concat chunks
        result <- expectRight (decode (J.objectWithKey "name" (retainedValue 1)) chunks)
        streamValue result `shouldBe` Right (Just (String "thing"))
        streamDigest result `shouldBe` digestOf body
        streamBytes result `shouldBe` BS.length body

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

-- Keep the test fold independent of an accumulated list of parser events.
decode :: J.Parser a -> [ByteString] -> Either LimitError (StreamResult (Maybe a))
decode parser = parseJsonChunks (MetadataBodyLimit (1024 * 1024)) parser (\_ value -> Right (Just value)) Nothing
