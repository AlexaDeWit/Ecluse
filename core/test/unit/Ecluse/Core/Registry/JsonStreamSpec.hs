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
import Ecluse.Core.Snapshot (digestOf)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)
import Ecluse.Test.Support (expectRight)

-- | Verify retained-depth boundaries, source identity and response cancellation.
spec :: Spec
spec = describe "readJsonStream" $ do
    forM_ [object [], Array mempty, String "leaf"] $ \value -> do
        it ("accepts one retained level for " <> show value) $ do
            result <- expectRight (decode (retainedValue 1) [toStrict (encode value)])
            streamValue result `shouldBe` Right (Just value)
        it ("refuses an exhausted retained level for " <> show value) $ do
            result <- expectRight (decode (withinRetainedDepth 0 (retainedValue 1)) [toStrict (encode value)])
            streamValue result `shouldSatisfy` isLeft

    it "preserves nested values and split escapes at every source boundary" $ do
        let body = "{\"keep\":{\"list\":[1,true,null,\"a\\\\b\\u00e9\"]},\"ignored\":{\"blob\":[1,2,3]}}"
            parser = retainedObject (\key -> if key == "keep" then retainedValue 10 else mempty)
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
                    (retainedObject (\key -> if key == "keep" then retainedValue 3 else mempty))
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
