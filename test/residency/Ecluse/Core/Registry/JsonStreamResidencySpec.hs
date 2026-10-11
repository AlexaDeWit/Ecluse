-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Decoded keys and strings outlive their source allocation and completed parser.
module Ecluse.Core.Registry.JsonStreamResidencySpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import Test.Hspec
import UnliftIO.Exception (bracket, evaluate)

import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue), retainedValue)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit))
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)
import Ecluse.Test.Registry.Source (assertSourceHeld, awaitSourceRelease, trackedSource)
import Ecluse.Test.Support (expectRight)

-- | Track the underlying allocation, with a retained-slice control that must prevent finalisation.
spec :: Spec
spec = describe "decoded text ownership" $ do
    it "keeps the source allocation alive through a retained ByteString slice" $ do
        released <- newEmptyMVar
        bracket
            (trackedSource "{}" released >>= newStablePtr . BS.take 1)
            freeStablePtr
            ( \root -> do
                assertSourceHeld released
                deRefStablePtr root >>= (`shouldBe` "{")
            )
        awaitSourceRelease released

    forM_ [("ASCII", "plain", "value"), ("Unicode", "clé😀", "été𝄞"), ("escaped", "key\n\"", "value\t\\"), ("long", T.replicate 40000 "k", T.replicate 40000 "v")] $ \(label, key, value) ->
        forM_ [7, 32768] $ \size ->
            it ("releases the source while retaining " <> label <> " text with chunks of " <> show size) $ do
                let expected = object [Key.fromText key .= value]
                released <- newEmptyMVar
                bracket
                    (decodeTracked size expected released >>= newStablePtr)
                    freeStablePtr
                    ( \root -> do
                        awaitSourceRelease released
                        deRefStablePtr root >>= (`shouldBe` expected)
                    )

decodeTracked :: Int -> Value -> MVar () -> IO Value
decodeTracked size expected released = do
    source <- trackedSource (toStrict (encode expected)) released
    let chunks = unfoldr (\rest -> if BS.null rest then Nothing else Just (BS.splitAt size rest)) source
    result <- expectRight (parseJsonChunks (MetadataBodyLimit (8 * 1024 * 1024)) (retainedValue 2) (\_ value -> Right (Just value)) Nothing chunks)
    value <- expectRight (streamValue result) >>= maybe (fail "missing retained value") pure
    void (evaluate (BS.length (toStrict (encode value))))
    pure value
