-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | JSON string bytes against aeson and frozen writer outputs, including sliced UTF-8.
module Ecluse.Core.Registry.Json.StringEncodingSpec (spec) where

import Control.Monad.ST (ST, runST)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Encoding qualified as Encoding
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Primitive.ByteArray (MutableByteArray, newByteArray, setByteArray, unsafeFreezeByteArray)
import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Registry.Json.StringEncoding
import Ecluse.Test.Registry.JsonBytes (genHostileText, legacyStringEncodings)

spec :: Spec
spec = modifyMaxSuccess (const 2000) $ do
    describe "encodeString" $
        it "writes every string as aeson writes it, escapes, controls and astral characters included" $
            hedgehog $ do
                text <- forAll genHostileText
                encodeString text === toStrict (encodingToLazyByteString (Encoding.text text))
                encodedLength text === BS.length (encodeString text)

    describe "writeEncodedSized" $ do
        it "matches aeson for sliced UTF-8 at a nonzero output offset, preserving surrounding bytes" $
            hedgehog $ do
                text <- forAll genHostileText
                let sliced = T.drop 1 (T.dropEnd 1 ("\x1f600" <> text <> "\\"))
                    expected = toStrict (encodingToLazyByteString (Encoding.text text))
                    len = BS.length expected
                    expectedBuffer = BS.replicate 3 0xa5 <> expected <> BS.replicate 3 0xa5
                writtenString len (writeEncodedSized sliced len) === (len + 3, expectedBuffer)
                writtenString len (writeEncoded sliced) === (len + 3, expectedBuffer)

        it "preserves the pinned writer's bytes for plain text, UTF-8, every control and escapes" $
            forM_ legacyStringEncodings $ \(text, expected) -> do
                let len = BS.length expected
                    expectedBuffer = BS.replicate 3 0xa5 <> expected <> BS.replicate 3 0xa5
                encodedLength text `shouldBe` len
                writtenString len (writeEncodedSized text len) `shouldBe` (len + 3, expectedBuffer)
                writtenString len (writeEncoded text) `shouldBe` (len + 3, expectedBuffer)

writtenString :: Int -> (forall st. MutableByteArray st -> Int -> ST st Int) -> (Int, ByteString)
writtenString len write = runST $ do
    target <- newByteArray (len + 6)
    setByteArray target 0 (len + 6) (0xa5 :: Word8)
    end <- write target 3
    bytes <- unsafeFreezeByteArray target
    pure (end, SBS.fromShort (SBS.ShortByteString bytes))
