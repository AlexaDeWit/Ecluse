-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The scratch buffer's writes against aeson's bytes for the same strings and integers.
module Ecluse.Core.Registry.Json.ScratchSpec (spec) where

import Control.Monad.ST (ST, runST)
import Data.Aeson (Value (Number), encode)
import Data.ByteString.Short qualified as SBS
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Registry.Json.Packed (encodeString)
import Ecluse.Core.Registry.Json.Scratch (Scratch, copyOut, decimalLength, newScratch, putDecimal, putEncodedText, rewindTo, scratchCursor)

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ describe "Scratch" $ do
    it "writes each string as aeson writes it, growing from a small buffer" $
        hedgehog $ do
            texts <- forAll (Gen.list (Range.linear 0 8) (Gen.text (Range.linear 0 40) (Gen.frequency [(3, Gen.unicode), (2, Gen.element hostile)])))
            written (\scratch -> traverse_ (putEncodedText scratch) texts) === foldMap encodeString texts

    it "writes each integer as aeson writes it, and counts its digits" $
        hedgehog $ do
            number <- forAll (Gen.frequency [(4, Gen.int Range.linearBounded), (1, Gen.element [minBound, maxBound, 0, -1])])
            written (`putDecimal` number) === toStrict (encode (Number (fromIntegral number)))
            decimalLength number === length (show number :: String)

    it "forgets what it wrote after the offset it rewinds to" $
        hedgehog $ do
            (kept, dropped, later) <- forAll ((,,) <$> Gen.text (Range.linear 0 300) Gen.unicode <*> Gen.text (Range.linear 0 300) Gen.unicode <*> Gen.text (Range.linear 0 300) Gen.unicode)
            let rewound scratch = do
                    putEncodedText scratch kept
                    mark <- scratchCursor scratch
                    putEncodedText scratch dropped
                    rewindTo scratch mark
                    putEncodedText scratch later
            written rewound === encodeString kept <> encodeString later
  where
    hostile = ['\0' .. '\x1f'] <> "\"\\\x7f\x2028\x1F600"

-- What the writes leave in a fresh scratch of 64 bytes, copied out.
written :: (forall st. Scratch st -> ST st ()) -> ByteString
written write = runST $ do
    scratch <- newScratch 1
    write scratch
    end <- scratchCursor scratch
    SBS.fromShort . SBS.ShortByteString <$> copyOut scratch 0 end
