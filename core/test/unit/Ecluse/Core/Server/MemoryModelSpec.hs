-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The compact charge for measured heap bytes against the expansion a cache applies to it.
module Ecluse.Core.Server.MemoryModelSpec (spec) where

import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Server.MemoryModel (chargeForResident, expandWireBytes, expandWireBytesInteger, saturatingCharge)

spec :: Spec
spec = do
    residentSpec
    saturationSpec

residentSpec :: Spec
residentSpec = describe "chargeForResident" $ do
    it "preserves ordinary expansion while retaining overflow-sized state terms" $ do
        expandWireBytes 331 `shouldBe` 2482
        expandWireBytesInteger 331 `shouldBe` 2482
        expandWireBytesInteger (toInteger (maxBound :: Int)) `shouldSatisfy` (> toInteger (maxBound :: Int))

    it "charges the fewest compact bytes whose expansion covers the heap bytes" $
        hedgehog $ do
            resident <- forAll (Gen.int (Range.linear 0 (256 * 1024 * 1024)))
            let charge = chargeForResident resident
            (expandWireBytes charge >= resident) === True
            (charge == 0 || expandWireBytes (charge - 1) < resident) === True

    it "charges nothing for nothing, and one compact byte for one heap byte" $ do
        chargeForResident 0 `shouldBe` 0
        chargeForResident 1 `shouldBe` 1

saturationSpec :: Spec
saturationSpec = describe "saturatingCharge" $
    it "keeps an Int-sized charge and clamps below zero and past maxBound" $ do
        saturatingCharge 4096 `shouldBe` 4096
        saturatingCharge (-1) `shouldBe` 0
        saturatingCharge (toInteger (maxBound :: Int) + 1) `shouldBe` maxBound
