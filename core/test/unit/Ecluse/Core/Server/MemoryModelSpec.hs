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

import Ecluse.Core.Server.MemoryModel (chargeForResident, expandWireBytes)

spec :: Spec
spec = describe "chargeForResident" $ do
    it "charges the fewest compact bytes whose expansion covers the heap bytes" $
        hedgehog $ do
            resident <- forAll (Gen.int (Range.linear 0 (256 * 1024 * 1024)))
            let charge = chargeForResident resident
            (expandWireBytes charge >= resident) === True
            (charge == 0 || expandWireBytes (charge - 1) < resident) === True

    it "charges nothing for nothing, and one compact byte for one heap byte" $ do
        chargeForResident 0 `shouldBe` 0
        chargeForResident 1 `shouldBe` 1
