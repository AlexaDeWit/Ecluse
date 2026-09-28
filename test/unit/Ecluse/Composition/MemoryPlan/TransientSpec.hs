-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The transient budget carved from the live target.
module Ecluse.Composition.MemoryPlan.TransientSpec (spec) where

import Hedgehog (assert, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Composition.MemoryPlan.Transient (
    idleLiveFloorBytes,
    liveTargetBytes,
    noCeilingTransientBytes,
    transientBudget,
    transientFloorBytes,
 )
import Ecluse.Composition.MemoryPlan.Types (TransientBudget (..))
import Ecluse.Composition.Support (mib)

spec :: Spec
spec = describe "transientBudget" $ do
    it "takes a quarter of the heap the nursery leaves as the live target" $ do
        -- 2 CPU / 512 MiB after the posture: -M 416 MiB, -A 32 MiB.
        liveTargetBytes (416 * mib) 2 (32 * mib) `shouldBe` 88 * mib
        let budget = transientBudget (Just (416 * mib)) 2 (32 * mib) (26 * mib)
        tbLiveTargetBytes budget `shouldBe` Just (88 * mib)
        tbBootBytes budget `shouldBe` 88 * mib - idleLiveFloorBytes - 26 * mib
        tbOverflowLiveBytes budget `shouldBe` Just (176 * mib)

    it "sets the live ceiling the sampler grows toward at a third of that heap" $ do
        let budget = transientBudget (Just (1728 * mib)) 4 (64 * mib) (100 * mib)
        tbLiveCeilingBytes budget `shouldBe` Just ((1728 - 256) * mib `div` 3)
        assert' (tbBootBytes budget + tbExplainedBytes budget <= (1728 - 256) * mib `div` 3)

    it "floors a small pod's budget" $
        tbBootBytes (transientBudget (Just (208 * mib)) 2 (16 * mib) (40 * mib)) `shouldBe` transientFloorBytes

    it "falls back to a large constant with no heap ceiling" $ do
        let budget = transientBudget Nothing 3 (64 * mib) (256 * mib)
        tbBootBytes budget `shouldBe` noCeilingTransientBytes
        tbLiveCeilingBytes budget `shouldBe` Nothing
        tbLiveTargetBytes budget `shouldBe` Nothing

    it "keeps the boot budget between the floor and the live ceiling for every pod (property)" $ hedgehog $ do
        ceiling' <- forAll (Gen.int (Range.linear (64 * mib) (64 * 1024 * mib)))
        caps <- forAll (Gen.int (Range.linear 1 64))
        area <- forAll (Gen.int (Range.linear (4 * mib) (64 * mib)))
        retained <- forAll (Gen.int (Range.linear 0 (4096 * mib)))
        let budget = transientBudget (Just ceiling') caps area retained
        assert (tbFloorBytes budget <= tbBootBytes budget)
        assert (tbBootBytes budget == tbFloorBytes budget || all (tbBootBytes budget + tbExplainedBytes budget <=) (tbLiveCeilingBytes budget))
  where
    assert' condition = condition `shouldBe` True
