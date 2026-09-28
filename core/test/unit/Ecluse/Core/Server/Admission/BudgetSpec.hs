-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.Admission.BudgetSpec (spec) where

import Data.IntSet qualified as IntSet
import Hedgehog (assert, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Server.Admission.Budget (
    EntryGate (..),
    GrowthGate (..),
    MeterView (..),
    entryDecision,
    growthDecision,
    roundUpToStep,
    scaleCharge,
 )

spec :: Spec
spec = describe "Ecluse.Core.Server.Admission.Budget" $ do
    chargeSpec
    entrySpec
    growthSpec

chargeSpec :: Spec
chargeSpec = describe "scaleCharge and roundUpToStep" $ do
    it "charges nothing for nothing, and at least one byte for any read" $ do
        scaleCharge 750 0 `shouldBe` 0
        scaleCharge 0 4096 `shouldBe` 0
        scaleCharge 750 1 `shouldBe` 1
        scaleCharge 1300 1000 `shouldBe` 1300

    it "rounds up to within one byte of the exact product (property)" $ hedgehog $ do
        permille <- forAll (Gen.int (Range.linear 1 5000))
        bytes <- forAll (Gen.int (Range.linear 1 (256 * 1024 * 1024)))
        let charged = scaleCharge permille bytes
        assert (charged * 1000 >= bytes * permille)
        assert ((charged - 1) * 1000 < bytes * permille)

    it "covers a shortfall with the fewest whole steps (property)" $ hedgehog $ do
        step <- forAll (Gen.int (Range.linear 1 (4 * 1024 * 1024)))
        shortfall <- forAll (Gen.int (Range.linear 1 (64 * 1024 * 1024)))
        let covered = roundUpToStep step shortfall
        covered `mod` step === 0
        assert (covered >= shortfall)
        assert (covered - shortfall < step)

entrySpec :: Spec
entrySpec = describe "entryDecision" $ do
    it "admits a step that fits an empty door" $
        entryDecision (view 10 0) 0 4 1 `shouldBe` EntryAdmit

    it "queues behind a paused read even when the step fits" $
        entryDecision (view 10 0){mvOldestWaiter = Just 3} 0 4 1 `shouldBe` EntryQueue

    it "queues behind earlier arrivals, and refuses at a full room" $ do
        entryDecision (view 10 0) 2 4 1 `shouldBe` EntryQueue
        entryDecision (view 10 10) 4 4 1 `shouldBe` EntryRefuse

    it "never admits past the budget (property)" $ hedgehog $ do
        budget <- forAll (Gen.int (Range.linear 0 1000))
        charged <- forAll (Gen.int (Range.linear 0 2000))
        step <- forAll (Gen.int (Range.linear 1 100))
        let gate = entryDecision (view budget charged) 0 8 step
        assert (gate /= EntryAdmit || charged + step <= budget)

growthSpec :: Spec
growthSpec = describe "growthDecision" $ do
    it "lets a fitting step through, whoever holds the token" $
        growthDecision (view 10 5){mvToken = Just 1} (serving [7]) 5 `shouldBe` GrowWithin

    it "lets the token holder overdraw" $
        growthDecision (view 10 10){mvToken = Just 7} (serving [7]) 5 `shouldBe` GrowOnToken

    it "lets work the token holder waits on overdraw on the holder's token" $
        growthDecision (view 10 10){mvToken = Just 1} (serving [9, 1]) 5 `shouldBe` GrowOnToken

    it "hands a free token to work that serves the oldest paused ticket only" $ do
        let full = (view 10 10){mvOldestWaiter = Just 3}
        growthDecision full (serving [3]) 5 `shouldBe` GrowTakeToken
        growthDecision full (serving [2]) 5 `shouldBe` GrowTakeToken
        growthDecision full (serving [8, 3]) 5 `shouldBe` GrowTakeToken
        growthDecision full (serving [4]) 5 `shouldBe` GrowWait

    it "makes everyone else wait while the token is held" $
        growthDecision (view 10 10){mvToken = Just 1, mvOldestWaiter = Just 0} (serving [0]) 5 `shouldBe` GrowWait

    it "lets exactly one of a full meter's paused tickets take a free token (property)" $ hedgehog $ do
        tickets <- forAll (Gen.list (Range.linear 1 20) (Gen.int (Range.linear 0 1000)))
        want <- forAll (Gen.int (Range.linear 1 100))
        let paused = ordNub tickets
            full = (view 50 50){mvOldestWaiter = listToMaybe (sort paused)}
            moving = [ticket | ticket <- paused, growthDecision full (serving [ticket]) want == GrowTakeToken]
        length moving === 1
  where
    serving = IntSet.fromList

view :: Int -> Int -> MeterView
view budget charged = MeterView{mvBudget = budget, mvCharged = charged, mvToken = Nothing, mvOldestWaiter = Nothing}
