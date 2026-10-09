-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The counts a run checks, and the committed floors against the scenarios and pod shapes a scheduled run measures.
module Ecluse.BenchLoad.ScenariosSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec

import Ecluse.BenchLoad.Floors (Calibration (calOperatingPoint, calRuns), Floors (floorsByShape, floorsCalibration), Pass (ConcurrencyOne, Loaded), loadFloors)
import Ecluse.BenchLoad.Harness (defaultLoadKnobs, operatingPoint)
import Ecluse.BenchLoad.Pod (PodShape (Limited, Unlimited), scheduledPodShapes)
import Ecluse.BenchLoad.Scenarios (checkedCounts)

spec :: Spec
spec = do
    describe "checkedCounts" $ do
        it "checks in-process work only under the unlimited shape, the one shape that runs it" $ do
            checkedCounts Unlimited `shouldSatisfy` Set.member ("npm/worker-mirroring", Loaded)
            checkedCounts twoCores `shouldSatisfy` Set.notMember ("npm/worker-mirroring", Loaded)
        it "checks the concurrency-one pass only of a scenario that joins it" $ do
            checkedCounts twoCores `shouldSatisfy` Set.member ("pypi/index-cold", ConcurrencyOne)
            checkedCounts twoCores `shouldSatisfy` Set.member ("npm/ramp", Loaded)
            checkedCounts twoCores `shouldSatisfy` Set.notMember ("npm/ramp", ConcurrencyOne)

    describe "the committed floors" $ do
        it "cover the pod shapes a scheduled run measures, and no other" $
            withCommitted $ \floors ->
                Map.keys (floorsByShape floors) `shouldMatchList` scheduledPodShapes
        it "hold a floor for every count a scheduled run checks, and for no other" $
            withCommitted $ \floors ->
                for_ scheduledPodShapes $ \shape -> do
                    let held = Map.keysSet (Map.findWithDefault Map.empty shape (floorsByShape floors))
                    (shape, Set.toList (checkedCounts shape `Set.difference` held), Set.toList (held `Set.difference` checkedCounts shape))
                        `shouldBe` (shape, [], [])
        it "were calibrated at the harness's default knobs, over every scenario" $
            withCommitted $ \floors ->
                calOperatingPoint (floorsCalibration floors) `shouldBe` operatingPoint defaultLoadKnobs Nothing
        it "record the ten runs they were calibrated from" $
            withCommitted $ \floors ->
                length (calRuns (floorsCalibration floors)) `shouldBe` 10
  where
    twoCores = Limited 2 (1024 * 1024 * 1024)

withCommitted :: (Floors -> Expectation) -> Expectation
withCommitted check = loadFloors >>= either (expectationFailure . toString) check
