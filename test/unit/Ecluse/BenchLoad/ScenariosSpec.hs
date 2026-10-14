-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The counts a run checks, and the committed floors against the scenarios and pod shapes a scheduled run measures.
module Ecluse.BenchLoad.ScenariosSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec

import Ecluse.BenchLoad.Floors (Calibration (calNpmLatency, calOperatingPoint, calPypiLatency, calRuns), Enforcement (Enforced), Floors (floorsByShape, floorsCalibration), LatencyCeiling (lcCeilingMs), Pass (ConcurrencyOne, Loaded), RunFacts (..), RunnerReading (RunnerReading), Trigger (Scheduled), enforce, loadFloors, unheldViolations)
import Ecluse.BenchLoad.Harness (UpstreamFixture (fixtureScenarios), defaultLoadKnobs, operatingPoint)
import Ecluse.BenchLoad.Pod (PodShape (Unlimited), scheduledPodShapes)
import Ecluse.BenchLoad.Scenarios (checkFixtureCoverage, checkedCounts, fixtures)
import Ecluse.BenchLoad.Selection (measurementConditionVersion)
import Ecluse.BenchLoad.Support (twoCores)

spec :: Spec
spec = do
    describe "load provider coverage" $ do
        it "covers every supported ecosystem" $
            checkFixtureCoverage fixtures `shouldBe` Right ()
        it "rejects an omitted provider" $
            checkFixtureCoverage (drop 1 fixtures) `shouldSatisfy` isLeft
        it "rejects a duplicate provider" $
            checkFixtureCoverage (fixtures <> fixtures) `shouldSatisfy` isLeft
        it "rejects a provider without scenarios" $
            checkFixtureCoverage (map (\fixture -> fixture{fixtureScenarios = []}) fixtures) `shouldSatisfy` isLeft
        it "rejects duplicate scenario identities" $
            checkFixtureCoverage (map (\fixture -> fixture{fixtureScenarios = fixtureScenarios fixture <> fixtureScenarios fixture}) fixtures) `shouldSatisfy` isLeft

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
        it "were calibrated at the harness's default knobs, over every scenario, with no pattern override" $
            withCommitted $ \floors ->
                calOperatingPoint (floorsCalibration floors) `shouldBe` operatingPoint defaultLoadKnobs Nothing []
        it "holds actual default runs at both native ceilings under every scheduled shape" $
            withCommitted $ \floors ->
                case calPypiLatency (floorsCalibration floors) of
                    Nothing -> expectationFailure "the committed calibration has no native PyPI ceiling"
                    Just pypiBound ->
                        for_ scheduledPodShapes $ \shape -> do
                            let facts =
                                    RunFacts
                                        { rfSettings = operatingPoint defaultLoadKnobs Nothing []
                                        , rfRunner = RunnerReading (Just "true") (Just "Linux") (Just "ARM64")
                                        , rfShape = shape
                                        , rfNpmLatencyMs = lcCeilingMs (calNpmLatency (floorsCalibration floors))
                                        , rfPypiLatencyMs = lcCeilingMs pypiBound
                                        , rfConditions = measurementConditionVersion
                                        }
                                enforcement = enforce floors facts
                            enforcement `shouldBe` Enforced shape (Map.findWithDefault Map.empty shape (floorsByShape floors))
                            unheldViolations Scheduled enforcement `shouldBe` []
        it "record at least the ten runs they were calibrated from" $
            withCommitted $ \floors ->
                length (calRuns (floorsCalibration floors)) `shouldSatisfy` (>= 10)

withCommitted :: (Floors -> Expectation) -> Expectation
withCommitted check = loadFloors >>= either (expectationFailure . toString) check
