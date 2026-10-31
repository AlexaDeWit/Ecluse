-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The counts a run checks, and the committed floors and sweeps against the scenarios and pod shapes a run measures.
module Ecluse.BenchLoad.ScenariosSpec (spec) where

import Data.Aeson (decodeStrict)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Floors (Calibration (calConditions, calConfirmationReruns, calFloorFraction, calOperatingPoint), Enforcement (Enforced), Floors (floorsByShape, floorsCalibration), OperatingPoint (opUpstreamLatencyMs), Pass (ConcurrencyOne, Loaded), RunFacts (..), RunnerReading (RunnerReading), Trigger (Scheduled), enforce, floorsPath, loadFloors, renderFloorsFile, unheldViolations)
import Ecluse.BenchLoad.Harness (Scenario (scenarioName), UpstreamFixture (fixtureEcosystem, fixtureScenarios), defaultLoadKnobs, operatingPoint)
import Ecluse.BenchLoad.Pod (PodShape (Unlimited), renderPodShape, scheduledPodShapes)
import Ecluse.BenchLoad.Scenarios (checkFixtureCoverage, checkedCounts, findScenario, fixtures, scenariosForSelection)
import Ecluse.BenchLoad.Selection (measurementConditionVersion, scenarioKey)
import Ecluse.BenchLoad.Support (twoCores)
import Ecluse.BenchLoad.Sweep (Sweep (swScenarios), loadSweep)

-- | Check native fixtures, floor coverage and default selection without starting load.
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
    describe "MAL qualification selection" $
        for_ ["npm/merge-cold-mal-hit", "npm/merge-cold-mal-mixed-heavy", "pypi/index-cold-mal-hit", "pypi/index-cold-mal-mixed-heavy", "rubygems/info-cold-mal-hit", "rubygems/info-cold-mal-mixed-heavy"] $ \name ->
            it ("requires explicit selection for " <> toString name) $ do
                isJust (findScenario name) `shouldBe` True
                checkedCounts twoCores `shouldSatisfy` Set.notMember (name, Loaded)

    describe "optional load cases" $ do
        for_ ["heavy-private-5pct", "pattern-restart"] $ \name ->
            it ("keeps " <> toString name <> " available only through explicit selection") $
                for_ fixtures $ \fixture -> do
                    let key = scenarioKey (fixtureEcosystem fixture) name
                    isJust (findScenario key) `shouldBe` True
                    map scenarioName (scenariosForSelection Nothing fixture) `shouldSatisfy` notElem name
                    map scenarioName (scenariosForSelection (Just [key]) fixture) `shouldSatisfy` elem name
        it "removes only the approved pass observations from the full grid" $
            sum (map (Set.size . Set.filter (not . T.isPrefixOf "rubygems/" . fst) . checkedCounts) scheduledPodShapes) `shouldBe` 486
        it "adds every native specification, read and worker pass" $
            sum (map (Set.size . checkedCounts) scheduledPodShapes) `shouldBe` 648
        it "keeps the focused directory diagnostic outside all full profiles" $ do
            isJust (findScenario "rubygems/versions-directory") `shouldBe` True
            for_ scheduledPodShapes $ \shape ->
                checkedCounts shape `shouldSatisfy` Set.notMember ("rubygems/versions-directory", Loaded)

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
        it "were calibrated at 20 ms of injected latency, under the harness's measurement conditions, at 60% of the median, with one confirmation re-run" $
            withCommitted $ \floors -> do
                opUpstreamLatencyMs (calOperatingPoint (floorsCalibration floors)) `shouldBe` 20
                calFloorFraction (floorsCalibration floors) `shouldBe` 0.6
                calConditions (floorsCalibration floors) `shouldBe` measurementConditionVersion
                calConfirmationReruns (floorsCalibration floors) `shouldBe` 1
        it "hold a default run under every scheduled shape" $
            withCommitted $ \floors ->
                for_ scheduledPodShapes $ \shape -> do
                    let facts =
                            RunFacts
                                { rfSettings = operatingPoint defaultLoadKnobs Nothing []
                                , rfRunner = RunnerReading (Just "true") (Just "Linux") (Just "ARM64")
                                , rfShape = shape
                                , rfConditions = measurementConditionVersion
                                }
                        enforcement = enforce floors facts
                    enforcement `shouldBe` Enforced shape 1 (Map.findWithDefault Map.empty shape (floorsByShape floors))
                    unheldViolations Scheduled enforcement `shouldBe` []
        it "are in the layout renderFloorsFile writes" $
            withCommitted $ \floors -> do
                committed <- decodeUtf8 <$> readFileBS floorsPath
                renderFloorsFile floors `shouldBe` committed
        it "key the shapes the workflow's scheduled matrix names, and so its baseline artifacts" $ do
            workflow <- decodeUtf8 <$> readFileBS ".github/workflows/bench-load.yml"
            scheduledMatrix workflow `shouldBe` Just (map renderPodShape scheduledPodShapes)

    describe "the committed capability sweep" $
        it "names only scenarios the harness defines" $
            loadSweep "capability-1634" >>= \case
                Left problem -> expectationFailure (toString problem)
                Right sweep -> filter (isNothing . findScenario) (toList (swScenarios sweep)) `shouldBe` []

-- The shapes the workflow's matrix lists when a run names none: the first quoted JSON list on its matrix line.
scheduledMatrix :: Text -> Maybe [Text]
scheduledMatrix workflow = listToMaybe $ do
    line <- lines workflow
    guard ("pod-shape: ${{ fromJSON(" `T.isInfixOf` line)
    listed <- maybeToList (T.stripPrefix "'" (snd (T.breakOn "'[" line)))
    maybeToList (decodeStrict (encodeUtf8 (T.takeWhile (/= '\'') listed)))

withCommitted :: (Floors -> Expectation) -> Expectation
withCommitted check = loadFloors >>= either (expectationFailure . toString) check
