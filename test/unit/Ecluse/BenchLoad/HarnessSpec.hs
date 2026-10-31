-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Scenario settings and required successes preserve each driver's load role.
module Ecluse.BenchLoad.HarnessSpec (spec) where

import Data.Aeson (Result (Error, Success), Value (Object), eitherDecode, encode, fromJSON, toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Floors (FloorCheck (AtLeast, NoFloor, Unchecked), Pass (ConcurrencyOne, Loaded))
import Ecluse.BenchLoad.Harness (LoadKnobs (..), LoadSummary (..), ProxyFigures (..), Scenario (..), ScenarioReport (..), defaultLoadKnobs, reportEvidence, scenarioKnobs, windowSuccesses)
import Ecluse.BenchLoad.Pod (CgroupReading (..))
import Ecluse.BenchLoad.Scenarios (findScenario)
import Ecluse.BenchLoad.Support (proxyWithPeak, scenarioReport)
import Ecluse.BenchLoad.Verdict (ProxyEnding (..), RunEvidence (reSuccesses), runViolations)

spec :: Spec
spec = do
    describe "combined burst evidence" $
        for_ ["npm/herd-under-heavy-private", "pypi/herd-under-heavy-private"] $ \name -> do
            let report = combinedReport name
            it (toString name <> " holds only background successes to the floor") $ do
                reSuccesses (reportEvidence report) `shouldBe` [("concurrent load", 20)]
                runViolations (AtLeast 20 34) (reportEvidence report) `shouldBe` []
                runViolations (AtLeast 21 35) (reportEvidence report) `shouldSatisfy` any (T.isInfixOf "below the floor of 21")
                windowSuccesses report `shouldBe` 20
                let admitted = report{srLoad = (srLoad report){lsSuccesses = 3}}
                windowSuccesses admitted `shouldBe` 23
                reSuccesses (reportEvidence admitted) `shouldBe` [("concurrent load", 20)]
            it (toString name <> " keeps required background and missing-floor failures") $ do
                let withoutSuccess = report{srCompanion = (\load -> load{lsSuccesses = 0}) <$> srCompanion report}
                runViolations Unchecked (reportEvidence withoutSuccess) `shouldSatisfy` any (T.isInfixOf "no successful responses (concurrent load)")
                runViolations (NoFloor Loaded) (reportEvidence report) `shouldSatisfy` any (T.isInfixOf "no loaded cell")
            it (toString name <> " retains every process failure with a refused burst") $ do
                for_ [HeapOverflow, StoppedByHarness, ExitedWith 2, KilledBySignal 15] $ \ending ->
                    runViolations Unchecked (reportEvidence report{srProxy = Just (proxyWithPeak 0){pfEnding = ending}}) `shouldSatisfy` (not . null)
                let killed = (proxyWithPeak 0){pfEnding = KernelOomKill, pfCgroup = Just (CgroupReading Nothing Nothing Nothing (Map.singleton "oom_kill" 1) mempty mempty Nothing)}
                runViolations Unchecked (reportEvidence report{srProxy = Just killed}) `shouldSatisfy` any (T.isInfixOf "the kernel OOM-killed the proxy")
                runViolations Unchecked (reportEvidence report{srProxy = Just (proxyWithPeak 0){pfExitedEarly = True}}) `shouldSatisfy` any (T.isInfixOf "exited before the harness stopped it")
    describe "ordinary load evidence" $ do
        it "preserves the diagnostic role through the child report JSON" $ do
            let report = combinedReport "npm/herd-under-heavy-private"
            case eitherDecode (encode report) of
                Left problem -> expectationFailure problem
                Right decoded -> runViolations (AtLeast 20 34) (reportEvidence decoded) `shouldBe` []
        it "defaults historical reports to strict success checks" $
            case toJSON (combinedReport "npm/herd-under-heavy-private") of
                Object fields -> case fromJSON (Object (KeyMap.delete "srDiagnosticBurst" fields)) of
                    Error problem -> expectationFailure problem
                    Success decoded -> runViolations Unchecked (reportEvidence decoded) `shouldSatisfy` any (T.isInfixOf "no successful responses (burst)")
                _ -> expectationFailure "the report did not encode as an object"
        it "keeps a zero-success burst strict without the diagnostic driver role" $ do
            let report = (combinedReport "npm/herd"){srDiagnosticBurst = False}
            runViolations Unchecked (reportEvidence report) `shouldSatisfy` any (T.isInfixOf "no successful responses (burst)")
        it "keeps both loads strict for an ordinary paired scenario" $ do
            let report = (combinedReport "npm/warm-under-cold"){srDiagnosticBurst = False}
            runViolations (AtLeast 21 35) (reportEvidence report) `shouldSatisfy` ((== 2) . length)
        it "does not discard success evidence when the background is missing" $ do
            let report = (combinedReport "npm/herd-under-heavy-private"){srCompanion = Nothing}
            runViolations Unchecked (reportEvidence report) `shouldSatisfy` any (T.isInfixOf "no successful responses (burst)")
    describe "tarball-ceiling settings" $
        case findScenario "npm/tarball-ceiling" of
            Nothing -> it "exists" (expectationFailure "missing npm/tarball-ceiling scenario")
            Just ceilingScenario -> do
                it "uses one service connection and preserves four-times loaded scaling" $ do
                    lkConcurrency (scenarioKnobs ConcurrencyOne knobs ceilingScenario) `shouldBe` 1
                    lkConcurrency (scenarioKnobs Loaded knobs ceilingScenario) `shouldBe` 148
                it "keeps the fixed private delay at a different global latency in both passes" $
                    for_ [Loaded, ConcurrencyOne] $ \whichPass ->
                        scenarioKnobs whichPass knobs ceilingScenario
                            `shouldBe` knobs{lkConcurrency = if whichPass == Loaded then 148 else 1, lkUpstreamLatencyMicros = 2_000}
                it "declares the actual private-hit role unavailable for public attribution" $
                    scenarioPublicLegUnavailableReason ceilingScenario `shouldBe` Just "the private-hit artifact relay has no public leg"
    describe "ordinary scenario settings" $
        for_ ["npm/merge-cold", "pypi/index-cold"] $ \name ->
            case findScenario name of
                Nothing -> it (toString name <> " exists") (expectationFailure ("missing scenario: " <> toString name))
                Just ordinary ->
                    it (toString name <> " keeps every non-concurrency knob and public attribution") $ do
                        scenarioKnobs Loaded knobs ordinary `shouldBe` knobs
                        scenarioKnobs ConcurrencyOne knobs ordinary `shouldBe` knobs{lkConcurrency = 1}
                        scenarioPublicLegUnavailableReason ordinary `shouldBe` Nothing
  where
    knobs =
        defaultLoadKnobs
            { lkConcurrency = 37
            , lkDurationSeconds = 7
            , lkUpstreamLatencyMicros = 17_000
            , lkPayloadBytes = 1024
            , lkCacheMaxEntries = 2
            , lkWorkingSet = 11
            , lkServeMaxInFlight = Just 6
            , lkPublicConnectionsPerHost = Just 3
            , lkPrivateConnectionsPerHost = Just 4
            }

combinedReport :: Text -> ScenarioReport
combinedReport name =
    let report = scenarioReport name 0 100 0 0
     in report
            { srLoad = (srLoad report){lsLabel = "burst"}
            , srCompanion = Just (srLoad (scenarioReport name 20 30 1 2)){lsLabel = "concurrent load"}
            , srDiagnosticBurst = True
            }
