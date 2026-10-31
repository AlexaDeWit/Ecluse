-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Set a sweep job's arms beside its anchors, from reports the harness renders, as Markdown and as JSON.
module Ecluse.BenchReport.SweepSpec (spec) where

import Data.Aeson (Value (Array, Bool, Null, Object, String), parseJSON, toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Floors (Cell (Floored), PassSelection (LoadedPassOnly))
import Ecluse.BenchLoad.Harness (LoadKnobs (lkDurationSeconds), ProxyFigures (..), ScenarioReport (..))
import Ecluse.BenchLoad.Pod (CgroupReading (CgroupReading), PodShape (MemoryOnly), ProxyTuning (..), noTuning, parseCpuList)
import Ecluse.BenchLoad.Report (Section (sectionKnobs), renderReports)
import Ecluse.BenchLoad.RtsWindow (RtsSnapshot (..))
import Ecluse.BenchLoad.Selection (fixtureSection)
import Ecluse.BenchLoad.Support (floorsAtTwoCores, loadedSection, placedWindow, proxyWithPeak, scenarioReport, twoCores)
import Ecluse.BenchLoad.Sweep (Arm (..), BootValues (..), Phase (..), Sweep (..), SweepJob (..))
import Ecluse.BenchLoad.Verdict (ProxyEnding (HeapOverflow))
import Ecluse.BenchReport.Sweep (ArmResult (..), Failure (MemoryHighEvents), JobComparison (..), ReferenceFloor (NotApplicable), ReferenceFloors (ReferenceFloors, rfUpstreamLatencyMs), ScenarioResult (scrFailures, scrFloor), SlotResult (..), compareJob, renderComparison, slotVoid)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))

spec :: Spec
spec = do
    let rendered = T.lines (renderComparison (compareJob commit reference sweep job [anchorOf 1000 4000, Right (armReport [arm "npm/merge-cold" 1100, arm "pypi/index-cold" 3600]), anchorOf 1050 4000]))
        rows reports = T.lines (renderComparison (compareJob commit reference sweep job reports))
        summary reports = T.concat (filter ("| 2 |" `T.isPrefixOf`) (rows reports))
    describe "renderComparison" $ do
        it "sets each scenario's successes beside the mean of the job's anchors and the reference floor" $ do
            rendered `shouldSatisfy` elem "| npm/merge-cold | 1100 | 1025.0 | 1.073 | 338 | 100.00 ms | 200.00 ms | 300.0 MiB | clean shutdown |"
            rendered `shouldSatisfy` elem "| pypi/index-cold | 3600 | 4000.0 | 0.900 | 964 | 100.00 ms | 200.00 ms | 300.0 MiB | clean shutdown |"
        it "scores an arm by the geometric mean of its scored ratios, and shows each slot's boot values and safety" $ do
            rendered `shouldSatisfy` elem "| 2 | `test` | as expected | clean | held | 0.983 |"
            rendered `shouldSatisfy` elem "| 1 | `anchor` | as expected | clean | held | anchor |"
        it "shows how far the closing anchor drifted from the opening one, and names the anchor slots" $ do
            rendered `shouldSatisfy` elem "| npm/merge-cold | 1000 | 1050 | +5.0% |"
            rendered `shouldSatisfy` any (T.isInfixOf "the anchor's successes in slots 1 and 3, scenario by scenario")
        it "leaves an arm unscored, naming the scenario, when a scored scenario has no anchor ratio" $
            summary [anchorOf 0 4000, Right (armReport [arm "npm/merge-cold" 1100, arm "pypi/index-cold" 3600]), anchorOf 0 4000]
                `shouldSatisfy` T.isSuffixOf "| n/a (no anchor ratio for npm/merge-cold) |"
        it "voids an arm whose proxy reported another posture than the arm must show" $
            summary [anchorOf 1000 4000, Right (armReport [(arm "npm/merge-cold" 1100){srRtsEnd = Just (posture 4)}]), anchorOf 1000 4000]
                `shouldBe` "| 2 | `test` | **void**: capabilities 4, expected 2 | clean | held | 1.100 |"
        it "voids an arm on an unclean ending, a memory.high event, a peak above its pod's memory, and a count below its floor" $ do
            let unsafe = (arm "pypi/index-cold" 900){srProxy = Just (figures 1100 3){pfEnding = HeapOverflow}}
            for_ ["ending heap overflow", "memory.high events 3", "peak 1100.0 MiB above the pod's 1024 MiB", "900 successes below the floor of 964"] $ \problem ->
                summary [anchorOf 1000 4000, Right (armReport [unsafe]), anchorOf 1000 4000] `shouldSatisfy` T.isInfixOf problem
        it "names each scenario whose load generator lost its headroom" $
            summary [anchorOf 1000 4000, Right (armReport [(arm "npm/merge-cold" 1100){srProxy = Just (figures 300 0){pfCpu = Just (placedWindow 1_800)}}]), anchorOf 1000 4000]
                `shouldSatisfy` T.isInfixOf "**not held** in npm/merge-cold (saturated)"
        it "names the commit and the window, and holds no count to a floor calibrated at another window" $ do
            let short = rows [Right (shortReport [arm "pypi/index-cold" 4000]), Right (shortReport [arm "pypi/index-cold" 900]), Right (shortReport [arm "pypi/index-cold" 4000])]
            short `shouldSatisfy` elem "Commit `abc123`, 100 connections x 5 s windows."
            short `shouldSatisfy` elem "| 2 | `test` | as expected | clean | held | 0.225 |"
            short `shouldSatisfy` elem "| pypi/index-cold | 900 | 4000.0 | 0.225 | 964, not applicable to this window or latency | 100.00 ms | 200.00 ms | 300.0 MiB | clean shutdown |"
        it "holds no count to a floor calibrated at another injected latency" $ do
            let elsewhere = T.lines (renderComparison (compareJob commit reference{rfUpstreamLatencyMs = 20} sweep job [anchorOf 1000 4000, Right (armReport [arm "pypi/index-cold" 900]), anchorOf 1000 4000]))
            elsewhere `shouldSatisfy` elem "| pypi/index-cold | 900 | 4000.0 | 0.225 | 964, not applicable to this window or latency | 100.00 ms | 200.00 ms | 300.0 MiB | clean shutdown |"
            elsewhere `shouldSatisfy` elem "| 2 | `test` | as expected | clean | held | 0.225 |"
        it "voids an arm whose report could not be read, and still compares the rest" $ do
            let missing = rows [anchorOf 1000 4000, Left "could not read slot-2.md", anchorOf 1000 4000]
            missing `shouldSatisfy` any (T.isInfixOf "| 2 | `test` | n/a | **void**: the report could not be read. could not read slot-2.md")
            missing `shouldSatisfy` elem "| npm/merge-cold | 1000 | 1000 | 0.0% |"

    describe "compareJob" $ do
        it "keeps each failure as a fact, and voids only the slot that failed" $ do
            let compared = compareJob commit reference sweep job [anchorOf 1000 4000, Right (armReport [(arm "npm/merge-cold" 1100){srProxy = Just (figures 300 2)}]), anchorOf 1000 4000]
            map slotVoid (cmSlots compared) `shouldBe` [False, True, False]
            let short = compareJob commit reference sweep job [Right (shortReport [arm "pypi/index-cold" 900])]
            map scrFloor (concatMap arScenarios (rights (map slResult (cmSlots short)))) `shouldBe` [NotApplicable 964]
            concatMap (concatMap scrFailures . arScenarios) (rights (map slResult (cmSlots compared))) `shouldBe` [MemoryHighEvents 2]
        it "encodes every slot, scenario, failure, and the drift for analysis across jobs" $ do
            let encoded = toJSON (compareJob commit reference sweep job [anchorOf 1000 4000, Right (armReport [(arm "npm/merge-cold" 1100){srProxy = Just (figures 300 2)}]), anchorOf 1050 4000])
            field "job" encoded `shouldBe` Just (String "J1")
            field "commit" encoded `shouldBe` Just (String "abc123")
            (number <=< field "concurrency") encoded `shouldBe` Just 100
            (number <=< field "durationSeconds") encoded `shouldBe` Just 30
            (field "unread" <=< element 1 <=< field "slots") encoded `shouldBe` Just Null
            (field "unscoredScenarios" <=< element 1 <=< field "slots") encoded `shouldBe` Just (Array mempty)
            (number <=< field "successes" <=< element 0 <=< field "scenarios" <=< element 1 <=< field "slots") encoded `shouldBe` Just 1100
            (number <=< field "anchorMean" <=< element 0 <=< field "scenarios" <=< element 1 <=< field "slots") encoded `shouldBe` Just 1025
            (number <=< field "ratio" <=< element 0 <=< field "scenarios" <=< element 1 <=< field "slots") encoded `shouldSatisfy` maybe False (\ratio -> abs (ratio - 1100 / 1025) < 1e-9)
            (number <=< field "floor" <=< element 0 <=< field "scenarios" <=< element 1 <=< field "slots") encoded `shouldBe` Just 338
            (field "floorApplies" <=< element 0 <=< field "scenarios" <=< element 1 <=< field "slots") encoded `shouldBe` Just (Bool True)
            (field "void" <=< element 1 <=< field "slots") encoded `shouldBe` Just (Bool True)
            (number <=< field "score" <=< element 1 <=< field "slots") encoded `shouldSatisfy` maybe False (\score -> abs (score - 1100 / 1025) < 1e-9)
            (field "kind" <=< element 0 <=< field "failures" <=< element 0 <=< field "scenarios" <=< element 1 <=< field "slots") encoded `shouldBe` Just (String "memoryHighEvents")
            (number <=< field "count" <=< element 0 <=< field "failures" <=< element 0 <=< field "scenarios" <=< element 1 <=< field "slots") encoded `shouldBe` Just 2
            (number <=< field "changePercent" <=< element 0 <=< field "scenarios" <=< field "drift") encoded `shouldSatisfy` maybe False (\change -> abs (change - 5) < 1e-9)
            (field "score" <=< element 0 <=< field "slots") encoded `shouldBe` Just Null
        it "encodes why a slot has no result, and the scored scenarios that left an arm unscored" $ do
            let unread = toJSON (compareJob commit reference sweep job [anchorOf 1000 4000, Left "could not read slot-2.md", anchorOf 1000 4000])
                unscored = toJSON (compareJob commit reference sweep job [anchorOf 0 4000, Right (armReport [arm "npm/merge-cold" 1100]), anchorOf 0 4000])
            (field "unread" <=< element 1 <=< field "slots") unread `shouldBe` Just (String "the report could not be read. could not read slot-2.md")
            (field "unscoredScenarios" <=< element 1 <=< field "slots") unscored `shouldBe` Just (toJSON ["npm/merge-cold" :: Text])

field :: Text -> Value -> Maybe Value
field name = \case
    Object entries -> KeyMap.lookup (Key.fromText name) entries
    _ -> Nothing

number :: Value -> Maybe Double
number = parseMaybe parseJSON

element :: Int -> Value -> Maybe Value
element index = \case
    Array values -> toList values !!? index
    _ -> Nothing

-- The floors at their calibrated 100 connections x 30 s and 5 ms, the latency the arm reports inject, and the commit the job names.
reference :: ReferenceFloors
reference = ReferenceFloors 100 30 5 (Map.singleton twoCores (Map.mapMaybe floorOf floorsAtTwoCores))
  where
    floorOf = \case
        Floored least _ _ -> Just least
        _ -> Nothing

commit :: Maybe Text
commit = Just "abc123"

job :: SweepJob
job = SweepJob "J1" ("anchor" :| ["test", "anchor"])

-- A two-arm sweep: the anchor and a test arm, each expected to show two capabilities and 40 requests of admission.
sweep :: Sweep
sweep =
    Sweep
        { swName = "test"
        , swUpstreamLatencyMs = 5
        , swPasses = LoadedPassOnly
        , swScenarios = "npm/merge-cold" :| ["pypi/index-cold"]
        , swScored = ["npm/merge-cold", "pypi/index-cold"]
        , swAnchor = "anchor"
        , swArms = Map.fromList [("anchor", sweepArm), ("test", sweepArm)]
        , swPhases = Phase "A" False (job :| []) :| []
        }
  where
    sweepArm = Arm (MemoryOnly (1024 * 1024 * 1024)) noTuning{ptCores = Just 2, ptCpus = rightToMaybe (parseCpuList "2-3")} Nothing (1024 * 1024 * 1024) twoCores (BootValues 2 64 (Just 832) 40)

anchorOf :: Int -> Int -> Either Text Text
anchorOf npm pypi = Right (armReport [arm "npm/merge-cold" npm, arm "pypi/index-cold" pypi])

-- One arm's report, both ecosystems in one section, as the harness renders a loaded pass.
armReport :: [ScenarioReport] -> Text
armReport scenarios = fixtureSection Npm [renderReports (loadedSection Npm 5_000 scenarios)]

-- The same report from a short check's 5-second windows.
shortReport :: [ScenarioReport] -> Text
shortReport scenarios = fixtureSection Npm [renderReports section{sectionKnobs = (sectionKnobs section){lkDurationSeconds = 5}}]
  where
    section = loadedSection Npm 5_000 scenarios

-- A scenario whose proxy showed the anchor's posture, a 300 MiB peak, and no memory.high event.
arm :: Text -> Int -> ScenarioReport
arm name successes = (scenarioReport name successes 0 100 200){srRtsEnd = Just (posture 2), srProxy = Just (figures 300 0)}

-- A proxy with this peak in MiB and this many memory.high events, admitting 40 requests and keeping its headroom.
figures :: Int -> Int -> ProxyFigures
figures peakMib highEvents =
    (proxyWithPeak (peakMib * 1024 * 1024))
        { pfCgroup = Just (CgroupReading (Just (1024 * 1024 * 1024)) (Just (peakMib * 1024 * 1024)) Nothing (Map.fromList [("high", highEvents)]) mempty mempty Nothing)
        , pfBootLines = ["runtime: serve admission 40 (computed from 2 capabilities)"]
        , pfCpu = Just (placedWindow 1_000)
        }

posture :: Int -> RtsSnapshot
posture capabilities =
    RtsSnapshot
        { rsAllocatedBytes = 0
        , rsGcs = 0
        , rsMajorGcs = 0
        , rsGcCpuNs = 0
        , rsCpuNs = 0
        , rsGcElapsedNs = 0
        , rsMaxLiveBytes = 0
        , rsMaxMemInUseBytes = 0
        , rsMaxLargeObjectsBytes = 0
        , rsCumulativeLiveBytes = 0
        , rsLiveBytes = 0
        , rsMemInUseBytes = 0
        , rsCapabilities = capabilities
        , rsProcessors = 2
        , rsMaxHeapBytes = Just (832 * 1024 * 1024)
        , rsAllocAreaBytes = 64 * 1024 * 1024
        , rsCompactAlways = False
        , rsCompactThresholdPercent = 30
        }
