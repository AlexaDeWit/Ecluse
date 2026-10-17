-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Compare rendered load reports with main without allocation or memory figures, and sweep arms with their anchors.
module Ecluse.BenchReport.AgainstMain.LoadSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.CpuUse (CpuTicks (CpuTicks), CpuWindow (..))
import Ecluse.BenchLoad.Exposition (summariseGauge)
import Ecluse.BenchLoad.Floors (PassSelection (LoadedPassOnly))
import Ecluse.BenchLoad.Harness (LoadKnobs (lkUpstreamLatencyMicros), ProxyFigures (..), ScenarioReport (..), defaultLoadKnobs)
import Ecluse.BenchLoad.Pod (CgroupReading (..), PodShape (MemoryOnly), ProxyTuning (..), noTuning, parseCpuList)
import Ecluse.BenchLoad.Report (Section (..), renderLoadSaturation, renderReports, renderThrash)
import Ecluse.BenchLoad.RtsWindow (RtsSnapshot (..), RtsWindow (RtsWindow))
import Ecluse.BenchLoad.Selection (fixtureSection)
import Ecluse.BenchLoad.Support (floorsAtTwoCores, scenarioReport, slowNetwork, twoCores)
import Ecluse.BenchLoad.Sweep (Arm (..), BootValues (..), Phase (..), Sweep (..), SweepJob (..))
import Ecluse.BenchLoad.Verdict (ProxyEnding (CleanShutdown, HeapOverflow))
import Ecluse.BenchReport.AgainstMain (Baseline (Baseline, NoBaseline), Missing (DownloadFailed), Origin (Origin), sharedBaselineNote)
import Ecluse.BenchReport.AgainstMain.Load (againstAnchors, againstMain)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))

spec :: Spec
spec = do
    anchorSpec
    describe "againstMain, over the reports the harness renders" $ do
        let rendered = T.lines (againstMain (Baseline origin (report 178 onMainNpm onMainPyPI)) (Right (report 12 hereNpm herePyPI)))
        it "leads with how the whole job moved, which one runner measured" $
            rendered `shouldSatisfy` elem "**Whole job.** Scenarios compared: 4. Successes rose in 2, fell in 1, and held in 1. The median change is +5.0%, and the changes run from -5.0% to +50.0%."
        it "gives each ecosystem its own section" $
            filter (T.isPrefixOf "### ") rendered `shouldBe` ["### npm", "### pypi", "### Reading the comparison"]
        it "says plainly when no scenario of an ecosystem moved against the others" $
            rendered `shouldSatisfy` elem "Scenarios compared: 3. Successes rose in 2, fell in 0, and held in 1. The median change is +10.0%, and the changes run from 0.0% to +50.0%. None moved the other way. One runner measured them all, so the runner can be the cause."
        it "sets each scenario's successes, refusals, and latency beside those on main" $ do
            rendered `shouldSatisfy` elem "| npm/merge-cold | 1000 | 1100 | +10.0% | 300 | 250 | 1000.00 ms | 900.00 ms | -10.0% | 2000.00 ms | 2100.00 ms | +5.0% | in harness |"
            rendered `shouldSatisfy` elem "| pypi/index-cold | 4000 | 3800 | -5.0% | 0 | 0 | 500.00 ms | 550.00 ms | +10.0% | 1000.00 ms | 1000.00 ms | 0.0% | in harness |"
        it "keeps the scenarios the change does not touch in the same table" $
            rendered `shouldSatisfy` elem "| npm/tarball-hot-path | 16000 | 24000 | +50.0% | 0 | 0 | 180.00 ms | 120.00 ms | -33.3% | 200.00 ms | 150.00 ms | -25.0% | in harness |"
        it "reads a ramp's count past the note the report prints after it" $
            rendered `shouldSatisfy` elem "| npm/ramp | 800 | 800 | 0.0% | 50 | 50 | 1400.00 ms | 1400.00 ms | 0.0% | 2400.00 ms | 2400.00 ms | 0.0% | in harness |"
        it "shows the operating point rows that differ, for the ecosystem they belong to" $ do
            rendered `shouldSatisfy` elem "| injected upstream latency | 178.0 ms | 12.0 ms |"
            filter (== "Operating point rows that differ:") rendered `shouldBe` ["Operating point rows that differ:"]
            filter (T.isPrefixOf "| pod shape |") rendered `shouldBe` []

        it "says that every run shares this one baseline" $
            rendered `shouldSatisfy` elem sharedBaselineNote
        it "compares no allocation: a proxy that allocated leaves the section as it was" $ do
            let allocating = map (\r -> r{srRtsWindow = Just (RtsWindow 9_000_000_000 40 4 2_000_000 90_000_000 3_000_000 50_000_000)})
            report 12 (allocating hereNpm) (allocating herePyPI) `shouldNotBe` report 12 hereNpm herePyPI
            T.lines (againstMain (Baseline origin (report 178 onMainNpm onMainPyPI)) (Right (report 12 (allocating hereNpm) (allocating herePyPI))))
                `shouldBe` rendered

        it "compares no memory: proxy memory changes leave the comparison unchanged" $ do
            let withMemory peak = map (\r -> r{srProxy = Just (proxyMemory peak)})
                memoryReport peak = report 12 (withMemory peak hereNpm) (withMemory peak herePyPI)
                baseline = Baseline origin (memoryReport (64 * 1024 * 1024))
            memoryReport (128 * 1024 * 1024) `shouldSatisfy` T.isInfixOf "128.0 MiB / 1024.0 MiB"
            memoryReport (128 * 1024 * 1024) `shouldNotBe` memoryReport (64 * 1024 * 1024)
            againstMain baseline (Right (memoryReport (128 * 1024 * 1024)))
                `shouldBe` againstMain baseline (Right (memoryReport (64 * 1024 * 1024)))

    describe "againstMain, over a report cut down to its tables" $ do
        let glance rows = T.unlines ("| scenario | successes | refusals | success p50 | success p99 | ending |" : "| --- | --: | --: | --: | --: | --- |" : rows)
            compared onMain here = T.lines (againstMain (Baseline origin (glance onMain)) (Right (glance here)))
        it "shows the ending on main beside this run's when the two differ" $
            compared ["| [npm/herd](#npm/herd) | 30 | 70 | 1.00 ms | 2.00 ms | clean shutdown |"] ["| [npm/herd](#npm/herd) | 30 | 70 | 1.00 ms | 2.00 ms | **heap overflow** |"]
                `shouldSatisfy` elem "| npm/herd | 30 | 30 | 0.0% | 70 | 70 | 1.00 ms | 1.00 ms | 0.0% | 2.00 ms | 2.00 ms | 0.0% | **heap overflow** (on main: clean shutdown) |"
        it "shows no change for a figure one side lacks, or for no success on main" $ do
            compared ["| npm/herd | 0 | 70 | n/a | n/a | clean shutdown |"] ["| npm/herd | 30 | 70 | 1.00 ms | 2.00 ms | clean shutdown |"]
                `shouldSatisfy` elem "| npm/herd | 0 | 30 | n/a | 70 | 70 | n/a | 1.00 ms | n/a | n/a | 2.00 ms | n/a | clean shutdown |"
        it "names the scenarios only one side ran, and compares none of them" $ do
            let rendered = compared ["| npm/herd | 30 | 70 | 1.00 ms | 2.00 ms | clean shutdown |", "| npm/gone | 1 | 0 | 1.00 ms | 2.00 ms | clean shutdown |"] ["| npm/herd | 30 | 70 | 1.00 ms | 2.00 ms | clean shutdown |", "| pypi/new | 1 | 0 | 1.00 ms | 2.00 ms | clean shutdown |"]
            rendered `shouldSatisfy` elem "Only in this run: `pypi/new`."
            rendered `shouldSatisfy` elem "Only on `main`: `npm/gone`."
            filter (T.isPrefixOf "### ") rendered `shouldBe` ["### npm", "### Reading the comparison"]
        it "shows a knob that only one side printed" $ do
            let knobs rows = T.unlines ("| knob | value |" : "| --- | --- |" : rows) <> "\n" <> glance ["| npm/herd | 30 | 70 | 1.00 ms | 2.00 ms | clean shutdown |"]
                rendered = T.lines (againstMain (Baseline origin (knobs ["| load | 100 connections x 30 s |", "| old knob | 1 |"])) (Right (knobs ["| load | 100 connections x 60 s |", "| new knob | 2 |"])))
            rendered `shouldSatisfy` elem "| load | 100 connections x 30 s | 100 connections x 60 s |"
            rendered `shouldSatisfy` elem "| new knob | not printed | 2 |"
            rendered `shouldSatisfy` elem "| old knob | 1 | not printed |"

    describe "a missing side" $ do
        it "prints no baseline, with the reason, and compares nothing" $ do
            let rendered = againstMain (NoBaseline DownloadFailed) (Right (report 12 hereNpm herePyPI))
            rendered `shouldSatisfy` T.isInfixOf "**No baseline.** The fetch could not download the artifact of the run on `main`."
            rendered `shouldNotSatisfy` T.isInfixOf "###"
        it "prints no baseline for a report on main without a scenario table" $
            againstMain (Baseline origin "the harness crashed") (Right (report 12 hereNpm herePyPI))
                `shouldSatisfy` T.isInfixOf "could not be read. It holds no scenario table."
        it "has nothing to compare in the report of the GC-thrash probe" $
            againstMain (Baseline origin (report 178 onMainNpm onMainPyPI)) (Right (renderThrash "npm/heavy-private" [("2cpu-1024mib", Left "the scenario process exited 1")]))
                `shouldSatisfy` T.isInfixOf "**Nothing to compare.** This run's report could not be read. It holds no scenario table."

anchorSpec :: Spec
anchorSpec = describe "againstAnchors, over the reports the harness renders" $ do
    let compared reports = T.lines (againstAnchors (Map.singleton twoCores floorsAtTwoCores) sweep (SweepJob "J1" ("anchor" :| ["test", "anchor"])) reports)
        anchorOf npm pypi = Right (armReport [arm "npm/merge-cold" npm, arm "pypi/index-cold" pypi])
        rendered = compared [anchorOf 1000 4000, Right (armReport [arm "npm/merge-cold" 1100, arm "pypi/index-cold" 3600]), anchorOf 1050 4000]
    it "sets each scenario's successes beside the mean of the job's anchors and the reference floor" $ do
        rendered `shouldSatisfy` elem "| npm/merge-cold | 1100 | 1025.0 | 1.073 | 338 | 100.00 ms | 200.00 ms | 300.0 MiB | clean shutdown |"
        rendered `shouldSatisfy` elem "| pypi/index-cold | 3600 | 4000.0 | 0.900 | 964 | 100.00 ms | 200.00 ms | 300.0 MiB | clean shutdown |"
    it "scores an arm by the geometric mean of its scored ratios, and shows each slot's boot values and safety" $ do
        rendered `shouldSatisfy` elem "| 2 | `test` | as expected | clean | held | 0.983 |"
        rendered `shouldSatisfy` elem "| 1 | `anchor` | as expected | clean | held | anchor |"
    it "shows how far the closing anchor drifted from the opening one, and names the anchor slots" $ do
        rendered `shouldSatisfy` elem "| npm/merge-cold | 1000 | 1050 | +5.0% |"
        rendered `shouldSatisfy` any (T.isInfixOf "the anchor's successes in slots 1 and 3, scenario by scenario")
    it "voids an arm whose proxy reported another posture than the arm must show" $
        compared [anchorOf 1000 4000, Right (armReport [(arm "npm/merge-cold" 1100){srRtsEnd = Just (posture 4)}]), anchorOf 1000 4000]
            `shouldSatisfy` elem "| 2 | `test` | **void**: capabilities 4, expected 2 | clean | held | 1.100 |"
    it "voids an arm on an unclean ending, a memory.high event, a peak above its pod's memory, and a count below its floor" $ do
        let unsafe = (arm "pypi/index-cold" 900){srProxy = Just (figures 1100 3){pfEnding = HeapOverflow}}
            summary = T.concat (filter ("| 2 |" `T.isPrefixOf`) (compared [anchorOf 1000 4000, Right (armReport [unsafe]), anchorOf 1000 4000]))
        for_ ["ending heap overflow", "memory.high events 3", "peak 1100.0 MiB above the pod's 1024 MiB", "900 successes below the floor of 964"] $ \problem ->
            summary `shouldSatisfy` T.isInfixOf problem
    it "names each scenario whose load generator lost its headroom" $
        compared [anchorOf 1000 4000, Right (armReport [(arm "npm/merge-cold" 1100){srProxy = Just (figures 300 0){pfCpu = Just (cpu 1_800)}}]), anchorOf 1000 4000]
            `shouldSatisfy` any (T.isInfixOf "**not held** in npm/merge-cold (saturated)")
    it "voids an arm whose report could not be read, and still compares the rest" $ do
        let missing = compared [anchorOf 1000 4000, Left "could not read slot-2.md", anchorOf 1000 4000]
        missing `shouldSatisfy` any (T.isInfixOf "| 2 | `test` | n/a | **void**: the report could not be read. could not read slot-2.md")
        missing `shouldSatisfy` elem "| npm/merge-cold | 1000 | 1000 | 0.0% |"

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
        , swPhases = Phase "A" False (SweepJob "J1" ("anchor" :| ["test", "anchor"]) :| []) :| []
        }
  where
    sweepArm = Arm (MemoryOnly (1024 * 1024 * 1024)) noTuning{ptCores = Just 2, ptCpus = rightToMaybe (parseCpuList "2-3")} Nothing (1024 * 1024 * 1024) twoCores (BootValues 2 64 (Just 832) 40)

-- One arm's report, both ecosystems in one section, as the harness renders a loaded pass.
armReport :: [ScenarioReport] -> Text
armReport scenarios = fixtureSection Npm [renderReports (section Npm 5_000 scenarios)]

-- A scenario whose proxy showed the anchor's posture, a 300 MiB peak, and no memory.high event.
arm :: Text -> Int -> ScenarioReport
arm name successes = (scenarioReport name successes 0 100 200){srRtsEnd = Just (posture 2), srProxy = Just (figures 300 0)}

figures :: Int -> Int -> ProxyFigures
figures peakMib highEvents =
    (proxyMemory (peakMib * 1024 * 1024))
        { pfCgroup = Just (CgroupReading (Just (1024 * 1024 * 1024)) (Just (peakMib * 1024 * 1024)) Nothing (Map.fromList [("high", highEvents)]) mempty mempty)
        , pfBootLines = ["runtime: serve admission 40 (computed from 2 capabilities)"]
        , pfCpu = Just (cpu 1_000)
        }

-- A window in which the harness cgroup used half its CPUs and CPU 0 spent this many of 2000 ticks busy.
cpu :: Int -> CpuWindow
cpu busy = CpuWindow 30 (rightToMaybe (parseCpuList "2-3")) (Just 45) (rightToMaybe (parseCpuList "0-1")) (Just 30) (Map.fromList [(0, CpuTicks busy 2_000)]) 0.01

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

origin :: Origin
origin = Origin "abc123" "https://example.test/runs/7" "2026-01-02T03:04:05Z"

-- A report as the driver joins it: each ecosystem's section, the npm one under the given injected latency in milliseconds.
report :: Int -> [ScenarioReport] -> [ScenarioReport] -> Text
report npmLatencyMs npm pypi =
    T.intercalate
        "\n"
        [ fixtureSection Npm [renderReports (section Npm (npmLatencyMs * 1_000) npm), renderLoadSaturation [] npm]
        , fixtureSection PyPI [renderReports (section PyPI 5_000 pypi)]
        ]

section :: Ecosystem -> Int -> [ScenarioReport] -> Section
section ecosystem latencyMicros loaded =
    Section
        { sectionKnobs = defaultLoadKnobs{lkUpstreamLatencyMicros = latencyMicros}
        , sectionCapabilities = 3
        , sectionProcessors = 4
        , sectionShape = twoCores
        , sectionEcosystem = ecosystem
        , sectionEnforcement = slowNetwork
        , sectionConcurrencyOne = []
        , sectionLoaded = loaded
        }

onMainNpm, hereNpm, onMainPyPI, herePyPI :: [ScenarioReport]
onMainNpm = [scenarioReport "npm/merge-cold" 1000 300 1000 2000, scenarioReport "npm/tarball-hot-path" 16000 0 180 200, ramp "npm/ramp" 800 50 1400 2400]
hereNpm = [scenarioReport "npm/merge-cold" 1100 250 900 2100, scenarioReport "npm/tarball-hot-path" 24000 0 120 150, ramp "npm/ramp" 800 50 1400 2400]
onMainPyPI = [scenarioReport "pypi/index-cold" 4000 0 500 1000]
herePyPI = [scenarioReport "pypi/index-cold" 3800 0 550 1000]

-- A ramp, whose figures are those of its last step.
ramp :: Text -> Int -> Int -> Double -> Double -> ScenarioReport
ramp name successes refusals p50 p99 = single{srSteps = [srLoad single]}
  where
    single = scenarioReport name successes refusals p50 p99

proxyMemory :: Int -> ProxyFigures
proxyMemory peak =
    ProxyFigures
        { pfIdleRts = Nothing
        , pfIdleCgroupBytes = Nothing
        , pfCgroup = Just (CgroupReading (Just (1024 * 1024 * 1024)) (Just peak) Nothing mempty mempty mempty)
        , pfWindowCgroup = Nothing
        , pfWindowThrottledUsec = Nothing
        , pfCpu = Nothing
        , pfEnding = CleanShutdown
        , pfExitedEarly = False
        , pfStderrTail = ""
        , pfBootLines = []
        , pfRuleLines = []
        , pfBootRetries = []
        , pfInFlight = summariseGauge []
        , pfTasks = summariseGauge []
        , pfAdmissionSeries = []
        , pfCacheWindow = Nothing
        }
