-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The load comparison reads the report the harness renders, sets each scenario's successes,
refusals, and latency beside those on @main@ in a section for each ecosystem, shows the
operating point rows that differ, and never fails on a missing side.
-}
module Ecluse.BenchReport.AgainstMain.LoadSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Harness (LoadKnobs (lkUpstreamLatencyMicros), LoadSummary (..), ScenarioReport (..), defaultLoadKnobs)
import Ecluse.BenchLoad.Latency (Percentiles (Percentiles))
import Ecluse.BenchLoad.Report (Section (..), renderLoadSaturation, renderReports, renderThrash)
import Ecluse.BenchLoad.Selection (fixtureSection)
import Ecluse.BenchLoad.Support (slowNetwork)
import Ecluse.BenchReport.AgainstMain (Baseline (Baseline, NoBaseline), Origin (Origin))
import Ecluse.BenchReport.AgainstMain.Load (againstMain)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))

spec :: Spec
spec = do
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
            let rendered = againstMain (NoBaseline "The artifact could not be downloaded.") (Right (report 12 hereNpm herePyPI))
            rendered `shouldSatisfy` T.isInfixOf "**No baseline.** The artifact could not be downloaded."
            rendered `shouldNotSatisfy` T.isInfixOf "###"
        it "prints no baseline for a report on main without a scenario table" $
            againstMain (Baseline origin "the harness crashed") (Right (report 12 hereNpm herePyPI))
                `shouldSatisfy` T.isInfixOf "could not be read. It holds no scenario table."
        it "has nothing to compare in the report of the GC-thrash probe" $
            againstMain (Baseline origin (report 178 onMainNpm onMainPyPI)) (Right (renderThrash "npm/heavy-private" [("2cpu-1024mib", Left "the scenario process exited 1")]))
                `shouldSatisfy` T.isInfixOf "**Nothing to compare.** This run's report could not be read. It holds no scenario table."

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
        , sectionShape = "2cpu-1gib"
        , sectionEcosystem = ecosystem
        , sectionEnforcement = slowNetwork
        , sectionConcurrencyOne = []
        , sectionLoaded = loaded
        }

onMainNpm, hereNpm, onMainPyPI, herePyPI :: [ScenarioReport]
onMainNpm = [scenario "npm/merge-cold" 1000 300 1000 2000, scenario "npm/tarball-hot-path" 16000 0 180 200, ramp "npm/ramp" 800 50 1400 2400]
hereNpm = [scenario "npm/merge-cold" 1100 250 900 2100, scenario "npm/tarball-hot-path" 24000 0 120 150, ramp "npm/ramp" 800 50 1400 2400]
onMainPyPI = [scenario "pypi/index-cold" 4000 0 500 1000]
herePyPI = [scenario "pypi/index-cold" 3800 0 550 1000]

-- One scenario's loaded pass: its successes, its refusals, and its success p50 and p99 in milliseconds.
scenario :: Text -> Int -> Int -> Double -> Double -> ScenarioReport
scenario name successes refusals p50 p99 =
    ScenarioReport
        { srName = name
        , srDescription = "A scenario."
        , srShape = "2cpu-1gib"
        , srLoad = window
        , srCompanion = Nothing
        , srSteps = []
        , srReplayTotals = Nothing
        , srRtsSource = "the proxy"
        , srRtsWindow = Nothing
        , srRtsEnd = Nothing
        , srRetainedBytes = Nothing
        , srProxy = Nothing
        , srEvidence = ""
        }
  where
    window =
        LoadSummary
            { lsLabel = "window"
            , lsConnections = 100
            , lsElapsedSeconds = 30
            , lsCompleted = successes + refusals
            , lsSuccesses = successes
            , lsRefusals = refusals
            , lsOtherStatuses = 0
            , lsTransportFailures = 0
            , lsDeadlineAborts = 0
            , lsLatency = Percentiles (Just p50) Nothing (Just p99) Nothing
            , lsNote = ""
            }

-- A ramp, whose figures are those of its last step.
ramp :: Text -> Int -> Int -> Double -> Double -> ScenarioReport
ramp name successes refusals p50 p99 = single{srSteps = [srLoad single]}
  where
    single = scenario name successes refusals p50 p99
