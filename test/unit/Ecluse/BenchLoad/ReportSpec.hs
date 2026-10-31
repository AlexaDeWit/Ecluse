-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Service attribution subtracts the injected latency. Floor cells and breach confirmations render. CPU reports distinguish placed and unplaced proxies.
module Ecluse.BenchLoad.ReportSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.Text qualified as T
import Test.Hspec

import Data.Map.Strict qualified as Map

import Ecluse.BenchLoad.Floors (Cell (Floored, ReportOnlyCell, Uncalibrated), Enforcement (Enforced), Pass (ConcurrencyOne, Loaded))
import Ecluse.BenchLoad.Harness (LoadKnobs (lkProxyTuning), LoadSummary (lsElapsedSeconds, lsFirstByteLatency, lsLabel, lsLatency, lsTransferredBodyBytes), ProxyFigures (pfBurstMemory, pfCpu), Scenario (scenarioPublicLegUnavailableReason, scenarioUpstreamLatencyMicros), ScenarioReport (..))
import Ecluse.BenchLoad.Latency (Percentiles (..))
import Ecluse.BenchLoad.Normalise (LiveFetch (LiveFetch, NoLiveFetch))
import Ecluse.BenchLoad.Pod (CgroupReading (CgroupReading), ProxyTuning (ptCpus), burstMemory, noTuning, parseCpuList)
import Ecluse.BenchLoad.Report (Section (sectionEnforcement, sectionKnobs), renderConfirmations, renderReports, renderServiceTime)
import Ecluse.BenchLoad.Scenarios (findScenario)
import Ecluse.BenchLoad.Support (calibrationRunIds, floorsAtTwoCores, loadedSection, placedWindow, proxyWithPeak, scenarioReport, slowNetwork, twoCores)
import Ecluse.BenchLoad.Verdict (ProxyEnding (CleanShutdown, HeapOverflow), RunEvidence (RunEvidence))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))

spec :: Spec
spec = do
    serviceTimeSpec
    confirmationSpec
    describe "renderReports" $ do
        it "reports observed transfer bytes without deriving them from fixture sizes" $ do
            let plain = scenarioReport "npm/merge-cold" 100 4 2 5
                observed = plain{srLoad = (srLoad plain){lsTransferredBodyBytes = Just 1234, lsElapsedSeconds = 2}}
                rendered = renderReports (loadedSection Npm 5_000 [observed])
            rendered `shouldSatisfy` T.isInfixOf "| transferred response body bytes / bytes per second (all statuses) | 1234 / 617.0 |"
            renderReports (loadedSection Npm 5_000 [plain]) `shouldSatisfy` T.isInfixOf "(all statuses) | unavailable |"
        it "distinguishes successful duration quantiles from all-response first body bytes" $ do
            let plain = scenarioReport "npm/merge-cold" 100 4 2 5
                observed = plain{srLoad = (srLoad plain){lsFirstByteLatency = Just (Percentiles (Just 1) Nothing (Just 3) (Just 4) Nothing), lsLatency = Percentiles (Just 2) Nothing (Just 4) (Just 5) Nothing}}
                rendered = renderReports (loadedSection Npm 5_000 [observed])
            rendered `shouldSatisfy` T.isInfixOf "success latency p95"
            rendered `shouldSatisfy` T.isInfixOf "first body byte p50 / p95 / p99 (all completed responses, including refusals)"
            renderReports (loadedSection Npm 5_000 [plain]) `shouldSatisfy` T.isInfixOf "including refusals) | unavailable"
        it "keeps burst refusals visible beside a background-only floor" $ do
            let name = "npm/herd-under-heavy-private"
                plain = scenarioReport name 0 100 0 0
                burst = plain{srLoad = (srLoad plain){lsLabel = "burst"}, srCompanion = Just (srLoad (scenarioReport name 20 30 1 2)){lsLabel = "concurrent load"}, srDiagnosticBurst = True}
                section = (loadedSection Npm 5_000 [burst]){sectionEnforcement = Enforced twoCores 1 (Map.singleton (name, Loaded) (Floored 10 17 calibrationRunIds))}
                rendered = renderReports section
            rendered `shouldSatisfy` T.isInfixOf "| 100 | 0 | 10 (background only, median 17) | 100 | 0 |"
            rendered `shouldSatisfy` T.isInfixOf "| concurrent load: successes / refusals / transport failures | 20 / 30 / 0 |"
            rendered `shouldSatisfy` T.isInfixOf "including zero successes"
        it "shows each cell as its floor and median, uncalibrated, or report-only with its reason" $ do
            let cellOf cell = renderReports (loadedSection Npm 5_000 [scenarioReport "npm/merge-cold" 100 0 2 5]){sectionEnforcement = Enforced twoCores 1 (Map.singleton ("npm/merge-cold", Loaded) cell)}
            cellOf (Floored 338 564 calibrationRunIds) `shouldSatisfy` T.isInfixOf "| 100 | 338 (median 564) | 0 |"
            cellOf Uncalibrated `shouldSatisfy` T.isInfixOf "| 100 | uncalibrated | 0 |"
            cellOf (ReportOnlyCell "its counts vary") `shouldSatisfy` T.isInfixOf "| 100 | report-only: its counts vary | 0 |"
            renderReports (loadedSection Npm 5_000 [scenarioReport "npm/merge-cold" 100 0 2 5]) `shouldSatisfy` T.isInfixOf "| 100 | not held | 0 |"
        it "shows the cgroup's memory through a burst only for a scenario that sent one" $ do
            let sock = CgroupReading Nothing Nothing (Just (900 * mib)) mempty (Map.fromList [("anon", 600 * mib), ("sock", 250 * mib)]) mempty Nothing
                plain = (scenarioReport "npm/heavy-private" 100 0 255 300){srProxy = Just (proxyWithPeak (950 * mib))}
                burst = plain{srProxy = (\p -> p{pfBurstMemory = Just (burstMemory 10 [sock])}) <$> srProxy plain}
                burstLines = T.lines (renderReports (loadedSection Npm 5_000 [burst]))
            burstLines `shouldSatisfy` elem "| highest memory.current in the burst, then memory.stat anon / file / kernel / sock | 900.0 MiB, then 600.0 MiB / 0.0 MiB / 0.0 MiB / 250.0 MiB |"
            burstLines `shouldSatisfy` elem "| highest memory.stat sock in the burst, then memory.current | 250.0 MiB, then 900.0 MiB |"
            T.lines (renderReports (loadedSection Npm 5_000 [plain])) `shouldNotSatisfy` any (T.isInfixOf "in the burst")
        it "says a burst under the unlimited control had no cgroup to read" $ do
            let unbounded = (scenarioReport "npm/herd-under-heavy-private" 100 0 255 300){srProxy = Just (proxyWithPeak 0){pfBurstMemory = Just (burstMemory 10 [])}}
                burstLines = filter (T.isInfixOf "burst") (T.lines (renderReports (loadedSection Npm 5_000 [unbounded])))
            filter (T.isPrefixOf "| cgroup") burstLines `shouldBe` ["| cgroup readings through the burst | no cgroup |"]
        it "shows busy time on the proxy's CPUs that the proxy did not spend only for a proxy placed on its own CPUs" $ do
            let report = (scenarioReport "npm/merge-cold" 100 0 255 300){srProxy = Just (proxyWithPeak (300 * 1024 * 1024)){pfCpu = Just (placedWindow 1_000)}}
                unplaced = loadedSection Npm 5_000 [report]
                placed = unplaced{sectionKnobs = (sectionKnobs unplaced){lkProxyTuning = noTuning{ptCpus = rightToMaybe (parseCpuList "2-3")}}}
                foreignRow = T.isPrefixOf "| busy time on the proxy's CPUs that the proxy did not spend |"
            T.lines (renderReports placed) `shouldSatisfy` any foreignRow
            T.lines (renderReports unplaced) `shouldNotSatisfy` any foreignRow

confirmationSpec :: Spec
confirmationSpec = describe "renderConfirmations" $ do
    let held = Enforced twoCores 1 floorsAtTwoCores
        counted name n = Right (RunEvidence name [("", n)] 0 (Just CleanShutdown) False)
    it "lists each pass below its floor on its first attempt, every attempt's count, and whether the re-run cleared it" $ do
        let rendered =
                renderConfirmations
                    held
                    [ (("npm/merge-cold", Loaded), counted "npm/merge-cold" 330 :| [counted "npm/merge-cold" 340])
                    , (("pypi/index-cold", ConcurrencyOne), counted "pypi/index-cold" 120 :| [counted "pypi/index-cold" 110])
                    , (("pypi/index-cold", Loaded), counted "pypi/index-cold" 2000 :| [])
                    ]
        T.lines rendered `shouldSatisfy` elem "| npm/merge-cold | loaded | 338 | 330, then 340 | cleared |"
        T.lines rendered `shouldSatisfy` elem "| pypi/index-cold | concurrencyOne | 134 | 120, then 110 | **failed** |"
        rendered `shouldNotSatisfy` T.isInfixOf "| pypi/index-cold | loaded |"
        rendered `shouldSatisfy` T.isInfixOf "re-runs on a fresh proxy at the end of the job, once."
    it "says no pass breached, and that a run that is not held re-runs nothing" $ do
        renderConfirmations held [(("npm/merge-cold", Loaded), counted "npm/merge-cold" 400 :| []), (("npm/herd", Loaded), Right (RunEvidence "npm/herd" [("", 0)] 0 (Just HeapOverflow) False) :| [])]
            `shouldSatisfy` T.isInfixOf "No scenario pass breached its floor on its first attempt."
        renderConfirmations slowNetwork [] `shouldSatisfy` T.isInfixOf "This run is not held to the floors, so no breach is re-run."

serviceTimeSpec :: Spec
serviceTimeSpec = describe "renderServiceTime" $ do
    case findScenario "npm/tarball-ceiling" of
        Nothing -> it "has a ceiling scenario" (expectationFailure "missing npm/tarball-ceiling scenario")
        Just ceilingScenario -> do
            let report = (scenarioReport "npm/tarball-ceiling" 100 0 2.5 4){srPublicLegUnavailableReason = scenarioPublicLegUnavailableReason ceilingScenario}
            it "keeps raw totals and a reason without subtracting either public baseline" $
                for_ [LiveFetch 150 8, NoLiveFetch] $ \live -> do
                    let rendered = renderServiceTime 20_000 live [report]
                    T.lines rendered `shouldSatisfy` elem "| npm/tarball-ceiling | 2.5 ms | n/a | n/a | 4.0 ms | n/a | n/a | n/a | unavailable: the private-hit artifact relay has no public leg |"
                    rendered `shouldNotSatisfy` T.isInfixOf "0.0 ms (0%)"
            it "preserves the attribution reason across the child report's JSON boundary" $
                case eitherDecode (encode report) of
                    Left message -> expectationFailure message
                    Right decoded -> renderServiceTime 20_000 NoLiveFetch [decoded] `shouldBe` renderServiceTime 20_000 NoLiveFetch [report]
    it "subtracts the injected latency once, places the overhead at the live distance, and labels the p99 tail" $ do
        let report = scenarioReport "npm/merge-cold" 100 0 255 300
            rendered = renderServiceTime 20_000 (LiveFetch 150 8) [report]
        T.lines rendered `shouldSatisfy` elem "| npm/merge-cold | 255.0 ms | 20.0 ms (8%) | 235.0 ms (92%) | 300.0 ms | 20.0 ms (7%) | 280.0 ms (93%) | 385.0 ms | available |"
        rendered `shouldSatisfy` T.isInfixOf "p50 is primary. p99 is the tail, GC included."
        T.lines (renderServiceTime 20_000 NoLiveFetch [report]) `shouldSatisfy` elem "| npm/merge-cold | 255.0 ms | 20.0 ms (8%) | 235.0 ms (92%) | 300.0 ms | 20.0 ms (7%) | 280.0 ms (93%) | n/a | available |"
    it "keeps Ruby ceiling totals without attributing a fixed delay to a different live baseline" $
        case findScenario "rubygems/gem-ceiling" of
            Nothing -> expectationFailure "missing rubygems/gem-ceiling scenario"
            Just ceilingScenario -> do
                scenarioUpstreamLatencyMicros ceilingScenario `shouldBe` Just 2_000
                let report = (scenarioReport "rubygems/gem-ceiling" 100 0 8 12){srPublicLegUnavailableReason = scenarioPublicLegUnavailableReason ceilingScenario}
                    rendered = renderServiceTime 20_000 (LiveFetch 50 9) [report]
                T.lines rendered `shouldSatisfy` elem "| rubygems/gem-ceiling | 8.0 ms | n/a | n/a | 12.0 ms | n/a | n/a | n/a | unavailable: the fixture uses a fixed 2 ms upstream delay instead of the run's injected latency |"
                rendered `shouldNotSatisfy` T.isInfixOf "0.0 ms (0%)"
                rendered `shouldSatisfy` T.isInfixOf "Rows without an applicable public baseline retain their raw totals."
                rendered `shouldNotSatisfy` T.isInfixOf "Private-hit relays have no public leg."

mib :: Int
mib = 1024 * 1024
