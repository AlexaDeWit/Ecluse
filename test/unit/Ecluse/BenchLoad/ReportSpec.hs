-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Project scenario reports into service attribution without discarding raw private-hit latency, and render a placed proxy's CPU time.
module Ecluse.BenchLoad.ReportSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.Text qualified as T
import Test.Hspec

import Data.Map.Strict qualified as Map

import Ecluse.BenchLoad.Harness (LoadKnobs (lkProxyTuning), ProxyFigures (pfBurstMemory, pfCpu), Scenario (scenarioPublicLegUnavailableReason), ScenarioReport (..))
import Ecluse.BenchLoad.Normalise (BaselineSource (InjectedFallback, MeasuredRtt))
import Ecluse.BenchLoad.Pod (CgroupReading (CgroupReading), ProxyTuning (ptCpus), burstMemory, noTuning, parseCpuList)
import Ecluse.BenchLoad.Report (Section (sectionKnobs), renderReports, renderServiceTime)
import Ecluse.BenchLoad.Scenarios (findScenario)
import Ecluse.BenchLoad.Support (loadedSection, placedWindow, proxyWithPeak, scenarioReport)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))

spec :: Spec
spec = do
    serviceTimeSpec
    describe "renderReports" $ do
        it "shows the cgroup's memory through a burst only for a scenario that sent one" $ do
            let sock = CgroupReading Nothing Nothing (Just (900 * mib)) mempty (Map.fromList [("anon", 600 * mib), ("sock", 250 * mib)]) mempty Nothing
                plain = (scenarioReport "npm/heavy-private" 100 0 255 300){srProxy = Just (proxyWithPeak (950 * mib))}
                burst = plain{srProxy = (\p -> p{pfBurstMemory = Just (burstMemory 10 [sock])}) <$> srProxy plain}
                burstLines = T.lines (renderReports (loadedSection Npm 5_000 [burst]))
            burstLines `shouldSatisfy` elem "| highest memory.current in the burst, then memory.stat anon / file / kernel / sock | 900.0 MiB, then 600.0 MiB / 0.0 MiB / 0.0 MiB / 250.0 MiB |"
            burstLines `shouldSatisfy` elem "| highest memory.stat sock in the burst, then memory.current | 250.0 MiB, then 900.0 MiB |"
            T.lines (renderReports (loadedSection Npm 5_000 [plain])) `shouldNotSatisfy` any (T.isInfixOf "in the burst")
        it "shows busy time on the proxy's CPUs that the proxy did not spend only for a proxy placed on its own CPUs" $ do
            let report = (scenarioReport "npm/merge-cold" 100 0 255 300){srProxy = Just (proxyWithPeak (300 * 1024 * 1024)){pfCpu = Just (placedWindow 1_000)}}
                unplaced = loadedSection Npm 5_000 [report]
                placed = unplaced{sectionKnobs = (sectionKnobs unplaced){lkProxyTuning = noTuning{ptCpus = rightToMaybe (parseCpuList "2-3")}}}
                foreignRow = T.isPrefixOf "| busy time on the proxy's CPUs that the proxy did not spend |"
            T.lines (renderReports placed) `shouldSatisfy` any foreignRow
            T.lines (renderReports unplaced) `shouldNotSatisfy` any foreignRow

serviceTimeSpec :: Spec
serviceTimeSpec = describe "renderServiceTime" $ do
    case findScenario "npm/tarball-ceiling" of
        Nothing -> it "has a ceiling scenario" (expectationFailure "missing npm/tarball-ceiling scenario")
        Just ceilingScenario -> do
            let report = (scenarioReport "npm/tarball-ceiling" 100 0 2.5 4){srPublicLegUnavailableReason = scenarioPublicLegUnavailableReason ceilingScenario}
            it "keeps raw totals and a reason without subtracting either public baseline" $
                for_ [MeasuredRtt 150 8, InjectedFallback 17] $ \baseline -> do
                    let rendered = renderServiceTime baseline [report]
                    T.lines rendered `shouldSatisfy` elem "| npm/tarball-ceiling | 2.5 ms | n/a | n/a | 4.0 ms | n/a | n/a | unavailable: the private-hit artifact relay has no public leg |"
                    rendered `shouldNotSatisfy` T.isInfixOf "0.0 ms (0%)"
            it "preserves the attribution reason across the child report's JSON boundary" $
                case eitherDecode (encode report) of
                    Left message -> expectationFailure message
                    Right decoded -> renderServiceTime (InjectedFallback 17) [decoded] `shouldBe` renderServiceTime (InjectedFallback 17) [report]
    it "preserves public-only subtraction and the labelled p99 tail for ordinary scenarios" $ do
        let report = scenarioReport "npm/merge-cold" 100 0 255 300
            rendered = renderServiceTime (MeasuredRtt 150 8) [report]
        T.lines rendered `shouldSatisfy` elem "| npm/merge-cold | 255.0 ms | 150.0 ms (59%) | 105.0 ms (41%) | 300.0 ms | 150.0 ms (50%) | 150.0 ms (50%) | available |"
        rendered `shouldSatisfy` T.isInfixOf "p50 is primary. p99 is the tail, GC included."

mib :: Int
mib = 1024 * 1024
