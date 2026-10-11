-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Project scenario reports into service attribution without discarding raw private-hit latency.
module Ecluse.BenchLoad.ReportSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Harness (Scenario (scenarioPublicLegUnavailableReason), ScenarioReport (..))
import Ecluse.BenchLoad.Normalise (BaselineSource (InjectedFallback, MeasuredRtt))
import Ecluse.BenchLoad.Report (renderServiceTime)
import Ecluse.BenchLoad.Scenarios (findScenario)
import Ecluse.BenchLoad.Support (scenarioReport)

spec :: Spec
spec = describe "renderServiceTime" $ do
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
