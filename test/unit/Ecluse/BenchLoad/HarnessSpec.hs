-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Resolve actual scenario settings for loaded and concurrency-one passes.
module Ecluse.BenchLoad.HarnessSpec (spec) where

import Test.Hspec

import Ecluse.BenchLoad.Floors (Pass (ConcurrencyOne, Loaded))
import Ecluse.BenchLoad.Harness (LoadKnobs (..), Scenario (..), defaultLoadKnobs, loadKnobsFromEnv, scenarioKnobs)
import Ecluse.BenchLoad.Pod (ProxyTuning (ptCpus), parseCpuList)
import Ecluse.BenchLoad.Scenarios (findScenario)
import Ecluse.Test.Env (withEnvVars)

spec :: Spec
spec = do
    describe "proxy placement defaults" $ do
        it "reads both 3cpu shapes with CPUs 1-3 and preserves explicit placement" $
            for_ ["3cpu-1gib", "3cpu-2gib"] $ \shape ->
                for_ [("", "1-3"), ("2-3", "2-3")] $ \(override, expected) ->
                    withEnvVars ["BENCH_LOAD_POD", "BENCH_LOAD_PROXY_CPUS"] [("BENCH_LOAD_POD", shape), ("BENCH_LOAD_PROXY_CPUS", override)] $ do
                        knobsFromEnv <- loadKnobsFromEnv
                        ptCpus (lkProxyTuning knobsFromEnv) `shouldBe` rightToMaybe (parseCpuList expected)
        it "keeps the unlimited and 2cpu controls unplaced" $
            for_ ["unlimited", "2cpu-1gib"] $ \shape ->
                withEnvVars ["BENCH_LOAD_POD", "BENCH_LOAD_PROXY_CPUS"] [("BENCH_LOAD_POD", shape)] $ do
                    knobsFromEnv <- loadKnobsFromEnv
                    ptCpus (lkProxyTuning knobsFromEnv) `shouldBe` Nothing
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
