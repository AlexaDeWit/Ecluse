-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Resolve actual scenario settings for loaded and concurrency-one passes.
module Ecluse.BenchLoad.HarnessSpec (spec) where

import Test.Hspec

import Ecluse.BenchLoad.Floors (Pass (ConcurrencyOne, Loaded))
import Ecluse.BenchLoad.Harness (LoadKnobs (..), Scenario (..), defaultLoadKnobs, scenarioKnobs)
import Ecluse.BenchLoad.Scenarios (findScenario)

spec :: Spec
spec = do
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
