-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory variants preserve their counterpart's policy and measurement settings.
module Ecluse.BenchLoad.AdvisoriesSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Advisories (allRulesAdvisories, shippedAdvisories)
import Ecluse.BenchLoad.Error (BenchLoadError (BenchLoadError))
import Ecluse.BenchLoad.Harness (Driver (DriveInProcess), Scenario (..), Target (Target), defaultLoadKnobs, scenario)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))

spec :: Spec
spec = describe "advisory variants" $ do
    let counterpart = (scenario "merge-cold" "The no-database load." (\_ k -> k (Target Nothing (DriveInProcess (pure []))))){scenarioServiceTime = False, scenarioConcurrencyScale = 4, scenarioUpstreamLatencyMicros = Just 2_000, scenarioPublicLegUnavailableReason = Just "no public leg"}
    it "suffix the counterpart's name and extend its description" $ do
        scenarioName (shippedAdvisories Npm counterpart) `shouldBe` "merge-cold-advisories"
        scenarioName (allRulesAdvisories Npm counterpart) `shouldBe` "merge-cold-all-advisory-rules"
        scenarioDescription (allRulesAdvisories Npm counterpart) `shouldSatisfy` T.isPrefixOf "The no-database load. The proxy syncs"
    it "keep the counterpart's passes, settings, and attribution role" $
        for_ [shippedAdvisories Npm counterpart, allRulesAdvisories Npm counterpart] $ \variant -> do
            scenarioServiceTime variant `shouldBe` False
            scenarioConcurrencyScale variant `shouldBe` 4
            scenarioUpstreamLatencyMicros variant `shouldBe` Just 2_000
            scenarioPublicLegUnavailableReason variant `shouldBe` Just "no public leg"
            scenarioInProcess variant `shouldBe` False
    it "refuse an in-process counterpart, which boots no proxy to sync the database" $
        scenarioBoot (shippedAdvisories Npm counterpart{scenarioInProcess = True}) defaultLoadKnobs (const pass)
            `shouldThrow` (\(BenchLoadError message) -> "boots no proxy" `T.isInfixOf` message)
