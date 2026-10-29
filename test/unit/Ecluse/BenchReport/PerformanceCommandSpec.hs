-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Required measurement conditions must not come from the measured declarations.
module Ecluse.BenchReport.PerformanceCommandSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.BenchReport.PerformanceCommand (compileObligation)
import Ecluse.Test.Contract.Performance (PerformanceObligation (..))
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "independent conditions" $ do
    it "keeps the allocation requirement when the native producer reports one pass" $ do
        obligation <- expectRight (compileObligation "allocation" "ecosystem-npm" (Map.fromList [("compiler", "other"), ("optimisation", "-O0"), ("profiling", "on"), ("mode", "other"), ("passes", "1")]))
        Map.lookup "passes" (poConditions obligation) `shouldBe` Just "5"
        Map.lookup "mode" (poConditions obligation) `shouldBe` Just "captures"
        Map.lookup "optimisation" (poConditions obligation) `shouldBe` Just "-O1"
        Map.lookup "profiling" (poConditions obligation) `shouldBe` Just "off"
        Map.lookup "compiler" (poConditions obligation) `shouldNotBe` Just "other"
    it "keeps the benchmark precision and RTS requirement" $ do
        obligation <- expectRight (compileObligation "benchmark" "ecosystem-pypi" (Map.fromList [("compiler", "other"), ("optimisation", "-O0"), ("profiling", "on"), ("rts", "-N1"), ("stdev", "100")]))
        Map.lookup "rts" (poConditions obligation) `shouldBe` Just "-T"
        Map.lookup "stdev" (poConditions obligation) `shouldBe` Just "10"
    it "requires the PyPI worker and managed reads in a full unlimited load receipt" $ do
        let conditions = Map.fromList [("compiler", "declared"), ("optimisation", "-O1"), ("profiling", "off"), ("podShape", "unlimited"), ("durationSeconds", "30"), ("concurrency", "100"), ("serveMaxInFlight", "automatic"), ("affinity", "taskset -c 0 oha")]
        obligation <- expectRight (compileObligation "load" "ecosystem-pypi" conditions)
        for_ ["worker-mirroring", "mirror-native-baseline", "mirror-managed-warm", "mirror-managed-revision"] $ \scenario ->
            for_ ["loaded", "concurrencyOne"] $ \whichPass ->
                poRows obligation `shouldSatisfy` elem ("pypi/" <> scenario <> "/" <> whichPass)
    it "requires every native Cargo capture and allocation leg independently" $ do
        let conditions = Map.fromList [("compiler", "declared"), ("optimisation", "-O1"), ("profiling", "off"), ("mode", "captures"), ("passes", "5")]
        obligation <- expectRight (compileObligation "allocation" "ecosystem-cargo" conditions)
        poRows obligation
            `shouldMatchList` [ "cargo/" <> package <> "/" <> leg
                              | package <- ["tokio", "windows", "aws-sdk-ec2"]
                              , leg <- ["full", "singleVersion", "fullShippedAdvisories", "fullAllAdvisoryRules"]
                              ]
    it "requires all native RubyGems captures and allocation legs independently" $ do
        let conditions = Map.fromList [("compiler", "declared"), ("optimisation", "-O1"), ("profiling", "off"), ("mode", "captures"), ("passes", "5")]
        obligation <- expectRight (compileObligation "allocation" "ecosystem-rubygems" conditions)
        poRows obligation
            `shouldMatchList` [ "rubygems/" <> package <> "/" <> leg
                              | package <- ["rails", "nokogiri", "aws-sdk-ec2"]
                              , leg <- ["full", "singleVersion", "fullShippedAdvisories", "fullAllAdvisoryRules"]
                              ]
    it "requires native quick-spec and whole-version worker observations" $ do
        let conditions = Map.fromList [("compiler", "declared"), ("optimisation", "-O1"), ("profiling", "off"), ("podShape", "unlimited"), ("durationSeconds", "30"), ("concurrency", "100"), ("serveMaxInFlight", "automatic"), ("affinity", "taskset -c 0 oha")]
        obligation <- expectRight (compileObligation "load" "ecosystem-rubygems" conditions)
        for_ ["quick-gemspec", "worker-mirroring"] $ \scenario ->
            for_ ["loaded", "concurrencyOne"] $ \whichPass ->
                poRows obligation `shouldSatisfy` elem ("rubygems/" <> scenario <> "/" <> whichPass)
    it "rejects a missing measurement declaration" $
        compileObligation "allocation" "ecosystem-npm" Map.empty `shouldSatisfy` isLeft
