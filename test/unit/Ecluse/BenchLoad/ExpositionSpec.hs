-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the scrape reading the advisory wait, the in-flight gauge, and the cache evidence depend on.
module Ecluse.BenchLoad.ExpositionSpec (spec) where

import Test.Hspec

import Ecluse.BenchLoad.Exposition (
    CacheOutcomes (..),
    GaugeSummary (..),
    Sample (..),
    advisoryDatabaseInstalled,
    cacheWindow,
    commonLabels,
    parseExposition,
    renderSample,
    ruleFailuresWindow,
    seriesTotal,
    storeOutcomes,
    summariseGauge,
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Telemetry.Metrics (CacheStore (FullStore, VersionStore))

exposition :: Text
exposition =
    unlines
        [ "# HELP ecluse_serve_admission_in_flight in-flight metadata parses"
        , "# TYPE ecluse_serve_admission_in_flight gauge"
        , "ecluse_serve_admission_in_flight{job=\"ecluse\",service_name=\"ecluse\"} 3"
        , "ecluse_metadata_cache_version_requests{job=\"ecluse\",service_name=\"ecluse\",result=\"hit\"} 7"
        , "ecluse_metadata_cache_version_requests{job=\"ecluse\",service_name=\"ecluse\",result=\"miss\"} 2"
        , "ecluse_odd{job=\"ecluse\",service_name=\"ecluse\",note=\"a \\\"quoted\\\", comma\"} +Inf"
        , "unlabelled_total 5"
        , ""
        , "not a sample line"
        ]

spec :: Spec
spec = do
    describe "parseExposition" $ do
        let samples = parseExposition exposition
        it "reads labelled and unlabelled samples and skips the rest" $
            map sampleName samples
                `shouldBe` ["ecluse_serve_admission_in_flight", "ecluse_metadata_cache_version_requests", "ecluse_metadata_cache_version_requests", "ecluse_odd", "unlabelled_total"]
        it "unescapes a label value and reads an infinite value" $ do
            let odd' = find ((== "ecluse_odd") . sampleName) samples
            (lookupLabel "note" =<< odd') `shouldBe` Just "a \"quoted\", comma"
            (isInfinite . sampleValue <$> odd') `shouldBe` Just True
    describe "seriesTotal" $ do
        let samples = parseExposition exposition
        it "sums the series whose labels include the given pairs" $ do
            seriesTotal "ecluse_metadata_cache_version_requests" [] samples `shouldBe` Just 9
            seriesTotal "ecluse_metadata_cache_version_requests" [("result", "hit")] samples `shouldBe` Just 7
        it "keeps an absent metric apart from a zero one" $
            seriesTotal "ecluse_missing" [] samples `shouldBe` Nothing
    describe "advisoryDatabaseInstalled" $ do
        let installed = parseExposition "ecluse_advisory_database_age_seconds{job=\"ecluse\",ecosystem=\"npm\"} 0\n"
        it "reads the ecosystem's database age, even a zero one, as an installed database" $
            advisoryDatabaseInstalled Npm installed `shouldBe` True
        it "waits while the age is absent or names another ecosystem" $ do
            advisoryDatabaseInstalled PyPI installed `shouldBe` False
            advisoryDatabaseInstalled Npm (parseExposition exposition) `shouldBe` False
    describe "ruleFailuresWindow" $ do
        let failures n = parseExposition ("ecluse_rule_effectful_failures{cause=\"transient\"} " <> show (n :: Int) <> "\necluse_rule_effectful_failures{cause=\"permanent\"} 1\n")
        it "counts the failures recorded between the two scrapes across every cause" $
            ruleFailuresWindow (failures 2) (failures 9) `shouldBe` 7
        it "reads a counter not yet created as zero" $
            ruleFailuresWindow [] (failures 0) `shouldBe` 1
    describe "renderSample" $
        it "drops the labels every series repeats" $ do
            let samples = parseExposition exposition
                resource = commonLabels samples
            resource `shouldBe` []
            let labelled = filter (not . null . sampleLabels) samples
            commonLabels labelled `shouldBe` ["job", "service_name"]
            map (renderSample (commonLabels labelled)) (take 2 labelled)
                `shouldBe` ["ecluse_serve_admission_in_flight 3.0", "ecluse_metadata_cache_version_requests{result=hit} 7.0"]
    describe "summariseGauge" $ do
        it "counts misses apart from readings" $
            summariseGauge [Just 1, Nothing, Just 3, Just 2]
                `shouldBe` GaugeSummary{gsSamples = 3, gsMissed = 1, gsMax = Just 3, gsMean = Just 2, gsLast = Just 2}
        it "reports nothing for a window with no reading" $
            summariseGauge [Nothing] `shouldBe` GaugeSummary 0 1 Nothing Nothing Nothing
    describe "storeOutcomes" $
        it "reads a store's outcomes, and zero for a store with no series" $ do
            storeOutcomes (parseExposition exposition) VersionStore `shouldBe` CacheOutcomes 7 2 0
            storeOutcomes (parseExposition exposition) FullStore `shouldBe` CacheOutcomes 0 0 0
    describe "cacheWindow" $
        it "counts what each store recorded between the two scrapes" $
            cacheWindow
                (parseExposition "ecluse_metadata_cache_requests{result=\"miss\"} 2\n")
                (parseExposition "ecluse_metadata_cache_requests{result=\"miss\"} 6\necluse_metadata_cache_requests{result=\"collapsed\"} 3\necluse_metadata_cache_assembled_requests{result=\"hit\"} 1\n")
                `shouldBe` [("full", CacheOutcomes 0 4 3), ("version", CacheOutcomes 0 0 0), ("assembled", CacheOutcomes 1 0 0)]
  where
    lookupLabel key s = snd <$> find ((== key) . fst) (sampleLabels s)
