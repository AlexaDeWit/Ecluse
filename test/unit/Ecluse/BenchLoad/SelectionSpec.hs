-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin ecosystem isolation, baseline attribution, and the cache churn bound.
module Ecluse.BenchLoad.SelectionSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Normalise (BaselineSource (InjectedFallback, MeasuredRtt))
import Ecluse.BenchLoad.Selection (evictionEntries, fixtureBaseline, fixtureSection, selectScenario)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))

-- | Check identity isolation, ecosystem report groups, baseline sources, and eviction limits.
spec :: Spec
spec = do
    describe "scenario selection" $ do
        let fixtures = [(Npm, [("cached-public-hit", "npm result" :: Text)]), (PyPI, [("cached-public-hit", "pypi result")])]
        it "keeps duplicate local scenario names separate" $ do
            selectScenario "npm/cached-public-hit" fixtures `shouldBe` Just "npm result"
            selectScenario "pypi/cached-public-hit" fixtures `shouldBe` Just "pypi result"
        it "refuses an unqualified name or another ecosystem" $ do
            selectScenario "cached-public-hit" fixtures `shouldBe` Nothing
            selectScenario "rubygems/cached-public-hit" fixtures `shouldBe` Nothing
    describe "report grouping" $
        it "keeps the load, service, and saturation views inside each ecosystem section" $ do
            let output = fixtureSection Npm ["npm load", "npm service", "npm saturation"] <> fixtureSection PyPI ["pypi load", "pypi service", "pypi saturation"]
            T.splitOn "# " output
                `shouldBe` ["", "npm load scenarios\n\nnpm load\nnpm service\nnpm saturation", "pypi load scenarios\n\npypi load\npypi service\npypi saturation"]
    describe "baseline selection" $ do
        it "does not attribute npm's measured RTT to PyPI" $ do
            fixtureBaseline Npm 5_000 (MeasuredRtt 80 9) `shouldBe` MeasuredRtt 80 9
            fixtureBaseline PyPI 5_000 (MeasuredRtt 80 9) `shouldBe` InjectedFallback 5
        it "keeps each configured fallback independent when npm probing fails" $
            fixtureBaseline PyPI 2_000 (InjectedFallback 10) `shouldBe` InjectedFallback 2
    describe "the eviction cache" $ do
        it "forces churn for the three-project corpus at the default and larger bounds" $ do
            evictionEntries 3 3 `shouldBe` Right 2
            evictionEntries 100 3 `shouldBe` Right 2
        it "preserves a smaller configured cache and clamps an invalid zero bound" $ do
            evictionEntries 1 3 `shouldBe` Right 1
            evictionEntries 0 3 `shouldBe` Right 1
        it "refuses a working set with no possible eviction pair" $ do
            evictionEntries 3 1 `shouldBe` Left "cache-evicts-large requires at least two corpus projects"
            evictionEntries 3 0 `shouldBe` Left "cache-evicts-large requires at least two corpus projects"
