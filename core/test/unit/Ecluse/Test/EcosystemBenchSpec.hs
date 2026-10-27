-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Measurement provider coverage fails independently of corpus and service availability.
module Ecluse.Test.EcosystemBenchSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Test.EcosystemBench (benchmarkEcosystems, checkBenchmarkCoverage)

-- | Missing, duplicate and unshipped providers cannot erase or satisfy an obligation.
spec :: Spec
spec = describe "measurement provider coverage" $ do
    it "covers the independent obligations and production adapters" $
        checkBenchmarkCoverage benchmarkEcosystems `shouldBe` Right ()
    it "does not depend on provider ordering" $
        checkBenchmarkCoverage [PyPI, Cargo, Npm] `shouldBe` Right ()
    it "rejects each omitted ecosystem" $
        for_ [[Npm, PyPI], [Cargo, PyPI], [Cargo, Npm], []] $ \providers ->
            checkBenchmarkCoverage providers `shouldSatisfy` isLeft
    it "rejects a duplicate provider" $
        checkBenchmarkCoverage [Cargo, Npm, PyPI, Npm] `shouldSatisfy` isLeft
    it "rejects an ecosystem without a production adapter" $
        checkBenchmarkCoverage [Cargo, Npm, PyPI, RubyGems] `shouldSatisfy` isLeft
