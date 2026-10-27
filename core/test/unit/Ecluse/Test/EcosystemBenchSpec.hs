-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Measurement provider coverage fails independently of corpus and service availability.
module Ecluse.Test.EcosystemBenchSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Registry.RubyGems.Marshal (DependencyRow (..), encodeDependencies)
import Ecluse.Test.Corpus.RubyGems (benchGem, syntheticInfoBytes)
import Ecluse.Test.EcosystemBench (EcosystemBench (ebCheckServed), benchmarkEcosystems, checkBenchmarkCoverage, ecosystemBenchFor)
import Ecluse.Test.Support (expectRight)

-- | Missing, duplicate and unshipped providers cannot erase or satisfy an obligation.
spec :: Spec
spec = describe "measurement provider coverage" $ do
    it "covers the independent obligations and production adapters" $
        checkBenchmarkCoverage benchmarkEcosystems `shouldBe` Right ()
    it "does not depend on provider ordering" $
        checkBenchmarkCoverage [PyPI, Cargo, RubyGems, Npm] `shouldBe` Right ()
    it "rejects each omitted ecosystem" $
        for_ [[Npm, PyPI, Cargo], [Cargo, PyPI, RubyGems], [Cargo, Npm, RubyGems], [Npm, PyPI, RubyGems], []] $ \providers ->
            checkBenchmarkCoverage providers `shouldSatisfy` isLeft
    it "rejects a duplicate provider" $
        checkBenchmarkCoverage [Cargo, Npm, PyPI, RubyGems, Npm] `shouldSatisfy` isLeft
    it "rejects a missing native provider" $
        checkBenchmarkCoverage [Cargo, Npm, PyPI] `shouldSatisfy` isLeft

    it "checks native served fields independently instead of inventing policy facts" $ do
        provider <- maybe (fail "missing RubyGems benchmark provider") pure (ecosystemBenchFor RubyGems)
        let original = syntheticInfoBytes 2
            firstRow = DependencyRow "bench-gem" "1.0.0" "ruby" [("benchmark-dependency", ">= 0")]
            secondRow = DependencyRow "bench-gem" "1.0.1" "ruby" [("benchmark-dependency", ">= 0")]
        native <- expectRight (encodeDependencies maxBound [firstRow, secondRow])
        ebCheckServed provider benchGem original native `shouldReturn` Right ()
        for_ [[firstRow], [firstRow{dependencyName = "other"}, secondRow], [firstRow{dependencyPlatform = "java"}, secondRow], [firstRow{dependencyRequirements = []}, secondRow]] $ \rows -> do
            changed <- expectRight (encodeDependencies maxBound rows)
            ebCheckServed provider benchGem original changed >>= (`shouldSatisfy` isLeft)
