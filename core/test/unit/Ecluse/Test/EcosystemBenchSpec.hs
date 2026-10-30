-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Measurement provider coverage fails independently of corpus and service availability.
module Ecluse.Test.EcosystemBenchSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Test.Corpus.RubyGems (benchGem, syntheticInfoBytes)
import Ecluse.Test.EcosystemBench (EcosystemBench (ebCheckServed), benchmarkEcosystems, checkBenchmarkCoverage, checkCoverageAgainst, ecosystemBenchFor)

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
    it "rejects providers that differ from the production adapters" $ do
        checkCoverageAgainst [Cargo, Npm, PyPI, RubyGems] [Cargo, Npm, PyPI, RubyGems] `shouldBe` Right ()
        checkCoverageAgainst [Cargo, Npm, PyPI] [Cargo, Npm, PyPI, RubyGems] `shouldSatisfy` isLeft

    it "checks native served fields independently instead of inventing policy facts" $ do
        provider <- maybe (fail "missing RubyGems benchmark provider") pure (ecosystemBenchFor RubyGems)
        let original = syntheticInfoBytes 2
            mutate needle replacement = encodeUtf8 (T.replace needle replacement (decodeUtf8 original))
        ebCheckServed provider benchGem original original `shouldReturn` Right ()
        for_
            [ syntheticInfoBytes 1
            , mutate "1.0.0 " "1.0.0-java "
            , mutate "1.0.0 " "2.0.0 "
            , mutate "benchmark-dependency:>= 0" ""
            , mutate (T.replicate 64 "0") (T.replicate 64 "1")
            , mutate "created_at:2020" "created_at:2021"
            , mutate "|checksum:" "|ruby:>= 99,checksum:"
            ]
            $ \changed -> do
                checked <- ebCheckServed provider benchGem original changed
                checked `shouldSatisfy` isLeft
