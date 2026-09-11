-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Core.Osv.EcosystemSpec (spec) where

import Prelude hiding (universe)

import Data.Universe.Class (Universe (universe))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Osv.Ecosystem (
    OsvEcosystem (OsvEcosystem, osvEcosystemTag, osvExportDirectory, osvMaxAdvisoryFanOut, osvWireName),
    osvEcosystemFor,
    osvEcosystemNamed,
 )

spec :: Spec
spec = do
    describe "osvEcosystemFor -- osv.dev's spelling beside Ecluse's" $ do
        it "spells npm the same on both halves" $
            osvEcosystemFor Npm `shouldBe` OsvEcosystem{osvExportDirectory = "npm", osvWireName = "npm", osvEcosystemTag = Just Npm, osvMaxAdvisoryFanOut = 256}

        it "reads PyPI's export from osv.dev's capitalised directory" $
            osvExportDirectory (osvEcosystemFor PyPI) `shouldBe` "PyPI"

        it "names PyPI's artifact by the wire spelling, which is what the sync reads" $
            osvWireName (osvEcosystemFor PyPI) `shouldBe` "pypi"

        it "splits RubyGems the same way" $
            osvEcosystemFor RubyGems `shouldBe` OsvEcosystem{osvExportDirectory = "RubyGems", osvWireName = "rubygems", osvEcosystemTag = Just RubyGems, osvMaxAdvisoryFanOut = 256}

        it "sizes the PyPI fan-out bound above the widest advisory that feed publishes" $
            -- The largest PyPI advisory measured names 2459 ranges, so an ordinary compile
            -- stays quiet and a genuinely anomalous one still speaks.
            osvMaxAdvisoryFanOut (osvEcosystemFor PyPI) `shouldBe` 4096

        it "keeps npm on the bound its own export was measured against" $
            osvMaxAdvisoryFanOut (osvEcosystemFor Npm) `shouldBe` 256

        it "keeps every ecosystem's wire half equal to the tag's own name, so the two cannot drift" $
            map (osvWireName . osvEcosystemFor) universe `shouldBe` map ecosystemName (universe :: [Ecosystem])

    describe "osvEcosystemNamed -- what a one-shot compile's name resolves to" $ do
        it "resolves a served name to its pair, so a run against pypi reaches osv.dev's directory" $
            osvEcosystemNamed "pypi" `shouldBe` osvEcosystemFor PyPI

        it "spells an unserved name itself on both halves, so that export still compiles" $
            osvEcosystemNamed "Go" `shouldBe` OsvEcosystem{osvExportDirectory = "Go", osvWireName = "Go", osvEcosystemTag = Nothing, osvMaxAdvisoryFanOut = 256}

        it "carries no version grammar for an unserved name, so its bounds go unjudged" $
            osvEcosystemTag (osvEcosystemNamed "Go") `shouldBe` Nothing
