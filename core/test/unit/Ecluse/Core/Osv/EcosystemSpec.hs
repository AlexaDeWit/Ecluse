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
    OsvEcosystem (OsvEcosystem, osvExportDirectory, osvWireName),
    osvEcosystemFor,
    osvEcosystemNamed,
 )

spec :: Spec
spec = do
    describe "osvEcosystemFor -- osv.dev's spelling beside Ecluse's" $ do
        it "spells npm the same on both halves, which is why one name carried both for so long" $
            osvEcosystemFor Npm `shouldBe` OsvEcosystem{osvExportDirectory = "npm", osvWireName = "npm"}

        it "reads PyPI's export from osv.dev's capitalised directory" $
            osvExportDirectory (osvEcosystemFor PyPI) `shouldBe` "PyPI"

        it "names PyPI's artifact by the wire spelling, which is what the sync reads" $
            osvWireName (osvEcosystemFor PyPI) `shouldBe` "pypi"

        it "splits RubyGems the same way" $
            osvEcosystemFor RubyGems `shouldBe` OsvEcosystem{osvExportDirectory = "RubyGems", osvWireName = "rubygems"}

        it "keeps every ecosystem's wire half equal to the tag's own name, so the two cannot drift" $
            map (osvWireName . osvEcosystemFor) universe `shouldBe` map ecosystemName (universe :: [Ecosystem])

    describe "osvEcosystemNamed -- the pair a one-shot compile's name resolves to" $ do
        it "resolves a served name to its pair, so a run against pypi reaches osv.dev's directory" $
            osvEcosystemNamed "pypi" `shouldBe` osvEcosystemFor PyPI

        it "spells an unserved name itself on both halves, so that export still compiles" $
            osvEcosystemNamed "Go" `shouldBe` OsvEcosystem{osvExportDirectory = "Go", osvWireName = "Go"}
