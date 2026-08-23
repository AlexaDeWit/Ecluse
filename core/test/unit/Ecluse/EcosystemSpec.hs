-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.EcosystemSpec (spec) where

import Data.Universe.Class (universe)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName, parseEcosystem, prefixFor)

{- | The wire vocabulary of the ecosystem tag. 'ecosystemName' reads its answer out of the
table, so the round trip over 'universe' is what keeps an omitted value unrepresentable.
-}
spec :: Spec
spec = describe "Ecluse.Core.Ecosystem" $ do
    it "names every ecosystem distinctly and parses each name back" $ do
        let names = map ecosystemName universe
        map parseEcosystem names `shouldBe` map Just universe
        length (ordNub names) `shouldBe` length names

    it "rejects a name the build does not serve" $
        parseEcosystem "cargo" `shouldBe` Nothing

    it "derives a single-segment mount prefix from the name" $
        prefixFor Npm `shouldBe` ("npm" :| [])
