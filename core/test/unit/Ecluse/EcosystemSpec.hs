-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
module Ecluse.EcosystemSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm), parseEcosystem, prefixFor)
import Ecluse.Test.WireVocab (wireRoundTrips)

spec :: Spec
spec = describe "Ecluse.Core.Ecosystem" $ do
    wireRoundTrips @Ecosystem

    it "rejects a name the build does not serve" $
        parseEcosystem "cargo" `shouldBe` Nothing

    it "derives a single-segment mount prefix from the name" $
        prefixFor Npm `shouldBe` ("npm" :| [])
