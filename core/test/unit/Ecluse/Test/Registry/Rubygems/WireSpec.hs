-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Test.Registry.Rubygems.WireSpec (spec) where

import Data.Aeson (eitherDecode)
import Test.Hspec

import Ecluse.Test.Registry.Rubygems.Wire (VersionListing, listingVersions)

spec :: Spec
spec = describe "Ecluse.Test.Registry.Rubygems.Wire" $ do
    it "reads each entry's number, preserving order" $ do
        let body = "[{\"number\":\"7.1.0\",\"platform\":\"ruby\"},{\"number\":\"7.0.8\"}]"
        (listingVersions <$> eitherDecode body) `shouldBe` Right ["7.1.0", "7.0.8"]

    it "reads an empty array as no versions" $
        (listingVersions <$> eitherDecode "[]") `shouldBe` Right []

    it "fails to decode when an entry lacks a number" $
        (eitherDecode "[{\"platform\":\"ruby\"}]" :: Either String VersionListing) `shouldSatisfy` isLeft
