-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.BuildIdentitySpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import Ecluse.Core.BuildIdentity (productName, productVersion, userAgent)

{- | The identity an upstream sees on every outbound request. Its shape is a contract with
every operator who filters or logs on the agent, so it is pinned here, not at a call site.
-}
spec :: Spec
spec = describe "the proxy's build identity" $ do
    it "renders the User-Agent as the product name over the build version" $
        userAgent `shouldBe` encodeUtf8 (productName <> "/" <> productVersion)

    it "carries a version, so no request identifies an unnamed build" $
        productVersion `shouldNotBe` ""
