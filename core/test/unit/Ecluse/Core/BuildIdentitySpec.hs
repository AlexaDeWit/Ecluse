-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.BuildIdentitySpec (spec) where

import Network.HTTP.Client (Request (requestHeaders), defaultRequest)
import Network.HTTP.Types.Header (hUserAgent)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import Ecluse.Core.BuildIdentity (identifyRequest, productName, productVersion, userAgent)

spec :: Spec
spec = describe "the proxy's build identity" $ do
    it "renders the product, version and project URL" $
        userAgent `shouldBe` encodeUtf8 (productName <> "/" <> productVersion <> " (+https://ecluse-proxy.com)")

    it "carries a version, so no request identifies an unnamed build" $
        productVersion `shouldNotBe` ""

    it "adds the identity once without changing other request headers" $ do
        let request = defaultRequest{requestHeaders = [("Accept", "application/json")]}
        requestHeaders (identifyRequest (identifyRequest request))
            `shouldBe` [(hUserAgent, userAgent), ("Accept", "application/json")]

    it "preserves the caller's identity" $ do
        let request = defaultRequest{requestHeaders = [(hUserAgent, "custom/1")]}
        requestHeaders (identifyRequest request) `shouldBe` requestHeaders request
