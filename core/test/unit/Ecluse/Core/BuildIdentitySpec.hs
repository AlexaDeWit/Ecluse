-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.BuildIdentitySpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Client (ManagerSettings (managerModifyRequest), Request (requestHeaders), defaultManagerSettings, defaultRequest)
import Network.HTTP.Types.Header (hUserAgent)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import Ecluse.Core.BuildIdentity (identifyRequest, identityManagerSettings, parseOperatorContact, productName, productVersion, renderUserAgent, userAgent)

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

    it "renders a configured contact inside the project comment" $ do
        contact <- either (fail . toString) pure (parseOperatorContact "platform@example.com")
        renderUserAgent (Just contact)
            `shouldBe` encodeUtf8 (productName <> "/" <> productVersion <> " (+https://ecluse-proxy.com; contact: platform@example.com)")

    for_ ["a", T.replicate 256 "a", "https://example.com/contact", "a b"] $ \value ->
        it ("accepts contact " <> show value) $
            isRight (parseOperatorContact value) `shouldBe` True

    for_ ["", T.replicate 257 "a", "(", ")", "\\", "\r", "\n", "\t", "\0", "\DEL", "é"] $ \value ->
        it ("refuses contact " <> show value) $
            isLeft (parseOperatorContact value) `shouldBe` True

    it "chains the existing callback before identifying a request" $ do
        contact <- either (fail . toString) pure (parseOperatorContact "platform@example.com")
        let identity = renderUserAgent (Just contact)
            settings = defaultManagerSettings{managerModifyRequest = pure . (\request -> request{requestHeaders = ("X-Callback", "called") : requestHeaders request})}
        request <- managerModifyRequest (identityManagerSettings identity settings) (identifyRequest defaultRequest)
        requestHeaders request `shouldBe` [(hUserAgent, identity), ("X-Callback", "called")]

    it "sets a configured identity once on absent and default identities" $ do
        contact <- either (fail . toString) pure (parseOperatorContact "platform@example.com")
        let identity = renderUserAgent (Just contact)
            settings = identityManagerSettings identity defaultManagerSettings
        for_ [defaultRequest, identifyRequest defaultRequest] $ \request -> do
            once <- managerModifyRequest settings request
            twice <- managerModifyRequest settings once
            requestHeaders twice `shouldBe` [(hUserAgent, identity)]

    for_ ["custom/1", "aws-sdk-haskell/1"] $ \custom ->
        it ("preserves configured caller identity " <> toString custom) $ do
            contact <- either (fail . toString) pure (parseOperatorContact "platform@example.com")
            let identity = renderUserAgent (Just contact)
                request = defaultRequest{requestHeaders = [(hUserAgent, encodeUtf8 custom)]}
                settings = identityManagerSettings identity defaultManagerSettings
            identified <- managerModifyRequest settings request
            requestHeaders identified `shouldBe` requestHeaders request
