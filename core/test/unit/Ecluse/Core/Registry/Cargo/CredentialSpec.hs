-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native token presentation must not add an HTTP authentication scheme.
module Ecluse.Core.Registry.Cargo.CredentialSpec (spec) where

import Data.List (lookup)
import Network.HTTP.Client (redirectCount, requestHeaders)
import Test.Hspec

import Ecluse.Core.Credential (bareCredential, mkSecret)
import Ecluse.Core.Registry.Cargo.Credential (cargoCredential)
import Ecluse.Core.Registry.Request (attachCredential, credentialRecover, parseRequestEither)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "cargoCredential" $ do
    it "round trips the complete raw Authorization value" $ do
        let token = bareCredential (mkSecret "native token")
        request <- expectRight (parseRequestEither "https://registry.test/config.json")
        let attached = attachCredential cargoCredential (Just token) request
        lookup "Authorization" (requestHeaders attached) `shouldBe` Just "native token"
        credentialRecover cargoCredential (requestHeaders attached) `shouldBe` Just token
        redirectCount attached `shouldBe` 0
    it "does not invent a credential for an empty value" $
        credentialRecover cargoCredential [("Authorization", "")] `shouldBe` Nothing
    it "recovers nothing from more than one Authorization line" $
        credentialRecover cargoCredential [("Authorization", "first"), ("Authorization", "second")] `shouldBe` Nothing
    it "keeps a tab inside the recovered token" $
        credentialRecover cargoCredential [("Authorization", "native\ttoken")] `shouldBe` Just (bareCredential (mkSecret "native\ttoken"))
    it "recovers nothing from a control byte or a byte outside visible ASCII" $
        for_ ["native\x01token", "native\xC3\xA9token"] $ \raw ->
            credentialRecover cargoCredential [("Authorization", raw)] `shouldBe` Nothing
