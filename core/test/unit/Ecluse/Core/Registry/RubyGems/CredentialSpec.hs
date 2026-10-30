-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.RubyGems.CredentialSpec (spec) where

import Data.List (lookup)
import Data.Text qualified as T
import Network.HTTP.Client qualified as Client
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Ecluse.Core.Credential (ClientCredential (ClientCredential), bareCredential, mkSecret)
import Ecluse.Core.Registry.Adapter.Capability (PublishExtras (..))
import Ecluse.Core.Registry.Request (StoreOperation (StoreRead, StoreWrite), attachCredential, credentialRecover, presentedMapping)
import Ecluse.Core.Registry.RubyGems.Credential
import Ecluse.Test.Credential (basicReadsBearerWrites)
import Ecluse.Test.Support (parseRequestOrFail)

spec :: Spec
spec = do
    describe "native Ruby credentials" $ do
        it "preserves a private caller's Basic pair" $
            credentialRecover rubyReadCredential [("Authorization", "Basic YWxpY2U6c2VjcmV0")]
                `shouldBe` Just (ClientCredential (Just "alice") (mkSecret "secret"))
        it "keeps publication key text without scheme guessing" $ do
            credentialRecover rubyPublishCredential [("Authorization", "Bearer native-token")]
                `shouldBe` Just (bareCredential (mkSecret "Bearer native-token"))
            request <- parseRequestOrFail "https://registry.test/api/v1/gems"
            let key = Just (bareCredential (mkSecret "Bearer native-token"))
            lookup "Authorization" (Client.requestHeaders (attachCredential rubyPublishCredential key request))
                `shouldBe` Just "Bearer native-token"
        it "rejects duplicate publication credentials" $
            credentialRecover rubyPublishCredential [("Authorization", "first"), ("Authorization", "second")]
                `shouldBe` Nothing
        it "recovers a publication key with a tab whole and refuses control or non-ASCII bytes" $ do
            credentialRecover rubyPublishCredential [("Authorization", "native\tkey")]
                `shouldBe` Just (bareCredential (mkSecret "native\tkey"))
            for_ ["native\x01key", "native\xC3\xA9\&key"] $ \value ->
                credentialRecover rubyPublishCredential [("Authorization", value)] `shouldBe` Nothing
        it "renders a store-declared Bearer write beside the Basic read and pins redirects" $ do
            request <- parseRequestOrFail "https://registry.test/ruby/repository"
            let key = Just (bareCredential (mkSecret "secret"))
                reading = attachCredential (presentedMapping basicReadsBearerWrites StoreRead rubyReadCredential) (Just (ClientCredential (Just "aws") (mkSecret "secret"))) request
                writing = attachCredential (presentedMapping basicReadsBearerWrites StoreWrite rubyPublishCredential) key request
            lookup "Authorization" (Client.requestHeaders reading) `shouldBe` Just "Basic YXdzOnNlY3JldA=="
            lookup "Authorization" (Client.requestHeaders writing) `shouldBe` Just "Bearer secret"
            Client.redirectCount reading `shouldBe` 0
            Client.redirectCount writing `shouldBe` 0
        it "keeps public reads anonymous" $ do
            request <- parseRequestOrFail "https://registry.test/info/example"
            lookup "Authorization" (Client.requestHeaders (attachCredential rubyReadCredential Nothing request)) `shouldBe` Nothing
    describe "publication OTP" $ do
        it "retains only the native OTP header" $
            fmap rubyOtpHeaders (rubyPublishExtras [("OTP", "123456"), ("X-Secret", "ignored")])
                `shouldBe` Right [("OTP", "123456")]
        it "keeps OTP values redacted" $
            show (PublishOneTimePassword (mkSecret "123456")) `shouldSatisfy` (not . T.isInfixOf "123456")
        it "refuses duplicate, empty, control and oversized values" $
            for_ [[("OTP", "a"), ("OTP", "b")], [("OTP", "")], [("OTP", "a\r\nb")], [("OTP", " ")], [("OTP", encodeUtf8 (T.replicate 1025 "a"))]] $ \headers ->
                rubyPublishExtras headers `shouldSatisfy` isLeft
        it "does not invent OTP for a worker upload" $
            rubyPublishExtras [] `shouldBe` Right NoPublishExtras
