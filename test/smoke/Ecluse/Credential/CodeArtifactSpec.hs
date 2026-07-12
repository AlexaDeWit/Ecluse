-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Credential.CodeArtifactSpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import UnliftIO.Exception (try)

import Ecluse.Core.Credential (AuthToken (..), CredentialProvider (..), unSecret)
import Ecluse.Runtime.Credential.CodeArtifact (
    CodeArtifactConfig (..),
    newCodeArtifactProvider,
 )
import Ecluse.Test.Credential (noCredentialReporters)

{- | Smoke test for the one outbound-credential surface no emulator covers:
CodeArtifact @GetAuthorizationToken@. It makes a __live__ AWS call, so -- like the
live-registry oracles -- it is __allowed to fail by design__ and never gates a
merge.

It is __secret-gated__: it runs only when a sandbox CodeArtifact domain is
configured through the environment, and otherwise __pends__. A bare checkout, CI,
or any environment without AWS credentials therefore sees a skipped test, not a
red one. To exercise it, point it at a sandbox domain with credentials on the
ambient AWS chain (env vars, instance\/container role, SSO, STS):

> ECLUSE_SMOKE_CODEARTIFACT_REGION=us-east-1 \
> ECLUSE_SMOKE_CODEARTIFACT_DOMAIN=my-sandbox-domain \
> cabal test ecluse-smoke

The optional @ECLUSE_SMOKE_CODEARTIFACT_DOMAIN_OWNER@ overrides the owning
account when the domain is cross-account.
-}
spec :: Spec
spec = describe "live CodeArtifact GetAuthorizationToken" $
    it "mints a non-empty bearer token with an expiry" $ do
        mRegion <- lookupEnv "ECLUSE_SMOKE_CODEARTIFACT_REGION"
        mDomain <- lookupEnv "ECLUSE_SMOKE_CODEARTIFACT_DOMAIN"
        mOwner <- lookupEnv "ECLUSE_SMOKE_CODEARTIFACT_DOMAIN_OWNER"
        case (mRegion, mDomain) of
            (Just region, Just domain) -> do
                let config =
                        CodeArtifactConfig
                            { caRegion = T.pack region
                            , caDomain = T.pack domain
                            , caDomainOwner = T.pack <$> mOwner
                            , caDurationSeconds = Nothing
                            }
                outcome <- try (newCodeArtifactProvider noCredentialReporters config >>= currentToken)
                case outcome of
                    Left (e :: SomeException) ->
                        expectationFailure ("CodeArtifact mint failed: " <> show e)
                    Right token -> do
                        -- The mint reached CodeArtifact and returned a usable
                        -- bearer: a non-empty secret carrying its own expiry, so
                        -- the refresh policy can schedule off the real lifetime.
                        T.null (unSecret (authSecret token)) `shouldBe` False
                        authExpiresAt token `shouldSatisfy` isJust
            _ ->
                pendingWith
                    "CodeArtifact sandbox not configured \
                    \(set ECLUSE_SMOKE_CODEARTIFACT_REGION + _DOMAIN); smoke test skipped"
