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

{- | Smoke test for the one outbound-credential surface no emulator covers: CodeArtifact
@GetAuthorizationToken@. It makes a __live__ AWS call, so it is allowed to fail and never gates a
merge. It runs only when the environment configures a sandbox domain, and otherwise __pends__:

> ECL_SMOKE_CODEARTIFACT_REGION=us-east-1 \
> ECL_SMOKE_CODEARTIFACT_DOMAIN=my-sandbox-domain \
> cabal test ecluse-smoke

The optional @ECL_SMOKE_CODEARTIFACT_DOMAIN_OWNER@ overrides the owning account when the
domain is cross-account.
-}
spec :: Spec
spec = describe "live CodeArtifact GetAuthorizationToken" $
    it "mints a non-empty bearer token with an expiry" $ do
        mRegion <- lookupEnv "ECL_SMOKE_CODEARTIFACT_REGION"
        mDomain <- lookupEnv "ECL_SMOKE_CODEARTIFACT_DOMAIN"
        mOwner <- lookupEnv "ECL_SMOKE_CODEARTIFACT_DOMAIN_OWNER"
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
                        -- A usable bearer: non-empty, and carrying the real expiry the refresh
                        -- policy schedules off.
                        T.null (unSecret (authSecret token)) `shouldBe` False
                        authExpiresAt token `shouldSatisfy` isJust
            _ ->
                pendingWith
                    "CodeArtifact sandbox not configured \
                    \(set ECL_SMOKE_CODEARTIFACT_REGION + _DOMAIN); smoke test skipped"
