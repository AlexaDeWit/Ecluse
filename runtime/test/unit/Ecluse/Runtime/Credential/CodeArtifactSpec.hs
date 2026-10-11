-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | CodeArtifact mint parsing through an HTTP stub and the shared token contract.
module Ecluse.Runtime.Credential.CodeArtifactSpec (spec) where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Amazonka qualified as AWS
import Amazonka.Auth (fromKeys)

import Ecluse.Core.Credential (AuthToken (..), mkSecret)
import Ecluse.Runtime.Credential.CodeArtifact.Internal (CodeArtifactConfig (..), providerForEnv)
import Ecluse.Test.Contract.Credential (credentialContract)
import Ecluse.Test.Credential (noCredentialReporters)

-- | The stub covers mint request mapping and token parsing without a live account.
spec :: Spec
spec =
    describe "CodeArtifact GetAuthorizationToken (stubbed endpoint)"
        $ around
            ( \use ->
                testWithApplication (pure stubApp) $
                    stubEnv >=> (\env -> providerForEnv noCredentialReporters env config >>= use)
            )
        $ credentialContract "credential/codeartifact-stub" AuthToken{authSecret = mkSecret "the-canned-token", authExpiresAt = Just (posixSecondsToUTCTime 2000000000)}
  where
    config :: CodeArtifactConfig
    config =
        CodeArtifactConfig
            { caRegion = "us-east-1"
            , caDomain = "my-domain"
            , caDomainOwner = Nothing
            , caDurationSeconds = Nothing
            }

    -- The canned GetAuthorizationToken response: status 200, JSON body carrying the
    -- token and a Unix-epoch expiration (the API reference's response shape).
    stubApp :: Application
    stubApp _req respond =
        respond
            ( responseLBS
                status200
                [(hContentType, "application/json")]
                "{\"authorizationToken\":\"the-canned-token\",\"expiration\":2000000000}"
            )

    -- The stub ignores the SigV4 signature, so any well-formed dummy credentials suffice.
    stubEnv :: Int -> IO AWS.Env
    stubEnv port = do
        base <- AWS.newEnv (pure . fromKeys (AWS.AccessKey "AKIDtestkey") (AWS.SecretKey "testsecretkey"))
        pure (AWS.overrideService (AWS.setEndpoint False "127.0.0.1" port) base)
