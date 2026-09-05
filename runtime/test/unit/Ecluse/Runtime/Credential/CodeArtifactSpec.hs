-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Credential.CodeArtifactSpec (spec) where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Amazonka qualified as AWS
import Amazonka.Auth (fromKeys)

import Ecluse.Core.Credential (AuthToken (..), CredentialProvider (..), unSecret)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig (..), providerForEnv)
import Ecluse.Test.Credential (noCredentialReporters)

{- | Component test for the CodeArtifact credential leaf with no live AWS. An in-process HTTP
stub answers @GetAuthorizationToken@, driving the real request build, SigV4 signing, response
parse, and token plus expiry extraction. The mint is a control-plane AWS API call, not the npm
protocol, so an npm-registry emulator cannot stand in.
-}
spec :: Spec
spec = describe "CodeArtifact GetAuthorizationToken (stubbed endpoint)" $
    it "mints the token and expiry the endpoint returns, without live AWS" $
        testWithApplication (pure stubApp) $ \port -> do
            env <- stubEnv port
            token <- providerForEnv noCredentialReporters env config >>= currentToken
            unSecret (authSecret token) `shouldBe` "the-canned-token"
            authExpiresAt token `shouldBe` Just (posixSecondsToUTCTime 2000000000)
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
