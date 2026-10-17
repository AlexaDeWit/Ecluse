-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | How the env builder fails a discovery, and how its requests render: typed, and with no session token.
module Ecluse.Runtime.Aws.EnvSpec (spec) where

import Amazonka qualified as AWS
import Amazonka.CodeArtifact.GetAuthorizationToken qualified as CA
import Amazonka.CodeArtifact.Types qualified as CAT
import Amazonka.Env.Hooks qualified as Hooks
import Amazonka.S3 qualified as S3
import Data.Text qualified as T
import Network.HTTP.Client (parseRequest, requestHeaders)
import Test.Hspec
import UnliftIO.Exception (try)

import Ecluse.Runtime.Aws.Env (newAwsEnvWith)
import Ecluse.Runtime.Test.Aws (loopbackDiscovery, refusedAtClosedPort, withRefusedDiscovery)
import Ecluse.Test.Stub (closedAuthority, closedPort)

spec :: Spec
spec = describe "newAwsEnvWith" $ do
    it "raises a discovery whose STS call is refused by its transport cause at the dialled host:port, never the request" $
        withRefusedDiscovery $ \discovery ->
            void (newAwsEnvWith discovery Nothing Nothing S3.defaultService) `shouldThrow` refusedAtClosedPort

    it "redacts the session-token header in the show of a request the env signs" $ do
        env <- newAwsEnvWith (loopbackDiscovery closedPort) Nothing Nothing CAT.defaultService
        outcome <- try (AWS.runResourceT (AWS.send env (CA.newGetAuthorizationToken "a-domain")))
        case outcome of
            Left (err :: AWS.Error) -> do
                let shown = show err :: Text
                shown `shouldSatisfy` T.isInfixOf "(\"X-Amz-Security-Token\",\"<REDACTED>\")"
                shown `shouldNotSatisfy` T.isInfixOf "token-secret"
            Right _ -> expectationFailure "a closed port answered"

    it "redacts the SSO bearer header in the show of a request its hook passes" $ do
        env <- newAwsEnvWith (loopbackDiscovery closedPort) Nothing Nothing CAT.defaultService
        request <- parseRequest ("http://" <> toString closedAuthority)
        hooked <- Hooks.clientRequest (AWS.hooks env) env request{requestHeaders = [("x-amz-sso_bearer_token", "token-secret")]}
        let shown = show hooked :: Text
        shown `shouldSatisfy` T.isInfixOf "(\"x-amz-sso_bearer_token\",\"<REDACTED>\")"
        shown `shouldNotSatisfy` T.isInfixOf "token-secret"
