-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | How the env builder fails a discovery, reads a body that breaks, and renders its requests: typed, and with no session token.
module Ecluse.Runtime.Aws.Env.InternalSpec (spec) where

import Amazonka qualified as AWS
import Amazonka.CodeArtifact.GetAuthorizationToken qualified as CA
import Amazonka.CodeArtifact.Types qualified as CAT
import Amazonka.Env.Hooks qualified as Hooks
import Amazonka.S3 qualified as S3
import Data.Text qualified as T
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (InternalException), parseRequest, requestHeaders)
import Test.Hspec
import UnliftIO.Exception (try)

import Ecluse.Core.Fault (TransportCause (TransportUnreachable), TransportFault (tfCause))
import Ecluse.Runtime.Aws.Env.Internal (newAwsEnvWith)
import Ecluse.Runtime.Aws.Fault (classifyAwsTransport, displayAwsFaultT)
import Ecluse.Runtime.Test.Aws (holdsNoPlantedSecret, loopbackDiscovery, refusedAtClosedPort, refusedDiscovery)
import Ecluse.Test.Stub (closedAuthority, closedPort)
import Ecluse.Test.Stub.Socket (resetAfterPartialBodyOverTcp, withRawServer)

spec :: Spec
spec = describe "newAwsEnvWith" $ do
    it "raises a discovery whose signed STS call is refused by its transport cause at the dialled host:port, never the request" $
        void (newAwsEnvWith refusedDiscovery Nothing Nothing S3.defaultService) `shouldThrow` refusedAtClosedPort

    it "redacts the session-token header in the show of a request the env signs" $ do
        env <- newAwsEnvWith (loopbackDiscovery closedPort) Nothing Nothing CAT.defaultService
        outcome <- try (AWS.runResourceT (AWS.send env (CA.newGetAuthorizationToken "a-domain")))
        case outcome of
            Left (err :: AWS.Error) -> do
                let shown = show err :: Text
                shown `shouldSatisfy` T.isInfixOf "(\"X-Amz-Security-Token\",\"<REDACTED>\")"
                shown `shouldNotSatisfy` T.isInfixOf "token-secret"
            Right _ -> expectationFailure "a closed port answered"

    it "types a body read the peer resets after its 200, and renders the fault with no session token" $
        withRawServer resetAfterPartialBodyOverTcp $ \port -> do
            env <- newAwsEnvWith (loopbackDiscovery port) Nothing Nothing S3.defaultService
            outcome <- AWS.runResourceT (AWS.sendEither env (S3.newListObjectsV2 (S3.BucketName "records")))
            case outcome of
                Left err@(AWS.TransportError (HttpExceptionRequest _ (InternalException _))) -> do
                    tfCause (classifyAwsTransport err) `shouldBe` TransportUnreachable
                    displayAwsFaultT (toException err) `shouldSatisfy` holdsNoPlantedSecret
                Left err -> expectationFailure ("expected a typed body-read reset, got " <> show err)
                Right _ -> expectationFailure "a reset body was read as a listing"

    it "redacts the SSO bearer header in the show of a request its hook passes" $ do
        env <- newAwsEnvWith (loopbackDiscovery closedPort) Nothing Nothing CAT.defaultService
        request <- parseRequest ("http://" <> toString closedAuthority)
        hooked <- Hooks.clientRequest (AWS.hooks env) env request{requestHeaders = [("x-amz-sso_bearer_token", "token-secret")]}
        let shown = show hooked :: Text
        shown `shouldSatisfy` T.isInfixOf "(\"x-amz-sso_bearer_token\",\"<REDACTED>\")"
        shown `shouldNotSatisfy` T.isInfixOf "token-secret"
