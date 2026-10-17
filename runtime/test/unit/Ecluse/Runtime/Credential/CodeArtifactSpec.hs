-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The CodeArtifact mint through HTTP stubs and a closed port: the shared token contract, and how a failed mint renders.
module Ecluse.Runtime.Credential.CodeArtifactSpec (spec) where

import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Types (hContentType, status200, status403)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO (timeout)
import UnliftIO.Exception (tryAny)

import Amazonka qualified as AWS
import Amazonka.Auth (fromKeys)

import Ecluse.Core.Credential (AuthToken (..), CredentialProvider, mkSecret)
import Ecluse.Core.Fault (TransportCause (TransportUnreachable))
import Ecluse.Core.Supervision (BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros), superviseLoop, transientPolicy)
import Ecluse.Runtime.Aws.Fault (AwsFault (AwsStatus, AwsTransport), AwsRequestFailed (AwsRequestFailed))
import Ecluse.Runtime.Credential.CodeArtifact.Internal (CodeArtifactConfig (..), providerForEnv)
import Ecluse.Runtime.Test.CodeArtifact (closedEndpoint, refusedMint, sessionSecrets)
import Ecluse.Test.Contract.Credential (credentialContract)
import Ecluse.Test.Credential (noCredentialReporters)
import Ecluse.Test.Log (runJsonLog)

-- | The stubs cover request mapping, token parsing and failures without a live account.
spec :: Spec
spec = do
    describe "CodeArtifact GetAuthorizationToken (stubbed endpoint)" $
        around (withStubbedProvider tokenApp) $
            credentialContract "credential/codeartifact-stub" AuthToken{authSecret = mkSecret "the-canned-token", authExpiresAt = Just (posixSecondsToUTCTime 2000000000)}

    describe "a failed mint" $ do
        it "raises a refused connection by its cause at the dialled host:port, never the signed request" $
            refusedMint `shouldThrow` (== AwsRequestFailed closedEndpoint (AwsTransport TransportUnreachable))

        it "reaches the supervision line by that cause, with none of the request's secrets" $ do
            logged <- runJsonLog . void . timeout 200_000 . tryAny $ superviseLoop (transientPolicy "worker" fastRetry) (liftIO (void refusedMint))
            logged `shouldSatisfy` T.isInfixOf ("AWS request failed at " <> closedEndpoint <> ": the peer could not be reached")
            for_ sessionSecrets $ \secret -> logged `shouldSatisfy` (not . T.isInfixOf secret)

        it "raises a service refusal by its status and code at the endpoint, never the message that quotes the signed request" $
            testWithApplication (pure refusingApp) $ \port ->
                (stubEnv port >>= \env -> providerForEnv noCredentialReporters env stubConfig)
                    `shouldThrow` (== AwsRequestFailed ("127.0.0.1:" <> show port) (AwsStatus 403 "InvalidSignature"))
  where
    fastRetry = BackoffSchedule{bsBaseMicros = 1_000, bsCapMicros = 8_000}

-- Serve the application, and build a provider whose mint dials it.
withStubbedProvider :: Application -> (CredentialProvider -> IO ()) -> IO ()
withStubbedProvider application use =
    testWithApplication (pure application) $
        stubEnv >=> (\env -> providerForEnv noCredentialReporters env stubConfig >>= use)

stubConfig :: CodeArtifactConfig
stubConfig =
    CodeArtifactConfig
        { caRegion = "us-east-1"
        , caDomain = "my-domain"
        , caDomainOwner = Nothing
        , caDurationSeconds = Nothing
        }

-- The canned GetAuthorizationToken response: the token and a Unix-epoch expiration, as the API reference shapes it.
tokenApp :: Application
tokenApp _req respond =
    respond
        ( responseLBS
            status200
            [(hContentType, "application/json")]
            "{\"authorizationToken\":\"the-canned-token\",\"expiration\":2000000000}"
        )

-- A signature refusal whose message quotes a canonical request, session-token header included.
refusingApp :: Application
refusingApp _req respond =
    respond
        ( responseLBS
            status403
            [(hContentType, "application/json")]
            "{\"__type\":\"InvalidSignatureException\",\"message\":\"The Canonical String for this request should have been 'POST /v1/authorization-token x-amz-security-token:token-secret'\"}"
        )

-- The stub ignores the SigV4 signature, so any well-formed dummy credentials suffice.
stubEnv :: Int -> IO AWS.Env
stubEnv port = do
    base <- AWS.newEnv (pure . fromKeys (AWS.AccessKey "AKIDtestkey") (AWS.SecretKey "testsecretkey"))
    pure (AWS.overrideService (AWS.setEndpoint False "127.0.0.1" port) base)
