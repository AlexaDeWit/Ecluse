-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The CodeArtifact mint through HTTP stubs and a closed port: the shared token contract, and how a failed mint renders.
module Ecluse.Runtime.Credential.CodeArtifactSpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Types (hContentType, status200, status403)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.Core.Credential (AuthToken (..), CredentialProvider, mkSecret)
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), renderTransportCause)
import Ecluse.Core.Supervision (FaultDisposition (Transient))
import Ecluse.Runtime.Aws.Fault (AwsAnswer (AwsAnswer), AwsFault (AwsStatus), AwsParty (AwsEndpointAt), AwsRequestFailed (AwsRequestFailed))
import Ecluse.Runtime.Credential.CodeArtifact.Internal (CodeArtifactConfig (..), providerForEnv)
import Ecluse.Runtime.Test.Aws (canonicalRequestMessage, loopbackAwsEnv, plantedHeader, refusedAtClosedPort)
import Ecluse.Runtime.Test.CodeArtifact (refusedMint)
import Ecluse.Test.Contract.Credential (credentialContract)
import Ecluse.Test.Credential (noCredentialReporters)
import Ecluse.Test.Log (runJsonLog)
import Ecluse.Test.Stub (closedAuthority, requestSecrets)
import Ecluse.Test.Supervision (superviseBriefly)

-- | The stubs cover request mapping, token parsing and failures without a live account.
spec :: Spec
spec = do
    describe "CodeArtifact GetAuthorizationToken (stubbed endpoint)" $
        around (withStubbedProvider tokenApp) $
            credentialContract "credential/codeartifact-stub" AuthToken{authSecret = mkSecret "the-canned-token", authExpiresAt = Just (posixSecondsToUTCTime 2000000000)}

    describe "a failed mint" $ do
        it "raises a refused connection by its transport cause at the dialled host:port, never the signed request" $
            refusedMint `shouldThrow` refusedAtClosedPort

        it "reaches the supervision line by that cause, with none of the request's secrets" $ do
            logged <- runJsonLog (superviseBriefly Transient (liftIO refusedMint))
            logged `shouldSatisfy` T.isInfixOf ("AWS request failed at " <> closedAuthority <> ": ConnectionFailure")
            logged `shouldSatisfy` T.isInfixOf ("(" <> renderTransportCause TransportUnreachable <> ")")
            for_ requestSecrets $ \secret -> logged `shouldNotSatisfy` T.isInfixOf secret

        it "raises a signature refusal by its code, status and request id at the endpoint, never the canonical request it quotes" $
            testWithApplication (pure refusingApp) $ \port ->
                (loopbackAwsEnv port >>= \env -> providerForEnv noCredentialReporters env stubConfig)
                    `shouldThrow` (== AwsRequestFailed (AwsEndpointAt "127.0.0.1" port) (AwsStatus (AwsAnswer 403 "InvalidSignature" (Just "req-1") Nothing)))

-- Serve the application, and build a provider whose mint dials it.
withStubbedProvider :: Application -> (CredentialProvider -> IO ()) -> IO ()
withStubbedProvider application use =
    testWithApplication (pure application) $
        loopbackAwsEnv >=> (\env -> providerForEnv noCredentialReporters env stubConfig >>= use)

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

-- A signature refusal whose message quotes the canonical request, beside a planted response header.
refusingApp :: Application
refusingApp _req respond =
    respond
        ( responseLBS
            status403
            [(hContentType, "application/json"), ("x-amzn-RequestId", "req-1"), plantedHeader]
            (encode (object ["__type" .= ("InvalidSignatureException" :: Text), "message" .= canonicalRequestMessage]))
        )
