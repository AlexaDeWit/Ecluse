-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Loopback @amazonka@ envs and planted AWS answers, so a spec can check that no reader of a failure prints a secret.
module Ecluse.Runtime.Test.Aws (
    -- * Loopback envs
    loopbackAwsEnv,
    loopbackDiscovery,
    withRefusedDiscovery,

    -- * Planted answers
    canonicalRequestMessage,
    plantedHeader,
    plantedRefusal,
    refreshFailures,
    deliverRefreshFailure,

    -- * Readers
    awsTransportCause,
    refusedAtClosedPort,
    holdsNoPlantedSecret,
) where

import Amazonka qualified as AWS
import Amazonka.Auth (fromSession)
import Amazonka.Auth.Exception (AuthError (AuthServiceError, OtherAuthError, RetrievalError))
import Amazonka.Auth.STS (fromWebIdentity)
import Control.Concurrent (myThreadId)
import Control.Exception qualified as Exception
import Data.Text qualified as T
import Network.HTTP.Types (Header, status403)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Core.Fault (TransportCause (TransportUnreachable))
import Ecluse.Runtime.Aws.Fault (AwsFault (AwsHttp, AwsStatus), AwsParty (AwsEndpointAt), AwsRequestFailed (arfFault, arfParty))
import Ecluse.Test.Fault (httpTransportCause)
import Ecluse.Test.Stub (closedPort, refusedFetch, requestSecrets, signedRequest)

-- | A single-attempt env at a loopback port that signs with session credentials, as an IRSA, ECS or SSO role does.
loopbackAwsEnv :: Int -> IO AWS.Env
loopbackAwsEnv = AWS.newEnv . loopbackDiscovery

-- | A discovery that finds those session credentials and points the env at a loopback port.
loopbackDiscovery :: Int -> AWS.EnvNoAuth -> IO AWS.Env
loopbackDiscovery port = pure . pointedAtLoopback port . fromSession (AWS.AccessKey "AKIDtestkey") (AWS.SecretKey "testsecretkey") (AWS.SessionToken "token-secret")

-- | Hand the action a credential discovery that asks STS for a web identity's credentials at 'closedPort'.
withRefusedDiscovery :: ((AWS.EnvNoAuth -> IO AWS.Env) -> IO a) -> IO a
withRefusedDiscovery use =
    withSystemTempDirectory "web-identity" $ \dir -> do
        let tokenFile = dir <> "/token"
        writeFileText tokenFile "token-secret"
        use (fromWebIdentity tokenFile "arn:aws:iam::123456789012:role/ecluse" Nothing . pointedAtLoopback closedPort)

-- | A signature refusal's message: its first line, then the canonical request AWS expected, session token included.
canonicalRequestMessage :: Text
canonicalRequestMessage =
    "The request signature we calculated does not match the signature you provided.\n\nThe Canonical String for this request should have been\n'POST\n/\n\nhost:127.0.0.1\nx-amz-security-token:token-secret\n'"

-- | A response header with a secret value, which no rendering of the answer may carry.
plantedHeader :: Header
plantedHeader = ("x-amzn-planted", "header-secret")

-- | A 403 from @service@ with this error code and message, the 'plantedHeader', and request id @req-1@.
plantedRefusal :: AWS.Abbrev -> Text -> Text -> AWS.Error
plantedRefusal service code message = AWS.ServiceError (plantedServiceError service code message)

-- | What @amazonka@'s background refresh hands the discovering thread when a refresh carrying 'requestSecrets' fails.
refreshFailures :: IO [AuthError]
refreshFailures = do
    refused <- refusedFetch =<< signedRequest
    pure
        [ RetrievalError refused
        , AuthServiceError (plantedServiceError "SSO" "UnauthorizedException" canonicalRequestMessage)
        , OtherAuthError (toException (AWS.TransportError refused))
        ]

-- | The transport cause a raised fault carries, when the transport failed.
awsTransportCause :: AwsFault -> Maybe TransportCause
awsTransportCause = \case
    AwsStatus _ -> Nothing
    AwsHttp http -> httpTransportCause http

-- | Whether a request failed unanswered at 'closedPort', and renders with no planted secret.
refusedAtClosedPort :: AwsRequestFailed -> Bool
refusedAtClosedPort failed =
    arfParty failed == AwsEndpointAt "127.0.0.1" closedPort
        && awsTransportCause (arfFault failed) == Just TransportUnreachable
        && holdsNoPlantedSecret (toText (displayException failed))

-- | Hand the calling thread a refresh failure the way amazonka does. Base 'Exception.throwTo' leaves it a synchronous exception to a handler.
deliverRefreshFailure :: AuthError -> IO ()
deliverRefreshFailure failure = myThreadId >>= (`Exception.throwTo` failure)

-- | Whether a rendering holds none of the 'requestSecrets' and not the 'plantedHeader'.
holdsNoPlantedSecret :: Text -> Bool
holdsNoPlantedSecret rendered = not (any (`T.isInfixOf` rendered) (decodeUtf8 (snd plantedHeader) : requestSecrets))

plantedServiceError :: AWS.Abbrev -> Text -> Text -> AWS.ServiceError
plantedServiceError service code message =
    AWS.ServiceError' service status403 [plantedHeader] (AWS.newErrorCode code) (Just (AWS.ErrorMessage message)) (Just (AWS.RequestId "req-1"))

-- Path-style addressing reaches S3 at the port. Every other service ignores it.
pointedAtLoopback :: Int -> AWS.Env' withAuth -> AWS.Env' withAuth
pointedAtLoopback port env =
    (AWS.once (AWS.overrideService (\service -> AWS.setEndpoint False "127.0.0.1" port service{AWS.s3AddressingStyle = AWS.S3AddressingStylePath}) env))
        { AWS.retryCheck = \_ _ -> False
        }
