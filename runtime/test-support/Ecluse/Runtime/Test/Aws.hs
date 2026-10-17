-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Loopback @amazonka@ envs and planted AWS answers, so a spec can check that no reader of a failure prints a secret.
module Ecluse.Runtime.Test.Aws (
    -- * Loopback envs
    loopbackAwsEnv,
    refusedDiscovery,

    -- * Planted answers
    canonicalRequestMessage,
    plantedHeader,
    plantedRefusal,

    -- * Readers
    awsTransportCause,
    refusedAtClosedPort,
) where

import Amazonka qualified as AWS
import Amazonka.Auth (fromSession)
import Amazonka.Auth.STS (fromAssumedRole)
import Data.Text qualified as T
import Network.HTTP.Types (Header, status403)

import Ecluse.Core.Fault (TransportCause (TransportUnreachable))
import Ecluse.Runtime.Aws.Fault (AwsFault (AwsHttp, AwsStatus, AwsUndecodable), AwsParty (AwsEndpointAt), AwsRequestFailed (arfFault, arfParty))
import Ecluse.Test.Fault (httpTransportCause)
import Ecluse.Test.Stub (closedPort, requestSecrets)

-- | A single-attempt env at a loopback port that signs with session credentials, as an IRSA, ECS or SSO role does.
loopbackAwsEnv :: Int -> IO AWS.Env
loopbackAwsEnv port = AWS.newEnv (loopbackAt port)

-- | A discovery that assumes a role over session credentials at 'closedPort', so its refused STS call carries the session token in a header.
refusedDiscovery :: AWS.EnvNoAuth -> IO AWS.Env
refusedDiscovery = fromAssumedRole "arn:aws:iam::123456789012:role/ecluse" "ecluse" <=< loopbackAt closedPort

-- | A signature refusal's message: its first line, then the canonical request AWS expected, session token included.
canonicalRequestMessage :: Text
canonicalRequestMessage =
    "The request signature we calculated does not match the signature you provided.\n\nThe Canonical String for this request should have been\n'POST\n/\n\nhost:127.0.0.1\nx-amz-security-token:token-secret\n'"

-- | A response header with a secret value, which no rendering of the answer may carry.
plantedHeader :: Header
plantedHeader = ("x-amzn-planted", "header-secret")

-- | A 403 from @service@ with this error code and message, the 'plantedHeader', and request id @req-1@.
plantedRefusal :: AWS.Abbrev -> Text -> Text -> AWS.Error
plantedRefusal service code message =
    AWS.ServiceError (AWS.ServiceError' service status403 [plantedHeader] (AWS.newErrorCode code) (Just (AWS.ErrorMessage message)) (Just (AWS.RequestId "req-1")))

-- | The transport cause a raised fault carries, when the transport failed.
awsTransportCause :: AwsFault -> Maybe TransportCause
awsTransportCause = \case
    AwsStatus _ -> Nothing
    AwsUndecodable _ -> Nothing
    AwsHttp http -> httpTransportCause http

-- | Whether a request failed unanswered at 'closedPort', and renders with none of the 'requestSecrets'.
refusedAtClosedPort :: AwsRequestFailed -> Bool
refusedAtClosedPort failed =
    arfParty failed == AwsEndpointAt "127.0.0.1" closedPort
        && awsTransportCause (arfFault failed) == Just TransportUnreachable
        && not (any (`T.isInfixOf` toText (displayException failed)) requestSecrets)

loopbackAt :: Int -> AWS.EnvNoAuth -> IO AWS.Env
loopbackAt port = pure . pointedAtLoopback port . fromSession (AWS.AccessKey "AKIDtestkey") (AWS.SecretKey "testsecretkey") (AWS.SessionToken "token-secret")

-- Path-style addressing reaches S3 at the port. Every other service ignores it.
pointedAtLoopback :: Int -> AWS.Env' withAuth -> AWS.Env' withAuth
pointedAtLoopback port env =
    (AWS.once (AWS.overrideService (\service -> AWS.setEndpoint False "127.0.0.1" port service{AWS.s3AddressingStyle = AWS.S3AddressingStylePath}) env))
        { AWS.retryCheck = \_ _ -> False
        }
