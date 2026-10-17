-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @amazonka@ environment every AWS adapter builds. The SQS backend, the S3 clients, and the
CodeArtifact mint all reach @amazonka@ through 'newAwsEnv', so an emulator or a VPC endpoint is
configured the same way on every path.
-}
module Ecluse.Runtime.Aws.Env (
    AwsEndpoint (..),
    newAwsEnv,
    newAwsEnvWith,
) where

import Amazonka qualified as AWS

import Ecluse.Runtime.Aws.Fault (retypeAwsFailure)

-- | An AWS-compatible endpoint on a non-default host: a local emulator (@ministack@) in tests, or a VPC endpoint.
data AwsEndpoint = AwsEndpoint
    { endpointSecure :: Bool
    -- ^ Whether to connect over HTTPS (an emulator is usually plain HTTP).
    , endpointHost :: Text
    -- ^ The host to connect to (e.g. @"localhost"@).
    , endpointPort :: Int
    -- ^ The port to connect to (e.g. @4566@ for ministack).
    }
    deriving stock (Eq, Show)

-- | An env for @service@ over discovered credentials. A failed AWS or HTTP call during discovery raises 'Ecluse.Runtime.Aws.Fault.AwsRequestFailed'.
newAwsEnv :: Maybe Text -> Maybe AwsEndpoint -> AWS.Service -> IO AWS.Env
newAwsEnv = newAwsEnvWith AWS.discover

-- | 'newAwsEnv' over a caller's discovery, so a test can fail it at a stub. An absent region or override keeps @amazonka@'s own resolution.
newAwsEnvWith :: (AWS.EnvNoAuth -> IO AWS.Env) -> Maybe Text -> Maybe AwsEndpoint -> AWS.Service -> IO AWS.Env
newAwsEnvWith discovery mRegion mEndpoint service = do
    base <- retypeAwsFailure (AWS.newEnv discovery)
    pure (overridden (scoped base))
  where
    scoped env = maybe env (\region -> env{AWS.region = AWS.Region' region}) mRegion

    overridden env = maybe env (\endpoint -> AWS.configureService (pointedAt endpoint) env) mEndpoint

    pointedAt endpoint =
        AWS.setEndpoint
            (endpointSecure endpoint)
            (encodeUtf8 (endpointHost endpoint))
            (endpointPort endpoint)
            service
