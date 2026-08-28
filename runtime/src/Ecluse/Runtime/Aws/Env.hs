-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @amazonka@ environment every AWS adapter builds: credentials discovered the
standard AWS way, an optional region, and an optional endpoint override.

'AwsEndpoint' is the one endpoint-override record in the tree. The SQS backend, the S3
advisory client, and the CodeArtifact mint all reach @amazonka@ through 'newAwsEnv', so an
emulator or a VPC endpoint is configured the same way on every path.
-}
module Ecluse.Runtime.Aws.Env (
    AwsEndpoint (..),
    newAwsEnv,
) where

import Amazonka qualified as AWS

{- | Where an AWS-compatible endpoint lives, for pointing an adapter at a non-default host:
a local emulator (@ministack@) in tests, or a VPC endpoint.
-}
data AwsEndpoint = AwsEndpoint
    { endpointSecure :: Bool
    -- ^ Whether to connect over HTTPS (an emulator is usually plain HTTP).
    , endpointHost :: Text
    -- ^ The host to connect to (e.g. @"localhost"@).
    , endpointPort :: Int
    -- ^ The port to connect to (e.g. @4566@ for ministack).
    }
    deriving stock (Eq, Show)

{- | Build an env for @service@. A region scopes it only when given, and an override
reconfigures @service@ only when given, so an absent value keeps @amazonka@'s own resolution.
-}
newAwsEnv :: Maybe Text -> Maybe AwsEndpoint -> AWS.Service -> IO AWS.Env
newAwsEnv mRegion mEndpoint service = do
    base <- AWS.newEnv AWS.discover
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
