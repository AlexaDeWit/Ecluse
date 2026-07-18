-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The S3 edge of the @amazonka@ adapters: build an S3-configured @amazonka@ env,
honouring an optional custom endpoint override.

Ecosystem-agnostic and free of the composition shell: the caller (Pilot's export
loop or the proxy's advisory sync) resolves the @(secure, host, port)@ override from
configuration and passes the pre-parsed tuple in. The env is the private state a
cloud capability's smart constructor captures (the boundary
@docs\/architecture\/cloud-backends.md@ describes), so it is built here and sealed by
the capability that owns it: 'Ecluse.Runtime.Cve.Sync.newS3CveSource' for the sync
consumer, 'Ecluse.Runtime.Pilot.Export.exportToS3' for the producer.
-}
module Ecluse.Runtime.Aws.S3 (
    buildS3Env,
) where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3

{- | Build an @amazonka@ env for S3, applying an optional custom endpoint override
(the pre-parsed @(secure, host, port)@). With 'Nothing' the env uses @amazonka@'s
default endpoint and credential resolution.
-}
buildS3Env :: Maybe (Bool, Text, Int) -> IO AWS.Env
buildS3Env mEndpoint = do
    env <- AWS.newEnv AWS.discover
    pure $ case mEndpoint of
        Just endpoint -> AWS.configureService (customS3Endpoint endpoint) env
        Nothing -> env

-- The S3 service pointed at a custom endpoint, in path-addressing style: an emulator
-- or VPC endpoint has no virtual-host DNS for the bucket.
customS3Endpoint :: (Bool, Text, Int) -> AWS.Service
customS3Endpoint (secure, host, port) =
    (AWS.setEndpoint secure (encodeUtf8 host) port S3.defaultService)
        { AWS.s3AddressingStyle = AWS.S3AddressingStylePath
        }
