-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The S3 edge of the @amazonka@ adapters: an S3-configured env over an optional
endpoint override, built through 'Ecluse.Runtime.Aws.Env.newAwsEnv'.

Ecosystem-agnostic and free of the composition shell. The caller (Pilot's export loop or
the proxy's advisory sync) passes the resolved override down. The env is the private state
a cloud capability's smart constructor captures, the boundary
@docs\/architecture\/cloud-backends.md@ describes. It is built here and sealed by the
capability that owns it: 'Ecluse.Runtime.Cve.Sync.newS3CveSource' for the sync consumer,
'Ecluse.Runtime.Pilot.Export.exportToS3' for the producer.
-}
module Ecluse.Runtime.Aws.S3 (
    buildS3Env,
) where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3

import Ecluse.Runtime.Aws.Env (AwsEndpoint, newAwsEnv)

{- | Build an env for S3 under an optional endpoint override. 'Nothing' keeps @amazonka@'s
default endpoint and credential resolution.
-}
buildS3Env :: Maybe AwsEndpoint -> IO AWS.Env
buildS3Env mEndpoint = newAwsEnv Nothing mEndpoint pathAddressedS3

-- Path-addressing style, which only takes effect under an override: an emulator or VPC
-- endpoint has no virtual-host DNS for the bucket.
pathAddressedS3 :: AWS.Service
pathAddressedS3 = S3.defaultService{AWS.s3AddressingStyle = AWS.S3AddressingStylePath}
