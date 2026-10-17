-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @amazonka@ environment every AWS adapter builds. The SQS backend, the S3 clients, and the
CodeArtifact mint all build theirs through 'newAwsEnv', so an emulator or a VPC endpoint is
configured the same way on every path.
-}
module Ecluse.Runtime.Aws.Env (
    AwsEndpoint (..),
    newAwsEnv,
) where

import Ecluse.Runtime.Aws.Env.Internal (AwsEndpoint (..), newAwsEnv)
