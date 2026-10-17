-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A CodeArtifact mint whose signed request carries planted secrets to a closed port.
module Ecluse.Runtime.Test.CodeArtifact (
    refusedMint,
) where

import Ecluse.Core.Credential (CredentialProvider)
import Ecluse.Core.Credential.Refresh (noCredentialReporters)
import Ecluse.Runtime.Credential.CodeArtifact.Internal (CodeArtifactConfig (..), providerForEnv)
import Ecluse.Runtime.Test.Aws (loopbackAwsEnv)
import Ecluse.Test.Stub (closedPort)

-- | Build a CodeArtifact provider, whose mint at construction fails at 'closedPort'. The domain travels in the query.
refusedMint :: IO CredentialProvider
refusedMint = do
    env <- loopbackAwsEnv closedPort
    providerForEnv noCredentialReporters env CodeArtifactConfig{caRegion = "us-east-1", caDomain = "query-secret", caDomainOwner = Nothing, caDurationSeconds = Nothing}
