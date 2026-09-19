-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The AWS CodeArtifact leaf of the outbound-credential handle: mint a short-lived registry
bearer token through @GetAuthorizationToken@, carrying its real expiry so the refresh policy
schedules off the token's own lifetime. Caching, proactive refresh, single-flight, and the
breaker are the cloud-agnostic policy of "Ecluse.Core.Credential.Refresh", which this leaf
wires its mint into. This is __control plane__ only: the data plane that uses the token stays
on @http-client@. The @amazonka@ 'Env' is built once at provider creation and captured in the
mint closure, so the backend's state never reaches the proxy's @Env@. "Ecluse.Runtime.Credential.CodeArtifact.Internal" implements it.
-}
module Ecluse.Runtime.Credential.CodeArtifact (
    -- * Configuration
    CodeArtifactConfig (..),

    -- * The provider
    newCodeArtifactProvider,
) where

import Ecluse.Runtime.Credential.CodeArtifact.Internal (
    CodeArtifactConfig (..),
    newCodeArtifactProvider,
 )
