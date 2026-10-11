-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Cached credentials with serial minting and one recovery owner per provider.
Valid tokens remain available during outages. Expired callers wait for healthy mints,
but receive 'BreakerOpen' during owned recovery. Cancellation releases ownership without
counting a mint failure. See @docs/architecture/cloud-backends.md@ for operator policy.
-}
module Ecluse.Core.Credential.Refresh (
    -- * Configuration
    RefreshConfig (..),
    defaultRefreshConfig,

    -- * The refreshing provider
    refreshingProvider,

    -- * Telemetry reporters
    RefreshReporter (..),
    noRefreshReporter,
    CredentialReporters (..),
    noCredentialReporters,

    -- * Failure
    CredentialError (..),
) where

import Ecluse.Core.Credential.Refresh.Internal (
    CredentialError (..),
    CredentialReporters (..),
    RefreshConfig (..),
    RefreshReporter (..),
    defaultRefreshConfig,
    noCredentialReporters,
    noRefreshReporter,
    refreshingProvider,
 )
