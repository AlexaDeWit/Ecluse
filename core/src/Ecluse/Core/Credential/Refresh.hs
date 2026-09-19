-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The refresh, cache, expiry, and concurrency policy behind a
'Ecluse.Core.Credential.CredentialProvider'.

The policy is identical for every cloud, so it lives here once, parameterised over a per-cloud
'rcMint' leaf and an injected 'rcClock'. Only 'rcMint' touches a network.
"Ecluse.Core.Credential.Refresh.Internal" implements it.

== What reaches a caller

It serves a cached token, refreshes it in the background under a single-flight claim before
expiry, and keeps serving a valid one through a mint outage behind a circuit breaker, so only
an expired token with a still-failing mint reaches a caller as an exception
(@docs\/architecture\/cloud-backends.md@).
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
