-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The refresh, cache, expiry, and concurrency policy behind a
'Ecluse.Core.Credential.CredentialProvider'.

The hard part of outbound auth is the /policy/ around the cloud call, not the call
itself. Serve a cached token, refresh it before it expires, never stampede the token
API, and stay up across a transient mint outage. That policy is identical for every
cloud, so it lives here once. The module parameterises it over a tiny per-cloud
'rcMint' leaf (CodeArtifact's @GetAuthorizationToken@, an ADC OAuth2 token, …) and an
injected 'rcClock'. Only 'rcMint' touches a network. Everything else is
deterministic, so a unit test drives the whole policy with a fake clock and a fake
mint (see @docs\/architecture\/cloud-backends.md@ → "Credential Provider").

== The policy

* __Proactive, background refresh.__ A refresh fires when the clock passes a fraction
  ('rcRefreshAt', ~80%) of the token's lifetime. 'rcJitter' desynchronises a cohort of
  instances, and a hard floor keeps the refresh ahead of expiry. The current token
  stays valid throughout, so the request hot path __never blocks on a mint__ in the
  common case. The refresh runs in the background and swaps the token in when it
  lands.

* __Single-flight.__ At most one mint is ever in flight per provider (an STM flag). A
  cohort of callers that crosses the threshold together therefore never stampedes the
  cloud token API. The rest serve the still-valid cached token.

* __Serve-stale on failure, behind a circuit breaker.__ A failing mint does not fail
  the caller while the cached token is still valid. The wrapper keeps serving that
  token and retries later. Repeated failures __trip a circuit breaker__ that
  fast-fails further mints for a cooldown ('rcBreakerCooldown'). A single half-open
  probe then tests recovery, so a sustained outage neither hammers the token API nor
  adds latency. Only an __expired__ token together with a still-failing mint surfaces
  as an exception to the caller. The breaker shares its shape with the effectful-rule
  tier (see @docs\/architecture\/rules-engine.md@ → "Effectful-rule failure").

A 'CredentialProvider' backs the mirror-target __write__ only. A fully failed refresh
therefore touches only the mirror publish, never the client serve path.

The implementation lives in "Ecluse.Core.Credential.Refresh.Internal". This module
re-exports only the stable surface a caller needs.
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

    -- * Failure
    CredentialError (..),
) where

import Ecluse.Core.Credential.Refresh.Internal (
    CredentialError (..),
    CredentialReporters (..),
    RefreshConfig (..),
    RefreshReporter (..),
    defaultRefreshConfig,
    noRefreshReporter,
    refreshingProvider,
 )
