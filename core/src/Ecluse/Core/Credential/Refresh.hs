{- | The refresh / cache / expiry / concurrency policy behind a
'Ecluse.Core.Credential.CredentialProvider'.

The interesting part of outbound auth is not the cloud call but the /policy/
around it: serve a cached token, refresh it proactively before it expires, never
stampede the token API, and stay up across a transient mint outage. That policy
is identical for every cloud, so it lives here once, parameterised over a tiny
per-cloud 'rcMint' leaf (CodeArtifact's @GetAuthorizationToken@, an ADC OAuth2
token, …) and an injected 'rcClock'. Only 'rcMint' touches a network; everything
else is deterministic, so the whole policy is unit-tested with a fake clock and a
fake mint (see @docs\/architecture\/cloud-backends.md@ → "Credential Provider").

== The policy

* __Proactive, background refresh.__ A token is refreshed when the clock passes a
  fraction ('rcRefreshAt', ~80%) of its lifetime, with 'rcJitter' to desynchronise
  a cohort of instances, plus a hard floor near expiry. Because the current token
  stays valid during the refresh, the request hot path __never blocks on a mint__
  in the common case — the refresh runs in the background and swaps the token in
  when it lands.

* __Single-flight.__ At most one mint is ever in flight per provider (an STM flag),
  so a cohort of callers crossing the threshold together never stampedes the cloud
  token API; the rest serve the still-valid cached token.

* __Serve-stale on failure, behind a circuit breaker.__ A failing mint does not
  fail the caller while the cached token is still valid — the wrapper keeps serving
  it and retries later. Repeated failures __trip a circuit breaker__ that fast-fails
  further mints for a cooldown ('rcBreakerCooldown') before a single half-open
  probe tests recovery, so a sustained outage neither hammers the token API nor
  adds latency. Only an __expired__ token together with a still-failing mint
  surfaces as an exception to the caller (the breaker shares its shape with the
  effectful-rule tier — see
  @docs\/architecture\/rules-engine.md@ → "Effectful-rule failure").

A 'CredentialProvider' always backs the mirror-target __write__; under the default
@passthrough@ access strategy that is its only use, so even a fully failed refresh
touches only the mirror publish and never the client serve path. Where a mount
instead puts a provider on the private-upstream __read__ (the @service@ and
service-populated @delegated-cache@ strategies), that dependent operation /is/ a
client read, so an exhausted read credential degrades serving. The refresh policy
here is identical either way (see
@docs\/architecture\/access-model.md@ → "Credential supply").

The implementation lives in "Ecluse.Core.Credential.Refresh.Internal"; this module
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
