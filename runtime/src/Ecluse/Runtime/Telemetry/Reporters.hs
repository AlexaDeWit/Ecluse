-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The bridge from the telemetry-agnostic reporters the pre-telemetry providers carry
to the live @ecluse.*@ instruments. It also holds the deferral that lets a provider
built before the meter exists record once it does.

Boot builds the circuit breaker ("Ecluse.Core.Breaker") and the refreshing credential
provider ("Ecluse.Core.Credential.Refresh") __before__ the telemetry substrate exists,
the meter provider included. Neither can take a 'Metrics' at construction. Each instead
carries a small, telemetry-agnostic reporter callback. This module supplies those
callbacks, backed by a 'DeferredMetrics' cell. The cell is __inert__ and records
nothing until the composition root has built the instruments and called
'installMetrics', and __live__ thereafter. That mirrors the no-op-meter discipline of
"Ecluse.Runtime.Telemetry.Instruments". Once installed, an inert handle still discards
every measurement, because telemetry off means the SDK's no-op meter. The providers
therefore record unconditionally either way.

@docs\/architecture\/observability.md@ describes the catalogue and the cardinality
rule.
-}
module Ecluse.Runtime.Telemetry.Reporters (
    -- * Deferred metric handle
    DeferredMetrics,
    newDeferredMetrics,
    installMetrics,

    -- * Reporters over the deferred handle
    deferredBreakerReporter,
    deferredRefreshReporter,
    deferredMirrorEnqueueFailure,

    -- * Breaker-state projection
    breakerStateOf,
) where

import Ecluse.Core.Breaker (Breaker (..), BreakerReporter (..))
import Ecluse.Core.Credential.Refresh (RefreshReporter (..))
import Ecluse.Core.Telemetry.Metrics (
    BreakerSource,
    BreakerState,
    CredentialResult (RefreshFailed, Refreshed),
    Provider,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Runtime.Telemetry.Instruments (
    Metrics,
    recordBreakerState,
    recordCredentialRefresh,
    recordCredentialTokenTtl,
    recordMirrorEnqueueFailure,
 )

{- | A 'Metrics' handle that may not exist yet, so a reporter can be wired before the boot phase
builds the instruments. A record through it while empty is a no-op.
-}
newtype DeferredMetrics = DeferredMetrics (IORef (Maybe Metrics))

-- | A fresh, empty 'DeferredMetrics': every reporter over it is inert until 'installMetrics'.
newDeferredMetrics :: IO DeferredMetrics
newDeferredMetrics = DeferredMetrics <$> newIORef Nothing

{- | Install the live instruments, so every reporter over this handle records through them.
The composition root calls this once, after 'newMetrics'.
-}
installMetrics :: DeferredMetrics -> Metrics -> IO ()
installMetrics (DeferredMetrics ref) = writeIORef ref . Just

-- Run an action with the live instruments if installed. A no-op while still empty.
withDeferredMetrics :: DeferredMetrics -> (Metrics -> IO ()) -> IO ()
withDeferredMetrics (DeferredMetrics ref) record = readIORef ref >>= maybe pass record

-- | Record a breaker's state to @ecluse.rule.breaker.state@ under the given source.
deferredBreakerReporter :: DeferredMetrics -> BreakerSource -> BreakerReporter
deferredBreakerReporter deferred source =
    BreakerReporter $ \breaker ->
        withDeferredMetrics deferred $ \metrics ->
            recordBreakerState metrics source (breakerStateOf breaker)

{- | A 'RefreshReporter' that records each refresh outcome to @ecluse.credential.refresh@ and the
reported remaining lifetime to @ecluse.credential.token.ttl.seconds@ under the given provider.
-}
deferredRefreshReporter :: DeferredMetrics -> Provider -> RefreshReporter
deferredRefreshReporter deferred provider =
    RefreshReporter
        { onRefreshSucceeded = report Refreshed
        , onRefreshFailed = report RefreshFailed
        }
  where
    report :: CredentialResult -> Maybe Int -> IO ()
    report result mTtlSeconds =
        withDeferredMetrics deferred $ \metrics -> do
            recordCredentialRefresh metrics provider result
            whenJust mTtlSeconds (recordCredentialTokenTtl metrics provider)

{- | An action recording one mirror enqueue failure to @ecluse.mirror.enqueue.failures@.
The composition root hangs it on the enqueue buffer's callbacks
('Ecluse.Core.Queue.newEnqueueBuffer'), which cannot fire before 'installMetrics' runs.
-}
deferredMirrorEnqueueFailure :: DeferredMetrics -> IO ()
deferredMirrorEnqueueFailure deferred =
    withDeferredMetrics deferred recordMirrorEnqueueFailure

{- | Project the breaker's runtime state ("Ecluse.Core.Breaker") onto the bounded gauge value
the catalogue records ("Ecluse.Core.Telemetry.Metrics"). The consecutive-failure tally a
'Closed' breaker carries is not observable, so it collapses to the single closed value.
-}
breakerStateOf :: Breaker -> BreakerState
breakerStateOf = \case
    Closed{} -> Metric.Closed
    HalfOpen -> Metric.HalfOpen
    Open{} -> Metric.Open
