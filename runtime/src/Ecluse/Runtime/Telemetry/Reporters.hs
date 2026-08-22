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

{- | A 'Metrics' handle that may not exist yet: empty until the telemetry substrate has
built the instruments, then live. The pre-telemetry boot phase builds providers that
record through reporters closed over this. They can therefore be wired before the meter
exists, and become live once it does. A record through it while empty is a no-op.
-}
newtype DeferredMetrics = DeferredMetrics (IORef (Maybe Metrics))

-- | A fresh, empty 'DeferredMetrics': every reporter over it is inert until 'installMetrics'.
newDeferredMetrics :: IO DeferredMetrics
newDeferredMetrics = DeferredMetrics <$> newIORef Nothing

{- | Install the live instruments, making every reporter over this handle record through
them from now on. Called once by the composition root after 'newMetrics' has built the
instruments (which are themselves inert when telemetry is off).
-}
installMetrics :: DeferredMetrics -> Metrics -> IO ()
installMetrics (DeferredMetrics ref) = writeIORef ref . Just

-- Run an action with the live instruments if installed. A no-op while still empty.
withDeferredMetrics :: DeferredMetrics -> (Metrics -> IO ()) -> IO ()
withDeferredMetrics (DeferredMetrics ref) record = readIORef ref >>= maybe pass record

{- | A 'BreakerReporter' that records a breaker's state to @ecluse.rule.breaker.state@
under the given source, through the deferred handle (inert until it is installed).
-}
deferredBreakerReporter :: DeferredMetrics -> BreakerSource -> BreakerReporter
deferredBreakerReporter deferred source =
    BreakerReporter $ \breaker ->
        withDeferredMetrics deferred $ \metrics ->
            recordBreakerState metrics source (breakerStateOf breaker)

{- | A 'RefreshReporter' that records each refresh outcome to @ecluse.credential.refresh@
(by result) and the reported remaining lifetime to @ecluse.credential.token.ttl.seconds@,
both under the given provider. It records through the deferred handle, inert until it
is installed.
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

{- | An action recording one mirror enqueue failure to @ecluse.mirror.enqueue.failures@,
through the deferred handle (inert until it is installed). The composition root hangs
it on the enqueue buffer's drop and delivery-failure callbacks
('Ecluse.Core.Queue.newEnqueueBuffer'). Those are wired before the instruments exist
but cannot fire until the server is serving, well after 'installMetrics'.
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
