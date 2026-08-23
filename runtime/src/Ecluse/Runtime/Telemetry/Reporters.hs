-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The bridge from the telemetry-agnostic reporter callbacks the credential and breaker
layers carry to the live @ecluse.*@ instruments. Boot builds the breaker and the
refreshing credential provider before the telemetry substrate exists, so neither can take
a 'Metrics'. Each carries a callback instead, backed here by a 'DeferredMetrics' cell:
inert until the composition root calls 'installMetrics', live after. Telemetry off means
the SDK's no-op meter, so the providers record unconditionally either way.
@docs\/architecture\/observability.md@ describes the catalogue and the cardinality rule.
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
) where

import Ecluse.Core.Breaker (BreakerReporter (..), breakerState)
import Ecluse.Core.Credential.Refresh (RefreshReporter (..))
import Ecluse.Core.Telemetry.Metrics (
    BreakerSource,
    CredentialResult (RefreshFailed, Refreshed),
    Provider,
 )
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
            recordBreakerState metrics source (breakerState breaker)

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
