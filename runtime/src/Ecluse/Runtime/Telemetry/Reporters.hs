-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bridge boot-time reporters to installed instruments, retaining active credential expiries.
module Ecluse.Runtime.Telemetry.Reporters (
    DeferredMetrics,
    newDeferredMetrics,
    installMetrics,
    deferredBreakerReporter,
    deferredRefreshReporter,
    deferredMirrorEnqueueFailure,
) where

import Data.Foldable1 (minimum1)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import Data.Universe.Class (universe)

import Ecluse.Core.Breaker (BreakerReporter (..), breakerState)
import Ecluse.Core.Credential.Refresh (RefreshReporter (..))
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Telemetry.Metrics (
    BreakerSource,
    CredentialResult (RefreshFailed, Refreshed),
    Provider,
 )
import Ecluse.Runtime.Telemetry.Instruments (
    Metrics,
    recordBreakerState,
    recordCredentialRefresh,
    recordMirrorEnqueueFailure,
    registerCredentialTokenTtl,
 )

-- | Boot-time reporters retain expiry state before the metric instruments exist.
data DeferredMetrics = DeferredMetrics
    { dmMetrics :: IORef (Maybe Metrics)
    , dmExpiries :: IORef (Map Ecosystem (Provider, UTCTime))
    , dmClock :: IO UTCTime
    }

-- | Create deferred reporters with a clock read at metric collection.
newDeferredMetrics :: IO UTCTime -> IO DeferredMetrics
newDeferredMetrics clock = DeferredMetrics <$> newIORef Nothing <*> newIORef Map.empty <*> pure clock

-- | Install instruments once after boot, including observations received before installation.
installMetrics :: DeferredMetrics -> Metrics -> IO ()
installMetrics deferred metrics = do
    registerCredentialTokenTtl metrics (dmClock deferred) $ do
        expiries <- readIORef (dmExpiries deferred)
        pure
            [ (provider, expiry)
            | provider <- universe
            , Just expiry <- [viaNonEmpty minimum1 [stamp | (label, stamp) <- Map.elems expiries, label == provider]]
            ]
    writeIORef (dmMetrics deferred) (Just metrics)

withDeferredMetrics :: DeferredMetrics -> (Metrics -> IO ()) -> IO ()
withDeferredMetrics deferred record = readIORef (dmMetrics deferred) >>= maybe pass record

-- | Record a breaker's state under the given source.
deferredBreakerReporter :: DeferredMetrics -> BreakerSource -> BreakerReporter
deferredBreakerReporter deferred source =
    BreakerReporter $ \breaker ->
        withDeferredMetrics deferred $ \metrics ->
            recordBreakerState metrics source (breakerState breaker)

{- | Use the shared credential's canonical ecosystem as its bounded internal identity, never a label.
A non-expiring replacement removes that identity. Expired credentials remain until replacement.
-}
deferredRefreshReporter :: DeferredMetrics -> Ecosystem -> Provider -> RefreshReporter
deferredRefreshReporter deferred identity provider =
    RefreshReporter
        { onRefreshSucceeded = report Refreshed
        , onRefreshFailed = report RefreshFailed
        }
  where
    report :: CredentialResult -> Maybe UTCTime -> IO ()
    report result expiry = do
        atomicModifyIORef' (dmExpiries deferred) $ \expiries ->
            (maybe (Map.delete identity expiries) (\stamp -> Map.insert identity (provider, stamp) expiries) expiry, ())
        withDeferredMetrics deferred $ \metrics -> recordCredentialRefresh metrics provider result

-- | Record one mirror enqueue failure after instrument installation.
deferredMirrorEnqueueFailure :: DeferredMetrics -> IO ()
deferredMirrorEnqueueFailure deferred =
    withDeferredMetrics deferred recordMirrorEnqueueFailure
