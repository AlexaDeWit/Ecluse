-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Shared local capacity and transactional maintenance of separately typed stores.
module Ecluse.Core.Server.Cache.Backend.Local.Internal (
    LocalPool,
    newLocalPool,
    newLocalPoolWithClock,
    registerStore,
    runPool,
    poolFits,
    adjustPool,
    poolEnabled,
    poolAcceptsWeight,
) where

import System.Clock (Clock (Monotonic), TimeSpec, getTime)
import UnliftIO.Exception (mask_)
import UnliftIO.MVar (withMVar)

import Ecluse.Core.Server.Cache.Backend (CacheOccupancy (..))

-- | The pool owns one aggregate counter. Registered actions hold no duplicate entry index.
data LocalPool = LocalPool
    { lpMaxEntries :: Int
    , lpMaxBytes :: Int
    , lpOccupancy :: TVar CacheOccupancy
    , lpStores :: TVar [StoreMaintenance]
    , lpLock :: MVar ()
    , lpNow :: IO TimeSpec
    }

data StoreMaintenance = StoreMaintenance
    { smExpire :: TimeSpec -> STM ()
    , smObserve :: STM (CacheOccupancy, CacheOccupancy -> IO ())
    }

-- | Build one aggregate bound for all registered local stores.
newLocalPool :: Int -> Int -> IO LocalPool
newLocalPool = newLocalPoolWithClock (getTime Monotonic)

-- | Supply the monotonic clock for deterministic expiry checks.
newLocalPoolWithClock :: IO TimeSpec -> Int -> Int -> IO LocalPool
newLocalPoolWithClock now entries bytes =
    LocalPool (max 0 entries) (max 0 bytes)
        <$> newTVarIO (CacheOccupancy 0 0)
        <*> newTVarIO []
        <*> newMVar ()
        <*> pure now

-- | Register maintenance without erasing a store's key or value type.
registerStore :: LocalPool -> (TimeSpec -> STM ()) -> STM (CacheOccupancy, CacheOccupancy -> IO ()) -> IO ()
registerStore pool expire observe =
    atomically (modifyTVar' (lpStores pool) (StoreMaintenance expire observe :))

-- | Expiry, entry changes and accounting commit together. Gauge reports keep commit order.
runPool :: LocalPool -> (TimeSpec -> STM a) -> IO a
runPool pool action = withMVar (lpLock pool) $ \() -> mask_ $ do
    now <- lpNow pool
    (result, reports) <- atomically $ do
        stores <- readTVar (lpStores pool)
        before <- traverse smObserve stores
        traverse_ (`smExpire` now) stores
        result <- action now
        after <- traverse smObserve stores
        let reports = zipWith report before after
        pure (result, reports)
    sequence_ reports
    pure result
  where
    report (before, _) (after, record) = when (before /= after) (record after)

-- | Test aggregate capacity for one additional entry without overflowing byte arithmetic.
poolFits :: LocalPool -> Int -> STM Bool
poolFits pool weight = do
    occupancy <- readTVar (lpOccupancy pool)
    pure (occEntries occupancy < lpMaxEntries pool && occBytes occupancy <= lpMaxBytes pool - weight)

-- | Apply the same committed delta as a store's own occupancy.
adjustPool :: LocalPool -> Int -> Int -> STM ()
adjustPool pool entries bytes = modifyTVar' (lpOccupancy pool) $ \occupancy ->
    CacheOccupancy (occEntries occupancy + entries) (occBytes occupancy + bytes)

-- | Zero aggregate capacity disables weighing and retention.
poolEnabled :: LocalPool -> Bool
poolEnabled pool = lpMaxEntries pool > 0 && lpMaxBytes pool > 0

-- | Reject invalid and unretainable weights before mutating a store.
poolAcceptsWeight :: LocalPool -> Int -> Bool
poolAcceptsWeight pool weight = weight >= 0 && weight < maxBound && weight <= lpMaxBytes pool
