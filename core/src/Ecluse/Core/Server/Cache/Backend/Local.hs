-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Local TTL retention with shared bounds and per-store recency eviction.
module Ecluse.Core.Server.Cache.Backend.Local (
    newLocalRetention,
    newPooledRetention,
    LocalPool,
    newLocalPool,
) where

import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.HashSet qualified as HashSet
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)
import System.Clock (TimeSpec, fromNanoSecs)

import Ecluse.Core.Server.Cache.Backend (CacheOccupancy (..), Recency (..), RetentionOperations (..))
import Ecluse.Core.Server.Cache.Backend.Local.Internal
import Ecluse.Core.Server.Cache.Types (StoreBudget (..))

data Weighted v = Weighted
    { wValue :: v
    , wWeight :: Int
    , wStamp :: Integer
    , wExpires :: TimeSpec
    }

data LocalStore k v = LocalStore
    { lsStore :: Cache k (Weighted v)
    , lsPool :: LocalPool
    , lsFloor :: StoreBudget
    , lsWeigh :: v -> Int
    , lsClock :: TVar Integer
    , lsRecency :: TVar (Map Integer k)
    , lsTTL :: TimeSpec
    , lsOccupancy :: TVar CacheOccupancy
    , lsExpiry :: TVar (Map TimeSpec (HashSet k))
    , lsRecord :: TVar (CacheOccupancy -> IO ())
    }

-- | Build a standalone bounded store. Zero bounds disable insertion without weighing values.
newLocalRetention :: (Hashable k) => NominalDiffTime -> Int -> Int -> (v -> Int) -> IO (RetentionOperations k v)
newLocalRetention ttl maxEntries maxBytes weigh = do
    pool <- newLocalPool maxEntries maxBytes
    newPooledRetention pool ttl (StoreBudget 0 0) weigh

-- | Share aggregate capacity, evicting only this store's entries above its floor.
newPooledRetention :: (Hashable k) => LocalPool -> NominalDiffTime -> StoreBudget -> (v -> Int) -> IO (RetentionOperations k v)
newPooledRetention pool ttl floorBudget weigh = do
    store <- Cache.newCache Nothing
    clock <- newTVarIO 0
    recency <- newTVarIO Map.empty
    occupancy <- newTVarIO (CacheOccupancy 0 0)
    expiry <- newTVarIO Map.empty
    record <- newTVarIO (const pass)
    let local = LocalStore store pool floorBudget weigh clock recency (toTimeSpec ttl) occupancy expiry record
    registerStore pool (purgeExpired local) ((,) <$> readTVar occupancy <*> readTVar record)
    pure RetentionOperations{roLookup = lookupStore local, roInsert = insertBounded local}

insertBounded :: (Hashable k) => LocalStore k v -> (CacheOccupancy -> IO ()) -> IO () -> k -> v -> IO ()
insertBounded local record refused key value
    | not (poolEnabled pool) = refused
    | not (poolAcceptsWeight pool weight) = refused
    | otherwise = do
        retained <- runPool pool $ \now -> do
            writeTVar (lsRecord local) record
            deleteStored local key
            fits <- evictToBudget local weight
            when fits (insertStored local now key value weight)
            pure fits
        unless retained refused
  where
    pool = lsPool local
    weight = lsWeigh local value

insertStored :: (Hashable k) => LocalStore k v -> TimeSpec -> k -> v -> Int -> STM ()
insertStored local now key value weight = do
    stamp <- nextStamp local
    let expires = now + lsTTL local
        weighted = Weighted value weight stamp expires
    Cache.insertSTM key weighted (lsStore local) Nothing
    modifyTVar' (lsExpiry local) (Map.insertWith HashSet.union expires (HashSet.singleton key))
    modifyTVar' (lsRecency local) (Map.insert stamp key)
    adjustOccupancy local 1 weight

evictToBudget :: (Hashable k) => LocalStore k v -> Int -> STM Bool
evictToBudget local incoming = do
    fits <- poolFits (lsPool local) incoming
    if fits
        then pure True
        else do
            recency <- readTVar (lsRecency local)
            case Map.lookupMin recency of
                Nothing -> pure False
                Just (_, key) -> do
                    held <- Cache.lookupSTM False key (lsStore local) (fromNanoSecs 0)
                    occupancy <- readTVar (lsOccupancy local)
                    case held of
                        Just weighted | aboveFloor occupancy weighted -> do
                            deleteStored local key
                            evictToBudget local incoming
                        _ -> pure False
  where
    aboveFloor occupancy weighted =
        occEntries occupancy - 1 >= max 0 (sbMinEntries (lsFloor local))
            && occBytes occupancy - wWeight weighted >= max 0 (sbMinBytes (lsFloor local))

deleteStored :: (Hashable k) => LocalStore k v -> k -> STM ()
deleteStored local key = do
    held <- Cache.lookupSTM False key (lsStore local) (fromNanoSecs 0)
    for_ held $ \weighted -> do
        Cache.deleteSTM key (lsStore local)
        modifyTVar' (lsExpiry local) (Map.update dropKey (wExpires weighted))
        modifyTVar' (lsRecency local) (Map.delete (wStamp weighted))
        adjustOccupancy local (-1) (negate (wWeight weighted))
  where
    dropKey bucket =
        let remaining = HashSet.delete key bucket
         in if HashSet.null remaining then Nothing else Just remaining

adjustOccupancy :: LocalStore k v -> Int -> Int -> STM ()
adjustOccupancy local entries bytes = do
    modifyTVar' (lsOccupancy local) $ \occupancy ->
        CacheOccupancy (occEntries occupancy + entries) (occBytes occupancy + bytes)
    adjustPool (lsPool local) entries bytes

purgeExpired :: (Hashable k) => LocalStore k v -> TimeSpec -> STM ()
purgeExpired local now = do
    expiry <- readTVar (lsExpiry local)
    case Map.lookupMin expiry of
        Just (deadline, bucket) | deadline < now -> do
            traverse_ (deleteStored local) bucket
            purgeExpired local now
        _ -> pass

nextStamp :: LocalStore k v -> STM Integer
nextStamp local = do
    stamp <- (+ 1) <$> readTVar (lsClock local)
    writeTVar (lsClock local) stamp
    pure stamp

lookupStore :: (Hashable k) => LocalStore k v -> (CacheOccupancy -> IO ()) -> Recency -> k -> IO (Maybe v)
lookupStore local record recency key = runPool (lsPool local) $ \now -> do
    writeTVar (lsRecord local) record
    held <- Cache.lookupSTM False key (lsStore local) now
    for_ held $ \weighted -> case recency of
        PreserveRecency -> pass
        RefreshRecency -> do
            stamp <- nextStamp local
            modifyTVar' (lsRecency local) (Map.insert stamp key . Map.delete (wStamp weighted))
            Cache.insertSTM key weighted{wStamp = stamp} (lsStore local) Nothing
    pure (wValue <$> held)

toTimeSpec :: NominalDiffTime -> TimeSpec
toTimeSpec ttl = fromNanoSecs (max 0 (round (realToFrac ttl * 1e9 :: Double) :: Integer))
