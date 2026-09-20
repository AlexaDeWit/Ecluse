-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Local TTL retention with shared bounds and per-store recency eviction.
module Ecluse.Core.Server.Cache.Backend.Local (
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

-- | Share aggregate capacity, evicting only this store's entries above its floor.
newPooledRetention :: (Hashable k) => LocalPool -> NominalDiffTime -> StoreBudget -> (v -> Int) -> IO (RetentionOperations k v)
newPooledRetention pool ttl floorBudget weigh = do
    store <- Cache.newCache Nothing
    clock <- newTVarIO 0
    recency <- newTVarIO Map.empty
    occupancy <- newTVarIO (CacheOccupancy 0 0)
    expiry <- newTVarIO Map.empty
    record <- newTVarIO (const pass)
    let storeState = LocalStore store pool floorBudget weigh clock recency (toTimeSpec ttl) occupancy expiry record
    registerStore pool (purgeExpired storeState) ((,) <$> readTVar occupancy <*> readTVar record)
    pure RetentionOperations{roLookup = lookupStore storeState, roInsert = insertBounded storeState}

insertBounded :: (Hashable k) => LocalStore k v -> (CacheOccupancy -> IO ()) -> IO () -> k -> v -> IO ()
insertBounded storeState record refused key value
    | not (poolEnabled pool) = refused
    | not (poolAcceptsWeight pool weight) = refused
    | otherwise = do
        retained <- runPool pool $ \now -> do
            writeTVar (lsRecord storeState) record
            deleteStored storeState key
            fits <- evictToBudget storeState weight
            when fits (insertStored storeState now key value weight)
            pure fits
        unless retained refused
  where
    pool = lsPool storeState
    weight = lsWeigh storeState value

insertStored :: (Hashable k) => LocalStore k v -> TimeSpec -> k -> v -> Int -> STM ()
insertStored storeState now key value weight = do
    stamp <- nextStamp storeState
    let expires = now + lsTTL storeState
        weighted = Weighted value weight stamp expires
    Cache.insertSTM key weighted (lsStore storeState) Nothing
    modifyTVar' (lsExpiry storeState) (Map.insertWith HashSet.union expires (HashSet.singleton key))
    modifyTVar' (lsRecency storeState) (Map.insert stamp key)
    adjustOccupancy storeState 1 weight

evictToBudget :: (Hashable k) => LocalStore k v -> Int -> STM Bool
evictToBudget storeState incoming = do
    fits <- poolFits (lsPool storeState) incoming
    if fits
        then pure True
        else do
            recency <- readTVar (lsRecency storeState)
            case Map.lookupMin recency of
                Nothing -> pure False
                Just (_, key) -> do
                    held <- Cache.lookupSTM False key (lsStore storeState) (fromNanoSecs 0)
                    occupancy <- readTVar (lsOccupancy storeState)
                    case held of
                        Just weighted | aboveFloor occupancy weighted -> do
                            deleteStored storeState key
                            evictToBudget storeState incoming
                        _ -> pure False
  where
    aboveFloor occupancy weighted =
        occEntries occupancy - 1 >= max 0 (sbMinEntries (lsFloor storeState))
            && occBytes occupancy - wWeight weighted >= max 0 (sbMinBytes (lsFloor storeState))

deleteStored :: (Hashable k) => LocalStore k v -> k -> STM ()
deleteStored storeState key = do
    held <- Cache.lookupSTM False key (lsStore storeState) (fromNanoSecs 0)
    for_ held $ \weighted -> do
        Cache.deleteSTM key (lsStore storeState)
        modifyTVar' (lsExpiry storeState) (Map.update dropKey (wExpires weighted))
        modifyTVar' (lsRecency storeState) (Map.delete (wStamp weighted))
        adjustOccupancy storeState (-1) (negate (wWeight weighted))
  where
    dropKey bucket =
        let remaining = HashSet.delete key bucket
         in if HashSet.null remaining then Nothing else Just remaining

adjustOccupancy :: LocalStore k v -> Int -> Int -> STM ()
adjustOccupancy storeState entries bytes = do
    modifyTVar' (lsOccupancy storeState) $ \occupancy ->
        CacheOccupancy (occEntries occupancy + entries) (occBytes occupancy + bytes)
    adjustPool (lsPool storeState) entries bytes

purgeExpired :: (Hashable k) => LocalStore k v -> TimeSpec -> STM ()
purgeExpired storeState now = do
    expiry <- readTVar (lsExpiry storeState)
    case Map.lookupMin expiry of
        Just (deadline, bucket) | deadline < now -> do
            traverse_ (deleteStored storeState) bucket
            purgeExpired storeState now
        _ -> pass

nextStamp :: LocalStore k v -> STM Integer
nextStamp storeState = do
    stamp <- (+ 1) <$> readTVar (lsClock storeState)
    writeTVar (lsClock storeState) stamp
    pure stamp

lookupStore :: (Hashable k) => LocalStore k v -> (CacheOccupancy -> IO ()) -> Recency -> k -> IO (Maybe v)
lookupStore storeState record recency key = runPool (lsPool storeState) $ \now -> do
    writeTVar (lsRecord storeState) record
    held <- Cache.lookupSTM False key (lsStore storeState) now
    for_ held $ \weighted -> case recency of
        PreserveRecency -> pass
        RefreshRecency -> do
            stamp <- nextStamp storeState
            modifyTVar' (lsRecency storeState) (Map.insert stamp key . Map.delete (wStamp weighted))
            Cache.insertSTM key weighted{wStamp = stamp} (lsStore storeState) Nothing
    pure (wValue <$> held)

toTimeSpec :: NominalDiffTime -> TimeSpec
toTimeSpec ttl = fromNanoSecs (max 0 (round (realToFrac ttl * 1e9 :: Double) :: Integer))
