-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Local retention with TTL expiry and bounded accounted bytes.
module Ecluse.Core.Server.Cache.Backend.Local (
    newLocalRetention,
) where

import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)
import System.Clock (Clock (Monotonic), TimeSpec, fromNanoSecs, getTime)
import UnliftIO.MVar (withMVar)

import Ecluse.Core.Server.Cache.Backend (CacheOccupancy (..), Recency (..), RetentionOperations (..))

data Weighted v = Weighted
    { wValue :: v
    , wWeight :: Int
    -- ^ The value's estimated resident footprint in bytes, fixed at insert.
    , wStamp :: IORef Word64
    -- ^ The value's last-access stamp, bumped on every hit and read by eviction.
    , wExpires :: TimeSpec
    }

-- Local state never owns in-flight results.
data LocalStore k v = LocalStore
    { sfStore :: Cache k (Weighted v)
    -- ^ This wrapper owns expiry so removals also update occupancy.
    , sfMaxEntries :: Int
    -- ^ The entry-count bound enforced on insert.
    , sfMaxBytes :: Int
    -- ^ The resident-byte budget enforced on insert.
    , sfWeigh :: v -> Int
    -- ^ Estimate a value's resident footprint in bytes, fixed into its 'Weighted' at insert.
    , sfClock :: IORef Word64
    , sfTTL :: TimeSpec
    , sfOccupancy :: TVar CacheOccupancy
    , sfExpiry :: TVar (Map TimeSpec (HashMap k Int))
    -- ^ Exactly one indexed weight per retained key. Empty deadline buckets are removed.
    , sfInsertLock :: MVar ()
    }

-- | Build a bounded store. Zero bounds disable insertion without weighing values.
newLocalRetention :: (Hashable k) => NominalDiffTime -> Int -> Int -> (v -> Int) -> IO (RetentionOperations k v)
newLocalRetention ttl maxEntries maxBytes weigh = do
    -- Expiry belongs to this wrapper so deletion and accounting share one transaction.
    store <- Cache.newCache Nothing
    clock <- newIORef 0
    occupancy <- newTVarIO (CacheOccupancy 0 0)
    expiry <- newTVarIO Map.empty
    insertLock <- newMVar ()
    let storeState =
            LocalStore
                { sfStore = store
                , sfMaxEntries = max 0 maxEntries
                , sfMaxBytes = max 0 maxBytes
                , sfWeigh = weigh
                , sfClock = clock
                , sfTTL = toTimeSpec ttl
                , sfOccupancy = occupancy
                , sfExpiry = expiry
                , sfInsertLock = insertLock
                }

    let readValue record recency key = case recency of
            PreserveRecency -> lookupStore record storeState key
            RefreshRecency -> lookupStoreTouching record storeState key
        writeValue record refused = insertBounded record refused storeState
    pure RetentionOperations{roLookup = readValue, roInsert = writeValue}

insertBounded :: (Hashable k) => (CacheOccupancy -> IO ()) -> IO () -> LocalStore k v -> k -> v -> IO ()
insertBounded recordOccupancy recordRefused sf key value
    | sfMaxEntries sf == 0 || sfMaxBytes sf == 0 = recordRefused
    | weight == maxBound || weight > sfMaxBytes sf = recordRefused
    | otherwise = withMVar (sfInsertLock sf) $ \() -> do
        nowT <- getTime Monotonic
        observeOccupancy recordOccupancy sf $ atomically $ do
            purgeExpired sf nowT
            deleteStored sf key
        evictToBudget recordOccupancy sf weight
        stamp <- nextStamp sf
        stampRef <- newIORef stamp
        insertedAt <- getTime Monotonic
        let expires = insertedAt + sfTTL sf
            weighted = Weighted{wValue = value, wWeight = weight, wStamp = stampRef, wExpires = expires}
        observeOccupancy recordOccupancy sf $ atomically $ do
            Cache.insertSTM key weighted (sfStore sf) Nothing
            modifyTVar' (sfExpiry sf) (Map.insertWith HashMap.union expires (HashMap.singleton key weight))
            modifyTVar' (sfOccupancy sf) $ \occ ->
                CacheOccupancy (occEntries occ + 1) (occBytes occ + weight)
  where
    weight = sfWeigh sf value

evictToBudget :: (Hashable k) => (CacheOccupancy -> IO ()) -> LocalStore k v -> Int -> IO ()
evictToBudget recordOccupancy sf incoming = do
    occupancy <- readTVarIO (sfOccupancy sf)
    unless (fits occupancy) $ do
        held <- Cache.toList (sfStore sf)
        stamped <- traverse stampOf held
        go (sortOn fst stamped)
  where
    stampOf (k, w, _) = do
        s <- readIORef (wStamp w)
        pure (s, k)

    fits occ = occBytes occ <= sfMaxBytes sf - incoming && occEntries occ < sfMaxEntries sf

    go [] = pass
    go ((_, k) : rest) = do
        removed <- observeOccupancy recordOccupancy sf $ atomically $ do
            occ <- readTVar (sfOccupancy sf)
            if fits occ
                then pure False
                else deleteStored sf k $> True
        when removed (go rest)

deleteStored :: (Hashable k) => LocalStore k v -> k -> STM ()
deleteStored sf key = do
    held <- Cache.lookupSTM False key (sfStore sf) (fromNanoSecs 0)
    for_ held $ \weighted -> do
        Cache.deleteSTM key (sfStore sf)
        modifyTVar' (sfExpiry sf) (Map.update dropKey (wExpires weighted))
        subtractOccupancy sf 1 (wWeight weighted)
  where
    dropKey bucket =
        let remaining = HashMap.delete key bucket
         in if HashMap.null remaining then Nothing else Just remaining

subtractOccupancy :: LocalStore k v -> Int -> Int -> STM ()
subtractOccupancy sf entries bytes =
    modifyTVar' (sfOccupancy sf) $ \occ ->
        CacheOccupancy (occEntries occ - entries) (occBytes occ - bytes)

purgeExpired :: (Hashable k) => LocalStore k v -> TimeSpec -> STM ()
purgeExpired sf nowT = do
    expiry <- readTVar (sfExpiry sf)
    case Map.minViewWithKey expiry of
        Just ((deadline, bucket), rest) | deadline < nowT -> do
            HashMap.foldrWithKey deleteExpired pass bucket
            writeTVar (sfExpiry sf) rest
            purgeExpired sf nowT
        _ -> pass
  where
    deleteExpired key weight remaining = do
        Cache.deleteSTM key (sfStore sf)
        subtractOccupancy sf 1 weight
        remaining

nextStamp :: LocalStore k v -> IO Word64
nextStamp sf = atomicModifyIORef' (sfClock sf) (\n -> let n' = n + 1 in (n', n'))

touch :: LocalStore k v -> Weighted v -> IO ()
touch sf weighted = nextStamp sf >>= writeIORef (wStamp weighted)

-- | Read without fetching or refreshing recency, reporting any expired entry's removal.
lookupStore :: (Hashable k) => (CacheOccupancy -> IO ()) -> LocalStore k v -> k -> IO (Maybe v)
lookupStore recordOccupancy sf key = fmap wValue <$> lookupWeighted recordOccupancy sf key

-- | Read without fetching and refresh recency on a hit, reporting expiry removals.
lookupStoreTouching :: (Hashable k) => (CacheOccupancy -> IO ()) -> LocalStore k v -> k -> IO (Maybe v)
lookupStoreTouching recordOccupancy sf key =
    lookupWeighted recordOccupancy sf key >>= traverse (\weighted -> wValue weighted <$ touch sf weighted)

lookupWeighted :: (Hashable k) => (CacheOccupancy -> IO ()) -> LocalStore k v -> k -> IO (Maybe (Weighted v))
lookupWeighted recordOccupancy sf key = do
    nowT <- getTime Monotonic
    held <- atomically (Cache.lookupSTM False key (sfStore sf) nowT)
    case held of
        Just weighted | wExpires weighted < nowT -> lookupAfterExpiry recordOccupancy sf key
        _ -> pure held

lookupAfterExpiry :: (Hashable k) => (CacheOccupancy -> IO ()) -> LocalStore k v -> k -> IO (Maybe (Weighted v))
lookupAfterExpiry recordOccupancy sf key = withMVar (sfInsertLock sf) $ \() -> do
    nowT <- getTime Monotonic
    observeOccupancy recordOccupancy sf (atomically (lookupWeightedSTM sf key nowT))

-- All mutations and their absolute gauge updates hold sfInsertLock, so samples cannot reorder.
observeOccupancy :: (CacheOccupancy -> IO ()) -> LocalStore k v -> IO a -> IO a
observeOccupancy recordOccupancy sf action = do
    before <- readTVarIO (sfOccupancy sf)
    result <- action
    after <- readTVarIO (sfOccupancy sf)
    when (before /= after) (recordOccupancy after)
    pure result

lookupWeightedSTM :: (Hashable k) => LocalStore k v -> k -> TimeSpec -> STM (Maybe (Weighted v))
lookupWeightedSTM sf key nowT = do
    held <- Cache.lookupSTM False key (sfStore sf) nowT
    case held of
        Just weighted | wExpires weighted < nowT -> deleteStored sf key $> Nothing
        _ -> pure held

toTimeSpec :: NominalDiffTime -> TimeSpec
toTimeSpec ttl = fromNanoSecs (max 0 (round (realToFrac ttl * 1e9 :: Double) :: Integer))
