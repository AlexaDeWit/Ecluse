-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | TTL stores with single-flight fetches and bounded accounted bytes.
Each store serialises eviction and insertion while followers share the leader's result.
-}
module Ecluse.Core.Server.Cache.Store (
    -- * The store
    SingleFlight,
    newSingleFlight,
    newSingleFlightObserved,
    StoreEvent (..),
    ReuseKind (..),
    RemovalCause (..),

    -- * Resolution
    resolveSingleFlight,

    -- * Reads
    lookupStore,
    lookupStoreTouching,

    -- * Occupancy
    CacheOccupancy (..),
) where

import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)
import System.Clock (Clock (Monotonic), TimeSpec, fromNanoSecs, getTime, toNanoSecs)
import UnliftIO.Exception (SomeAsyncException, mask, throwIO)
import UnliftIO.MVar (withMVar)

import Ecluse.Core.InFlight (guardInFlight)
import Ecluse.Core.Telemetry.Metrics qualified as Metric

data Weighted v = Weighted
    { wValue :: v
    , wWeight :: Int
    -- ^ The value's estimated resident footprint in bytes, fixed at insert.
    , wStamp :: IORef Word64
    -- ^ The value's last-access stamp, bumped on every hit and read by eviction.
    , wExpires :: TimeSpec
    }

-- | A bounded store whose concurrent misses share one fetch per key.
data SingleFlight e k v = SingleFlight
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
    , sfInFlight :: TVar (Map k (TMVar (FlightOutcome e v)))
    , sfObserver :: Maybe (StoreEvent k -> IO ())
    }

-- | Keys and byte weights, never values. Times are monotonic nanoseconds, expiry identifies a generation.
data StoreEvent k
    = Inserted k Int Integer Integer
    | Reused k ReuseKind Int Integer
    | Removed k RemovalCause Int Integer
    | Rejected k Int
    | Joined k
    deriving stock (Eq, Show, Functor)

-- | Expiry or pressure from bytes followed by entry count. Both pressures can apply.
data RemovalCause = Expiry | Capacity Bool Bool
    deriving stock (Eq, Show)

-- | A resolution refreshes recency. A full-store probe does not.
data ReuseKind = ResolvedHit | ProbeHit
    deriving stock (Eq, Show)

data FlightOutcome e v
    = FlightValue v
    | FlightFault e
    | FlightOrphaned SomeException

-- | Build a store with positive bounds. A weight of 'maxBound' means uncacheable.
newSingleFlight :: NominalDiffTime -> Int -> Int -> (v -> Int) -> IO (SingleFlight e k v)
newSingleFlight = newSingleFlightObserved Nothing

-- | Mutation observers hold the mutation lock and must not re-enter the store.
newSingleFlightObserved :: Maybe (StoreEvent k -> IO ()) -> NominalDiffTime -> Int -> Int -> (v -> Int) -> IO (SingleFlight e k v)
newSingleFlightObserved observer ttl maxEntries maxBytes weigh = do
    -- Expiry belongs to this wrapper so deletion and accounting share one transaction.
    store <- Cache.newCache Nothing
    clock <- newIORef 0
    occupancy <- newTVarIO (CacheOccupancy 0 0)
    expiry <- newTVarIO Map.empty
    inFlight <- newTVarIO Map.empty
    insertLock <- newMVar ()
    pure
        SingleFlight
            { sfStore = store
            , sfMaxEntries = max 1 maxEntries
            , sfMaxBytes = max 1 maxBytes
            , sfWeigh = weigh
            , sfClock = clock
            , sfTTL = toTimeSpec ttl
            , sfOccupancy = occupancy
            , sfExpiry = expiry
            , sfInsertLock = insertLock
            , sfInFlight = inFlight
            , sfObserver = observer
            }

{- | Share concurrent fetches and release waiters on failure or cancellation.
Occupancy callbacks hold the mutation lock and must not re-enter the store.
-}
resolveSingleFlight ::
    (Hashable k, Ord k) =>
    (Metric.CacheResult -> IO ()) ->
    (CacheOccupancy -> IO ()) ->
    IO () ->
    SingleFlight e k v ->
    k ->
    IO (Either e v) ->
    IO (Either e v)
resolveSingleFlight recordRequest recordOccupancy recordRefused sf key fetch = mask $ \restore ->
    let resolveAt reportRequest nowT = do
            decision <- atomically (decideSingleFlight sf key nowT)
            case decision of
                Expired -> do
                    -- Report removal before claiming. A throwing callback must not orphan a leader.
                    _ <- lookupAfterExpiry recordOccupancy sf key
                    getTime Monotonic >>= resolveAt reportRequest
                Hit weighted -> do
                    reportRequest Metric.Hit
                    touch sf weighted
                    observeEvent sf (Reused key ResolvedHit (wWeight weighted) (toNanoSecs (wExpires weighted)))
                    pure (Right (wValue weighted))
                Follow marker -> do
                    reportRequest Metric.Collapsed
                    observeEvent sf (Joined key)
                    outcome <- restore (atomically (readTMVar marker))
                    case outcome of
                        FlightValue fetched -> pure (Right fetched)
                        FlightFault fault -> pure (Left fault)
                        FlightOrphaned err -> case fromException err of
                            Just (_ :: SomeAsyncException) ->
                                -- A retry keeps the original classification and the outer cancellation mask.
                                getTime Monotonic >>= resolveAt (const pass)
                            -- Preserve synchronous leader faults outside the typed fetch channel.
                            Nothing -> throwIO err
                Lead marker ->
                    guardInFlight id (orphan marker) (atomically deregister) $ do
                        reportRequest Metric.Miss
                        fetched <- restore fetch
                        atomically (putTMVar marker (either FlightFault FlightValue fetched))
                        traverse_ (insertBounded recordOccupancy recordRefused sf key) (rightToMaybe fetched)
                        pure fetched
     in getTime Monotonic >>= resolveAt recordRequest
  where
    deregister :: STM ()
    deregister = do
        inFlight <- readTVar (sfInFlight sf)
        writeTVar (sfInFlight sf) (Map.delete key inFlight)

insertBounded :: (Hashable k) => (CacheOccupancy -> IO ()) -> IO () -> SingleFlight e k v -> k -> v -> IO ()
insertBounded recordOccupancy recordRefused sf key value
    | weight == maxBound || weight > sfMaxBytes sf = recordRefused >> observeEvent sf (Rejected key weight)
    | otherwise = withMVar (sfInsertLock sf) $ \() -> do
        nowT <- getTime Monotonic
        observeExpiry sf nowT $ observeOccupancy recordOccupancy sf $ atomically $ do
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
        observeEvent sf (Inserted key weight (toNanoSecs insertedAt) (toNanoSecs expires))
  where
    weight = sfWeigh sf value

evictToBudget :: (Hashable k) => (CacheOccupancy -> IO ()) -> SingleFlight e k v -> Int -> IO ()
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
        removed <- observeEviction sf incoming k $ observeOccupancy recordOccupancy sf $ atomically $ do
            occ <- readTVar (sfOccupancy sf)
            if fits occ
                then pure False
                else deleteStored sf k $> True
        when removed (go rest)

deleteStored :: (Hashable k) => SingleFlight e k v -> k -> STM ()
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

subtractOccupancy :: SingleFlight e k v -> Int -> Int -> STM ()
subtractOccupancy sf entries bytes =
    modifyTVar' (sfOccupancy sf) $ \occ ->
        CacheOccupancy (occEntries occ - entries) (occBytes occ - bytes)

purgeExpired :: (Hashable k) => SingleFlight e k v -> TimeSpec -> STM ()
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

nextStamp :: SingleFlight e k v -> IO Word64
nextStamp sf = atomicModifyIORef' (sfClock sf) (\n -> let n' = n + 1 in (n', n'))

touch :: SingleFlight e k v -> Weighted v -> IO ()
touch sf weighted = nextStamp sf >>= writeIORef (wStamp weighted)

-- | Read without fetching or refreshing recency, reporting any expired entry's removal.
lookupStore :: (Hashable k) => (CacheOccupancy -> IO ()) -> SingleFlight e k v -> k -> IO (Maybe v)
lookupStore recordOccupancy sf key = fmap wValue <$> lookupWeighted recordOccupancy sf key

-- | Read without fetching and refresh recency on a hit, reporting expiry removals.
lookupStoreTouching :: (Hashable k) => (CacheOccupancy -> IO ()) -> SingleFlight e k v -> k -> IO (Maybe v)
lookupStoreTouching recordOccupancy sf key =
    lookupWeighted recordOccupancy sf key >>= traverse (\weighted -> wValue weighted <$ touch sf weighted)

lookupWeighted :: (Hashable k) => (CacheOccupancy -> IO ()) -> SingleFlight e k v -> k -> IO (Maybe (Weighted v))
lookupWeighted recordOccupancy sf key = do
    nowT <- getTime Monotonic
    held <- atomically (Cache.lookupSTM False key (sfStore sf) nowT)
    result <- case held of
        Just weighted | wExpires weighted < nowT -> lookupAfterExpiry recordOccupancy sf key
        _ -> pure held
    for_ result (\weighted -> observeEvent sf (Reused key ProbeHit (wWeight weighted) (toNanoSecs (wExpires weighted))))
    pure result

lookupAfterExpiry :: (Hashable k) => (CacheOccupancy -> IO ()) -> SingleFlight e k v -> k -> IO (Maybe (Weighted v))
lookupAfterExpiry recordOccupancy sf key = withMVar (sfInsertLock sf) $ \() -> do
    nowT <- getTime Monotonic
    observeLookupExpiry sf key nowT $ observeOccupancy recordOccupancy sf (atomically (lookupWeightedSTM sf key nowT))

observeEvent :: SingleFlight e k v -> StoreEvent k -> IO ()
observeEvent sf event = case sfObserver sf of
    Nothing -> pass
    Just emit -> emit event
{-# INLINE observeEvent #-}

observeExpiry :: SingleFlight e k v -> TimeSpec -> IO a -> IO a
observeExpiry sf nowT action = case sfObserver sf of
    Nothing -> action
    Just emit -> do
        expiry <- readTVarIO (sfExpiry sf)
        result <- action
        for_ (Map.toList (Map.takeWhileAntitone (< nowT) expiry)) $ \(deadline, bucket) ->
            for_ (HashMap.toList bucket) (\(key, weight) -> emit (Removed key Expiry weight (toNanoSecs deadline)))
        pure result
{-# INLINE observeExpiry #-}

observeEviction :: (Hashable k) => SingleFlight e k v -> Int -> k -> IO Bool -> IO Bool
observeEviction sf incoming key action = case sfObserver sf of
    Nothing -> action
    Just emit -> do
        occupancy <- readTVarIO (sfOccupancy sf)
        held <- atomically (Cache.lookupSTM False key (sfStore sf) (fromNanoSecs 0))
        removed <- action
        when removed $ for_ held $ \weighted ->
            emit (Removed key (Capacity (occBytes occupancy > sfMaxBytes sf - incoming) (occEntries occupancy >= sfMaxEntries sf)) (wWeight weighted) (toNanoSecs (wExpires weighted)))
        pure removed
{-# INLINE observeEviction #-}

observeLookupExpiry :: (Hashable k) => SingleFlight e k v -> k -> TimeSpec -> IO a -> IO a
observeLookupExpiry sf key nowT action = case sfObserver sf of
    Nothing -> action
    Just emit -> do
        held <- atomically (Cache.lookupSTM False key (sfStore sf) (fromNanoSecs 0))
        result <- action
        for_ held $ \weighted ->
            when (wExpires weighted < nowT) $
                emit (Removed key Expiry (wWeight weighted) (toNanoSecs (wExpires weighted)))
        pure result
{-# INLINE observeLookupExpiry #-}

-- All mutations and their absolute gauge updates hold sfInsertLock, so samples cannot reorder.
observeOccupancy :: (CacheOccupancy -> IO ()) -> SingleFlight e k v -> IO a -> IO a
observeOccupancy recordOccupancy sf action = do
    before <- readTVarIO (sfOccupancy sf)
    result <- action
    after <- readTVarIO (sfOccupancy sf)
    when (before /= after) (recordOccupancy after)
    pure result

lookupWeightedSTM :: (Hashable k) => SingleFlight e k v -> k -> TimeSpec -> STM (Maybe (Weighted v))
lookupWeightedSTM sf key nowT = do
    held <- Cache.lookupSTM False key (sfStore sf) nowT
    case held of
        Just weighted | wExpires weighted < nowT -> deleteStored sf key $> Nothing
        _ -> pure held

-- A hit carries the weighted entry, so the caller can bump its recency without a second read.
data Decision e v
    = Expired
    | Hit (Weighted v)
    | Follow (TMVar (FlightOutcome e v))
    | Lead (TMVar (FlightOutcome e v))

-- Expired entries claim nothing. New claims stay masked until guardInFlight owns them.
decideSingleFlight :: (Hashable k, Ord k) => SingleFlight e k v -> k -> TimeSpec -> STM (Decision e v)
decideSingleFlight sf key nowT = do
    held <- Cache.lookupSTM False key (sfStore sf) nowT
    case held of
        Just weighted | wExpires weighted < nowT -> pure Expired
        Just weighted -> pure (Hit weighted)
        Nothing -> do
            inFlight <- readTVar (sfInFlight sf)
            case Map.lookup key inFlight of
                Just marker -> pure (Follow marker)
                Nothing -> do
                    marker <- newEmptyTMVar
                    writeTVar (sfInFlight sf) (Map.insert key marker inFlight)
                    pure (Lead marker)

-- Hand the escaping error to blocked followers so they unblock rather than park forever.
-- Fills only when empty, so an escape after a successful publish never clobbers the result.
orphan :: TMVar (FlightOutcome e v) -> SomeException -> IO ()
orphan marker err =
    atomically $ do
        unfilled <- isEmptyTMVar marker
        when unfilled (putTMVar marker (FlightOrphaned err))

-- | Entry count and summed accounted bytes after a store mutation.
data CacheOccupancy = CacheOccupancy
    { occEntries :: Int
    , occBytes :: Int
    }
    deriving stock (Eq, Show)

toTimeSpec :: NominalDiffTime -> TimeSpec
toTimeSpec ttl = fromNanoSecs (max 0 (round (realToFrac ttl * 1e9 :: Double) :: Integer))
