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
import System.Clock (Clock (Monotonic), TimeSpec, fromNanoSecs, getTime)
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
    }

data FlightOutcome e v
    = FlightValue v
    | FlightFault e
    | FlightOrphaned SomeException

-- | Build a store with positive bounds. A weight of 'maxBound' means uncacheable.
newSingleFlight :: NominalDiffTime -> Int -> Int -> (v -> Int) -> IO (SingleFlight e k v)
newSingleFlight ttl maxEntries maxBytes weigh = do
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
            }

-- | Share a fetch across concurrent misses. Failed and cancelled leaders release their waiters.
resolveSingleFlight ::
    (Hashable k, Ord k) =>
    IO () ->
    (Metric.CacheResult -> IO ()) ->
    (CacheOccupancy -> IO ()) ->
    SingleFlight e k v ->
    k ->
    IO (Either e v) ->
    IO (Either e v)
resolveSingleFlight afterClaim recordRequest recordInsert sf key fetch = mask $ \restore -> do
    nowT <- getTime Monotonic
    -- One atomic decision point under the enclosing 'mask'. A 'Lead' must reach
    -- 'guardInFlight' with no interruptible point between, or the claimed slot leaks.
    decision <- atomically (decideSingleFlight sf key nowT)
    case decision of
        Hit weighted -> do
            recordRequest Metric.Hit
            touch sf weighted
            pure (Right (wValue weighted))
        Follow marker -> do
            recordRequest Metric.Miss
            outcome <- restore (atomically (readTMVar marker))
            case outcome of
                FlightValue fetched -> pure (Right fetched)
                FlightFault fault -> pure (Left fault)
                FlightOrphaned err -> case fromException err of
                    Just (_ :: SomeAsyncException) ->
                        -- Restore cancellation during retries. Keep the original miss count.
                        restore (resolveSingleFlight afterClaim (const pass) recordInsert sf key fetch)
                    -- Preserve synchronous leader faults outside the typed fetch channel.
                    Nothing -> throwIO err
        Lead marker -> do
            -- Mask publication and insertion so cancellation cannot strand followers.
            (outcome, occupancy) <- guardInFlight id (orphan marker) (atomically deregister) $ do
                recordRequest Metric.Miss
                fetched <- restore (afterClaim >> fetch)
                atomically (putTMVar marker (either FlightFault FlightValue fetched))
                -- The join collapses "nothing fetched" and "fetched but oversized,
                -- served uncached" into one no-insert outcome for the telemetry.
                inserted <- join <$> traverse (insertBounded sf key) (rightToMaybe fetched)
                pure (fetched, inserted)
            traverse_ recordInsert occupancy
            pure outcome
  where
    deregister :: STM ()
    deregister = do
        inFlight <- readTVar (sfInFlight sf)
        writeTVar (sfInFlight sf) (Map.delete key inFlight)

insertBounded :: (Hashable k) => SingleFlight e k v -> k -> v -> IO (Maybe CacheOccupancy)
insertBounded sf key value
    | weight == maxBound || weight > sfMaxBytes sf = pure Nothing
    | otherwise = withMVar (sfInsertLock sf) $ \() -> do
        nowT <- getTime Monotonic
        atomically $ do
            purgeExpired sf nowT
            deleteStored sf key
        evictToBudget sf weight
        stamp <- nextStamp sf
        stampRef <- newIORef stamp
        insertedAt <- getTime Monotonic
        let expires = insertedAt + sfTTL sf
            weighted = Weighted{wValue = value, wWeight = weight, wStamp = stampRef, wExpires = expires}
        atomically $ do
            Cache.insertSTM key weighted (sfStore sf) Nothing
            modifyTVar' (sfExpiry sf) (Map.insertWith HashMap.union expires (HashMap.singleton key weight))
            modifyTVar' (sfOccupancy sf) $ \occ ->
                CacheOccupancy (occEntries occ + 1) (occBytes occ + weight)
            Just <$> readTVar (sfOccupancy sf)
  where
    weight = sfWeigh sf value

evictToBudget :: (Hashable k) => SingleFlight e k v -> Int -> IO ()
evictToBudget sf incoming = do
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
        removed <- atomically $ do
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
            traverse_ (\key -> Cache.deleteSTM key (sfStore sf)) (HashMap.keys bucket)
            subtractOccupancy sf (HashMap.size bucket) (sum bucket)
            writeTVar (sfExpiry sf) rest
            purgeExpired sf nowT
        _ -> pass

nextStamp :: SingleFlight e k v -> IO Word64
nextStamp sf = atomicModifyIORef' (sfClock sf) (\n -> let n' = n + 1 in (n', n'))

touch :: SingleFlight e k v -> Weighted v -> IO ()
touch sf weighted = nextStamp sf >>= writeIORef (wStamp weighted)

-- | Read without fetching or refreshing recency.
lookupStore :: (Hashable k) => SingleFlight e k v -> k -> IO (Maybe v)
lookupStore sf key = fmap wValue <$> lookupWeighted sf key

-- | Read without fetching and refresh recency on a hit.
lookupStoreTouching :: (Hashable k) => SingleFlight e k v -> k -> IO (Maybe v)
lookupStoreTouching sf key =
    lookupWeighted sf key >>= traverse (\weighted -> wValue weighted <$ touch sf weighted)

lookupWeighted :: (Hashable k) => SingleFlight e k v -> k -> IO (Maybe (Weighted v))
lookupWeighted sf key = do
    nowT <- getTime Monotonic
    atomically (lookupWeightedSTM True sf key nowT)

lookupWeightedSTM :: (Hashable k) => Bool -> SingleFlight e k v -> k -> TimeSpec -> STM (Maybe (Weighted v))
lookupWeightedSTM eager sf key nowT = do
    held <- Cache.lookupSTM False key (sfStore sf) nowT
    case held of
        Just weighted | wExpires weighted < nowT -> do
            when eager (deleteStored sf key)
            pure Nothing
        _ -> pure held

-- The one atomic resolve decision: a fresh hit, follow an in-flight fetch, or lead a new
-- one. A hit carries the weighted entry so the caller can bump its recency.
data Decision e v
    = Hit (Weighted v)
    | Follow (TMVar (FlightOutcome e v))
    | Lead (TMVar (FlightOutcome e v))

-- The one atomic resolve decision for a key: a fresh hit wins, else follow the key's
-- in-flight fetch, else install a marker and lead. Runs inside 'resolveSingleFlight''s mask.
decideSingleFlight :: (Hashable k, Ord k) => SingleFlight e k v -> k -> TimeSpec -> STM (Decision e v)
decideSingleFlight sf key nowT = do
    hit <- lookupWeightedSTM False sf key nowT
    case hit of
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

-- | Entry count and summed accounted bytes after a retaining insert.
data CacheOccupancy = CacheOccupancy
    { occEntries :: Int
    , occBytes :: Int
    }

-- Convert a 'NominalDiffTime' (seconds) to the @cache@ library's monotonic
-- 'TimeSpec' via 'fromNanoSecs', clamping a negative TTL to zero.
toTimeSpec :: NominalDiffTime -> TimeSpec
toTimeSpec ttl = fromNanoSecs (max 0 (round (realToFrac ttl * 1e9 :: Double) :: Integer))
