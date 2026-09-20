-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Local request coalescing with optional, separately owned retention.
module Ecluse.Core.Server.Cache.Store (
    SingleFlight,
    newSingleFlight,
    newSingleFlightWithBackend,
    resolveSingleFlight,
    lookupStoreWithFailure,
    lookupStoreTouching,
    CacheOccupancy (..),
) where

import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)
import UnliftIO.Exception (SomeAsyncException, mask, throwIO)

import Ecluse.Core.InFlight (guardInFlight)
import Ecluse.Core.Server.Cache.Backend.Internal
import Ecluse.Core.Server.Cache.Backend.Local (newLocalBackend)
import Ecluse.Core.Telemetry.Metrics qualified as Metric

-- | Active requests own flight results. Completion removes the only registry reference.
data SingleFlight e k v = SingleFlight
    { sfBackend :: Maybe (RetentionBackend k v)
    , sfInFlight :: TVar (Map k (TMVar (FlightOutcome e v)))
    }

data FlightOutcome e v
    = FlightValue v
    | FlightFault e
    | FlightOrphaned SomeException

-- | Build a local bounded store. A weight of 'maxBound' means uncacheable.
newSingleFlight :: (Hashable k) => NominalDiffTime -> Int -> Int -> (v -> Int) -> IO (SingleFlight e k v)
newSingleFlight ttl maxEntries maxBytes weigh =
    newLocalBackend ttl maxEntries maxBytes weigh >>= newSingleFlightWithBackend . Just

-- | Coalesce requests without retaining completed values when the backend is absent.
newSingleFlightWithBackend :: Maybe (RetentionBackend k v) -> IO (SingleFlight e k v)
newSingleFlightWithBackend backend = SingleFlight backend <$> newTVarIO Map.empty

{- | Share active work and release waiters on failure or cancellation.
Capacity refusals and external backend faults use the refusal callback.
-}
resolveSingleFlight ::
    (Ord k) =>
    (Metric.CacheResult -> IO ()) ->
    (CacheOccupancy -> IO ()) ->
    IO () ->
    SingleFlight e k v ->
    k ->
    IO (Either e v) ->
    IO (Either e v)
resolveSingleFlight recordRequest recordOccupancy recordRefused sf key fetch = mask $ \restore ->
    let resolveAt reportRequest = do
            decision <- atomically (claimFlight sf key)
            case decision of
                Follow marker -> do
                    reportRequest Metric.Collapsed
                    outcome <- restore (atomically (readTMVar marker))
                    case outcome of
                        FlightValue fetched -> pure (Right fetched)
                        FlightFault fault -> pure (Left fault)
                        FlightOrphaned err -> case fromException err of
                            Just (_ :: SomeAsyncException) -> resolveAt (const pass)
                            Nothing -> throwIO err
                Lead marker ->
                    guardInFlight id (orphan marker) (atomically deregister) $ do
                        held <- restore (readBackend recordOccupancy recordRefused RefreshRecency sf key)
                        fetched <- case held of
                            Just value -> reportRequest Metric.Hit $> Right value
                            Nothing -> do
                                reportRequest Metric.Miss
                                result <- restore fetch
                                for_ (rightToMaybe result) $ \value ->
                                    for_ (sfBackend sf) $ \backend ->
                                        restore (rbInsert backend recordOccupancy recordRefused recordRefused key value)
                                pure result
                        atomically (putTMVar marker (either FlightFault FlightValue fetched))
                        pure fetched
     in resolveAt recordRequest
  where
    deregister = modifyTVar' (sfInFlight sf) (Map.delete key)

-- | Probe retention and report external failure without starting an upstream fetch.
lookupStoreWithFailure :: (CacheOccupancy -> IO ()) -> IO () -> SingleFlight e k v -> k -> IO (Maybe v)
lookupStoreWithFailure record failed = readBackend record failed PreserveRecency

-- | Read without fetching and refresh recency on a hit, reporting expiry removals.
lookupStoreTouching :: (CacheOccupancy -> IO ()) -> SingleFlight e k v -> k -> IO (Maybe v)
lookupStoreTouching record = readBackend record pass RefreshRecency

readBackend :: (CacheOccupancy -> IO ()) -> IO () -> Recency -> SingleFlight e k v -> k -> IO (Maybe v)
readBackend record failed recency sf key = case sfBackend sf of
    Nothing -> record (CacheOccupancy 0 0) $> Nothing
    Just backend -> rbLookup backend record failed recency key

data Decision e v
    = Follow (TMVar (FlightOutcome e v))
    | Lead (TMVar (FlightOutcome e v))

claimFlight :: (Ord k) => SingleFlight e k v -> k -> STM (Decision e v)
claimFlight sf key = do
    inFlight <- readTVar (sfInFlight sf)
    case Map.lookup key inFlight of
        Just marker -> pure (Follow marker)
        Nothing -> do
            marker <- newEmptyTMVar
            writeTVar (sfInFlight sf) (Map.insert key marker inFlight)
            pure (Lead marker)

orphan :: TMVar (FlightOutcome e v) -> SomeException -> IO ()
orphan marker err = atomically (void (tryPutTMVar marker (FlightOrphaned err)))
