-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | One TTL- and STM-backed single-flight store under a resident-byte budget. Every
store of the metadata cache ("Ecluse.Core.Server.Cache") takes this shape, factored here
so the machinery is written once over any key and value.

The @cache@ library supplies the TTL store. Two properties it does not provide on
its own are layered here:

* __Resident-byte budget with recency-aware eviction.__ @cache@ expires by TTL but
  bounds neither entry count nor memory. Each value is wrapped with an estimate of its
  resident footprint (from the store's injected weigher) and a last-access stamp bumped
  on every hit. An insert first purges expired entries. It then evicts the
  __least-recently-used__ entries until the incoming value fits within both the
  resident-byte budget and the entry-count bound. Recency keeps a re-accessed hot head
  resident under pressure while shedding the one-shot tail. The byte budget bounds
  memory more faithfully than a count alone. A value whose weight alone exceeds the byte
  budget is __passed through uncached__. The caller still serves it: the per-value
  ceiling is the caller's concern, an upstream body cap. Nothing resident is evicted to
  make room that cannot exist, so the store's budget genuinely bounds its residency.
  Inserts serialise on a per-store lock, so two leaders' evict-then-insert sequences
  cannot interleave past the budget. The lock is post-fetch cold path only, and a
  leader publishes its marker __before__ inserting, so no follower ever blocks on it.

* __Single-flight.__ @cache@'s own @fetchWithCache@ is lookup-then-fetch in plain
  'IO', so two concurrent misses would both fetch. 'resolveSingleFlight' instead
  installs an in-flight marker atomically, so the first miss fetches while
  concurrent misses wait on its result. The leader inserts the result into the store
  __before__ removing its marker. A caller arriving the instant the fetch returns
  therefore finds either the store entry or the marker, never a gap. It never re-leads a
  redundant fetch. A fetch's typed failure reaches every waiter and caches nothing. A
  claimed slot is always eventually filled and de-registered, even when the leader dies
  to an async exception (see 'resolveSingleFlight').

The store never knows which cache it serves. The key and value are type parameters, and
the weigher enters at construction. Telemetry enters per resolution as two callbacks:
the hit\/miss recording and the post-insert occupancy recording. The domain semantics
live entirely with the caller: what a key means, which upstream a value came from, and
what may be shared across clients.
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
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)
import System.Clock (Clock (Monotonic), TimeSpec, fromNanoSecs, getTime)
import UnliftIO.Exception (SomeAsyncException, mask, throwIO)
import UnliftIO.MVar (withMVar)

import Ecluse.Core.InFlight (guardInFlight)
import Ecluse.Core.Telemetry.Metrics qualified as Metric

{- | A stored value with the weight the byte budget needs and the stamp eviction needs.
The stamp sits outside the STM store, so a hit refreshes recency without writing it.
-}
data Weighted v = Weighted
    { wValue :: v
    -- ^ The cached value.
    , wWeight :: Int
    -- ^ The value's estimated resident footprint in bytes, fixed at insert.
    , wStamp :: IORef Word64
    -- ^ The value's last-access stamp, bumped on every hit and read by eviction.
    }

{- | A bounded, TTL- and STM-backed store whose misses collapse onto one fetch per key.
Opaque: build it with 'newSingleFlight' and drive it through 'resolveSingleFlight'.
-}
data SingleFlight e k v = SingleFlight
    { sfStore :: Cache k (Weighted v)
    -- ^ The TTL- and STM-backed store (the @cache@ library), holding weighted values.
    , sfMaxEntries :: Int
    -- ^ The entry-count bound enforced on insert.
    , sfMaxBytes :: Int
    -- ^ The resident-byte budget enforced on insert.
    , sfWeigh :: v -> Int
    -- ^ Estimate a value's resident footprint in bytes, fixed into its 'Weighted' at insert.
    , sfClock :: IORef Word64
    {- ^ The store's logical access clock, bumped to issue each entry's recency stamp on
    insert and on every hit.
    -}
    , sfInsertLock :: MVar ()
    {- ^ Serialises the purge\/evict\/insert sequence. Without it, two different-key leaders
    both read the pre-insert resident sum and both admit, landing the store past its byte
    budget. Held only on the post-fetch cold path, and never inside an STM transaction.
    -}
    , sfInFlight :: TVar (Map k (TMVar (FlightOutcome e v)))
    {- ^ Entries currently being fetched, so concurrent misses coalesce onto one fetch. The
    marker carries the leader's __typed__ outcome, so a fetch failure reaches every follower
    as the same value the leader saw.
    -}
    }

{- The outcome an in-flight marker delivers to coalesced followers. The two failure arms
are held apart on purpose: 'FlightFault' is the fetch's own typed failure, handed to
every waiter with nothing cached, while 'FlightOrphaned' is an exception the follower
re-resolves or re-raises. -}
data FlightOutcome e v
    = FlightValue v
    | FlightFault e
    | FlightOrphaned SomeException

{- | Build a store from its TTL, entry-count bound, resident-byte budget, and value
weigher, in that order. Each bound is clamped to at least one.
-}
newSingleFlight :: NominalDiffTime -> Int -> Int -> (v -> Int) -> IO (SingleFlight e k v)
newSingleFlight ttl maxEntries maxBytes weigh = do
    store <- Cache.newCache (Just (toTimeSpec ttl))
    clock <- newIORef 0
    inFlight <- newTVarIO Map.empty
    insertLock <- newMVar ()
    pure
        SingleFlight
            { sfStore = store
            , sfMaxEntries = max 1 maxEntries
            , sfMaxBytes = max 1 maxBytes
            , sfWeigh = weigh
            , sfClock = clock
            , sfInsertLock = insertLock
            , sfInFlight = inFlight
            }

{- | Resolve a key through the store: serve a fresh hit, follow an in-flight fetch, or
lead a new one. @afterClaim@ is a test hook run on the leading thread between the claim
and the fetch (production passes @pure ()@), @recordRequest@ counts the hit or miss, and
@recordInsert@ refreshes the occupancy gauges after a leading insert.

The fetch runs exactly once per key under concurrent callers. A failed fetch caches
__nothing__ and its typed 'Left' reaches every waiter. A claimed slot is always
eventually filled and de-registered, even under an async exception, and the insert
precedes de-registration, so a caller arriving as the fetch returns follows rather than
re-leads.
-}
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
            -- Bump recency outside the STM transaction: a hit updates the per-entry stamp
            -- without writing the shared store, and the eviction still sees it.
            touch sf weighted
            pure (Right (wValue weighted))
        Follow marker -> do
            -- A follower coalesced onto an in-flight fetch is a miss for this caller
            -- (no fresh entry was present), exactly as the leader's miss is.
            recordRequest Metric.Miss
            outcome <- restore (atomically (readTMVar marker))
            case outcome of
                FlightValue fetched -> pure (Right fetched)
                -- The typed hand-off: the leader's fetch reported a failure value, so
                -- every waiter receives the same 'Left', and nothing was cached.
                FlightFault fault -> pure (Left fault)
                FlightOrphaned err -> case fromException err of
                    Just (_ :: SomeAsyncException) ->
                        -- Leader cancelled (a client disconnect, say): re-resolve rather than
                        -- die with it. The retry runs under @restore@ because a bare recursion
                        -- re-enters masked, which would run the retried fetch and its parse
                        -- uncancellable. @recordRequest@ is silenced so this caller's counted
                        -- miss stays one 'Metric.Miss' across any number of retries.
                        restore (resolveSingleFlight afterClaim (const pass) recordInsert sf key fetch)
                    -- A synchronous escape broke the fetch's total contract. Re-raise it
                    -- as an invariant break, never laundered into the typed channel.
                    Nothing -> throwIO err
        Lead marker -> do
            recordRequest Metric.Miss
            -- Only the fetch runs under @restore@. The publish and the insert run under the
            -- enclosing 'mask', so a cancel after the fetch returns still delivers to followers
            -- and still inserts. 'guardInFlight' frees the slot on every exit and hands any
            -- escape to a waiting follower.
            (outcome, occupancy) <- guardInFlight id (orphan marker) (atomically deregister) $ do
                fetched <- restore (afterClaim >> fetch)
                atomically (putTMVar marker (either FlightFault FlightValue fetched))
                -- The join collapses "nothing fetched" and "fetched but oversized,
                -- served uncached" into one no-insert outcome for the telemetry.
                inserted <- join <$> traverse (insertBounded sf key) (rightToMaybe fetched)
                pure (fetched, inserted)
            -- The leader inserted, so refresh the occupancy gauges (a follower never does).
            traverse_ recordInsert occupancy
            pure outcome
  where
    deregister :: STM ()
    deregister = do
        inFlight <- readTVar (sfInFlight sf)
        writeTVar (sfInFlight sf) (Map.delete key inFlight)

{- | Insert a fetched value, purging expired entries and evicting the least recently used
until it fits the byte budget and the entry-count bound. Returns the store's occupancy
after a retaining insert, for the residency telemetry.

A value heavier than the whole byte budget is __not retained__: 'Nothing' comes back,
nothing resident is evicted, and the caller serves it uncached, so one pathological
document can never flush the store.
-}
insertBounded :: (Hashable k) => SingleFlight e k v -> k -> v -> IO (Maybe CacheOccupancy)
insertBounded sf key value
    | weight > sfMaxBytes sf = pure Nothing
    | otherwise = withMVar (sfInsertLock sf) $ \() -> do
        Cache.purgeExpired (sfStore sf)
        evictToBudget sf weight
        stamp <- nextStamp sf
        stampRef <- newIORef stamp
        Cache.insert (sfStore sf) key (Weighted{wValue = value, wWeight = weight, wStamp = stampRef})
        Just <$> occupancyOf sf
  where
    weight = sfWeigh sf value

{- | Evict least-recently-used entries until an incoming value of the given weight fits
the byte budget and the entry-count bound, or until the store is empty. Emptying stops
the sweep too, so the incoming value is admitted afterwards either way. The scan runs
only on a leader's post-fetch insert, so iterating the held entries stays off the hot path.
-}
evictToBudget :: (Hashable k) => SingleFlight e k v -> Int -> IO ()
evictToBudget sf incoming = do
    held <- Cache.toList (sfStore sf)
    stamped <- traverse stampOf held
    let resident = sum [wWeight w | (_, w, _) <- held]
        oldestFirst = sortOn (\(stamp, _, _) -> stamp) stamped
    go oldestFirst resident (length held)
  where
    stampOf (k, w, _) = do
        s <- readIORef (wStamp w)
        pure (s, k, wWeight w)

    fits resident count = resident + incoming <= sfMaxBytes sf && count < sfMaxEntries sf

    go victims resident count
        | fits resident count = pass
        | otherwise = case victims of
            [] -> pass
            ((_, k, weight) : rest) -> do
                Cache.delete (sfStore sf) k
                go rest (resident - weight) (count - 1)

-- The store's occupancy after an insert: the entry count and the summed resident weight
-- of the held entries, the values the residency telemetry reports.
occupancyOf :: SingleFlight e k v -> IO CacheOccupancy
occupancyOf sf = do
    held <- Cache.toList (sfStore sf)
    pure CacheOccupancy{occEntries = length held, occBytes = sum [wWeight w | (_, w, _) <- held]}

-- Issue the next logical access stamp from the store's clock: a strictly increasing
-- 'Word64', so a larger stamp is more recent.
nextStamp :: SingleFlight e k v -> IO Word64
nextStamp sf = atomicModifyIORef' (sfClock sf) (\n -> let n' = n + 1 in (n', n'))

-- Bump a held entry's recency to the current logical time, marking it most-recently-used.
-- Runs in plain 'IO' (never STM), so a hit refreshes recency without writing the store.
touch :: SingleFlight e k v -> Weighted v -> IO ()
touch sf weighted = nextStamp sf >>= writeIORef (wStamp weighted)

{- | Look up a stored value without fetching on a miss and without bumping recency: the
read-only view for inspection and tests. 'Nothing' is a miss or an expired entry.
-}
lookupStore :: (Hashable k) => SingleFlight e k v -> k -> IO (Maybe v)
lookupStore sf key = fmap wValue <$> Cache.lookup (sfStore sf) key

{- | Look up a stored value like 'lookupStore', but bump the entry's recency on a hit:
the serve path's read. It ages an entry exactly as a hit through 'resolveSingleFlight'
does, still never fetches, and 'Nothing' is a miss or an expired entry.
-}
lookupStoreTouching :: (Hashable k) => SingleFlight e k v -> k -> IO (Maybe v)
lookupStoreTouching sf key =
    Cache.lookup (sfStore sf) key >>= traverse (\weighted -> wValue weighted <$ touch sf weighted)

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
    hit <- Cache.lookupSTM False key (sfStore sf) nowT
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

{- | A store's occupancy after a leader's insert: the held entry count and their summed
resident weight, the values the occupancy and residency gauges report.
-}
data CacheOccupancy = CacheOccupancy
    { occEntries :: Int
    , occBytes :: Int
    }

-- Convert a 'NominalDiffTime' (seconds) to the @cache@ library's monotonic
-- 'TimeSpec' via 'fromNanoSecs', clamping a negative TTL to zero.
toTimeSpec :: NominalDiffTime -> TimeSpec
toTimeSpec ttl = fromNanoSecs (max 0 (round (realToFrac ttl * 1e9 :: Double) :: Integer))
