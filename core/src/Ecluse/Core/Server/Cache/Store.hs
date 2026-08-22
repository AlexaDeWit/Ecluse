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

{- | A stored value paired with the bookkeeping the resident-byte budget and the
recency-aware eviction need. That is the value\'s estimated resident weight, fixed at
insert, and its last-access stamp, a per-entry cell bumped on each hit. The stamp lives
outside the STM store, so a hit updates recency without writing the shared container.
Eviction reads the stamp to pick the least-recently-used victim.
-}
data Weighted v = Weighted
    { wValue :: v
    -- ^ The cached value.
    , wWeight :: Int
    -- ^ The value's estimated resident footprint in bytes, fixed at insert.
    , wStamp :: IORef Word64
    -- ^ The value's last-access stamp, bumped on every hit and read by eviction.
    }

{- | One TTL- and STM-backed store with the resident-byte budget, the entry-count bound,
and the in-flight map that gives single-flight. Opaque: built with 'newSingleFlight' and
driven through 'resolveSingleFlight' and the read-only views. Entries are wrapped in
'Weighted', so the byte budget and the least-recently-used eviction have the weight and
the access stamp they need.
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
    {- ^ Serialises the purge\/evict\/insert sequence. Without it, two different-key
    leaders can both read the pre-insert resident sum and both admit, which lands the
    store past its byte budget. Held only on the post-fetch cold path (never by a
    hit or a follower), and never inside an STM transaction.
    -}
    , sfInFlight :: TVar (Map k (TMVar (FlightOutcome e v)))
    {- ^ Entries currently being fetched, so concurrent misses coalesce onto one
    fetch rather than each launching its own. The marker carries the leader's
    __typed__ outcome, so a fetch failure reaches every follower as the same value
    the leader saw.
    -}
    }

{- The outcome an in-flight marker delivers to coalesced followers. It is the fetched
value, the fetch's typed failure (nothing cached), or the error that killed the leader
before it published either. The fault and orphan arms are held apart on purpose. A
'FlightFault' is the fetch's own total channel, handed typed to every waiter. A
'FlightOrphaned' is an exception event the follower re-resolves or re-raises: an async
cancellation, or a leader that broke the fetch's total contract. -}
data FlightOutcome e v
    = FlightValue v
    | FlightFault e
    | FlightOrphaned SomeException

{- | Build a store from its tunables (the TTL, the entry-count bound, and the
resident-byte budget) and a value weigher. The TTL is converted to the @cache@
library's monotonic 'TimeSpec', and both bounds are clamped to at least one. The
access clock starts at zero and the in-flight map empty.
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

{- | The store's single-flight resolution. A fresh hit short-circuits. Otherwise the
caller leads one fetch (installing an in-flight marker) or follows an in-flight one.
The @recordRequest@ callback records the hit\/miss counter, or ignores it. The
@recordInsert@ callback refreshes the occupancy gauges from the post-insert
'CacheOccupancy' after a leader insert, or ignores it. Each caller therefore wires its
own telemetry without the resolution logic knowing which store it serves. The first
argument is a hook run on the leading thread at the single-flight claim → fetch-runner
handoff. A test can then park a leader in that window deterministically. Production
callers pass @pure ()@.

A hit bumps the entry's recency stamp before returning it. The bump runs in plain 'IO',
so recency is updated without writing the shared STM store and a hit never contends with
a concurrent resolution. On a miss the fetch runs exactly once even under concurrent
callers. A successful fetch is cached, subject to the TTL, the entry-count bound, and the
resident-byte budget. A failed fetch caches __nothing__, and its typed 'Left' reaches
every waiter.

A claimed slot is __always eventually filled and de-registered__, even under an async
exception in the claim → runner window. The claim commits under a 'mask', and the run
goes straight to 'Ecluse.Core.InFlight.guardInFlight'. That frees the slot on every exit
and hands the orphaning error to any waiting follower, which closes the single-flight
orphan window. An async orphan re-resolves __under @restore@__, so the retried fetch and
its parse stay cancellable, never masked, and the already-recorded miss is not counted
again. A synchronous one re-raises: the fetch is total, so that arm is the invariant
channel, not an outcome. A follower's own wait stays interruptible. The result is
inserted __before__ the slot is de-registered. A caller arriving the instant the fetch
returns therefore becomes a follower, and does not re-lead a redundant fetch.
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
    -- One atomic decision point under the enclosing 'mask'. A 'Hit' or 'Follow' claims
    -- nothing: its wait runs under @restore@, interruptible. A 'Lead' installs the
    -- marker and hands the run to 'guardInFlight' with no interruptible point between.
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
                        -- Leader cancelled (e.g. a client disconnect): re-resolve rather
                        -- than die with it, under the outer @restore@. A bare recursion
                        -- re-enters under this @mask@. Its inner @mask@ would then hand back
                        -- a @restore@ to 'MaskedInterruptible', and run the whole retried
                        -- fetch and its CPU-bound parse masked. The @restore@ here unmasks
                        -- first, so the retry's own mask restores to unmasked and the fetch
                        -- stays cancellable. The retry silences @recordRequest@: this
                        -- caller's miss was counted above. One logical miss therefore stays
                        -- one 'Metric.Miss' across any number of orphan retries.
                        restore (resolveSingleFlight afterClaim (const pass) recordInsert sf key fetch)
                    -- A leader that escaped synchronously broke the fetch's total
                    -- contract. That is an invariant break, re-raised as-is for the outer
                    -- boundary, never laundered into the typed channel.
                    Nothing -> throwIO err
        Lead marker -> do
            recordRequest Metric.Miss
            -- Only the fetch runs under @restore@, cancellable. The publish and insert run
            -- under the enclosing 'mask', so a cancel after the fetch returns still
            -- delivers and inserts. 'guardInFlight' is passed 'id'. It frees the slot on
            -- every exit and, on an escape before the marker is filled, hands the error to
            -- followers via 'orphan'. The insert precedes de-registration, so "collapse to
            -- one fetch" holds even for a caller arriving the instant the fetch returns. A
            -- 'Left' publishes the fault to every waiter and inserts nothing, so a failed
            -- fetch caches nothing by construction.
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

{- | Insert a freshly fetched value into a store, enforcing the resident-byte budget and
the entry-count bound. Expired entries are purged first, the cheap reclaim. The
least-recently-used entries are then evicted until the incoming value fits within both
bounds. The value is inserted with its estimated weight and a fresh recency stamp.

A value whose weight alone exceeds the byte budget is __not retained__. 'Nothing' is
returned, nothing resident is evicted (room that cannot exist is not made), and the
caller serves the value uncached. The budget therefore bounds the store's residency, and
one pathological document can never flush it. The whole purge\/evict\/insert sequence
runs under the store's insert lock. Two different-key leaders therefore cannot both read
the pre-insert resident sum and both admit past the budget. Returns the store's occupancy
after a retaining insert, for the residency telemetry.
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
the resident-byte budget and the entry-count bound, or until the store is empty. The
sweep scans the store, orders entries by ascending recency stamp (oldest
first), and drops the coldest one at a time. It stops once @resident + incoming@ is
within the budget and the count leaves room for one more. Reaching an empty store also
stops the sweep, so the incoming value is always admitted afterwards. The scan runs only
on a leader's insert (the cold path after a fetch), so iterating the held entries is off
the hot path.
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

{- | Look up a key's stored value without fetching on a miss and without bumping recency:
the store's read-only view, for inspection and tests. A 'Nothing' is a miss or an expired
entry. This never triggers a fetch and never collapses (use 'resolveSingleFlight' on a
serve path).
-}
lookupStore :: (Hashable k) => SingleFlight e k v -> k -> IO (Maybe v)
lookupStore sf key = fmap wValue <$> Cache.lookup (sfStore sf) key

{- | Look up a key's stored value like 'lookupStore', but __bump the entry's recency__ on
a hit: the serve path's read. An entry read through it stays resident under the
least-recently-used eviction rather than ageing out in insert order. It is the same
'Cache.lookup' 'lookupStore' runs, followed by the same 'touch' a 'Hit' takes. A read
here and a hit through 'resolveSingleFlight' therefore age an entry identically. It still
never fetches and never collapses, and a 'Nothing' is a miss or an expired entry. The bump
is a plain 'IORef' write, never STM, so a read does not contend with a concurrent
resolution.
-}
lookupStoreTouching :: (Hashable k) => SingleFlight e k v -> k -> IO (Maybe v)
lookupStoreTouching sf key =
    Cache.lookup (sfStore sf) key >>= traverse (\weighted -> wValue weighted <$ touch sf weighted)

-- The outcome of the one atomic resolve decision: a fresh hit, follow an in-flight
-- fetch, or lead a new one. A hit carries the weighted entry, so the caller can bump
-- its recency.
data Decision e v
    = Hit (Weighted v)
    | Follow (TMVar (FlightOutcome e v))
    | Lead (TMVar (FlightOutcome e v))

-- The one atomic resolve decision for a key. A fresh, unexpired hit wins, else follow
-- the key's in-flight fetch, else install an in-flight marker and lead. One STM
-- transaction, run inside 'resolveSingleFlight''s mask.
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

-- The orphan hand-off: an escape before the marker was filled. Fill it with the error
-- so blocked followers unblock rather than parking forever. 'guardInFlight' frees the
-- slot separately. Fills only when empty, so an escape after a successful publish never
-- clobbers the result.
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
