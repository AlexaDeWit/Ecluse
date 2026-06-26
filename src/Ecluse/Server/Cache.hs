{- | The short-TTL, size-bounded metadata cache shared by the serve paths.

Resolving a package re-fetches its upstream packument, parses it, and evaluates
the rules. To avoid repeating the fetch+parse, the result — a coherent pair of the
parsed __packument metadata__ ('PackageInfo') and the __raw document__ it was decoded
from ('CacheEntry') — is held here in a short-TTL, size-bounded, STM-backed cache
(the @cache@ library backs the TTL store). Both serve paths share it: a packument
request and the tarball-gating fetch that follows reuse one fetch+parse, and
concurrent resolutions of a popular package __collapse to one upstream call__
(single-flight).

== Per-source key

A packument is fetched from __two distinct upstreams__ — a private origin and a public
origin — whose documents differ for the same package, so one entry cannot represent
both. The key is therefore @(source, package)@: the source is the upstream's base
URL, which distinguishes any cached origin without naming a credential, so distinct
upstreams never cross-contaminate and the key never blurs the trust split.

== Credential-free; sharing is the caller's policy

This cache is __strategy-neutral__: its key carries __no credential dimension__ (it
is @(source, package)@) and its value is a canonical document, so it stores nothing
derived from a caller's credential. Whether a given origin is /handed/ to it — and so
shared across clients — is the serve path's decision, not the cache's.

Under the default @passthrough@ access strategy only the __anonymous public origin__ is
cached: the trusted private upstream is the __per-client authority__ — it re-authorises
each client's request with that client's own forwarded credential — so the serve path
fetches it per request and never hands it to this cache. Were a private entry cached
under @passthrough@, the credential-free key would let one client's entry serve another
client's private document within the TTL, bypassing the upstream's authorisation. The
public origin is anonymous (no client credential), so one shared entry serves every client
without crossing any trust boundary. Other strategies make a shared private entry safe
by authorising each serve before it is returned (see
@docs\/architecture\/access-model.md@ → "Caching"); that gate lives on the serve path,
never in this credential-free store.

== Coherent pair

An entry holds the parsed 'PackageInfo' __and__ the raw 'Value' it was decoded from,
so a hit returns a typed view and the exact bytes that produced it — never a
mismatched pair. The packument serve path needs both: it decides over the typed view
but serves the raw document edited in place, and the two must describe the same fetch.

What is cached is the __metadata, not the verdict__. The rules are re-evaluated on
the cached metadata each request, so time-sensitive rules
('Ecluse.Core.Rules.Types.AllowIfPublishedBefore') and the separately-synced advisory
tier stay correct — only each upstream's fetch+parse is memoised, never a
decision. The TTL is short and brief staleness is benign and even aligned with the
resilience posture: a brand-new publish need not appear instantly (see
@docs\/architecture\/web-layer.md@ → "Metadata cache").

Two properties the @cache@ library does not provide on its own are layered here:

* __Size bound.__ @cache@ expires by TTL but never bounds entry count, so an
  insert that would exceed 'cacheMaxEntries' first purges expired entries and then
  evicts surplus ones — a safety valve against unbounded growth under a flood of
  distinct packages, not a precision LRU (eviction order among live entries is
  unspecified).

* __Single-flight.__ @cache@'s own @fetchWithCache@ is lookup-then-fetch in plain
  'IO', so two concurrent misses would both fetch. 'resolveMetadata' instead
  installs an in-flight marker atomically, so the first miss fetches while
  concurrent misses wait on its result — collapsing a thundering herd to one
  upstream call. The leader inserts the result into the store __before__ removing
  its in-flight marker, so a caller arriving in the instant the fetch returns still
  finds either the store entry or the marker (never a gap) and never re-leads a
  redundant fetch.
-}
module Ecluse.Server.Cache (
    -- * Configuration
    CacheConfig (..),
    defaultCacheConfig,

    -- * The cache handle
    MetadataCache,
    newMetadataCache,

    -- * Cache entries
    Source (..),
    CacheEntry (..),

    -- * Resolution
    resolveMetadata,
    resolveMetadataWith,
    cachedMetadata,
    cacheSize,
) where

import Data.Aeson (Value)
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)
import System.Clock (Clock (Monotonic), TimeSpec (TimeSpec), getTime)
import UnliftIO.Exception (mask, throwIO, try, withException)

import Ecluse.Core.Package (
    PackageInfo,
    PackageName,
    pkgCanonical,
    pkgEcosystem,
    pkgNamespace,
    renderScope,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Telemetry.Instruments (Metrics, recordCacheEntries, recordCacheRequest)

-- ── configuration ────────────────────────────────────────────────────────────

{- | The metadata cache's tunables, sourced from configuration (see
"Ecluse.Config"): how long a parsed packument stays fresh, and how many distinct
@(source, package)@ entries the cache holds before it evicts.
-}
data CacheConfig = CacheConfig
    { cacheTtl :: NominalDiffTime
    {- ^ How long a cached 'CacheEntry' is served before it is re-fetched. Short
    by design — brief staleness is benign, and conditional-GET revalidates.
    -}
    , cacheMaxEntries :: Int
    {- ^ The maximum number of distinct @(source, package)@ entries held; an insert
    past this evicts.
    -}
    }
    deriving stock (Eq, Show)

{- | The default cache tunables: a 60-second TTL and 1024 entries — short enough
that a new publish appears promptly, large enough to absorb a normal install's
working set of packages.
-}
defaultCacheConfig :: CacheConfig
defaultCacheConfig =
    CacheConfig
        { cacheTtl = 60
        , cacheMaxEntries = 1024
        }

-- ── cache entries ──────────────────────────────────────────────────────────────

{- | Which upstream a cached packument was fetched from — the dimension that
partitions the cache by source so distinct upstreams never share an entry.

The discriminator is the upstream's __base URL__: an upstream is addressed at a
distinct URL, and the URL names a location, never a credential, so keying on it
keeps the trust split intact (the cached origin is fetched with its own token, supplied
through its fetch action; the source carries none). Under the default @passthrough@
strategy only the anonymous public origin is cached, so in practice the cache holds one
source per package; the dimension keeps the key honest about /which/ upstream an entry
is, never blurring the split.
-}
newtype Source = Source Text
    deriving stock (Eq, Ord, Show)

{- | A coherent cache entry: the parsed 'PackageInfo' paired with the raw 'Value' it
was decoded from. A hit returns both, so a caller gets a typed view to decide over
and the exact bytes that produced it — the packument serve path edits the raw 'Value'
in place and must keep its typed decision coherent with those bytes.
-}
data CacheEntry = CacheEntry
    { entryInfo :: PackageInfo
    -- ^ The typed packument view the rules and merge reason over.
    , entryRaw :: Value
    -- ^ The raw upstream document the served body is built from, edited in place.
    }
    deriving stock (Eq, Show)

-- ── the cache handle ─────────────────────────────────────────────────────────

{- | The key a 'CacheEntry' is cached under: the upstream 'Source' paired with the
package's identity, rendered to a stable 'Text'. The package identity is distinct
from a display name so two encodings of the same scoped package share one entry, and
the source dimension keeps distinct upstreams apart — equality and ordering match
@(Source, PackageName)@ identity (the @cache@ library needs a 'Hashable' key, which
the opaque 'PackageName' does not expose, so the identity is projected to this key
here rather than via an orphan instance).
-}
newtype CacheKey = CacheKey Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable)

{- | Project a 'Source' and a 'PackageName' to their cache key (the source's base URL
joined with the package's identity, not its display form).
-}
cacheKey :: Source -> PackageName -> CacheKey
cacheKey (Source source) name =
    CacheKey
        ( source
            <> "\x1f"
            <> show (pkgEcosystem name)
            <> "\x1f"
            <> maybe "" renderScope (pkgNamespace name)
            <> "\x1f"
            <> pkgCanonical name
        )

{- | The metadata-cache handle: the TTL store, the size bound, and the in-flight
map that gives single-flight. Opaque — built with 'newMetadataCache' and reached
only through the accessors. Lives in 'Ecluse.Env.Env' (one per process), so every
request shares the same cache and its connection-collapsing.
-}
data MetadataCache = MetadataCache
    { mcStore :: Cache CacheKey CacheEntry
    -- ^ The TTL- and STM-backed store (the @cache@ library).
    , mcMaxEntries :: Int
    -- ^ The entry-count bound enforced on insert.
    , mcInFlight :: TVar (Map CacheKey (TMVar (Either SomeException CacheEntry)))
    {- ^ Entries currently being fetched, so concurrent misses coalesce onto one
    fetch rather than each launching their own.
    -}
    }

{- | Build a metadata cache from its configuration. The TTL is converted to the
@cache@ library's monotonic 'TimeSpec'; the in-flight map starts empty.
-}
newMetadataCache :: CacheConfig -> IO MetadataCache
newMetadataCache cfg = do
    store <- Cache.newCache (Just (toTimeSpec (cacheTtl cfg)))
    inFlight <- newTVarIO Map.empty
    pure
        MetadataCache
            { mcStore = store
            , mcMaxEntries = max 1 (cacheMaxEntries cfg)
            , mcInFlight = inFlight
            }

-- ── resolution ───────────────────────────────────────────────────────────────

{- | Resolve a package's metadata from one upstream 'Source', reusing the cache and
collapsing concurrent misses.

On a fresh, unexpired hit the cached 'CacheEntry' is returned and the fetch action
is never run. On a miss the action runs exactly once even under concurrent callers:
the first installs an in-flight marker and fetches, the others wait on its result.
A successful fetch is cached (subject to the TTL and size bound); a failed fetch
caches __nothing__ (so a transient upstream error does not poison the cache) and is
re-raised to every waiter.

A claimed in-flight slot is __always eventually filled and de-registered__, even if
the leader is hit by an async exception (a request timeout, a killed handler thread)
between claiming the slot and completing: the claim and the leader's cleanup-armed
run live under __one mask__, so no interruptible point sits in the gap, and any
exception before normal completion fills the marker with that error — unblocking
every waiting follower with it rather than leaving them parked forever — and frees
the slot so a later call re-leads. This closes the single-flight orphan window
(without it, a cancelled leader would wedge that @(source, package)@ key until
restart). A follower's own wait on the marker stays interruptible.

The 'Source' partitions the cache: distinct upstreams of the same package resolve
under distinct keys and never cross-contaminate. The fetch action supplies the origin's
own credential, so reading through one source never blurs another's trust posture.
Under the default @passthrough@ strategy only the anonymous public origin is resolved
here — the trusted private origin is the per-client authority and is fetched per request,
never cached, so a shared entry can never serve one client another's private document.

The result is always re-decided by the caller's rules on each request — only the
fetch+parse is memoised, never the verdict.

Each resolution records the @ecluse.metadata_cache.requests@ hit\/miss counter (a
coalescing follower counts as a miss, like the leader it waits on), and a leader's
insert refreshes the @ecluse.metadata_cache.entries@ occupancy gauge.
-}
resolveMetadata :: Metrics -> MetadataCache -> Source -> PackageName -> IO CacheEntry -> IO CacheEntry
resolveMetadata = resolveMetadataWith (pure ())

{- | As 'resolveMetadata', but with a hook run on the leading thread at the
single-flight claim → fetch-runner handoff: the window between the STM transaction
committing the in-flight claim and the leader's exception guard taking ownership of
the marker. It exists only so a test can deterministically park a leader in that
window and cancel it there, exercising the orphan-window guarantee; production always
passes @pure ()@ via 'resolveMetadata'.
-}
resolveMetadataWith :: IO () -> Metrics -> MetadataCache -> Source -> PackageName -> IO CacheEntry -> IO CacheEntry
resolveMetadataWith afterClaim metrics cache source name fetch = mask $ \restore -> do
    let key = cacheKey source name
    nowT <- getTime Monotonic
    -- One atomic decision point: a fresh hit short-circuits; otherwise become the
    -- leader (install an empty marker) or a follower (take the existing one). The
    -- decision and — for a leader — the run that owns the freshly claimed marker
    -- are under one 'mask', so no interruptible point sits between claiming the
    -- slot and arming the cleanup that always fills and de-registers it. A 'Hit' or
    -- 'Follow' claims nothing, so its wait runs under @restore@ and stays
    -- interruptible.
    decision <- atomically (decide key nowT)
    case decision of
        Hit entry -> do
            recordCacheRequest metrics Metric.Hit
            pure entry
        Follow marker -> do
            -- A follower coalesced onto an in-flight fetch is a miss for this caller
            -- (no fresh entry was present), exactly as the leader's miss is.
            recordCacheRequest metrics Metric.Miss
            restore (either throwIO pure =<< atomically (readTMVar marker))
        Lead marker -> do
            recordCacheRequest metrics Metric.Miss
            runLeader restore key marker
  where
    decide :: CacheKey -> TimeSpec -> STM Decision
    decide key nowT = do
        hit <- Cache.lookupSTM False key (mcStore cache) nowT
        case hit of
            Just entry -> pure (Hit entry)
            Nothing -> do
                inFlight <- readTVar (mcInFlight cache)
                case Map.lookup key inFlight of
                    Just marker -> pure (Follow marker)
                    Nothing -> do
                        marker <- newEmptyTMVar
                        writeTVar (mcInFlight cache) (Map.insert key marker inFlight)
                        pure (Lead marker)

    -- The leader fetches once, fills the marker, and de-registers itself. The claim
    -- is made under the caller's 'mask' and this run is reached with no interruptible
    -- point in between, so the 'withException' guard is armed before anything can
    -- interrupt. A synchronous fetch error is caught by 'try' and folded to a 'Left'
    -- that the normal failure path fills and de-registers; an __asynchronous__
    -- exception (a request timeout, a killed handler thread, 'ThreadKilled') landing
    -- anywhere in the claim → completion window is re-raised by 'try', so the guard
    -- fills the still-empty marker with that exception — unblocking every waiting
    -- follower with it, never a forever-park — and frees the slot so a later call
    -- re-leads, before the exception propagates to this (cancelled) thread. The fetch
    -- runs under @restore@ so it stays cancellable; the marker fill, store insert, and
    -- de-register run masked and never block, so the tail is uninterruptible.
    --
    -- On success the result is __inserted into the store before the in-flight slot
    -- is de-registered__: until 'insertBounded' completes the slot still exists, so
    -- a late caller in 'decide' becomes a follower on the marker rather than finding
    -- neither store hit nor in-flight slot and re-leading a redundant fetch. Insert
    -- then deregister thus makes "collapse to one upstream call" hold even for a
    -- caller arriving in the instant after the fetch returns. On failure nothing is
    -- cached and the slot is freed so a later retry re-fetches.
    runLeader restore key marker = do
        result <- try (restore (afterClaim >> fetch)) `withException` orphan key marker
        atomically (putTMVar marker result)
        case result of
            Right entry -> do
                insertBounded cache key entry
                atomically (deregister key)
                -- The leader inserted, so the occupancy gauge is refreshed off the
                -- post-insert size (a follower never inserts, so it never re-records).
                recordCacheEntries metrics =<< cacheSize cache
                pure entry
            Left err -> do
                atomically (deregister key)
                throwIO err

    -- The exception guard for the claim → completion window. 'withException' runs it
    -- with the offending exception, then re-raises that exception to the leader's own
    -- thread. Fill the still-empty marker with it (so blocked followers unblock with
    -- the error rather than parking forever), then free the slot so a later call
    -- re-leads. Fills only when empty: a synchronous fetch error is folded to a 'Left'
    -- by 'try' rather than re-raised, so the guard runs only for an exception 'try'
    -- did not record — and the empty check keeps it correct even so.
    orphan :: CacheKey -> TMVar (Either SomeException CacheEntry) -> SomeException -> IO ()
    orphan key marker err =
        atomically $ do
            unfilled <- isEmptyTMVar marker
            when unfilled (putTMVar marker (Left err))
            deregister key

    deregister :: CacheKey -> STM ()
    deregister key = do
        inFlight <- readTVar (mcInFlight cache)
        writeTVar (mcInFlight cache) (Map.delete key inFlight)

{- | Insert a freshly fetched entry, enforcing the size bound. Expired entries are
purged first (the cheap reclaim); if the cache is still at capacity, surplus
entries are evicted before the insert so the bound holds. The new entry is always
admitted.
-}
insertBounded :: MetadataCache -> CacheKey -> CacheEntry -> IO ()
insertBounded cache key entry = do
    Cache.purgeExpired (mcStore cache)
    n <- Cache.size (mcStore cache)
    when (n >= mcMaxEntries cache) (evictSurplus cache)
    Cache.insert (mcStore cache) key entry

-- Evict entries until there is room for one more, keeping the cache at or below
-- its bound. Eviction order among live entries is unspecified (the store is a
-- hash map): this is a flood safety valve, not a precision LRU.
evictSurplus :: MetadataCache -> IO ()
evictSurplus cache = do
    ks <- Cache.keys (mcStore cache)
    let surplus = length ks - (mcMaxEntries cache - 1)
    when (surplus > 0) (mapM_ (Cache.delete (mcStore cache)) (take surplus ks))

{- | Look up a package's cached entry for one 'Source' without fetching on a miss —
the cache's read-only view, for inspection and tests. A 'Nothing' is a miss or an
expired entry; this never triggers a fetch and never collapses (use 'resolveMetadata'
for the serve path).
-}
cachedMetadata :: MetadataCache -> Source -> PackageName -> IO (Maybe CacheEntry)
cachedMetadata cache source name = Cache.lookup (mcStore cache) (cacheKey source name)

-- | The number of entries currently held (including any not-yet-purged expired).
cacheSize :: MetadataCache -> IO Int
cacheSize cache = Cache.size (mcStore cache)

-- ── internals ────────────────────────────────────────────────────────────────

-- The outcome of the one atomic resolve decision: a fresh hit, follow an in-flight
-- fetch, or lead a new one.
data Decision
    = Hit CacheEntry
    | Follow (TMVar (Either SomeException CacheEntry))
    | Lead (TMVar (Either SomeException CacheEntry))

-- Convert a 'NominalDiffTime' (seconds) to the @cache@ library's monotonic
-- 'TimeSpec' (whole seconds + nanoseconds), clamping a negative TTL to zero.
toTimeSpec :: NominalDiffTime -> TimeSpec
toTimeSpec ttl =
    let nanos = max 0 (round (realToFrac ttl * 1e9 :: Double)) :: Integer
        billion = 1000000000
     in TimeSpec
            (fromInteger (nanos `div` billion))
            (fromInteger (nanos `mod` billion))
