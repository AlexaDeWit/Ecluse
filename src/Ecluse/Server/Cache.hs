{- | The short-TTL, size-bounded metadata cache shared by the serve paths.

Resolving a package re-fetches its upstream packument, parses it, and evaluates
the rules. To avoid repeating the fetch+parse, the parsed __packument metadata__
('PackageInfo') is held here in a short-TTL, size-bounded, STM-backed cache keyed
by package identity (the @cache@ library backs the TTL store). Both serve paths
share it: a packument request and the tarball-gating fetch that follows reuse one
fetch+parse, and concurrent resolutions of a popular package __collapse to one
upstream call__ (single-flight).

What is cached is the __metadata, not the verdict__. The rules are re-evaluated on
the cached metadata each request, so time-sensitive rules
('Ecluse.Rules.Types.AllowIfPublishedBefore') and the separately-synced advisory
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

    -- * Resolution
    resolveMetadata,
    primeMetadata,
    cachedMetadata,
    cacheSize,
) where

import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)
import System.Clock (Clock (Monotonic), TimeSpec (TimeSpec), getTime)
import UnliftIO.Exception (mask, throwIO, try)

import Ecluse.Package (
    PackageInfo,
    PackageName,
    pkgCanonical,
    pkgEcosystem,
    pkgNamespace,
    renderScope,
 )

-- ── configuration ────────────────────────────────────────────────────────────

{- | The metadata cache's tunables, sourced from configuration (see
"Ecluse.Config"): how long a parsed packument stays fresh, and how many distinct
packages the cache holds before it evicts.
-}
data CacheConfig = CacheConfig
    { cacheTtl :: NominalDiffTime
    {- ^ How long a cached 'PackageInfo' is served before it is re-fetched. Short
    by design — brief staleness is benign, and conditional-GET revalidates.
    -}
    , cacheMaxEntries :: Int
    -- ^ The maximum number of distinct packages held; an insert past this evicts.
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

-- ── the cache handle ─────────────────────────────────────────────────────────

{- | The key a 'PackageInfo' is cached under: the package's identity, rendered to
a stable 'Text'. Distinct from a display name so two encodings of the same scoped
package share one entry — equality and ordering match 'PackageName' identity (the
@cache@ library needs a 'Hashable' key, which the opaque 'PackageName' does not
expose, so the identity is projected to this key here rather than via an orphan
instance).
-}
newtype CacheKey = CacheKey Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable)

-- | Project a 'PackageName' to its cache key (its identity, not its display form).
cacheKey :: PackageName -> CacheKey
cacheKey name =
    CacheKey
        ( show (pkgEcosystem name)
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
    { mcStore :: Cache CacheKey PackageInfo
    -- ^ The TTL- and STM-backed store (the @cache@ library).
    , mcMaxEntries :: Int
    -- ^ The entry-count bound enforced on insert.
    , mcInFlight :: TVar (Map CacheKey (TMVar (Either SomeException PackageInfo)))
    {- ^ Packages currently being fetched, so concurrent misses coalesce onto one
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

{- | Resolve a package's metadata, reusing the cache and collapsing concurrent
misses.

On a fresh, unexpired hit the cached 'PackageInfo' is returned and the fetch
action is never run. On a miss the action runs exactly once even under concurrent
callers: the first installs an in-flight marker and fetches, the others wait on
its result. A successful fetch is cached (subject to the TTL and size bound);
a failed fetch caches __nothing__ (so a transient upstream error does not poison
the cache) and is re-raised to every waiter.

The result is always re-decided by the caller's rules on each request — only the
fetch+parse is memoised, never the verdict.
-}
resolveMetadata :: MetadataCache -> PackageName -> IO PackageInfo -> IO PackageInfo
resolveMetadata cache name fetch = do
    let key = cacheKey name
    nowT <- getTime Monotonic
    -- One atomic decision point: a fresh hit short-circuits; otherwise become the
    -- leader (install an empty marker) or a follower (take the existing one).
    decision <- atomically (decide key nowT)
    case decision of
        Hit info -> pure info
        Follow marker -> either throwIO pure =<< atomically (readTMVar marker)
        Lead marker -> runLeader key marker
  where
    decide :: CacheKey -> TimeSpec -> STM Decision
    decide key nowT = do
        hit <- Cache.lookupSTM False key (mcStore cache) nowT
        case hit of
            Just info -> pure (Hit info)
            Nothing -> do
                inFlight <- readTVar (mcInFlight cache)
                case Map.lookup key inFlight of
                    Just marker -> pure (Follow marker)
                    Nothing -> do
                        marker <- newEmptyTMVar
                        writeTVar (mcInFlight cache) (Map.insert key marker inFlight)
                        pure (Lead marker)

    -- The leader fetches once, fills the marker, and de-registers itself — even on
    -- exception, so a failed fetch unblocks waiters with the error and leaves the
    -- in-flight slot clean for a later retry. 'mask' keeps the marker fill, the
    -- store insert, and the de-register from being interrupted between each other.
    --
    -- On success the result is __inserted into the store before the in-flight slot
    -- is de-registered__: until 'insertBounded' completes the slot still exists, so
    -- a late caller in 'decide' becomes a follower on the marker rather than finding
    -- neither store hit nor in-flight slot and re-leading a redundant fetch. Insert
    -- then deregister thus makes "collapse to one upstream call" hold even for a
    -- caller arriving in the instant after the fetch returns. On failure nothing is
    -- cached and the slot is freed so a later retry re-fetches.
    runLeader :: CacheKey -> TMVar (Either SomeException PackageInfo) -> IO PackageInfo
    runLeader key marker = mask $ \restore -> do
        result <- try (restore fetch)
        atomically (putTMVar marker result)
        case result of
            Right info -> do
                insertBounded cache key info
                atomically (deregister key)
                pure info
            Left err -> do
                atomically (deregister key)
                throwIO err

    deregister :: CacheKey -> STM ()
    deregister key = do
        inFlight <- readTVar (mcInFlight cache)
        writeTVar (mcInFlight cache) (Map.delete key inFlight)

{- | Write a freshly fetched-and-parsed packument through to the cache (a
__write-through__), enforcing the size bound. Unlike 'resolveMetadata' — which on a
hit returns the /cached/ parse and discards the caller's — this always stores the
parse the caller hands it. Use it when the caller has already fetched the bytes and
must keep using /that/ parse, so its typed view and the raw bytes it was decoded from
stay coherent; the read-through 'resolveMetadata' is for callers that only need the
typed view and want concurrent misses collapsed. What a packument serve primes here
is what the tarball-gating 'resolveMetadata' then reuses.
-}
primeMetadata :: MetadataCache -> PackageName -> PackageInfo -> IO ()
primeMetadata cache name = insertBounded cache (cacheKey name)

{- | Insert a freshly fetched entry, enforcing the size bound. Expired entries are
purged first (the cheap reclaim); if the cache is still at capacity, surplus
entries are evicted before the insert so the bound holds. The new entry is always
admitted.
-}
insertBounded :: MetadataCache -> CacheKey -> PackageInfo -> IO ()
insertBounded cache key info = do
    Cache.purgeExpired (mcStore cache)
    n <- Cache.size (mcStore cache)
    when (n >= mcMaxEntries cache) (evictSurplus cache)
    Cache.insert (mcStore cache) key info

-- Evict entries until there is room for one more, keeping the cache at or below
-- its bound. Eviction order among live entries is unspecified (the store is a
-- hash map): this is a flood safety valve, not a precision LRU.
evictSurplus :: MetadataCache -> IO ()
evictSurplus cache = do
    ks <- Cache.keys (mcStore cache)
    let surplus = length ks - (mcMaxEntries cache - 1)
    when (surplus > 0) (mapM_ (Cache.delete (mcStore cache)) (take surplus ks))

{- | Look up a package's cached metadata without fetching on a miss — the cache's
read-only view, for inspection and tests. A 'Nothing' is a miss or an expired
entry; this never triggers a fetch and never collapses (use 'resolveMetadata' for
the serve path).
-}
cachedMetadata :: MetadataCache -> PackageName -> IO (Maybe PackageInfo)
cachedMetadata cache name = Cache.lookup (mcStore cache) (cacheKey name)

-- | The number of entries currently held (including any not-yet-purged expired).
cacheSize :: MetadataCache -> IO Int
cacheSize cache = Cache.size (mcStore cache)

-- ── internals ────────────────────────────────────────────────────────────────

-- The outcome of the one atomic resolve decision: a fresh hit, follow an in-flight
-- fetch, or lead a new one.
data Decision
    = Hit PackageInfo
    | Follow (TMVar (Either SomeException PackageInfo))
    | Lead (TMVar (Either SomeException PackageInfo))

-- Convert a 'NominalDiffTime' (seconds) to the @cache@ library's monotonic
-- 'TimeSpec' (whole seconds + nanoseconds), clamping a negative TTL to zero.
toTimeSpec :: NominalDiffTime -> TimeSpec
toTimeSpec ttl =
    let nanos = max 0 (round (realToFrac ttl * 1e9 :: Double)) :: Integer
        billion = 1000000000
     in TimeSpec
            (fromInteger (nanos `div` billion))
            (fromInteger (nanos `mod` billion))
