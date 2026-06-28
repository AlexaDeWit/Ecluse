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
('Ecluse.Core.Rules.Types.AllowIfOlderThan') and the separately-synced advisory
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

== Two coherent stores: the full packument and one version

This handle owns __two__ stores of the same shape (the TTL + size-bound + single-flight
machinery, 'SingleFlight', is shared between them):

  * the __full-packument__ store ('resolveMetadata' \/ 'cachedMetadata'), keyed by
    @(source, package)@, holding the 'CacheEntry' described above; and

  * a __single-version__ store ('resolveVersion' \/ 'cachedVersion'), keyed by
    @(source, package, version)__, holding just one version's
    'Ecluse.Core.Package.PackageDetails' (or its determined absence, a cached
    'Nothing') — the cold tarball gate's selectively-parsed result.

They are __isolated on writes__: a single-version resolution caches under its own key and
__never writes back__ to the full-packument store, so a cold tarball gate cannot
materialise a whole packument into the shared full cache (the residency the single-version
path exists to avoid). The serve path's single-version read consults the warm
full-packument store __read-only__ first (a packument @GET@ followed by its tarball gate
still collapses to one upstream call), and only falls back to leading its own
selective fetch into the version store when the full entry is cold — so the version store
holds entries for versions whose packument was never fetched in full, sized to the same
short TTL and bound. The single-version store is not yet separately metered (the
@ecluse.metadata_cache.*@ instruments stay about the full-packument store); that is a
noted follow-up.
-}
module Ecluse.Core.Server.Cache (
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

    -- * Single-version resolution
    resolveVersion,
    resolveVersionWith,
    cachedVersion,
) where

import Data.Aeson (Value)
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Map.Strict qualified as Map
import Data.Text.Short qualified as TS
import Data.Time (NominalDiffTime)
import System.Clock (Clock (Monotonic), TimeSpec (TimeSpec), getTime)
import UnliftIO.Exception (mask, throwIO)

import Ecluse.Core.InFlight (guardInFlight)
import Ecluse.Core.Package (
    PackageDetails,
    PackageInfo,
    PackageName,
    pkgCanonical,
    pkgEcosystem,
    pkgNamespace,
    renderScope,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort, mpCacheEntries, mpCacheRequest)
import Ecluse.Core.Version (Version, renderVersion)

-- ── configuration ────────────────────────────────────────────────────────────

{- | The metadata cache's tunables, sourced from configuration: how long a parsed
packument stays fresh, and how many distinct @(source, package)@ entries the cache
holds before it evicts.
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

{- The @(source, package)@ identity rendered to a stable 'Text': the source's base URL
joined with the package's identity (not its display form). The shared prefix of both
cache keys — the full-packument key is exactly this, the single-version key appends the
version — so the two stores partition on the same source\/package identity. -}
keyText :: Source -> PackageName -> Text
keyText (Source source) name =
    source
        <> "\x1f"
        <> show (pkgEcosystem name)
        <> "\x1f"
        <> maybe "" renderScope (pkgNamespace name)
        <> "\x1f"
        <> TS.toText (pkgCanonical name)

{- | Project a 'Source' and a 'PackageName' to their full-packument cache key (the
source's base URL joined with the package's identity, not its display form).
-}
cacheKey :: Source -> PackageName -> CacheKey
cacheKey source name = CacheKey (keyText source name)

{- | The key a single-version entry is cached under: the @(source, package)@ identity
'cacheKey' uses, with the rendered 'Version' appended — so distinct versions of one
package hold distinct entries, and the version store partitions on the same source as the
full store.
-}
newtype VersionKey = VersionKey Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable)

versionKey :: Source -> PackageName -> Version -> VersionKey
versionKey source name version = VersionKey (keyText source name <> "\x1f" <> renderVersion version)

{- | One TTL- and STM-backed store with the size bound and the in-flight map that gives
single-flight — the shape both the full-packument and single-version caches take, factored
so the resolution machinery ('resolveSingleFlight') is written once over either.
-}
data SingleFlight k v = SingleFlight
    { sfStore :: Cache k v
    -- ^ The TTL- and STM-backed store (the @cache@ library).
    , sfMaxEntries :: Int
    -- ^ The entry-count bound enforced on insert.
    , sfInFlight :: TVar (Map k (TMVar (Either SomeException v)))
    {- ^ Entries currently being fetched, so concurrent misses coalesce onto one
    fetch rather than each launching their own.
    -}
    }

-- Build a 'SingleFlight' store from the cache configuration. The TTL is converted to the
-- @cache@ library's monotonic 'TimeSpec'; the in-flight map starts empty.
newSingleFlight :: CacheConfig -> IO (SingleFlight k v)
newSingleFlight cfg = do
    store <- Cache.newCache (Just (toTimeSpec (cacheTtl cfg)))
    inFlight <- newTVarIO Map.empty
    pure
        SingleFlight
            { sfStore = store
            , sfMaxEntries = max 1 (cacheMaxEntries cfg)
            , sfInFlight = inFlight
            }

{- | The metadata-cache handle: the two single-flight stores (the full-packument cache and
the single-version cache). Opaque — built with 'newMetadataCache' and reached only through
the accessors. Lives in the composition root (one per process), so every request shares the
same caches and their connection-collapsing.
-}
data MetadataCache = MetadataCache
    { mcFull :: SingleFlight CacheKey CacheEntry
    -- ^ The full-packument store, keyed by @(source, package)@.
    , mcVersion :: SingleFlight VersionKey (Maybe PackageDetails)
    {- ^ The single-version store, keyed by @(source, package, version)@, holding one
    version's 'PackageDetails' (or its determined absence) — written only by the
    single-version path, never the full path.
    -}
    }

{- | Build a metadata cache from its configuration: the full-packument store and the
single-version store, each over the same TTL and size bound.
-}
newMetadataCache :: CacheConfig -> IO MetadataCache
newMetadataCache cfg = MetadataCache <$> newSingleFlight cfg <*> newSingleFlight cfg

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
between claiming the slot and completing: the claim commits under a 'mask' and the
leader's run is handed straight to 'Ecluse.Core.InFlight.guardInFlight', which frees the
slot on every exit and, on any exception before the marker is filled, hands that error
to every waiting follower rather than leaving them parked forever. This closes the
single-flight orphan window (without it, a cancelled leader would wedge that
@(source, package)@ key until restart). A follower's own wait on the marker stays
interruptible.

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
resolveMetadata :: MetricsPort -> MetadataCache -> Source -> PackageName -> IO CacheEntry -> IO CacheEntry
resolveMetadata = resolveMetadataWith (pure ())

{- | As 'resolveMetadata', but with a hook run on the leading thread at the
single-flight claim → fetch-runner handoff: the window between the STM transaction
committing the in-flight claim and the leader's exception guard taking ownership of
the marker. It exists only so a test can deterministically park a leader in that
window and cancel it there, exercising the orphan-window guarantee; production always
passes @pure ()@ via 'resolveMetadata'.
-}
resolveMetadataWith :: IO () -> MetricsPort -> MetadataCache -> Source -> PackageName -> IO CacheEntry -> IO CacheEntry
resolveMetadataWith afterClaim metrics cache source name =
    resolveSingleFlight
        afterClaim
        (mpCacheRequest metrics)
        (mpCacheEntries metrics =<< Cache.size (sfStore (mcFull cache)))
        (mcFull cache)
        (cacheKey source name)

{- | Resolve __one version's__ 'PackageDetails' (or its determined absence) from the
single-version cache, leading a selective fetch on a miss and collapsing concurrent misses
exactly as 'resolveMetadata' does for the full packument. The cached value is the
@'Maybe' 'PackageDetails'@ the fetch yields, so a version determined __absent__ over sound
metadata is cached as 'Nothing' (a negative entry) and re-served without a re-fetch within
the TTL.

This writes to the single-version store only — never the full-packument store — so a cold
tarball gate's selective parse cannot materialise a whole packument into the shared full
cache. Unlike 'resolveMetadata', the single-version store is not separately metered (a
noted follow-up), so this records no cache counter.
-}
resolveVersion :: MetadataCache -> Source -> PackageName -> Version -> IO (Maybe PackageDetails) -> IO (Maybe PackageDetails)
resolveVersion = resolveVersionWith (pure ())

{- | As 'resolveVersion', with the single-flight claim → fetch-runner handoff hook
'resolveMetadataWith' exposes, for the same orphan-window test (production passes @pure ()@
via 'resolveVersion').
-}
resolveVersionWith :: IO () -> MetadataCache -> Source -> PackageName -> Version -> IO (Maybe PackageDetails) -> IO (Maybe PackageDetails)
resolveVersionWith afterClaim cache source name version =
    resolveSingleFlight afterClaim (const pass) pass (mcVersion cache) (versionKey source name version)

{- The single-flight resolution shared by the full-packument and single-version caches: a
fresh hit short-circuits; otherwise the caller leads one fetch (installing an in-flight
marker) or follows an in-flight one. @recordRequest@ records the hit\/miss counter (or
ignores it) and @recordInsert@ refreshes the occupancy gauge after a leader insert (or
ignores it), so each store wires its own telemetry without the resolution logic knowing
which it serves.

On a miss the fetch runs exactly once even under concurrent callers; a successful fetch is
cached (subject to the TTL and size bound), a failed fetch caches __nothing__ and is
re-raised to every waiter. A claimed slot is __always eventually filled and de-registered__
even under an async exception in the claim → runner window: the claim commits under a
'mask' and the run is handed straight to 'Ecluse.Core.InFlight.guardInFlight', which frees
the slot on every exit and hands the orphaning error to any waiting follower (closing the
single-flight orphan window). A follower's own wait stays interruptible. The result is
inserted __before__ the slot is de-registered, so a caller arriving the instant the fetch
returns becomes a follower rather than re-leading a redundant fetch. -}
resolveSingleFlight ::
    (Hashable k, Ord k) =>
    IO () ->
    (Metric.CacheResult -> IO ()) ->
    IO () ->
    SingleFlight k v ->
    k ->
    IO v ->
    IO v
resolveSingleFlight afterClaim recordRequest recordInsert sf key fetch = mask $ \restore -> do
    nowT <- getTime Monotonic
    -- One atomic decision point under the enclosing 'mask': a 'Hit' or 'Follow' claims
    -- nothing (its wait runs under @restore@, interruptible); a 'Lead' installs the marker
    -- and hands the run to 'guardInFlight' with no interruptible point between.
    decision <- atomically (decide nowT)
    case decision of
        Hit entry -> do
            recordRequest Metric.Hit
            pure entry
        Follow marker -> do
            -- A follower coalesced onto an in-flight fetch is a miss for this caller
            -- (no fresh entry was present), exactly as the leader's miss is.
            recordRequest Metric.Miss
            restore (either throwIO pure =<< atomically (readTMVar marker))
        Lead marker -> do
            recordRequest Metric.Miss
            -- Only the fetch runs under @restore@ (cancellable); the publish + insert run
            -- under the enclosing 'mask' so a cancel after the fetch returns still delivers
            -- and inserts. 'guardInFlight' is passed 'id'; it frees the slot on every exit
            -- and, on a failure before the marker is filled, hands the error to followers
            -- via 'orphan'. The insert precedes de-registration, so "collapse to one fetch"
            -- holds even for a caller arriving the instant the fetch returns.
            entry <- guardInFlight id (orphan marker) (atomically deregister) $ do
                fetched <- restore (afterClaim >> fetch)
                atomically (putTMVar marker (Right fetched))
                insertBounded sf key fetched
                pure fetched
            -- The leader inserted, so refresh the occupancy gauge (a follower never does).
            recordInsert
            pure entry
  where
    decide nowT = do
        hit <- Cache.lookupSTM False key (sfStore sf) nowT
        case hit of
            Just entry -> pure (Hit entry)
            Nothing -> do
                inFlight <- readTVar (sfInFlight sf)
                case Map.lookup key inFlight of
                    Just marker -> pure (Follow marker)
                    Nothing -> do
                        marker <- newEmptyTMVar
                        writeTVar (sfInFlight sf) (Map.insert key marker inFlight)
                        pure (Lead marker)

    -- The orphan hand-off: a failure before the marker was filled. Fill it with the error
    -- so blocked followers unblock rather than parking forever; 'guardInFlight' frees the
    -- slot separately. Fills only when empty, so a failure after a successful publish never
    -- clobbers the result.
    orphan marker err =
        atomically $ do
            unfilled <- isEmptyTMVar marker
            when unfilled (putTMVar marker (Left err))

    deregister :: STM ()
    deregister = do
        inFlight <- readTVar (sfInFlight sf)
        writeTVar (sfInFlight sf) (Map.delete key inFlight)

{- | Insert a freshly fetched entry into a store, enforcing the size bound. Expired entries
are purged first (the cheap reclaim); if the store is still at capacity, surplus entries are
evicted before the insert so the bound holds. The new entry is always admitted.
-}
insertBounded :: (Hashable k) => SingleFlight k v -> k -> v -> IO ()
insertBounded sf key entry = do
    Cache.purgeExpired (sfStore sf)
    n <- Cache.size (sfStore sf)
    when (n >= sfMaxEntries sf) (evictSurplus sf)
    Cache.insert (sfStore sf) key entry

-- Evict entries until there is room for one more, keeping the store at or below its bound.
-- Eviction order among live entries is unspecified (the store is a hash map): this is a
-- flood safety valve, not a precision LRU.
evictSurplus :: (Hashable k) => SingleFlight k v -> IO ()
evictSurplus sf = do
    ks <- Cache.keys (sfStore sf)
    let surplus = length ks - (sfMaxEntries sf - 1)
    when (surplus > 0) (mapM_ (Cache.delete (sfStore sf)) (take surplus ks))

{- | Look up a package's cached full-packument entry for one 'Source' without fetching on a
miss — the cache's read-only view, for inspection and tests. A 'Nothing' is a miss or an
expired entry; this never triggers a fetch and never collapses (use 'resolveMetadata' for
the serve path).
-}
cachedMetadata :: MetadataCache -> Source -> PackageName -> IO (Maybe CacheEntry)
cachedMetadata cache source name = Cache.lookup (sfStore (mcFull cache)) (cacheKey source name)

{- | Look up a single-version cached entry for one @(source, package, version)@ without
fetching on a miss — the version store's read-only view (the hybrid serve path's negative\/
positive lookup before it leads a selective fetch). The outer 'Maybe' is the cache hit\/miss
(an expired or absent entry is 'Nothing'); the inner @'Maybe' 'PackageDetails'@ is the
cached result (a version determined absent is a cached @'Just' 'Nothing'@).
-}
cachedVersion :: MetadataCache -> Source -> PackageName -> Version -> IO (Maybe (Maybe PackageDetails))
cachedVersion cache source name version = Cache.lookup (sfStore (mcVersion cache)) (versionKey source name version)

-- | The number of full-packument entries currently held (including any not-yet-purged expired).
cacheSize :: MetadataCache -> IO Int
cacheSize cache = Cache.size (sfStore (mcFull cache))

-- ── internals ────────────────────────────────────────────────────────────────

-- The outcome of the one atomic resolve decision: a fresh hit, follow an in-flight
-- fetch, or lead a new one.
data Decision v
    = Hit v
    | Follow (TMVar (Either SomeException v))
    | Lead (TMVar (Either SomeException v))

-- Convert a 'NominalDiffTime' (seconds) to the @cache@ library's monotonic
-- 'TimeSpec' (whole seconds + nanoseconds), clamping a negative TTL to zero.
toTimeSpec :: NominalDiffTime -> TimeSpec
toTimeSpec ttl =
    let nanos = max 0 (round (realToFrac ttl * 1e9 :: Double)) :: Integer
        billion = 1000000000
     in TimeSpec
            (fromInteger (nanos `div` billion))
            (fromInteger (nanos `mod` billion))
