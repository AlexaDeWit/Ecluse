-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The short-TTL, size-bounded metadata cache shared by the serve paths.

Resolving a package re-fetches its upstream packument, parses it, and evaluates the
rules. This cache holds the result, so the fetch and the parse are not repeated. A
'CacheEntry' pairs the parsed __packument metadata__ ('PackageInfo') with the __raw
document__ it was decoded from. The store is short-TTL, size-bounded, and STM-backed
(the @cache@ library backs the TTL store). Both serve paths share it. A packument
request and the tarball-gating fetch that follows reuse one fetch and parse.
Concurrent resolutions of a popular package __collapse to one upstream call__
(single-flight).

== Per-source key

The proxy fetches a packument from two distinct upstreams, a private origin and a
public origin. Their documents differ for the same package, so one entry cannot
represent both. The key is @(source, package)@, where the source is the upstream's base
URL. A base URL distinguishes any cached origin without naming a credential, so distinct
upstreams never cross-contaminate and the key never blurs the trust split.

== Credential-free: sharing is the caller's policy

The key carries __no credential dimension__ and the value is a canonical document,
so the cache stores nothing derived from a caller's credential. Whether a given
origin is handed to it, and so shared across clients, is the serve path's decision.

Only the anonymous public origin is cached. The trusted private upstream is the per-client
authority: it re-authorises each request with that client's own forwarded credential. The
serve path therefore fetches it per request and never hands it here. Were a private entry
cached under @passthrough@, the credential-free key would let one client's entry serve another
client's private document within the TTL. That bypasses the upstream's authorisation.
The public origin is anonymous, so one shared entry serves every client without
crossing a trust boundary. The private origin is never cached (see
@docs\/architecture\/web-layer.md@, "Metadata cache").

== Coherent pair

An entry holds the parsed 'PackageInfo' __and__ the raw document ('CachedDoc') it was
decoded from. A hit therefore returns a typed view and the exact bytes that produced it.
The packument serve path needs both. It decides over the typed view but rebuilds the
served body from the raw document, and the two must describe the same fetch. The store
holds the raw document opaquely. It never reads the document, only hands it back to the
injected adapter capabilities that assemble and serialise the served body.

The cache holds the __metadata, not the verdict__. The rules are re-evaluated on the
cached metadata each request, so time-sensitive rules
('Ecluse.Core.Rules.Types.AllowIfOlderThan') and the separately-synced advisory tier
stay correct. Only each upstream's fetch and parse is memoised. The TTL is short, and
brief staleness is benign: a brand-new publish need not appear instantly (see
@docs\/architecture\/web-layer.md@, "Metadata cache").

The shared machinery ("Ecluse.Core.Server.Cache.Store") layers two properties onto every
store that the @cache@ library does not provide on its own:

* __Resident-byte budget with recency-aware eviction.__ @cache@ expires by TTL but
  bounds neither entry count nor memory. Each entry is wrapped with an estimate of its
  resident footprint and a last-access stamp bumped on every hit. A heavy packument,
  parsed plus raw, costs many times its wire size. An insert first purges expired
  entries. It then evicts the __least-recently-used__ entries until the incoming entry
  fits within its store's resident-byte budget and entry count ('StoreBudget'). Recency
  keeps a re-accessed hot head resident under pressure while shedding the one-shot tail.
  The byte budget bounds memory more faithfully than a count alone. An entry whose
  estimated footprint alone exceeds its store's byte budget is __served without being
  retained__. Nothing resident is evicted to make impossible room, so one pathological
  document can never flush a store.

* __Single-flight.__ @cache@'s own @fetchWithCache@ is lookup-then-fetch in plain
  'IO', so two concurrent misses would both fetch. 'resolveMetadata' instead
  installs an in-flight marker atomically, so the first miss fetches while concurrent
  misses wait on its result. The leader inserts the result into the store __before__
  removing its in-flight marker. A caller arriving the instant the fetch returns
  therefore finds either the store entry or the marker, never a gap. It never re-leads
  a redundant fetch.

== Two coherent stores: the full packument and one version

This handle owns __two__ stores of the same shape (the TTL + size-bound + single-flight
machinery, 'SingleFlight', is shared between them):

  * the __full-packument__ store ('resolveMetadata' \/ 'cachedMetadata'), keyed by
    @(source, package)@, holding the 'CacheEntry' described above.

  * a __single-version__ store ('resolveVersion' \/ 'cachedVersion'), keyed by
    @(source, package, version)@, holding just one version's
    'Ecluse.Core.Package.PackageDetails' (or its determined absence, a cached
    'Nothing'). This is the cold tarball gate's selectively-parsed result.

They are __isolated on writes__. A single-version resolution caches under its own key
and __never writes back__ to the full-packument store. A cold tarball gate therefore
cannot materialise a whole packument into the shared full cache. The serve path's
single-version read consults the warm full-packument store __read-only__ first. A
packument @GET@ followed by its tarball gate therefore still collapses to one upstream
call. Only when the full entry is cold does it lead its own selective fetch into the
version store.

Each store enforces its __own named sub-budget__ ('StoreBudget'). The three sub-budgets
are carved from one cache aggregate at the composition root and sum to it. The aggregate
therefore holds by arithmetic, while each class's eviction stays isolated: a
version-store flood can never evict the full store's hot head. Each store reports its
own residency gauge, the full-packument store under
@ecluse.metadata_cache.resident_bytes@ and the single-version store under
@ecluse.metadata_cache.version.resident_bytes@. The hit\/miss counter and the
entry-count occupancy gauge stay about the full-packument store.

A third store memoises the __assembled representation__ ('resolveAssembled'): the
encoded merged document, keyed by its derived validator
('Ecluse.Core.Server.Pipeline.Packument.packumentETag'). The key is a fingerprint of
every input the document is a function of. Those inputs are the origin bodies (the
private one by content digest), the survivor sets, and the mount base. That makes the
store __content-addressed__. An entry can never be served stale, because changed
inputs produce a different key and simply miss. The resident-byte budget is the real
bound here, not the TTL, which only trims dead entries early.

Cross-client safety follows from the same property. A lookup key includes the digest of
the private document __this request's own authorised fetch returned__. A client can
therefore only hit an entry whose bytes its own inputs would deterministically
re-produce. The store shares the transform, never the authorisation and never another
client's view. The private-origin caching prohibition is about credential-blind keying,
which a content key is not. Residency gauge:
@ecluse.metadata_cache.assembled.resident_bytes@.
-}
module Ecluse.Core.Server.Cache (
    -- * Configuration
    CacheConfig (..),
    StoreBudget (..),

    -- * The cache handle
    MetadataCache,
    newMetadataCache,

    -- * Cache entries
    Source (..),
    CacheEntry (..),
    weighCacheEntry,

    -- * Resolution
    resolveMetadata,
    cachedMetadata,

    -- * Single-version resolution
    resolveVersion,
    cachedVersion,

    -- * Assembled-representation resolution
    resolveAssembled,
) where

import Data.ByteString qualified as BS
import Data.Text.Short qualified as TS
import Data.Time (NominalDiffTime)

import Ecluse.Core.Package (
    PackageDetails,
    PackageInfo,
    PackageName,
    pkgCanonical,
    pkgEcosystem,
    pkgNamespace,
    renderScope,
 )
import Ecluse.Core.Registry.CachedDocument (CachedDoc, weighCachedDoc)
import Ecluse.Core.Registry.Metadata (ContentDigest, MetadataError)
import Ecluse.Core.Server.Cache.Store (
    CacheOccupancy (..),
    SingleFlight,
    lookupStore,
    lookupStoreTouching,
    newSingleFlight,
    resolveSingleFlight,
 )
import Ecluse.Core.Server.MemoryModel (expandWireBytes)
import Ecluse.Core.Telemetry.Record (
    MetricsPort,
    mpAssembledCacheResidentBytes,
    mpCacheEntries,
    mpCacheRequest,
    mpCacheResidentBytes,
    mpVersionCacheResidentBytes,
 )
import Ecluse.Core.Version (Version, renderVersion)

{- | One store's bounds: a maximum entry count and a resident-byte budget.
An insert past the byte budget evicts the least-recently-used entries until the budget holds.
-}
data StoreBudget = StoreBudget
    { sbMaxEntries :: Int
    -- ^ The maximum number of distinct entries held. An insert past this evicts.
    , sbMaxBytes :: Int
    -- ^ The resident-byte budget the held entries are kept under.
    }
    deriving stock (Eq, Show)

{- | The metadata cache's tunables, sourced from configuration. The three sub-budgets are carved
from one aggregate at @Ecluse.Composition.MemoryBudget.budgetCacheConfig@ and __sum to it__, so
that aggregate bounds the cache's total resident bytes.
-}
data CacheConfig = CacheConfig
    { cacheTtl :: NominalDiffTime
    {- ^ How long a cached 'CacheEntry' is served before it is re-fetched. Short
    by design: brief staleness is benign, and conditional-GET revalidates.
    -}
    , cacheFullBudget :: StoreBudget
    -- ^ The full-packument store's bounds, keyed by @(source, package)@.
    , cacheVersionBudget :: StoreBudget
    -- ^ The single-version store's bounds (small, flat-weighted entries).
    , cacheAssembledBudget :: StoreBudget
    -- ^ The assembled-representation store's bounds (exact strict-bytes weights).
    }
    deriving stock (Eq, Show)

{- | Which upstream a cached packument was fetched from, the dimension that partitions the cache by
source so distinct upstreams never share an entry.

The discriminator is the upstream's __base URL__. That URL names a location, never a credential, so
keying on it keeps the trust split intact.
-}
newtype Source = Source Text
    deriving stock (Eq, Ord, Show)

{- | The parsed 'PackageInfo' paired with the raw document ('CachedDoc') it was decoded from, so a
caller's typed decision stays coherent with the bytes served from it. The store holds the raw
document opaquely: it never reads it, only weighs it ('weighCachedDoc') for the budget.
-}
data CacheEntry = CacheEntry
    { entryInfo :: PackageInfo
    -- ^ The typed packument view the rules and merge reason over.
    , entryRaw :: CachedDoc
    -- ^ The raw upstream document the served body is built from.
    , entryDigest :: ContentDigest
    {- ^ Digest of the wire bytes both views were decoded from, computed once at the
    leader's fetch. It is the public origin's contribution to the serve path's derived
    ETag, amortised across every hit on this entry.
    -}
    }
    deriving stock (Eq, Show)

{- | Estimate a 'CacheEntry'\'s resident footprint as a fixed multiple of its raw document's
compact-encoded byte length ('weighCachedDoc'). The multiplier is the high end of the observed
resident-to-encoded ratio, because a memory budget must not under-count. The @O(document)@ encode
runs on a leader's insert, never on a hit.
-}
weighCacheEntry :: CacheEntry -> Int
weighCacheEntry e = weighEncodedBytes (weighCachedDoc (entryRaw e))

{- | Estimate a single-version entry's resident footprint. The store holds no raw document to
measure, so a present bounded manifest and a cached absence each weigh a flat figure.
-}
weighVersion :: Maybe PackageDetails -> Int
weighVersion = \case
    Just _ -> versionEntryBytes
    Nothing -> negativeEntryBytes

-- Scale through the one shared wire-to-resident model ("Ecluse.Core.Server.MemoryModel"), so this
-- weigher and the composition root's memory plan never drift on the expansion factor.
weighEncodedBytes :: Int64 -> Int
weighEncodedBytes = expandWireBytes . fromIntegral

-- The flat resident estimate for a present single-version entry (one bounded manifest) and
-- for a cached determined absence (a small negative entry).
versionEntryBytes :: Int
versionEntryBytes = 16 * 1024

negativeEntryBytes :: Int
negativeEntryBytes = 1024

{- | An assembled entry's footprint __is__ its strict bytes plus a small constant, so the budget
counts what is genuinely held rather than an estimate.
-}
weighAssembled :: ByteString -> Int
weighAssembled bytes = BS.length bytes + assembledEntryOverheadBytes

assembledEntryOverheadBytes :: Int
assembledEntryOverheadBytes = 256

{- | The key a 'CacheEntry' is cached under: the 'Source' paired with the package's identity, not
its display name, so two encodings of one scoped package share an entry. The @cache@ library needs
a 'Hashable' key that the opaque 'PackageName' does not expose, so the identity is projected here
rather than through an orphan instance.
-}
newtype CacheKey = CacheKey Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable)

{- Both cache keys share this prefix. The full-packument key is exactly this, and the
single-version key appends the version, so the two stores partition on the same source/package
identity. -}
keyText :: Source -> PackageName -> Text
keyText (Source source) name =
    source
        <> "\x1f"
        <> show (pkgEcosystem name)
        <> "\x1f"
        <> maybe "" renderScope (pkgNamespace name)
        <> "\x1f"
        <> TS.toText (pkgCanonical name)

-- | Project a 'Source' and a 'PackageName' to their full-packument cache key.
cacheKey :: Source -> PackageName -> CacheKey
cacheKey source name = CacheKey (keyText source name)

{- | The key a single-version entry is cached under: the identity 'cacheKey' uses with the rendered
'Version' appended, so the version store partitions on the same source as the full store.
-}
newtype VersionKey = VersionKey Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable)

versionKey :: Source -> PackageName -> Version -> VersionKey
versionKey source name version = VersionKey (keyText source name <> "\x1f" <> renderVersion version)

{- | The metadata-cache handle, opaque and built with 'newMetadataCache'. One lives in the
composition root per process, so every request shares the stores and their connection-collapsing.
-}
data MetadataCache = MetadataCache
    { mcFull :: SingleFlight MetadataError CacheKey CacheEntry
    -- ^ The full-packument store, keyed by @(source, package)@.
    , mcVersion :: SingleFlight MetadataError VersionKey (Maybe PackageDetails)
    {- ^ The single-version store, keyed by @(source, package, version)@. Only the single-version
    path writes it, never the full path.
    -}
    , mcAssembled :: SingleFlight Void Text ByteString
    {- ^ The assembled-representation store, keyed by the derived validator's rendered form: a
    content address over every serve input (see the module header). Only the packument serve tail
    writes and reads it. The 'Void' error slot states that the render has no domain failure.
    -}
    }

{- | Build a metadata cache from its configuration. The three stores share one TTL, and each is
sized from its __own__ sub-budget.
-}
newMetadataCache :: CacheConfig -> IO MetadataCache
newMetadataCache cfg =
    MetadataCache
        <$> newStore (cacheFullBudget cfg) weighCacheEntry
        <*> newStore (cacheVersionBudget cfg) weighVersion
        <*> newStore (cacheAssembledBudget cfg) weighAssembled
  where
    newStore :: StoreBudget -> (v -> Int) -> IO (SingleFlight e k v)
    newStore budget = newSingleFlight (cacheTtl cfg) (sbMaxEntries budget) (sbMaxBytes budget)

{- | Resolve a package's metadata from one upstream 'Source', reusing the cache and collapsing
concurrent misses. A failed fetch caches __nothing__, and its typed 'Left' reaches every waiter.

Only the fetch and the parse are memoised. The caller's rules re-decide the verdict per request.

The 'Source' partitions the cache, and the fetch action supplies that origin's own credential. The
trusted private origin is fetched per request and never cached, so a shared entry can never serve
one client another client's private document.

A claimed in-flight slot is always eventually filled and de-registered, even when an async exception
cancels the leader. Otherwise that key would wedge until restart.

Each resolution records the @ecluse.metadata_cache.requests@ hit\/miss counter, where a coalescing
follower counts as a miss like the leader it waits on.
-}
resolveMetadata :: MetricsPort -> MetadataCache -> Source -> PackageName -> IO (Either MetadataError CacheEntry) -> IO (Either MetadataError CacheEntry)
resolveMetadata = resolveMetadataWith (pure ())

{- As 'resolveMetadata', but with a hook run on the leading thread between the in-flight claim's
STM commit and the leader's exception guard taking the marker. 'resolveMetadata' passes @pure ()@,
which is the only caller. -}
resolveMetadataWith :: IO () -> MetricsPort -> MetadataCache -> Source -> PackageName -> IO (Either MetadataError CacheEntry) -> IO (Either MetadataError CacheEntry)
resolveMetadataWith afterClaim metrics cache source name =
    resolveSingleFlight
        afterClaim
        (mpCacheRequest metrics)
        ( \occ -> do
            mpCacheEntries metrics (occEntries occ)
            mpCacheResidentBytes metrics (occBytes occ)
        )
        (mcFull cache)
        (cacheKey source name)

{- | Resolve __one version's__ 'PackageDetails' from the single-version cache, leading a selective
fetch on a miss and collapsing concurrent misses as 'resolveMetadata' does.

A version determined absent is cached as 'Nothing' and re-served without a re-fetch within the TTL.
This writes the single-version store only, so a selective parse can never materialise a whole
packument into the shared full cache.
-}
resolveVersion :: MetricsPort -> MetadataCache -> Source -> PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails)) -> IO (Either MetadataError (Maybe PackageDetails))
resolveVersion = resolveVersionWith (pure ())

{- As 'resolveVersion', with the claim-to-fetch hook 'resolveMetadataWith' documents.
'resolveVersion' passes @pure ()@.
-}
resolveVersionWith :: IO () -> MetricsPort -> MetadataCache -> Source -> PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails)) -> IO (Either MetadataError (Maybe PackageDetails))
resolveVersionWith afterClaim metrics cache source name version =
    resolveSingleFlight
        afterClaim
        (const pass)
        (mpVersionCacheResidentBytes metrics . occBytes)
        (mcVersion cache)
        (versionKey source name version)

{- | Resolve the __assembled representation__ for one derived validator, leading the render on a
miss and collapsing concurrent identical renders.

The key is the rendered derived 'Ecluse.Core.Server.Conditional.ETag', a content address over every
input the served document is a function of. Changed inputs miss by construction, and a different
private view is a different key, so the store never serves stale bytes and never crosses a client
boundary.
-}
resolveAssembled :: MetricsPort -> MetadataCache -> Text -> IO ByteString -> IO ByteString
resolveAssembled metrics cache key render =
    either absurd id
        <$> resolveSingleFlight
            (pure ())
            (const pass)
            (mpAssembledCacheResidentBytes metrics . occBytes)
            (mcAssembled cache)
            key
            (Right <$> render)

{- | Look up a cached full-packument entry without fetching on a miss and __without bumping
recency__. The packument @GET@'s own 'resolveMetadata' hit drives the full store's recency, so the
tarball gate's read-only consult need not bump it. A 'Nothing' is a miss or an expired entry.
-}
cachedMetadata :: MetadataCache -> Source -> PackageName -> IO (Maybe CacheEntry)
cachedMetadata cache source name = lookupStore (mcFull cache) (cacheKey source name)

{- | Look up a single-version entry without fetching on a miss, __bumping its recency on a hit__.
This read is the version store's only steady-state access, so without the bump a warm entry would
age out of least-recently-used eviction in insert order.

The outer 'Maybe' is the cache hit or miss. The inner @'Maybe' 'PackageDetails'@ is the cached
result, where @'Just' 'Nothing'@ is a version determined absent.
-}
cachedVersion :: MetadataCache -> Source -> PackageName -> Version -> IO (Maybe (Maybe PackageDetails))
cachedVersion cache source name version = lookupStoreTouching (mcVersion cache) (versionKey source name version)
