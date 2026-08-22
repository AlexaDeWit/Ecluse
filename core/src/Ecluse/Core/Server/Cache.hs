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

Under the default @passthrough@ access strategy only the anonymous public origin is
cached. The trusted private upstream is the per-client authority: it re-authorises
each request with that client's own forwarded credential. The serve path therefore
fetches it per request and never hands it here. Were a private entry cached under
@passthrough@, the credential-free key would let one client's entry serve another
client's private document within the TTL. That bypasses the upstream's authorisation.
The public origin is anonymous, so one shared entry serves every client without
crossing a trust boundary. Other strategies make a shared private entry safe by
authorising each serve before returning it (see
@docs\/architecture\/access-model.md@ → "Caching"). That gate lives on the serve
path, never in this store.

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
@docs\/architecture\/web-layer.md@ → "Metadata cache").

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
    resolveMetadataWith,
    cachedMetadata,

    -- * Single-version resolution
    resolveVersion,
    resolveVersionWith,
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

{- | One store's bounds: the entry count and the resident-byte budget it keeps its held
entries under before it evicts. Each entry is weighted by an estimate of its resident
footprint. An insert past the byte budget evicts the least-recently-used entries until
the budget holds, which bounds memory more faithfully than the entry count alone.
-}
data StoreBudget = StoreBudget
    { sbMaxEntries :: Int
    -- ^ The maximum number of distinct entries held. An insert past this evicts.
    , sbMaxBytes :: Int
    -- ^ The resident-byte budget the held entries are kept under.
    }
    deriving stock (Eq, Show)

{- | The metadata cache's tunables, sourced from configuration: how long a parsed
packument stays fresh, and each store's own 'StoreBudget'. The three sub-budgets are
carved from one cache aggregate at the composition root
(@Ecluse.Composition.MemoryBudget.budgetCacheConfig@) and __sum to it__. The aggregate
therefore bounds the cache's total resident bytes, while each class's eviction pressure
stays its own.
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

{- | Which upstream a cached packument was fetched from: the dimension that
partitions the cache by source so distinct upstreams never share an entry.

The discriminator is the upstream's __base URL__. An upstream is addressed at a distinct
URL. That URL names a location, never a credential, so keying on it keeps the trust
split intact. The cached origin is fetched with its own token, supplied through its
fetch action, and the source carries none. Under the default @passthrough@ strategy only
the anonymous public origin is cached, so in practice the cache holds one source per
package. The dimension keeps the key honest about /which/ upstream an entry is, and
never blurs the split.
-}
newtype Source = Source Text
    deriving stock (Eq, Ord, Show)

{- | A coherent cache entry: the parsed 'PackageInfo' paired with the raw document
('CachedDoc') it was decoded from. A hit returns both, so a caller gets a typed view to
decide over and the exact bytes that produced it. The packument serve path rebuilds the
served body from the raw document, and must keep its typed decision coherent with those
bytes. The store holds the raw document opaquely: it never reads it, only weighs it
('weighCachedDoc') and hands it back to the injected adapter capabilities.
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

{- | Estimate a 'CacheEntry'\'s resident footprint in bytes as a fixed multiple of its raw
document's compact-encoded byte length ('weighCachedDoc'). The resident cost (the parsed
'PackageInfo' plus the raw document) is a near-constant multiple of the document's size.
Scaling the encoded length therefore estimates the footprint without measuring the parsed
structure. The encode is an @O(document)@ pass run only on a leader's insert (the cold
path after a fetch), never on a hit. The multiplier sits at the high end of the observed
resident-to-encoded ratio, so the estimate is an upper bound. A memory budget must not
systematically under-count.
-}
weighCacheEntry :: CacheEntry -> Int
weighCacheEntry e = weighEncodedBytes (weighCachedDoc (entryRaw e))

{- | Estimate a single-version entry's resident footprint in bytes. A present version's
'PackageDetails' is a single bounded manifest, so it is weighted at a flat per-version
figure. A cached determined absence (a negative entry) carries only a small fixed
overhead. The single-version store holds no raw document, so its weight is a fixed
estimate rather than an encoded-size multiple.
-}
weighVersion :: Maybe PackageDetails -> Int
weighVersion = \case
    Just _ -> versionEntryBytes
    Nothing -> negativeEntryBytes

-- Scale a raw document's encoded byte length to an estimated resident footprint,
-- through the one shared wire-to-resident model ("Ecluse.Core.Server.MemoryModel").
-- This weigher and the composition root's memory plan can then never drift on the
-- expansion factor.
weighEncodedBytes :: Int64 -> Int
weighEncodedBytes = expandWireBytes . fromIntegral

-- The flat resident estimate for a present single-version entry (one bounded manifest) and
-- for a cached determined absence (a small negative entry).
versionEntryBytes :: Int
versionEntryBytes = 16 * 1024

negativeEntryBytes :: Int
negativeEntryBytes = 1024

{- | An assembled entry's resident footprint __is__ its strict bytes, plus a small
constant for the key and spine. Unlike a parsed 'CacheEntry' there is no expanded
structure to estimate, so the budget counts what is genuinely held.
-}
weighAssembled :: ByteString -> Int
weighAssembled bytes = BS.length bytes + assembledEntryOverheadBytes

assembledEntryOverheadBytes :: Int
assembledEntryOverheadBytes = 256

{- | The key a 'CacheEntry' is cached under: the upstream 'Source' paired with the
package's identity, rendered to a stable 'Text'. The package identity is distinct from a
display name, so two encodings of the same scoped package share one entry. The source
dimension keeps distinct upstreams apart, and equality and ordering match
@(Source, PackageName)@ identity. The @cache@ library needs a 'Hashable' key, and the
opaque 'PackageName' does not expose one. The identity is projected to this key here
rather than through an orphan instance.
-}
newtype CacheKey = CacheKey Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable)

{- The @(source, package)@ identity rendered to a stable 'Text': the source's base URL
joined with the package's identity (not its display form). Both cache keys share this
prefix. The full-packument key is exactly this, and the single-version key appends the
version, so the two stores partition on the same source\/package identity. -}
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
'cacheKey' uses, with the rendered 'Version' appended. Distinct versions of one package
therefore hold distinct entries, and the version store partitions on the same source as
the full store.
-}
newtype VersionKey = VersionKey Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable)

versionKey :: Source -> PackageName -> Version -> VersionKey
versionKey source name version = VersionKey (keyText source name <> "\x1f" <> renderVersion version)

{- | The metadata-cache handle: the three single-flight stores (the full-packument
cache, the single-version cache, and the assembled-representation store). Opaque:
built with 'newMetadataCache' and reached only through the accessors. Lives in the
composition root (one per process), so every request shares the same caches and their
connection-collapsing.
-}
data MetadataCache = MetadataCache
    { mcFull :: SingleFlight MetadataError CacheKey CacheEntry
    -- ^ The full-packument store, keyed by @(source, package)@.
    , mcVersion :: SingleFlight MetadataError VersionKey (Maybe PackageDetails)
    {- ^ The single-version store, keyed by @(source, package, version)@, holding one
    version's 'PackageDetails' or its determined absence. Only the single-version path
    writes it, never the full path.
    -}
    , mcAssembled :: SingleFlight Void Text ByteString
    {- ^ The assembled-representation store: the encoded served document, keyed by its
    derived validator's rendered form. That form is a content address over every serve
    input (see the module header), and only the packument serve tail writes and reads
    the store. The 'Void' error slot states in the type that the assembled render has no
    domain failure. A bottom during the render is an invariant break, not an outcome.
    -}
    }

{- | Build a metadata cache from its configuration: the full-packument store, the
single-version store, and the assembled-representation store. Each runs over the same
TTL, sized from its __own__ sub-budget.
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

{- | Resolve a package's metadata from one upstream 'Source', reusing the cache and
collapsing concurrent misses.

On a fresh, unexpired hit the cached 'CacheEntry' is returned and the fetch action is
never run. On a miss the action runs exactly once, even under concurrent callers. The
first installs an in-flight marker and fetches, and the others wait on its result. A
successful fetch is cached, subject to the TTL and the size bound. A failed fetch caches
__nothing__, so a transient upstream error does not poison the cache. Its typed 'Left'
reaches every waiter, so a coalesced follower sees exactly the fault the leader saw.

A claimed in-flight slot is __always eventually filled and de-registered__. That holds
even when an async exception (a request timeout, a killed handler thread) hits the leader
between the claim and completion. The claim commits under a 'mask', and the leader's run
goes straight to 'Ecluse.Core.InFlight.guardInFlight'. That frees the slot on every exit.
On an escape before the marker is filled, it hands the error to every waiting follower
rather than leaving them parked forever. This closes the single-flight orphan window:
without it, a cancelled leader would wedge that @(source, package)@ key until restart.

A follower receiving an orphaned marker re-evaluates the resolve when the leader was
cancelled (async), re-entering interruptibly and counting its miss only once. It
re-raises when the leader escaped synchronously. The fetch's contract is total, so a
synchronous escape is an invariant break for the outer boundary, never laundered into
the typed channel. A follower's own wait on the marker stays interruptible.

The 'Source' partitions the cache: distinct upstreams of the same package resolve under
distinct keys and never cross-contaminate. The fetch action supplies the origin's own
credential, so reading through one source never blurs another's trust posture. Under the
default @passthrough@ strategy only the anonymous public origin is resolved here. The
trusted private origin is the per-client authority. The serve path fetches it per
request and never caches it, so a shared entry can never serve one client another's
private document.

The caller's rules re-decide the result on each request. Only the fetch and the parse
are memoised, never the verdict.

Each resolution records the @ecluse.metadata_cache.requests@ hit\/miss counter, where a
coalescing follower counts as a miss like the leader it waits on. A leader's insert
refreshes the @ecluse.metadata_cache.entries@ occupancy gauge and the
@ecluse.metadata_cache.resident_bytes@ residency gauge.
-}
resolveMetadata :: MetricsPort -> MetadataCache -> Source -> PackageName -> IO (Either MetadataError CacheEntry) -> IO (Either MetadataError CacheEntry)
resolveMetadata = resolveMetadataWith (pure ())

{- | As 'resolveMetadata', but with a hook run on the leading thread at the
single-flight claim → fetch-runner handoff. The window runs from the STM commit of the
in-flight claim to the leader's exception guard taking ownership of the marker. It
exists only so a test can deterministically park a leader in that
window and cancel it there, exercising the orphan-window guarantee. Production always
passes @pure ()@ via 'resolveMetadata'.
-}
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

{- | Resolve __one version's__ 'PackageDetails' (or its determined absence) from the
single-version cache. A miss leads a selective fetch and collapses concurrent misses,
exactly as 'resolveMetadata' does for the full packument. The cached value is the
@'Maybe' 'PackageDetails'@ the fetch yields. A version determined __absent__ over sound
metadata is therefore cached as 'Nothing' (a negative entry) and re-served without a
re-fetch within the TTL.

This writes to the single-version store only, never the full-packument store. A cold
tarball gate's selective parse therefore cannot materialise a whole packument into the
shared full cache. Unlike 'resolveMetadata', the store records no hit\/miss counter. A
leader's insert does refresh the single-version residency gauge
(@ecluse.metadata_cache.version.resident_bytes@), so the byte budget that bounds both
stores is observable on each.
-}
resolveVersion :: MetricsPort -> MetadataCache -> Source -> PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails)) -> IO (Either MetadataError (Maybe PackageDetails))
resolveVersion = resolveVersionWith (pure ())

{- | As 'resolveVersion', with the single-flight claim → fetch-runner handoff hook
'resolveMetadataWith' exposes, for the same orphan-window test (production passes @pure ()@
via 'resolveVersion').
-}
resolveVersionWith :: IO () -> MetricsPort -> MetadataCache -> Source -> PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails)) -> IO (Either MetadataError (Maybe PackageDetails))
resolveVersionWith afterClaim metrics cache source name version =
    resolveSingleFlight
        afterClaim
        (const pass)
        (mpVersionCacheResidentBytes metrics . occBytes)
        (mcVersion cache)
        (versionKey source name version)

{- | Resolve the __assembled representation__ for one derived validator. A miss leads
the render (assemble + encode) and collapses concurrent identical renders, exactly as
'resolveMetadata' does for a fetch.

The key is the rendered derived 'Ecluse.Core.Server.Conditional.ETag', a content address
over every input the served document is a function of. A hit is therefore byte-for-byte
the document this request's own inputs would deterministically produce. The store can
never serve stale bytes, because changed inputs miss by construction. It never crosses a
client boundary either: a different private view is a different key (see the module
header). Under the TTL-zero configuration the store degrades to pure single-flight
coalescing, the same behaviour as the sibling stores.

Like the single-version store it records no hit\/miss counter. A leader's insert
refreshes the @ecluse.metadata_cache.assembled.resident_bytes@ residency gauge, so the
byte budget's third occupant is observable alongside the other two.

The store's error slot is 'Void', because the render has no domain failure. The resolve
is folded back to a plain 'IO' 'ByteString' here ('absurd' discharges the impossible
'Left'), which keeps the serve tail's call shape unchanged.
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

{- | Look up a package's cached full-packument entry for one 'Source' without fetching on
a miss and __without bumping recency__: the cache's read-only view. Two readers share it:
the inspection and test probes, and the hybrid serve path's step-2 full-packument consult.
That consult is the tarball gate selecting one version from a warm entry, and it stays
read-only on purpose. The packument @GET@'s own 'resolveMetadata' hit drives the full
store's recency, so the gate's select need not bump it. The single-version store
differs: its only steady-state read is 'cachedVersion'. A 'Nothing' is a miss or an
expired entry, and this never triggers a fetch and never collapses (use
'resolveMetadata' for the serve path).
-}
cachedMetadata :: MetadataCache -> Source -> PackageName -> IO (Maybe CacheEntry)
cachedMetadata cache source name = lookupStore (mcFull cache) (cacheKey source name)

{- | Look up a single-version cached entry for one @(source, package, version)@ without
fetching on a miss, __bumping the entry's recency on a hit__. This is the hybrid serve
path's step-1 version consult, before it leads a selective fetch. The read is the version
store's only steady-state access, because a hit here short-circuits before
'resolveVersion' and its recency-bumping hit. Without the bump a warm version entry would
never refresh its recency, and would age out of the least-recently-used eviction in
insert order. The outer 'Maybe' is the cache hit\/miss, where an expired or absent entry
is 'Nothing'. The inner @'Maybe' 'PackageDetails'@ is the cached result, where a version
determined absent is a cached @'Just' 'Nothing'@. Never fetches and never collapses (use
'resolveVersion' to lead a selective fetch).
-}
cachedVersion :: MetadataCache -> Source -> PackageName -> Version -> IO (Maybe (Maybe PackageDetails))
cachedVersion cache source name version = lookupStoreTouching (mcVersion cache) (versionKey source name version)
