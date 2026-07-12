-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The short-TTL, size-bounded metadata cache shared by the serve paths.

Resolving a package re-fetches its upstream packument, parses it, and evaluates
the rules. To avoid repeating the fetch and parse, the result (a coherent pair of
the parsed __packument metadata__, 'PackageInfo', and the __raw document__ it was
decoded from, 'CacheEntry') is held here in a short-TTL, size-bounded, STM-backed
cache (the @cache@ library backs the TTL store). Both serve paths share it: a
packument request and the tarball-gating fetch that follows reuse one fetch and
parse, and concurrent resolutions of a popular package __collapse to one upstream
call__ (single-flight).

== Per-source key

A packument is fetched from two distinct upstreams, a private origin and a public
origin, whose documents differ for the same package, so one entry cannot represent
both. The key is @(source, package)@: the source is the upstream's base URL, which
distinguishes any cached origin without naming a credential, so distinct upstreams
never cross-contaminate and the key never blurs the trust split.

== Credential-free; sharing is the caller's policy

The key carries __no credential dimension__ and the value is a canonical document,
so the cache stores nothing derived from a caller's credential. Whether a given
origin is handed to it, and so shared across clients, is the serve path's decision.

Under the default @passthrough@ access strategy only the anonymous public origin is
cached. The trusted private upstream is the per-client authority: it re-authorises
each request with that client's own forwarded credential, so the serve path fetches
it per request and never hands it here. Were a private entry cached under
@passthrough@, the credential-free key would let one client's entry serve another
client's private document within the TTL, bypassing the upstream's authorisation.
The public origin is anonymous, so one shared entry serves every client without
crossing a trust boundary. Other strategies make a shared private entry safe by
authorising each serve before it is returned (see
@docs\/architecture\/access-model.md@ → "Caching"); that gate lives on the serve
path, never in this store.

== Coherent pair

An entry holds the parsed 'PackageInfo' __and__ the raw 'Value' it was decoded
from, so a hit returns a typed view and the exact bytes that produced it. The
packument serve path needs both: it decides over the typed view but serves the raw
document edited in place, and the two must describe the same fetch.

What is cached is the __metadata, not the verdict__. The rules are re-evaluated on
the cached metadata each request, so time-sensitive rules
('Ecluse.Core.Rules.Policy.AllowIfOlderThan') and the separately-synced advisory
tier stay correct; only each upstream's fetch and parse is memoised. The TTL is
short, and brief staleness is benign: a brand-new publish need not appear instantly
(see @docs\/architecture\/web-layer.md@ → "Metadata cache").

Two properties the @cache@ library does not provide on its own are layered onto
every store by the shared machinery ("Ecluse.Core.Server.Cache.Store"):

* __Resident-byte budget with recency-aware eviction.__ @cache@ expires by TTL but
  bounds neither entry count nor memory. Each entry is wrapped with an estimate of
  its resident footprint (a heavy packument, parsed plus raw, costs many times its
  wire size) and a last-access stamp bumped on every hit. An insert first purges
  expired entries, then evicts the __least-recently-used__ entries until the incoming
  entry fits within both a resident-byte budget ('cacheMaxBytes') and an entry count
  ('cacheMaxEntries'). Recency keeps a re-accessed hot head resident under pressure
  while shedding the one-shot tail; the byte budget bounds memory more faithfully
  than a count alone. The incoming entry is always admitted (the per-entry ceiling is
  the upstream body cap, not this budget).

* __Single-flight.__ @cache@'s own @fetchWithCache@ is lookup-then-fetch in plain
  'IO', so two concurrent misses would both fetch. 'resolveMetadata' instead
  installs an in-flight marker atomically, so the first miss fetches while concurrent
  misses wait on its result. The leader inserts the result into the store __before__
  removing its in-flight marker, so a caller arriving the instant the fetch returns
  still finds either the store entry or the marker (never a gap) and never re-leads a
  redundant fetch.

== Two coherent stores: the full packument and one version

This handle owns __two__ stores of the same shape (the TTL + size-bound + single-flight
machinery, 'SingleFlight', is shared between them):

  * the __full-packument__ store ('resolveMetadata' \/ 'cachedMetadata'), keyed by
    @(source, package)@, holding the 'CacheEntry' described above; and

  * a __single-version__ store ('resolveVersion' \/ 'cachedVersion'), keyed by
    @(source, package, version)@, holding just one version's
    'Ecluse.Core.Package.PackageDetails' (or its determined absence, a cached
    'Nothing'): the cold tarball gate's selectively-parsed result.

They are __isolated on writes__: a single-version resolution caches under its own
key and __never writes back__ to the full-packument store, so a cold tarball gate
cannot materialise a whole packument into the shared full cache. The serve path's
single-version read consults the warm full-packument store __read-only__ first (a
packument @GET@ followed by its tarball gate still collapses to one upstream call),
and only falls back to leading its own selective fetch into the version store when
the full entry is cold. Both stores enforce the resident-byte budget, and each
reports its own residency gauge: the full-packument store under
@ecluse.metadata_cache.resident_bytes@ and the single-version store under
@ecluse.metadata_cache.version.resident_bytes@. The hit\/miss counter and the
entry-count occupancy gauge stay about the full-packument store.

A third store memoises the __assembled representation__ ('resolveAssembled'): the
encoded merged document, keyed by its derived validator
('Ecluse.Core.Server.Pipeline.Packument.packumentETag'). The key is a fingerprint of
every input the document is a function of (the origin bodies, private included by
content digest; the survivor sets; the mount base), which makes the store
__content-addressed__: an entry can never be served stale, because changed inputs
produce a different key and simply miss. The resident-byte budget is the real bound
here, not the TTL, which only trims dead entries early. Cross-client safety follows
from the same property: a lookup key includes the digest of the private document
__this request's own authorised fetch returned__, so a client can only hit an entry
whose bytes its own inputs would deterministically re-produce. The transform is
shared, never the authorisation and never another client's view (the private-origin
caching prohibition is about credential-blind keying, which a content key is not).
Residency gauge: @ecluse.metadata_cache.assembled.resident_bytes@.
-}
module Ecluse.Core.Server.Cache (
    -- * Configuration
    CacheConfig (..),

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

import Data.Aeson (Value, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
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
import Ecluse.Core.Registry.Metadata (ContentDigest, MetadataError)
import Ecluse.Core.Server.Cache.Store (
    CacheOccupancy (..),
    SingleFlight,
    lookupStore,
    newSingleFlight,
    resolveSingleFlight,
 )
import Ecluse.Core.Telemetry.Record (
    MetricsPort,
    mpAssembledCacheResidentBytes,
    mpCacheEntries,
    mpCacheRequest,
    mpCacheResidentBytes,
    mpVersionCacheResidentBytes,
 )
import Ecluse.Core.Version (Version, renderVersion)

{- | The metadata cache's tunables, sourced from configuration: how long a parsed
packument stays fresh, how many distinct @(source, package)@ entries the cache holds,
and the resident-byte budget it keeps the held entries under before it evicts.
-}
data CacheConfig = CacheConfig
    { cacheTtl :: NominalDiffTime
    {- ^ How long a cached 'CacheEntry' is served before it is re-fetched. Short
    by design: brief staleness is benign, and conditional-GET revalidates.
    -}
    , cacheMaxEntries :: Int
    {- ^ The maximum number of distinct @(source, package)@ entries held; an insert
    past this evicts.
    -}
    , cacheMaxBytes :: Int
    {- ^ The resident-byte budget the held entries are kept under. Each entry is
    weighted by an estimate of its resident footprint, and an insert past this evicts
    the least-recently-used entries until the budget holds. A heavy packument (the
    parsed view plus its raw document) costs many times its wire size, so this bounds
    memory more faithfully than the entry count alone.
    -}
    }
    deriving stock (Eq, Show)

{- | Which upstream a cached packument was fetched from: the dimension that
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
and the exact bytes that produced it: the packument serve path edits the raw 'Value'
in place and must keep its typed decision coherent with those bytes.
-}
data CacheEntry = CacheEntry
    { entryInfo :: PackageInfo
    -- ^ The typed packument view the rules and merge reason over.
    , entryRaw :: Value
    -- ^ The raw upstream document the served body is built from, edited in place.
    , entryDigest :: ContentDigest
    {- ^ Digest of the wire bytes both views were decoded from, computed once at the
    leader's fetch -- the public origin's contribution to the serve path's derived
    ETag, amortised across every hit on this entry.
    -}
    }
    deriving stock (Eq, Show)

{- | Estimate a 'CacheEntry'\'s resident footprint in bytes as a fixed multiple of its raw
document's compact-encoded byte length. The resident cost (the parsed 'PackageInfo' plus the
raw 'Value') is a near-constant multiple of the document's size, so re-encoding the raw
'Value' and scaling it estimates the footprint without measuring the parsed structure. The
encode is an @O(document)@ pass run only on a leader's insert (the cold path after a fetch),
never on a hit. The multiplier is set at the high end of the observed resident-to-encoded
ratio so the estimate is an upper bound: a memory budget must not systematically under-count.
-}
weighCacheEntry :: CacheEntry -> Int
weighCacheEntry e = weighEncodedBytes (BSL.length (encode (entryRaw e)))

{- | Estimate a single-version entry's resident footprint in bytes. A present version's
'PackageDetails' is a single bounded manifest, so it is weighted at a flat per-version
figure; a cached determined absence (a negative entry) carries only a small fixed overhead.
The single-version store holds no raw document, so its weight is a fixed estimate rather than
an encoded-size multiple.
-}
weighVersion :: Maybe PackageDetails -> Int
weighVersion = \case
    Just _ -> versionEntryBytes
    Nothing -> negativeEntryBytes

-- Scale a raw document's encoded byte length to an estimated resident footprint. The factor
-- is 7.5 (applied as a halved integer to stay in 'Int' arithmetic): it sits at the high end
-- of the measured resident-to-encoded ratio, so the estimate upper-bounds resident bytes and
-- the budget never under-counts (leaner documents are over-estimated, which only over-evicts).
weighEncodedBytes :: Int64 -> Int
weighEncodedBytes encodedLen = fromIntegral (encodedLen * residentRatioNumerator `div` residentRatioDenominator)

residentRatioNumerator :: Int64
residentRatioNumerator = 15

residentRatioDenominator :: Int64
residentRatioDenominator = 2

-- The flat resident estimate for a present single-version entry (one bounded manifest) and
-- for a cached determined absence (a small negative entry).
versionEntryBytes :: Int
versionEntryBytes = 16 * 1024

negativeEntryBytes :: Int
negativeEntryBytes = 1024

{- | An assembled entry's resident footprint __is__ its strict bytes (plus a small
constant for the key and spine): unlike a parsed 'CacheEntry' there is no expanded
structure to estimate, so the budget counts what is genuinely held.
-}
weighAssembled :: ByteString -> Int
weighAssembled bytes = BS.length bytes + assembledEntryOverheadBytes

assembledEntryOverheadBytes :: Int
assembledEntryOverheadBytes = 256

{- | The key a 'CacheEntry' is cached under: the upstream 'Source' paired with the
package's identity, rendered to a stable 'Text'. The package identity is distinct
from a display name so two encodings of the same scoped package share one entry, and
the source dimension keeps distinct upstreams apart; equality and ordering match
@(Source, PackageName)@ identity (the @cache@ library needs a 'Hashable' key, which
the opaque 'PackageName' does not expose, so the identity is projected to this key
here rather than via an orphan instance).
-}
newtype CacheKey = CacheKey Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable)

{- The @(source, package)@ identity rendered to a stable 'Text': the source's base URL
joined with the package's identity (not its display form). The shared prefix of both
cache keys -- the full-packument key is exactly this, the single-version key appends the
version -- so the two stores partition on the same source\/package identity. -}
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
'cacheKey' uses, with the rendered 'Version' appended -- so distinct versions of one
package hold distinct entries, and the version store partitions on the same source as the
full store.
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
    version's 'PackageDetails' (or its determined absence), written only by the
    single-version path, never the full path.
    -}
    , mcAssembled :: SingleFlight Void Text ByteString
    {- ^ The assembled-representation store: the encoded served document, keyed by its
    derived validator's rendered form (a content address over every serve input; see
    the module header), written and read only by the packument serve tail. The
    'Void' error slot states in the type that the assembled render has no domain
    failure: a bottom during the render is an invariant break, not an outcome.
    -}
    }

{- | Build a metadata cache from its configuration: the full-packument store, the
single-version store, and the assembled-representation store, each over the same TTL
and size bound.
-}
newMetadataCache :: CacheConfig -> IO MetadataCache
newMetadataCache cfg =
    MetadataCache
        <$> newStore weighCacheEntry
        <*> newStore weighVersion
        <*> newStore weighAssembled
  where
    newStore :: (v -> Int) -> IO (SingleFlight e k v)
    newStore = newSingleFlight (cacheTtl cfg) (cacheMaxEntries cfg) (cacheMaxBytes cfg)

{- | Resolve a package's metadata from one upstream 'Source', reusing the cache and
collapsing concurrent misses.

On a fresh, unexpired hit the cached 'CacheEntry' is returned and the fetch action
is never run. On a miss the action runs exactly once even under concurrent callers:
the first installs an in-flight marker and fetches, the others wait on its result.
A successful fetch is cached (subject to the TTL and size bound); a failed fetch
caches __nothing__ (so a transient upstream error does not poison the cache) and its
typed 'Left' is handed to every waiter, so a coalesced follower sees exactly the
fault the leader saw.

A claimed in-flight slot is __always eventually filled and de-registered__, even if
the leader is hit by an async exception (a request timeout, a killed handler thread)
between claiming the slot and completing: the claim commits under a 'mask' and the
leader's run is handed straight to 'Ecluse.Core.InFlight.guardInFlight', which frees the
slot on every exit and, on an escape before the marker is filled, hands that error
to every waiting follower rather than leaving them parked forever. This closes the
single-flight orphan window (without it, a cancelled leader would wedge that
@(source, package)@ key until restart). A follower receiving an orphaned marker
re-evaluates the resolve when the leader was cancelled (async), and re-raises when
the leader escaped synchronously: the fetch's contract is total, so a synchronous
escape is an invariant break for the outer boundary, never laundered into the typed
channel. A follower's own wait on the marker stays interruptible.

The 'Source' partitions the cache: distinct upstreams of the same package resolve
under distinct keys and never cross-contaminate. The fetch action supplies the origin's
own credential, so reading through one source never blurs another's trust posture.
Under the default @passthrough@ strategy only the anonymous public origin is resolved
here: the trusted private origin is the per-client authority and is fetched per request,
never cached, so a shared entry can never serve one client another's private document.

The result is always re-decided by the caller's rules on each request -- only the
fetch+parse is memoised, never the verdict.

Each resolution records the @ecluse.metadata_cache.requests@ hit\/miss counter (a
coalescing follower counts as a miss, like the leader it waits on), and a leader's
insert refreshes the @ecluse.metadata_cache.entries@ occupancy gauge and the
@ecluse.metadata_cache.resident_bytes@ residency gauge.
-}
resolveMetadata :: MetricsPort -> MetadataCache -> Source -> PackageName -> IO (Either MetadataError CacheEntry) -> IO (Either MetadataError CacheEntry)
resolveMetadata = resolveMetadataWith (pure ())

{- | As 'resolveMetadata', but with a hook run on the leading thread at the
single-flight claim → fetch-runner handoff: the window between the STM transaction
committing the in-flight claim and the leader's exception guard taking ownership of
the marker. It exists only so a test can deterministically park a leader in that
window and cancel it there, exercising the orphan-window guarantee; production always
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
single-version cache, leading a selective fetch on a miss and collapsing concurrent misses
exactly as 'resolveMetadata' does for the full packument. The cached value is the
@'Maybe' 'PackageDetails'@ the fetch yields, so a version determined __absent__ over sound
metadata is cached as 'Nothing' (a negative entry) and re-served without a re-fetch within
the TTL.

This writes to the single-version store only, never the full-packument store, so a cold
tarball gate's selective parse cannot materialise a whole packument into the shared full
cache. Unlike 'resolveMetadata', the single-version store records no hit\/miss counter; a
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

{- | Resolve the __assembled representation__ for one derived validator, leading the
render (assemble + encode) on a miss and collapsing concurrent identical renders,
exactly as 'resolveMetadata' does for a fetch.

The key is the rendered derived 'Ecluse.Core.Server.Conditional.ETag' -- a content
address over every input the served document is a function of -- so a hit is
byte-for-byte the document this request's own inputs would deterministically produce:
the store can never serve stale bytes (changed inputs miss by construction) and never
crosses a client boundary (a different private view is a different key; see the
module header). Under the TTL-zero configuration the store degrades to pure
single-flight coalescing, the same behaviour as the sibling stores.

Like the single-version store it records no hit\/miss counter; a leader's insert
refreshes the @ecluse.metadata_cache.assembled.resident_bytes@ residency gauge, so
the byte budget's third occupant is observable alongside the other two.

The store's error slot is 'Void' -- the render has no domain failure -- so the
resolve is folded back to a plain 'IO' 'ByteString' here ('absurd' discharges the
impossible 'Left'), keeping the serve tail's call shape unchanged.
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

{- | Look up a package's cached full-packument entry for one 'Source' without fetching on a
miss: the cache's read-only view, for inspection and tests. A 'Nothing' is a miss or an
expired entry; this never triggers a fetch and never collapses (use 'resolveMetadata' for
the serve path).
-}
cachedMetadata :: MetadataCache -> Source -> PackageName -> IO (Maybe CacheEntry)
cachedMetadata cache source name = lookupStore (mcFull cache) (cacheKey source name)

{- | Look up a single-version cached entry for one @(source, package, version)@ without
fetching on a miss: the version store's read-only view (the hybrid serve path's negative\/
positive lookup before it leads a selective fetch). The outer 'Maybe' is the cache hit\/miss
(an expired or absent entry is 'Nothing'); the inner @'Maybe' 'PackageDetails'@ is the
cached result (a version determined absent is a cached @'Just' 'Nothing'@).
-}
cachedVersion :: MetadataCache -> Source -> PackageName -> Version -> IO (Maybe (Maybe PackageDetails))
cachedVersion cache source name version = lookupStore (mcVersion cache) (versionKey source name version)
