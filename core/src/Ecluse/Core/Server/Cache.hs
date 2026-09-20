-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Three metadata representations share single-flight with independent optional retention.
Public metadata and content-addressed responses follow the sharing policy in the web-layer architecture.
-}
module Ecluse.Core.Server.Cache (
    -- * Configuration
    CacheConfig (..),
    StoreBudget (..),

    -- * The cache handle
    MetadataCache,
    newMetadataCache,
    newMetadataCacheWithBackend,

    -- * Cache entries
    Source (..),
    CacheEntry (..),

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
    PackageInfo,
    PackageName,
    pkgCanonical,
    pkgEcosystem,
    pkgNamespace,
    renderScope,
 )
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (ContentDigest, MetadataError, VersionRead)
import Ecluse.Core.Server.Cache.Backend (RetentionBackend, supportsFullRetention)
import Ecluse.Core.Server.Cache.Store (
    CacheOccupancy (..),
    SingleFlight,
    lookupStoreTouching,
    lookupStoreWithFailure,
    newSingleFlight,
    newSingleFlightWithBackend,
    resolveSingleFlight,
 )
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Core.Version (Version, renderVersion)

-- | Limits for one store's entry count and accounted bytes.
data StoreBudget = StoreBudget
    { sbMaxEntries :: Int
    -- ^ The maximum number of distinct entries held. An insert past this evicts.
    , sbMaxBytes :: Int
    -- ^ The resident-byte budget the held entries are kept under.
    }
    deriving stock (Eq, Show)

-- | Retention bounds and the TTL for the local selected-version and assembled stores.
data CacheConfig = CacheConfig
    { cacheTtl :: NominalDiffTime
    , cacheFullBudget :: StoreBudget
    -- ^ Compatibility field, inactive for local retention. No local full store is allocated.
    , cacheVersionBudget :: StoreBudget
    -- ^ The single-version store's bounds (retained-field accounting).
    , cacheAssembledBudget :: StoreBudget
    -- ^ The assembled-representation store's bounds (exact strict-bytes weights).
    }
    deriving stock (Eq, Show)

-- | An upstream base URL partitions entries without carrying credentials.
newtype Source = Source Text
    deriving stock (Eq, Ord, Show)

-- | A typed view paired with the raw document and digest from the same fetch.
data CacheEntry = CacheEntry
    { entryInfo :: PackageInfo
    -- ^ The typed packument view the rules and merge reason over.
    , entryRaw :: CachedDoc
    -- ^ The raw upstream document the served body is built from.
    , entryBodyBytes :: Int
    -- ^ Decompressed source bytes, retained independently of the cache weight.
    , entryDigest :: ContentDigest
    }
    deriving stock (Eq, Show)

weighAssembled :: ByteString -> Int
weighAssembled bytes = BS.length bytes + assembledEntryOverheadBytes

assembledEntryOverheadBytes :: Int
assembledEntryOverheadBytes = 256

keyText :: Source -> PackageName -> Text
keyText (Source source) name =
    source
        <> "\x1f"
        <> show (pkgEcosystem name)
        <> "\x1f"
        <> maybe "" renderScope (pkgNamespace name)
        <> "\x1f"
        <> TS.toText (pkgCanonical name)

newtype VersionKey = VersionKey Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable)

versionKey :: Source -> PackageName -> Version -> VersionKey
versionKey source name version = VersionKey (keyText source name <> "\x1f" <> renderVersion version)

-- | Independent retention capabilities and process-local request coalescing.
data MetadataCache = MetadataCache
    { mcFull :: SingleFlight MetadataError Text CacheEntry
    -- ^ Full fetches partition by source, ecosystem, and package without local retention.
    , mcVersion :: SingleFlight MetadataError VersionKey VersionRead
    , mcAssembled :: SingleFlight Void Text ByteString
    }

-- | Build local retention for selected versions and assembled responses only.
newMetadataCache :: CacheConfig -> IO MetadataCache
newMetadataCache cfg = newMetadataCacheWithBackend cfg Nothing

{- | Supply optional external full retention. Local backends are always excluded.
Full retention owns its codec and bounds. Single-flight stays in this process.
-}
newMetadataCacheWithBackend :: CacheConfig -> Maybe (RetentionBackend Text CacheEntry) -> IO MetadataCache
newMetadataCacheWithBackend cfg fullBackend =
    MetadataCache
        <$> newSingleFlightWithBackend (fullBackend >>= \backend -> backend <$ guard (supportsFullRetention backend))
        <*> newStore (cacheVersionBudget cfg) weighVersion
        <*> newStore (cacheAssembledBudget cfg) weighAssembled
  where
    newStore :: (Hashable k) => StoreBudget -> (v -> Int) -> IO (SingleFlight e k v)
    newStore budget = newSingleFlight (cacheTtl cfg) (sbMaxEntries budget) (sbMaxBytes budget)

-- | Coalesce public metadata fetches. Failures reach all waiters and retain nothing.
resolveMetadata :: MetricsPort -> MetadataCache -> Source -> PackageName -> IO (Either MetadataError CacheEntry) -> IO (Either MetadataError CacheEntry)
resolveMetadata metrics cache source name =
    resolveSingleFlight
        (mpCacheRequest metrics)
        (recordFullOccupancy metrics)
        (mpCacheRefused metrics Metric.FullStore)
        (mcFull cache)
        (keyText source name)

-- | Cache a selectively decoded release or its absence. Oversized releases remain uncached.
resolveVersion :: MetricsPort -> MetadataCache -> Source -> PackageName -> Version -> IO (Either MetadataError VersionRead) -> IO (Either MetadataError VersionRead)
resolveVersion metrics cache source name version =
    resolveSingleFlight
        (mpVersionCacheRequest metrics)
        (mpVersionCacheResidentBytes metrics . occBytes)
        (mpCacheRefused metrics Metric.VersionStore)
        (mcVersion cache)
        (versionKey source name version)

-- | Memoise a response under the content digest of this request's authorised inputs.
resolveAssembled :: MetricsPort -> MetadataCache -> Text -> IO ByteString -> IO ByteString
resolveAssembled metrics cache key render =
    either absurd id
        <$> resolveSingleFlight
            (mpAssembledCacheRequest metrics)
            (mpAssembledCacheResidentBytes metrics . occBytes)
            (mpCacheRefused metrics Metric.AssembledStore)
            (mcAssembled cache)
            key
            (Right <$> render)

-- | Probe full metadata without fetching or refreshing recency. Report expiry but no request outcome.
cachedMetadata :: MetricsPort -> MetadataCache -> Source -> PackageName -> IO (Maybe CacheEntry)
cachedMetadata metrics cache source name = lookupStoreWithFailure (recordFullOccupancy metrics) (mpCacheRefused metrics Metric.FullStore) (mcFull cache) (keyText source name)

{- | Probe a version and refresh recency. Report expiry but no request outcome.
A read whose 'vrVersion' is 'Nothing' is a cached absence.
-}
cachedVersion :: MetricsPort -> MetadataCache -> Source -> PackageName -> Version -> IO (Maybe VersionRead)
cachedVersion metrics cache source name version = lookupStoreTouching (mpVersionCacheResidentBytes metrics . occBytes) (mcVersion cache) (versionKey source name version)

recordFullOccupancy :: MetricsPort -> CacheOccupancy -> IO ()
recordFullOccupancy metrics occ = do
    mpCacheEntries metrics (occEntries occ)
    mpCacheResidentBytes metrics (occBytes occ)
