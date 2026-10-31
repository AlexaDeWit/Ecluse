-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

{- | One provider owns metadata retention. Local single-flight shares active requests.
Public metadata and content-addressed responses follow the sharing policy in the web-layer architecture.
-}
module Ecluse.Core.Server.Cache (
    -- * Configuration
    CacheConfig (..),
    StoreBudget (..),

    -- * The cache handle
    MetadataCache,
    newMetadataCacheWithProvider,

    -- * Cache entries
    Source (..),
    CacheEntry (..),

    -- * Resolution
    resolveMetadata,
    resolveDirectory,

    -- * Single-version resolution
    resolveVersion,
    prepareVersion,

    -- * Assembled-representation resolution
    resolveAssembled,
) where

import Data.ByteString.Short qualified as SBS

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Metadata (MetadataError, VersionRead)
import Ecluse.Core.Registry.RubyGems.Directory (GemDirectory)
import Ecluse.Core.Server.Cache.Provider (CacheProvider, providerAssembled, providerDirectory, providerFull, providerVersion)
import Ecluse.Core.Server.Cache.Store (
    CacheOccupancy (..),
    PreparedStore,
    SingleFlight,
    executePrepared,
    newSingleFlightWithBackend,
    prepareStore,
    resolveSingleFlight,
 )
import Ecluse.Core.Server.Cache.Types
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Core.Version (Version)

-- | One provider supplies every retention capability beside process-local request coalescing.
data MetadataCache = MetadataCache
    { mcDirectory :: SingleFlight MetadataError CacheKey (GemDirectory, Int)
    , mcFull :: SingleFlight MetadataError CacheKey CacheEntry
    -- ^ Full fetches partition by source, ecosystem, and package without local retention.
    , mcVersion :: SingleFlight MetadataError CacheKey VersionRead
    , mcAssembled :: SingleFlight Void CacheKey ByteString
    }

-- | Create only transient flight state. The selected provider owns every retained representation.
newMetadataCacheWithProvider :: CacheProvider -> IO MetadataCache
newMetadataCacheWithProvider provider =
    MetadataCache
        <$> newSingleFlightWithBackend (providerDirectory provider)
        <*> newSingleFlightWithBackend (providerFull provider)
        <*> newSingleFlightWithBackend (providerVersion provider)
        <*> newSingleFlightWithBackend (providerAssembled provider)

-- | Coalesce public metadata fetches. Failures reach all waiters and retain nothing.
resolveMetadata :: MetricsPort -> MetadataCache -> Source -> PackageName -> IO (Either MetadataError CacheEntry) -> IO (Either MetadataError CacheEntry)
resolveMetadata metrics cache source name =
    resolveSingleFlight
        (mpCacheRequest metrics)
        (recordFullOccupancy metrics)
        (mpCacheRefused metrics Metric.FullStore)
        (mcFull cache)
        (fullKey source name)

-- | Cache a selectively decoded release or its absence. Oversized releases remain uncached.
resolveVersion :: MetricsPort -> MetadataCache -> Source -> PackageName -> Version -> IO (Either MetadataError VersionRead) -> IO (Either MetadataError VersionRead)
resolveVersion metrics cache source name version fetch =
    prepareVersion metrics cache source name version fetch >>= executePrepared

-- | Pin a selected local value, including absence, before any remote work.
prepareVersion :: MetricsPort -> MetadataCache -> Source -> PackageName -> Version -> IO (Either MetadataError VersionRead) -> IO (PreparedStore MetadataError VersionRead)
prepareVersion metrics cache source name version =
    prepareStore
        (mpVersionCacheRequest metrics)
        (mpVersionCacheResidentBytes metrics . occBytes)
        (mpCacheRefused metrics Metric.VersionStore)
        (mcVersion cache)
        (versionKey source name version)

-- | Memoise a response under a key that binds it to this request's authorised inputs.
resolveAssembled :: MetricsPort -> MetadataCache -> CacheKey -> IO ByteString -> IO ByteString
resolveAssembled metrics cache key render =
    either absurd id
        <$> resolveSingleFlight
            (mpAssembledCacheRequest metrics)
            (mpAssembledCacheResidentBytes metrics . occBytes)
            (mpCacheRefused metrics Metric.AssembledStore)
            (mcAssembled cache)
            key
            (Right <$> render)

recordFullOccupancy :: MetricsPort -> CacheOccupancy -> IO ()
recordFullOccupancy metrics occ = do
    mpCacheEntries metrics (occEntries occ)
    mpCacheResidentBytes metrics (occBytes occ)

-- | Refresh one public directory per origin and share active acquisitions.
resolveDirectory :: MetricsPort -> MetadataCache -> CacheKey -> IO (Either MetadataError GemDirectory) -> IO (Either MetadataError GemDirectory)
resolveDirectory metrics cache key fetch =
    fmap fst
        <$> resolveSingleFlight
            (mpDirectoryCacheRequest metrics)
            (mpDirectoryCacheResidentBytes metrics . occBytes)
            (mpCacheRefused metrics Metric.DirectoryStore)
            (mcDirectory cache)
            key
            (fmap (fmap (,SBS.length (cacheKeyIdentity key) + 256)) fetch)
