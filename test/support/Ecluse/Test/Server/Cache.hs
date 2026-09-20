-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cache fixtures, external adapter doubles, and historical full-entry accounting.
module Ecluse.Test.Server.Cache (
    -- * Cache configuration fixtures
    defaultCacheConfig,
    externalBackend,
    externalOperations,
    newLocalBackend,
    newLocalRetention,
    newSingleFlight,
    cachedMetadata,
    cachedVersion,
    weighCacheEntry,
) where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Time (NominalDiffTime)

import Ecluse.Core.Package (PackageDetails (pkgArtifacts), PackageInfo (infoVersions), PackageName, artEntryKey)
import Ecluse.Core.Registry.CachedDocument (foldCachedDoc)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataUndecodable), VersionRead)
import Ecluse.Core.Server.Cache (CacheConfig (..), CacheEntry (..), MetadataCache, Source, StoreBudget (..), resolveMetadata, resolveVersion)
import Ecluse.Core.Server.Cache.Backend (BackendStorage (ExternalStorage, LocalStorage), Recency, RetentionBackend, RetentionOperations (..), retentionBackend)
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool, newPooledRetention)
import Ecluse.Core.Server.Cache.Store (SingleFlight, newSingleFlightWithBackend)
import Ecluse.Core.Server.Cache.VersionWeight (weighEntryKey)
import Ecluse.Core.Server.MemoryModel (expandWireBytes)
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Version (Version)

-- | A 60-second TTL and 256 MiB shared by locally eligible stores.
defaultCacheConfig :: CacheConfig
defaultCacheConfig =
    CacheConfig
        { cacheTtl = 60
        , cacheMaxEntries = 1024
        , cacheMaxBytes = 256 * 1024 * 1024
        , cacheVersionBudget = StoreBudget 0 0
        , cacheAssembledBudget = StoreBudget 0 0
        }

-- | Adapt test operations to the same bounded backend contract used by production storage.
externalBackend :: Int -> (Recency -> k -> IO (Maybe v)) -> (k -> v -> IO ()) -> RetentionBackend k v
externalBackend micros readValue writeValue =
    retentionBackend (ExternalStorage micros) (externalOperations readValue writeValue)

-- | Historical full-entry charge for diagnostic comparisons, never local retention admission.
weighCacheEntry :: CacheEntry -> Int
weighCacheEntry entry =
    fromInteger (min (toInteger (maxBound :: Int)) (toInteger encodedWeight + keysWeight))
  where
    encodedWeight = expandWireBytes (fromIntegral (foldCachedDoc (BSL.length . encode) (entryRaw entry)))
    keysWeight = sum [weighEntryKey (artEntryKey artifact) | details <- toList (infoVersions (entryInfo entry)), artifact <- toList (pkgArtifacts details)]

-- | External doubles may ignore local recency hints and occupancy callbacks.
externalOperations :: (Recency -> k -> IO (Maybe v)) -> (k -> v -> IO ()) -> RetentionOperations k v
externalOperations readValue writeValue = RetentionOperations (const readValue) (\_ _ -> writeValue)

-- | Build a standalone bounded store. Zero bounds disable insertion without weighing values.
newLocalRetention :: (Hashable k) => NominalDiffTime -> Int -> Int -> (v -> Int) -> IO (RetentionOperations k v)
newLocalRetention ttl maxEntries maxBytes weigh = do
    pool <- newLocalPool maxEntries maxBytes
    newPooledRetention pool ttl (StoreBudget 0 0) weigh

-- | Build the shipped local operations through the same backend constructor as the provider.
newLocalBackend :: (Hashable k) => NominalDiffTime -> Int -> Int -> (v -> Int) -> IO (RetentionBackend k v)
newLocalBackend ttl entries bytes weigh = retentionBackend LocalStorage <$> newLocalRetention ttl entries bytes weigh

-- | Local retention fixture for generic coalescing and maintenance checks.
newSingleFlight :: (Hashable k) => NominalDiffTime -> Int -> Int -> (v -> Int) -> IO (SingleFlight e k v)
newSingleFlight ttl entries bytes weigh = newLocalBackend ttl entries bytes weigh >>= newSingleFlightWithBackend . Just

-- | Inspect retention with a failing origin double, so the probe never creates an entry.
cachedMetadata :: MetricsPort -> MetadataCache -> Source -> PackageName -> IO (Maybe CacheEntry)
cachedMetadata metrics cache source name = rightToMaybe <$> resolveMetadata metrics cache source name (pure (Left MetadataUndecodable))

-- | Inspect selected retention with a failing origin double and ordinary read recency.
cachedVersion :: MetricsPort -> MetadataCache -> Source -> PackageName -> Version -> IO (Maybe VersionRead)
cachedVersion metrics cache source name version = rightToMaybe <$> resolveVersion metrics cache source name version (pure (Left MetadataUndecodable))
