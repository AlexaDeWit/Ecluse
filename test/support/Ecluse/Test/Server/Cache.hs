-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cache fixtures, external adapter doubles, and historical full-entry accounting.
module Ecluse.Test.Server.Cache (
    -- * Cache configuration fixtures
    defaultCacheConfig,
    externalBackend,
    weighCacheEntry,
) where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BSL

import Ecluse.Core.Package (PackageDetails (pkgArtifacts), PackageInfo (infoVersions), artEntryKey)
import Ecluse.Core.Registry.CachedDocument (foldCachedDoc)
import Ecluse.Core.Server.Cache (CacheConfig (..), CacheEntry (..), StoreBudget (..))
import Ecluse.Core.Server.Cache.Backend (BackendStorage (ExternalStorage), Recency, RetentionBackend, retentionBackend)
import Ecluse.Core.Server.Cache.VersionWeight (weighEntryKey)
import Ecluse.Core.Server.MemoryModel (expandWireBytes)

-- | A 60-second TTL and 256 MiB split between locally eligible stores.
defaultCacheConfig :: CacheConfig
defaultCacheConfig =
    CacheConfig
        { cacheTtl = 60
        , cacheFullBudget = StoreBudget{sbMaxEntries = 0, sbMaxBytes = 0}
        , cacheVersionBudget = StoreBudget{sbMaxEntries = 4096, sbMaxBytes = (256 * 1024 * 1024) * 3 `div` 8}
        , cacheAssembledBudget = StoreBudget{sbMaxEntries = 1024, sbMaxBytes = 256 * 1024 * 1024 - (256 * 1024 * 1024) * 3 `div` 8}
        }

-- | Adapt test operations to the same bounded backend contract used by production storage.
externalBackend :: Int -> (Recency -> k -> IO (Maybe v)) -> (k -> v -> IO ()) -> RetentionBackend k v
externalBackend micros readValue writeValue =
    retentionBackend (ExternalStorage micros) (const readValue) (\_ _ -> writeValue)

-- | Historical full-entry charge for diagnostic comparisons, never local retention admission.
weighCacheEntry :: CacheEntry -> Int
weighCacheEntry entry =
    fromInteger (min (toInteger (maxBound :: Int)) (toInteger encodedWeight + keysWeight))
  where
    encodedWeight = expandWireBytes (fromIntegral (foldCachedDoc (BSL.length . encode) (entryRaw entry)))
    keysWeight = sum [weighEntryKey (artEntryKey artifact) | details <- toList (infoVersions (entryInfo entry)), artifact <- toList (pkgArtifacts details)]
