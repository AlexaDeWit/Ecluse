-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cache fixtures, external adapter doubles, and historical full-entry accounting.
module Ecluse.Test.Server.Cache (
    -- * Cache configuration fixtures
    defaultCacheConfig,
    newMetadataCache,
    newRetainingMetadataCache,
    localCacheProvider,
    externalBackend,
    externalOperations,
    newLocalBackend,
    newLocalRetention,
    newSingleFlight,
    validatorFor,
    assembledKeyFor,
    cachedMetadata,
    cachedVersion,
    weighCacheEntry,
    diagnosticDocumentValue,
) where

import Crypto.Hash (SHA256 (SHA256), hashWith)
import Data.Aeson (Value (Null), encode, object, toJSON, (.=))
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)

import Ecluse.Core.Package (PackageDetails (pkgArtifacts), PackageInfo (infoVersions), PackageName, artEntryKey, renderPackageName)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, cargoCached, npmCached, pypiSimpleCached, rubyGemsCached)
import Ecluse.Core.Registry.Cargo.Document (cargoRecords)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataUndecodable), VersionRead)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (grSource))
import Ecluse.Core.Server.Cache (CacheConfig (..), CacheEntry (..), MetadataCache, Source, StoreBudget (..), newMetadataCacheWithProvider, resolveMetadata, resolveVersion)
import Ecluse.Core.Server.Cache.Backend (BackendStorage (ExternalStorage, LocalStorage), Recency, RetentionBackend, RetentionOperations (..), retentionBackend)
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool, newPooledRetention)
import Ecluse.Core.Server.Cache.Provider (CacheProvider, cacheProvider, localCacheProviderWithPool)
import Ecluse.Core.Server.Cache.Store (SingleFlight, newSingleFlightWithBackend)
import Ecluse.Core.Server.Cache.Types (CacheKey, assembledKey)
import Ecluse.Core.Server.Cache.VersionWeight (weighEntryKey)
import Ecluse.Core.Server.Conditional (ETag, mkStrongETag)
import Ecluse.Core.Server.MemoryModel (expandWireBytes)
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Version (Version)
import Ecluse.Test.Registry.PyPI.Metadata (simpleValue)

-- | Isolate a fixture's cache from other stores while preserving the configured local bounds.
newMetadataCache :: CacheConfig -> IO MetadataCache
newMetadataCache config = localCacheProvider config >>= newMetadataCacheWithProvider

-- | A cache whose only retained store is an external full-document double that keeps every write in memory.
newRetainingMetadataCache :: IO MetadataCache
newRetainingMetadataCache = do
    values <- newIORef Map.empty
    let full = externalOperations (\_ key -> Map.lookup key <$> readIORef values) (\key value -> atomicModifyIORef' values (\stored -> (Map.insert key value stored, ())))
    newMetadataCacheWithProvider (cacheProvider (ExternalStorage 100000) Nothing (Just full) Nothing Nothing)

-- | Give a fixture its own pool for selected versions and assembled responses.
localCacheProvider :: CacheConfig -> IO CacheProvider
localCacheProvider config = do
    pool <- newLocalPool (cacheMaxEntries config) (cacheMaxBytes config)
    localCacheProviderWithPool pool config

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
    encodedWeight = expandWireBytes (fromIntegral (BSL.length (encode (diagnosticDocumentValue (entryRaw entry)))))
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

-- | A validator over the digest of some test bytes, in place of a served document's.
validatorFor :: ByteString -> ETag
validatorFor = mkStrongETag . hashWith SHA256

-- | An assembled key for a test label, through a validator over the label's digest.
assembledKeyFor :: Text -> CacheKey
assembledKeyFor = assembledKey . validatorFor . encodeUtf8

-- | Inspect retention with a failing origin double, so the probe never creates an entry.
cachedMetadata :: MetricsPort -> MetadataCache -> Source -> PackageName -> IO (Maybe CacheEntry)
cachedMetadata metrics cache source name = rightToMaybe <$> resolveMetadata metrics cache source name (pure (Left MetadataUndecodable))

-- | Inspect selected retention with a failing origin double and ordinary read recency.
cachedVersion :: MetricsPort -> MetadataCache -> Source -> PackageName -> Version -> IO (Maybe VersionRead)
cachedVersion metrics cache source name version = rightToMaybe <$> resolveVersion metrics cache source name version (pure (Left MetadataUndecodable))

-- | Diagnostic JSON preserves source fields. Its size is neither native wire bytes nor production cache charge.
diagnosticDocumentValue :: CachedDoc -> Value
diagnosticDocumentValue document = fromMaybe Null (snd npmCached document <|> (simpleValue <$> snd pypiSimpleCached document) <|> (toJSON . map snd . cargoRecords <$> snd cargoCached document) <|> (nativeValue <$> snd rubyGemsCached document))
  where
    nativeValue native =
        object
            [ "name" .= renderPackageName (gemName native)
            , "header" .= (decodeUtf8 (gemHeader native) :: Text)
            , "rows" .= [decodeUtf8 (grSource row) :: Text | row <- gemRows native]
            ]
