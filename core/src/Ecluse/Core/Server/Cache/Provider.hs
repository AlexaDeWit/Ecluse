-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | One selected storage provider owns all metadata retention capabilities.
module Ecluse.Core.Server.Cache.Provider (
    CacheProvider,
    cacheProvider,
    localCacheProviderWithPool,
    providerFull,
    providerDirectory,
    providerVersion,
    providerAssembled,
) where

import Data.ByteString qualified as BS

import Ecluse.Core.Registry.Metadata (VersionRead)
import Ecluse.Core.Registry.RubyGems.Directory (GemDirectory, directoryWeight)
import Ecluse.Core.Server.Cache.Backend (BackendStorage (LocalStorage), RetentionBackend, RetentionOperations, retentionBackend, supportsFullRetention)
import Ecluse.Core.Server.Cache.Backend.Local (LocalPool, newPooledRetention)
import Ecluse.Core.Server.Cache.Types
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)

-- | Missing capabilities remain uncached. They never fall back to another storage provider.
data CacheProvider = CacheProvider
    { cpDirectory :: Maybe (RetentionBackend CacheKey (GemDirectory, Int))
    , cpFull :: Maybe (RetentionBackend CacheKey CacheEntry)
    -- ^ Full metadata is absent for local storage, regardless of the supplied operations.
    , cpVersion :: Maybe (RetentionBackend CacheKey VersionRead)
    -- ^ Selected reads may use an adapter-owned projection without loading a full value locally.
    , cpAssembled :: Maybe (RetentionBackend CacheKey ByteString)
    }

-- | Public discovery shares the local pool without using a package identity.
providerDirectory :: CacheProvider -> Maybe (RetentionBackend CacheKey (GemDirectory, Int))
providerDirectory = cpDirectory

-- | Full retention from the selected provider, absent for local storage.
providerFull :: CacheProvider -> Maybe (RetentionBackend CacheKey CacheEntry)
providerFull = cpFull

-- | Selected retention from the same provider, without a full-document read.
providerVersion :: CacheProvider -> Maybe (RetentionBackend CacheKey VersionRead)
providerVersion = cpVersion

-- | Assembled retention from the same provider, with no local fallback.
providerAssembled :: CacheProvider -> Maybe (RetentionBackend CacheKey ByteString)
providerAssembled = cpAssembled

-- | Classify every operation together. Local storage cannot opt into full retention.
cacheProvider ::
    BackendStorage ->
    Maybe (RetentionOperations CacheKey (GemDirectory, Int)) ->
    Maybe (RetentionOperations CacheKey CacheEntry) ->
    Maybe (RetentionOperations CacheKey VersionRead) ->
    Maybe (RetentionOperations CacheKey ByteString) ->
    CacheProvider
cacheProvider storage directory full version assembled =
    CacheProvider
        { cpDirectory = retentionBackend storage <$> directory
        , cpFull = (retentionBackend storage <$> full) <* guard (supportsFullRetention storage)
        , cpVersion = retentionBackend storage <$> version
        , cpAssembled = retentionBackend storage <$> assembled
        }

-- | Share the existing retention bounds with another source-only metadata store.
localCacheProviderWithPool :: LocalPool -> CacheConfig -> IO CacheProvider
localCacheProviderWithPool pool config = do
    version <- newPooledRetention pool (cacheTtl config) (cacheVersionBudget config) weighVersion
    assembled <- newPooledRetention pool (cacheTtl config) (cacheAssembledBudget config) weighAssembled
    directory <- newPooledRetention pool (cacheTtl config) (StoreBudget 0 0) (\(value, keyBytes) -> directoryWeight value + keyBytes)
    pure (cacheProvider LocalStorage (Just directory) Nothing (Just version) (Just assembled))

weighAssembled :: ByteString -> Int
weighAssembled bytes = BS.length bytes + 256
