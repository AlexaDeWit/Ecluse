-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | One selected storage provider owns all metadata retention capabilities.
module Ecluse.Core.Server.Cache.Provider (
    CacheProvider,
    cacheProvider,
    localCacheProvider,
    providerFull,
    providerVersion,
    providerAssembled,
) where

import Data.ByteString qualified as BS

import Ecluse.Core.Registry.Metadata (VersionRead)
import Ecluse.Core.Server.Cache.Backend (BackendStorage (LocalStorage), RetentionBackend, RetentionOperations, retentionBackend, supportsFullRetention)
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool, newPooledRetention)
import Ecluse.Core.Server.Cache.Types
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)

-- | Missing capabilities remain uncached. They never fall back to another storage provider.
data CacheProvider = CacheProvider
    { cpFull :: Maybe (RetentionBackend CacheKey CacheEntry)
    -- ^ Full metadata is absent for local storage, regardless of the supplied operations.
    , cpVersion :: Maybe (RetentionBackend CacheKey VersionRead)
    -- ^ Selected reads may use an adapter-owned projection without loading a full value locally.
    , cpAssembled :: Maybe (RetentionBackend CacheKey ByteString)
    }

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
    Maybe (RetentionOperations CacheKey CacheEntry) ->
    Maybe (RetentionOperations CacheKey VersionRead) ->
    Maybe (RetentionOperations CacheKey ByteString) ->
    CacheProvider
cacheProvider storage full version assembled =
    CacheProvider
        { cpFull = (retentionBackend storage <$> full) <* guard (supportsFullRetention storage)
        , cpVersion = retentionBackend storage <$> version
        , cpAssembled = retentionBackend storage <$> assembled
        }

-- | Retain only selected versions and assembled responses in bounded local stores.
localCacheProvider :: CacheConfig -> IO CacheProvider
localCacheProvider config = do
    pool <- newLocalPool (cacheMaxEntries config) (cacheMaxBytes config)
    version <- newPooledRetention pool (cacheTtl config) (cacheVersionBudget config) weighVersion
    assembled <- newPooledRetention pool (cacheTtl config) (cacheAssembledBudget config) weighAssembled
    pure (cacheProvider LocalStorage Nothing (Just version) (Just assembled))

weighAssembled :: ByteString -> Int
weighAssembled bytes = BS.length bytes + 256
