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
import Ecluse.Core.Server.Cache.Backend.Local (newLocalRetention)
import Ecluse.Core.Server.Cache.Types
import Ecluse.Core.Server.Cache.VersionWeight (weighVersion)

-- | Missing capabilities remain uncached. They never fall back to another storage provider.
data CacheProvider = CacheProvider
    { providerFull :: Maybe (RetentionBackend Text CacheEntry)
    -- ^ Full metadata is absent for local storage, regardless of the supplied operations.
    , providerVersion :: Maybe (RetentionBackend Text VersionRead)
    -- ^ Selected reads may use an adapter-owned projection without loading a full value locally.
    , providerAssembled :: Maybe (RetentionBackend Text ByteString)
    }

-- | Classify every operation together. Local storage cannot opt into full retention.
cacheProvider :: BackendStorage -> Maybe (RetentionOperations Text CacheEntry) -> Maybe (RetentionOperations Text VersionRead) -> Maybe (RetentionOperations Text ByteString) -> CacheProvider
cacheProvider storage full version assembled =
    CacheProvider
        { providerFull = (retentionBackend storage <$> full) <* guard (supportsFullRetention storage)
        , providerVersion = retentionBackend storage <$> version
        , providerAssembled = retentionBackend storage <$> assembled
        }

-- | Retain only selected versions and assembled responses in bounded local stores.
localCacheProvider :: CacheConfig -> IO CacheProvider
localCacheProvider config = do
    version <- newStore (cacheVersionBudget config) weighVersion
    assembled <- newStore (cacheAssembledBudget config) weighAssembled
    pure (cacheProvider LocalStorage Nothing (Just version) (Just assembled))
  where
    newStore :: (Hashable k) => StoreBudget -> (value -> Int) -> IO (RetentionOperations k value)
    newStore budget = newLocalRetention (cacheTtl config) (sbMaxEntries budget) (sbMaxBytes budget)

weighAssembled :: ByteString -> Int
weighAssembled bytes = BS.length bytes + 256
