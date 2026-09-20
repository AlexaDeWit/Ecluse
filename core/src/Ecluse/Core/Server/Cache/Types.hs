-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Metadata retention budgets, source identities, and full fetch values.
module Ecluse.Core.Server.Cache.Types (StoreBudget (..), CacheConfig (..), Source (..), CacheEntry (..)) where

import Data.Time (NominalDiffTime)
import Ecluse.Core.Package (PackageInfo)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (ContentDigest)

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
