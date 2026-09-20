-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Metadata retention budgets, source identities, and full fetch values.
module Ecluse.Core.Server.Cache.Types (StoreBudget (..), CacheConfig (..), Source (..), CacheEntry (..)) where

import Data.Time (NominalDiffTime)
import Ecluse.Core.Package (PackageInfo)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (ContentDigest)

-- | Retention floors, used only to stop a store evicting its own live entries.
data StoreBudget = StoreBudget
    { sbMinEntries :: Int
    -- ^ Eviction stops before the retained entry count falls below this floor.
    , sbMinBytes :: Int
    -- ^ Eviction stops before accounted bytes fall below this floor.
    }
    deriving stock (Eq, Show)

-- | Retention bounds and the TTL for the local selected-version and assembled stores.
data CacheConfig = CacheConfig
    { cacheTtl :: NominalDiffTime
    , cacheMaxEntries :: Int
    -- ^ One entry bound shared by all eligible local stores.
    , cacheMaxBytes :: Int
    -- ^ One accounted-byte bound shared by all eligible local stores.
    , cacheVersionBudget :: StoreBudget
    -- ^ The selected-version eviction floor. It reserves no capacity.
    , cacheAssembledBudget :: StoreBudget
    -- ^ The assembled-response eviction floor. It reserves no capacity.
    }
    deriving stock (Eq, Show)

-- | An upstream base URL partitions entries without carrying credentials.
newtype Source = Source Text
    deriving stock (Eq, Ord, Show)

-- | A typed view paired with its source representation and complete fetch digest.
data CacheEntry = CacheEntry
    { entryInfo :: PackageInfo
    -- ^ The typed packument view the rules and merge reason over.
    , entryRaw :: CachedDoc
    -- ^ The source representation from which the adapter builds the served body.
    , entryBodyBytes :: Int
    -- ^ Decompressed source bytes, retained independently of the cache weight.
    , entryDigest :: ContentDigest
    }
    deriving stock (Eq, Show)
