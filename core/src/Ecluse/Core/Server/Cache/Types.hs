-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Metadata retention budgets, source identities, full fetch values, and the keys entries are stored under.
module Ecluse.Core.Server.Cache.Types (
    -- * Configuration
    StoreBudget (..),
    CacheConfig (..),

    -- * Entries
    Source (..),
    CacheEntry (..),

    -- * Keys
    CacheKey,
    fullKey,
    versionKey,
    assembledKey,
    cacheKeyStore,
    cacheKeyIdentity,
) where

import Data.Text.Short qualified as TS
import Data.Time (NominalDiffTime)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Package (PackageInfo, PackageName, pkgCanonical, pkgEcosystem, pkgNamespace, unScope)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (ContentDigest)
import Ecluse.Core.Server.Conditional (ETag, renderETag)
import Ecluse.Core.Server.Framing (frameComponents)
import Ecluse.Core.Telemetry.Metrics (CacheStore (AssembledStore, FullStore, VersionStore))
import Ecluse.Core.Version (Version, renderVersion)

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

{- | One entry's address in one store, built only by 'fullKey', 'versionKey' and 'assembledKey'.
Two keys are equal when their stores and their identities are, so no two stores share a key.
-}
data CacheKey = CacheKey
    { ckStore :: CacheStore
    , ckIdentity :: ShortByteString
    }
    deriving stock (Eq, Ord, Show)

instance Hashable CacheKey where
    hashWithSalt salt key = salt `hashWithSalt` ckStore key `hashWithSalt` ckIdentity key

-- | The store a key addresses.
cacheKeyStore :: CacheKey -> CacheStore
cacheKeyStore = ckStore

-- | A key's identity within its store: its components in a fixed order, each framed by its length.
cacheKeyIdentity :: CacheKey -> ShortByteString
cacheKeyIdentity = ckIdentity

-- | The key of one package's full metadata from one source.
fullKey :: Source -> PackageName -> CacheKey
fullKey source name = storeKey FullStore (packageComponents source name)

-- | The key of one selected version: an entry of its own, apart from the package's full metadata.
versionKey :: Source -> PackageName -> Version -> CacheKey
versionKey source name version =
    storeKey VersionStore (packageComponents source name <> [Just (encodeUtf8 (renderVersion version))])

{- | The key of an assembled response. The validator is its only component: it binds the body to
the inputs of the request that built it, and it carries no credential and no caller identity.
-}
assembledKey :: ETag -> CacheKey
assembledKey etag = storeKey AssembledStore [Just (encodeUtf8 (renderETag etag))]

-- The fields 'PackageName' equality reads, under the source: the scope is absent for an unscoped name.
packageComponents :: Source -> PackageName -> [Maybe ByteString]
packageComponents (Source source) name =
    [ Just (encodeUtf8 source)
    , Just (encodeUtf8 (ecosystemName (pkgEcosystem name)))
    , encodeUtf8 . unScope <$> pkgNamespace name
    , Just (TS.toByteString (pkgCanonical name))
    ]

storeKey :: CacheStore -> [Maybe ByteString] -> CacheKey
storeKey store components = CacheKey store (toShort (frameComponents components))
