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
    renderCacheKey,
    cacheKeyIdentity,
) where

import Crypto.Hash (SHA256 (SHA256), hashWith)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString qualified as BS
import Data.Text.Short qualified as TS
import Data.Time (NominalDiffTime)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Package (PackageInfo, PackageName, pkgCanonical, pkgEcosystem, pkgNamespace, unScope)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (ContentDigest)
import Ecluse.Core.Server.Conditional (ETag, renderETag)
import Ecluse.Core.Server.Framing (frameComponents)
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
Equality confirms the identity behind an equal rendering, so a digest collision reads as a miss.
-}
data CacheKey = CacheKey
    { ckRendered :: ShortByteString
    , ckIdentity :: ShortByteString
    }
    deriving stock (Show)

instance Eq CacheKey where
    a == b = renderCacheKey a == renderCacheKey b && cacheKeyIdentity a == cacheKeyIdentity b

instance Ord CacheKey where
    compare = comparing renderCacheKey <> comparing cacheKeyIdentity

instance Hashable CacheKey where
    hashWithSalt salt = hashWithSalt salt . renderCacheKey

{- | The one rendering, the key a store holds: @ecluse@, the envelope version, the store, its codec
version, and the hex SHA-256 of the identity, joined by colons.
-}
renderCacheKey :: CacheKey -> ShortByteString
renderCacheKey = ckRendered

-- | The length-framed components the digest covers: the entry's identity in the clear.
cacheKeyIdentity :: CacheKey -> ShortByteString
cacheKeyIdentity = ckIdentity

-- | The key of one package's full metadata from one source.
fullKey :: Source -> PackageName -> CacheKey
fullKey source name = storeKey "full" (packageComponents source name)

-- | The key of one selected version: an entry of its own, apart from the package's full metadata.
versionKey :: Source -> PackageName -> Version -> CacheKey
versionKey source name version =
    storeKey "version" (packageComponents source name <> [Just (encodeUtf8 (renderVersion version))])

{- | The key of an assembled response. The validator is its only component: it binds the body to
the inputs of the request that built it, and it carries no credential and no caller identity.
-}
assembledKey :: ETag -> CacheKey
assembledKey etag = storeKey "assembled" [Just (encodeUtf8 (renderETag etag))]

-- The fields 'PackageName' equality reads, under the source: the scope is absent for an unscoped name.
packageComponents :: Source -> PackageName -> [Maybe ByteString]
packageComponents (Source source) name =
    [ Just (encodeUtf8 source)
    , Just (encodeUtf8 (ecosystemName (pkgEcosystem name)))
    , encodeUtf8 . unScope <$> pkgNamespace name
    , Just (TS.toByteString (pkgCanonical name))
    ]

storeKey :: ByteString -> [Maybe ByteString] -> CacheKey
storeKey store components =
    CacheKey
        { ckRendered = toShort (BS.intercalate ":" ["ecluse", envelopeVersion, store, codecVersion, digest])
        , ckIdentity = toShort framed
        }
  where
    framed = frameComponents components
    digest = convertToBase Base16 (hashWith SHA256 framed)

-- The format versions a key's namespace names. A change to either leaves every older entry unreachable.
envelopeVersion, codecVersion :: ByteString
envelopeVersion = "0"
codecVersion = "0"
