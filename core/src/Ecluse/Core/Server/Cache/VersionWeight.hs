-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE MagicHash #-}

{- | Conservative accounting for selectively decoded releases.
The cache charges backing allocations and repeated structures without deduplicating sharing.
A retained raw version object is charged on the shared wire-to-resident model.
-}
module Ecluse.Core.Server.Cache.VersionWeight (weighVersion, weighEntryKey) where

import Data.Array.Byte (ByteArray (..))
import Data.Text.Internal qualified as Text
import Data.Text.Short qualified as TS
import Data.Time (UTCTime (..), diffTimeToPicoseconds, toModifiedJulianDay)
import GHC.Exts (Int (I#), sizeofByteArray#)

import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (..))
import Ecluse.Core.Registry.CachedDocument (weighCachedDoc)
import Ecluse.Core.Registry.Metadata (VersionDoc (vdDetails, vdRaw), VersionRead (vrUpstreamLatest, vrVersion))
import Ecluse.Core.Server.MemoryModel (expandWireBytes)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Core.Version (renderVersion)

-- | Estimate retained release bytes. 'maxBound' marks an uncacheable saturated estimate.
weighVersion :: VersionRead -> Int
weighVersion versionRead =
    fromInteger (min (toInteger (maxBound :: Int)) (versionPart + latestPart))
  where
    versionPart = maybe 1024 pairWeight (vrVersion versionRead)
    latestPart = maybe 0 (textWeight . renderVersion) (vrUpstreamLatest versionRead)

-- The digest is 32 bytes plus its wrapper. The raw object is weighed as the full store weighs it.
pairWeight :: Snapshot VersionDoc -> Integer
pairWeight (Snapshot _ doc) =
    64 + detailsWeight (vdDetails doc) + maybe 0 (toInteger . expandWireBytes . fromIntegral . weighCachedDoc) (vdRaw doc)

detailsWeight :: PackageDetails -> Integer
detailsWeight details =
    -- The base covers the entry, package record, scalar tags, and wrappers.
    -- Artifact node allowances include size and yank flags as well as record fields.
    16 * 1024
        + nameWeight (pkgName details)
        + textWeight rawVersion
        + 256 * toInteger rawLength
        + maybe 0 timeWeight (pkgPublishedAt details)
        + installWeight (pkgInstallCode details)
        + trustWeight (pkgTrust details)
        + availabilityWeight (pkgAvailability details)
        + itemsWeight artifactWeight (pkgArtifacts details)
        + itemsWeight textWeight (pkgLicenses details)
        + maybe 0 personWeight (pkgPublisher details)
  where
    -- The opaque parsed version has flat token lists and bounded numeric components.
    -- The per-byte allowance also covers RubyGems hyphen expansion and copied parser text.
    rawVersion = renderVersion (pkgVersion details)
    Text.Text _ _ rawLength = rawVersion

nameWeight :: PackageName -> Integer
nameWeight name =
    sum (map (textWeight . TS.toText) [pkgCanonical name, pkgBaseName name])
        + textWeight (renderPackageName name)
        + maybe 0 (textWeight . renderScope) (pkgNamespace name)

-- Decimal digits overestimate Integer payload bytes without depending on its heap layout.
timeWeight :: UTCTime -> Integer
timeWeight (UTCTime day time) =
    textWeight (show (toModifiedJulianDay day)) + textWeight (show (diffTimeToPicoseconds time))

-- Text slices can retain an entire input allocation. Count that allocation each time.
textWeight :: Text -> Integer
textWeight (Text.Text (ByteArray array) _ _) = 128 + toInteger (I# (sizeofByteArray# array))

-- The allowance covers a list cell, its element record, wrappers, and alignment.
itemsWeight :: (Foldable f) => (a -> Integer) -> f a -> Integer
itemsWeight weigh = foldl' (\total value -> total + 256 + weigh value) 0

installWeight :: CodeExecSignal -> Integer
installWeight = \case
    NoCodeOnInstall -> 0
    RunsCodeOnInstall reason -> textWeight reason
    CodeExecUnknown -> 0

trustWeight :: Trust -> Integer
trustWeight = \case
    Trusted evidence -> itemsWeight evidenceWeight evidence
    Untrusted -> 0
    TrustUnknown -> 0

evidenceWeight :: TrustEvidence -> Integer
evidenceWeight = \case
    Signed -> 0
    Attested -> 0
    MfaPublished -> 0
    OtherEvidence reason -> textWeight reason

availabilityWeight :: Availability -> Integer
availabilityWeight = \case
    Available -> 0
    Deprecated reason -> textWeight reason
    Yanked reason -> maybe 0 textWeight reason

artifactWeight :: Artifact -> Integer
artifactWeight artifact =
    weighEntryKey (artEntryKey artifact)
        + textWeight (artFilename artifact)
        + textWeight (artUrl artifact)
        + kindWeight (artKind artifact)
        + itemsWeight (textWeight . hashValue) (artHashes artifact)
        + maybe 0 textWeight (artInterpreter artifact)
        + maybe 0 textWeight (artProvenance artifact)

-- | Charge an entry coordinate, including any retained text backing allocation.
weighEntryKey :: EntryKey -> Integer
weighEntryKey = \case
    ArrayEntry _ -> 64
    ObjectEntry key -> 64 + textWeight key
    SingletonEntry -> 16

kindWeight :: ArtifactKind -> Integer
kindWeight = \case
    Tarball -> 0
    Sdist -> 0
    Wheel tag -> textWeight tag
    Gem platform -> textWeight platform

personWeight :: Person -> Integer
personWeight person =
    256
        + textWeight (personName person)
        + maybe 0 textWeight (personEmail person)
        + maybe 0 textWeight (personUrl person)
