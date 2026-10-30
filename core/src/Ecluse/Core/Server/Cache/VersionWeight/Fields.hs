-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Field charges shared by selected releases and durable mirror records. Aliases count per occurrence.
module Ecluse.Core.Server.Cache.VersionWeight.Fields (rawWeight, retainedRawWeight, detailsFieldsWeight, nameWeight, textWeight, itemsWeight, artifactWeight, weighEntryKey) where

import Data.Map.Strict qualified as Map
import Data.Text.Short qualified as TS
import Data.Text.Unsafe (lengthWord8)
import Data.Time (UTCTime (..), diffTimeToPicoseconds, toModifiedJulianDay)

import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (..))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, weighCachedDoc)
import Ecluse.Core.Server.MemoryModel (expandWireBytes, expandWireBytesInteger)
import Ecluse.Core.Text (textStorageBytes)
import Ecluse.Core.Version (renderVersion)

-- | Charge compact source size on the shared expansion without encoding.
rawWeight :: CachedDoc -> Integer
rawWeight = toInteger . expandWireBytes . fromIntegral . weighCachedDoc

-- | State retention uses Integer expansion. Selected entries keep their existing Int arithmetic.
retainedRawWeight :: CachedDoc -> Integer
retainedRawWeight = expandWireBytesInteger . toInteger . weighCachedDoc

-- | Preserve variable release charges independently of the selected-entry shell.
detailsFieldsWeight :: PackageDetails -> Integer
detailsFieldsWeight details =
    nameWeight (pkgName details)
        + textWeight rawVersion
        + 256 * toInteger rawLength
        + maybe 0 timeWeight (pkgPublishedAt details)
        + installWeight (pkgInstallCode details)
        + dependencyWeight (pkgDependencySources details)
        + itemsWeight artifactWeight (pkgArtifacts details)
  where
    -- Parsed tokens and bounded numeric components use this allowance, which also covers RubyGems hyphen expansion and copied parser text.
    rawVersion = renderVersion (pkgVersion details)
    rawLength = lengthWord8 rawVersion

-- | Charge all package spellings, including aliases.
nameWeight :: PackageName -> Integer
nameWeight name =
    sum (map (textWeight . TS.toText) [pkgCanonical name, pkgBaseName name])
        + textWeight (renderPackageName name)
        + maybe 0 (textWeight . renderScope) (pkgNamespace name)

-- Decimal digits overestimate Integer payload bytes without depending on its heap layout.
timeWeight :: UTCTime -> Integer
timeWeight (UTCTime day time) =
    textWeight (show (toModifiedJulianDay day)) + textWeight (show (diffTimeToPicoseconds time))

-- | Count the complete backing allocation of each text occurrence.
textWeight :: Text -> Integer
textWeight text = 128 + toInteger (textStorageBytes text)

-- | Include a list or map element allowance for each occurrence.
itemsWeight :: (Foldable f) => (a -> Integer) -> f a -> Integer
itemsWeight weigh = foldl' (\total value -> total + 256 + weigh value) 0

installWeight :: CodeExecSignal -> Integer
installWeight = \case
    NoCodeOnInstall -> 0
    RunsCodeOnInstall reason -> textWeight reason
    CodeExecUnknown -> 0

dependencyWeight :: DependencySources -> Integer
dependencyWeight = \case
    RegistryDependencies -> 0
    FetchesOutsideRegistry reason -> textWeight reason

-- | Charge coordinates, source strings, digests and companion declarations.
artifactWeight :: Artifact -> Integer
artifactWeight artifact =
    weighEntryKey (artEntryKey artifact)
        + textWeight (artFilename artifact)
        + textWeight (artUrl artifact)
        + itemsWeight (textWeight . hashValue) (artHashes artifact)
        + maybe 0 metadataWeight (artCoreMetadata artifact)

metadataWeight :: CoreMetadata -> Integer
metadataWeight (MetadataAvailable _) = 64
metadataWeight (MetadataDigests digests) = 64 + itemsWeight (\(name, value) -> textWeight name + textWeight value) (Map.toList digests)

-- | Charge an entry coordinate, including any retained text backing allocation.
weighEntryKey :: EntryKey -> Integer
weighEntryKey = \case
    ArrayEntry _ -> 64
    RecordEntry _ -> 64
    ObjectEntry key -> 64 + textWeight key
    SingletonEntry -> 16
