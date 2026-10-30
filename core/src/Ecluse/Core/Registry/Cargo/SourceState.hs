-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native facts of one index row, shared by projection and by retained mirror sources.
module Ecluse.Core.Registry.Cargo.SourceState (
    CargoRecordFacts (..),
    cargoRecordFacts,
    validateCargoSource,
    restrictCargoDocument,
    cargoSourceCompanions,
) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Set qualified as Set
import Data.Time (UTCTime)

import Ecluse.Core.Package (Artifact (..), Availability (Available, Yanked), HashAlg (SHA256), PackageDetails (..), mkHash)
import Ecluse.Core.Package.Entry (EntryKey, validRecordKey)
import Ecluse.Core.Registry (CompanionRequirement (DeclaredCompanions))
import Ecluse.Core.Registry.Cargo.Discovery (cargoArtifactCoordinate, coordinateFilename)
import Ecluse.Core.Registry.Cargo.Document (CargoDocument, cargoDocument, cargoRecords)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.Wire (validateCargoRecord)
import Ecluse.Core.Registry.ServedDocument (stringField)
import Ecluse.Core.Registry.WireSupport (parsePublishTime)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Version (renderVersion)

-- | A schema-checked row's facts before any download binding. Text fields keep the published spelling.
data CargoRecordFacts = CargoRecordFacts
    { crfName :: Text
    , crfVersion :: Text
    , crfChecksum :: Text
    , crfPublishedAt :: Maybe UTCTime
    -- ^ Absent or unparseable publication times stay unknown.
    , crfAvailability :: Availability
    }
    deriving stock (Eq, Show)

-- | Check the supported schema first, so the required fields below are present strings.
cargoRecordFacts :: Value -> Either Text CargoRecordFacts
cargoRecordFacts value = do
    validateCargoRecord value
    fields <- case value of
        Object objectFields -> Right objectFields
        _ -> Left "invalid Cargo record"
    name <- maybeToRight "missing Cargo package name" (stringField "name" fields)
    version <- maybeToRight "missing Cargo version" (stringField "vers" fields)
    checksum <- maybeToRight "missing Cargo checksum" (stringField "cksum" fields)
    pure
        CargoRecordFacts
            { crfName = name
            , crfVersion = version
            , crfChecksum = checksum
            , crfPublishedAt = KeyMap.lookup "pubtime" fields >>= parseMaybe parsePublishTime
            , crfAvailability = if KeyMap.lookup "yanked" fields == Just (Bool True) then Yanked else Available
            }

-- | One retained row must reproduce its admitted archive identity and policy facts, or restoration refuses it.
validateCargoSource :: PackageDetails -> CargoDocument -> Either Text ()
validateCargoSource details document = case (cargoRecords document, toList (pkgArtifacts details)) of
    ([(key, raw@(Object _))], [artifact]) -> do
        facts <- cargoRecordFacts raw
        unless (validRecordKey key && key == artEntryKey artifact) (Left "Cargo source coordinate disagrees with its artifact")
        unless (projectName (crfName facts) == Right (pkgName details) && crfVersion facts == renderVersion (pkgVersion details)) (Left "Cargo source identity disagrees with the admitted version")
        hash <- mkHash SHA256 (crfChecksum facts)
        coordinate <- cargoArtifactCoordinate (crfName facts) (pkgVersion details) hash
        unless (unFilename (coordinateFilename coordinate) == artFilename artifact && artHashes artifact == [hash]) (Left "Cargo source archive identity disagrees with its artifact")
        unless (crfPublishedAt facts == pkgPublishedAt details && crfAvailability facts == pkgAvailability details && isNothing (artCoreMetadata artifact) && isNothing (artSize artifact)) (Left "Cargo source policy facts disagree with the admitted version")
    _ -> Left "Cargo requires exactly one paired source row and archive"

-- | Keep the admitted rows in native order without renumbering their physical coordinates.
restrictCargoDocument :: Set EntryKey -> CargoDocument -> CargoDocument
restrictCargoDocument admitted = cargoDocument . filter ((`Set.member` admitted) . fst) . cargoRecords

-- | A retained crate needs a companion only where its artifact declares one, and index rows declare none.
cargoSourceCompanions :: CompanionRequirement
cargoSourceCompanions = DeclaredCompanions
