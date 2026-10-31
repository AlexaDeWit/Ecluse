-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Managed index files restore each retained row unchanged.
module Ecluse.Core.Registry.Cargo.Mirror (
    cargoMirrorDocuments,
    cargoStateFormat,
) where

import Data.Aeson (object, (.:), (.=))

import Ecluse.Core.Mirror.Documents (MirrorDocuments (..))
import Ecluse.Core.Mirror.State (MirrorVersion, fragmentDetails, fragmentReadData, versionFragments)
import Ecluse.Core.Mirror.State.Codec (forceSourceEntries, parseSourceEntries, sourceEntries, sourceEntriesCharge)
import Ecluse.Core.Mirror.State.Format (MirrorStateFormat (..), noProjectEnvelope)
import Ecluse.Core.Mirror.State.Weight (fragmentShellAllowance, sourceEntriesWeight)
import Ecluse.Core.Package (Artifact (..), PackageDetails (pkgArtifacts))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (cargoCached, cargoPacked)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument, cargoRecords, packCargoDocument)
import Ecluse.Core.Registry.Cargo.Manifest (cratesIoIndex)
import Ecluse.Core.Registry.Cargo.SourceState (cargoSourceCompanions)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (CargoMirrorRead))
import Ecluse.Core.Server.Cache.VersionWeight.Fields (retainedRawWeight)

-- | Versions follow their original index positions, so a restored row keeps its original record coordinate until rebasing.
cargoMirrorDocuments :: MirrorDocuments
cargoMirrorDocuments =
    MirrorDocuments
        { mdRestore = restoreCargoVersion
        , mdOrder = sortOn ((\(artifact :| _) -> artEntryKey artifact) . pkgArtifacts . vdDetails . snd)
        , mdEntryKey = RecordEntry
        , mdRekey = \rekey raw -> do
            native <- snd cargoCached raw
            pure (fst cargoCached (cargoDocument [(rekey key, value) | (key, value) <- cargoRecords native]))
        , mdAssemble = \_ docs -> fst cargoPacked (packCargoDocument (cargoDocument (concat [cargoRecords document | doc <- docs, Just raw <- [vdRaw doc], Just document <- [snd cargoCached raw]])))
        }

-- | Each fragment keeps its original crates.io rows under their record coordinates. Their source identity grants no target authority.
cargoStateFormat :: MirrorStateFormat
cargoStateFormat =
    MirrorStateFormat
        { msfKind = "cargo"
        , msfEncodeSource = \case
            CargoMirrorRead document -> Just (object ["kind" .= ("cargo" :: Text), "sourceIndex" .= cratesIoIndex, "entries" .= sourceEntries (cargoRecords document)])
            _ -> Nothing
        , msfDecodeSource = \value -> do
            sourceIndex <- value .: "sourceIndex"
            unless (sourceIndex == cratesIoIndex) (fail "Cargo mirror source is not crates.io")
            CargoMirrorRead . cargoDocument <$> parseSourceEntries "Cargo entry" value
        , msfDecodeCharge = sourceEntriesCharge
        , msfForceSource = \case
            CargoMirrorRead document -> forceSourceEntries (cargoRecords document)
            _ -> ()
        , msfEnvelope = noProjectEnvelope "Cargo"
        , msfFragmentShell = const fragmentShellAllowance
        , msfSourceWeight = \case
            CargoMirrorRead document -> retainedRawWeight (fst cargoCached document) + sourceEntriesWeight (cargoRecords document)
            _ -> 0
        , msfCompanions = cargoSourceCompanions
        }

restoreCargoVersion :: MirrorVersion -> [Artifact] -> Maybe VersionDoc
restoreCargoVersion mirror current = do
    fragment :| [] <- pure (versionFragments mirror)
    CargoMirrorRead source <- pure (fragmentReadData fragment)
    artifact :| [] <- pure (pkgArtifacts (fragmentDetails fragment))
    [(key, _)] <- pure (cargoRecords source)
    native <- find ((== artFilename artifact) . artFilename) current
    guard (key == artEntryKey artifact)
    pure (VersionDoc (fragmentDetails fragment){pkgArtifacts = artifact{artUrl = artUrl native} :| []} (Just (fst cargoCached source)))
