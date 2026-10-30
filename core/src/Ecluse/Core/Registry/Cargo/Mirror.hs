-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Managed index files restore each retained row unchanged.
module Ecluse.Core.Registry.Cargo.Mirror (
    cargoMirrorDocuments,
) where

import Ecluse.Core.Mirror.Documents (MirrorDocuments (..))
import Ecluse.Core.Mirror.State (MirrorVersion, fragmentDetails, fragmentReadData, versionFragments)
import Ecluse.Core.Package (Artifact (..), PackageDetails (pkgArtifacts))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry (CompanionRequirement (DeclaredCompanions))
import Ecluse.Core.Registry.CachedDocument (cargoCached, cargoPacked)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument, cargoRecords, packCargoDocument)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (CargoMirrorRead))

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
        , mdCompanions = DeclaredCompanions
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
