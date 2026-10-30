-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Project native identity, integrity, yank and publication facts without inventing execution evidence.
module Ecluse.Core.Registry.Cargo.Projection (projectCargoRecord) where

import Data.Aeson (Value (..))

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (Artifact (..), CodeExecSignal (CodeExecUnknown), DependencySources (DependencySourcesUnknown), HashAlg (SHA256), InvalidEntry, InvalidEntryKind (InvalidVersionManifest), PackageDetails (..), PackageName, mkHash, mkInvalidEntry)
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry.Cargo.Discovery (CargoDiscovery, bindCargoDownload, cargoArtifactCoordinate, coordinateFilename)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.SourceState (CargoRecordFacts (..), cargoRecordFacts)
import Ecluse.Core.Registry.WireSupport (Projection (..), checkNameAgreement)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Version (mkVersion)

-- | Bind the artifact to the exact published spelling and source position before admission.
projectCargoRecord :: PackageName -> EntryKey -> CargoDiscovery -> Value -> Either InvalidEntry PackageDetails
projectCargoRecord requested key discovery value = first invalid $ do
    facts <- cargoRecordFacts value
    name <- first (const "invalid Cargo package name") (projectName (crfName facts))
    case checkNameAgreement requested name () of
        NameMismatch _ -> Left "Cargo index name differs from the requested package"
        Projected () -> Right ()
    let version = mkVersion Cargo (crfVersion facts)
    checksum <- mkHash SHA256 (crfChecksum facts)
    coordinate <- cargoArtifactCoordinate (crfName facts) version checksum
    url <- bindCargoDownload discovery coordinate
    let !artifact = Artifact{artEntryKey = key, artFilename = unFilename (coordinateFilename coordinate), artUrl = url, artHashes = [checksum], artCoreMetadata = Nothing, artSize = Nothing}
    pure
        PackageDetails
            { pkgName = name
            , pkgVersion = version
            , pkgPublishedAt = crfPublishedAt facts
            , pkgInstallCode = CodeExecUnknown
            , pkgDependencySources = DependencySourcesUnknown
            , pkgAvailability = crfAvailability facts
            , pkgArtifacts = artifact :| []
            }
  where
    invalid = mkInvalidEntry InvalidVersionManifest (show key) Null
