-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Project native identity, integrity, yank and publication facts without inventing execution evidence.
module Ecluse.Core.Registry.Cargo.Projection (projectCargoRecord) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (Artifact (..), Availability (Available, Yanked), CodeExecSignal (CodeExecUnknown), HashAlg (SHA256), InvalidEntry, InvalidEntryKind (InvalidVersionManifest), PackageDetails (..), PackageName, mkHash, mkInvalidEntry)
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry.Cargo.Discovery (CargoDiscovery, bindCargoDownload, cargoArtifactCoordinate, coordinateFilename)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.Wire (validateCargoRecord)
import Ecluse.Core.Registry.ServedDocument (stringField)
import Ecluse.Core.Registry.WireSupport (Projection (..), checkNameAgreement, parsePublishTime)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Version (mkVersion)

-- | Bind the artifact to the exact published spelling and source position before admission.
projectCargoRecord :: PackageName -> EntryKey -> CargoDiscovery -> Value -> Either InvalidEntry PackageDetails
projectCargoRecord requested key discovery value = first invalid $ do
    validateCargoRecord value
    fields <- case value of Object objectFields -> Right objectFields; _ -> Left "invalid Cargo record"
    nameText <- maybeToRight "missing Cargo package name" (stringField "name" fields)
    name <- first (const "invalid Cargo package name") (projectName nameText)
    case checkNameAgreement requested name () of
        NameMismatch _ -> Left "Cargo index name differs from the requested package"
        Projected () -> Right ()
    versionText <- maybeToRight "missing Cargo version" (stringField "vers" fields)
    let version = mkVersion Cargo versionText
    checksumText <- maybeToRight "missing Cargo checksum" (stringField "cksum" fields)
    checksum <- mkHash SHA256 checksumText
    coordinate <- cargoArtifactCoordinate nameText version checksum
    url <- bindCargoDownload discovery coordinate
    let !artifact = Artifact{artEntryKey = key, artFilename = unFilename (coordinateFilename coordinate), artUrl = url, artHashes = [checksum], artCoreMetadata = Nothing, artSize = Nothing}
    pure
        PackageDetails
            { pkgName = name
            , pkgVersion = version
            , pkgPublishedAt = KeyMap.lookup "pubtime" fields >>= parseMaybe parsePublishTime
            , pkgInstallCode = CodeExecUnknown
            , pkgAvailability = if KeyMap.lookup "yanked" fields == Just (Bool True) then Yanked else Available
            , pkgArtifacts = artifact :| []
            }
  where
    invalid reason = mkInvalidEntry InvalidVersionManifest (show key) Null reason
