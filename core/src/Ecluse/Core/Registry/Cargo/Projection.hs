-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Project native identity, integrity, yank, publication and dependency facts without inventing execution evidence.
module Ecluse.Core.Registry.Cargo.Projection (projectCargoRecord) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Vector qualified as V

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (Artifact (..), CodeExecSignal (CodeExecUnknown), DependencySources (FetchesOutsideRegistry, RegistryDependencies), HashAlg (SHA256), InvalidEntry, InvalidEntryKind (InvalidVersionManifest), PackageDetails (..), PackageName, mkHash, mkInvalidEntry)
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.Cargo.Discovery (CargoDiscovery, bindCargoDownload, cargoArtifactCoordinate, coordinateFilename)
import Ecluse.Core.Registry.Cargo.Manifest (acceptedRegistries, namesOtherRegistry)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.SourceState (CargoRecordFacts (..), cargoRecordFacts)
import Ecluse.Core.Registry.ServedDocument (stringField)
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
            , pkgDependencySources = dependencySources value
            , pkgAvailability = crfAvailability facts
            , pkgArtifacts = artifact :| []
            }
  where
    invalid = mkInvalidEntry InvalidVersionManifest (show key) Null

{- | The first normal or build dependency whose index registry is neither the current index nor crates.io.
The reader knows no address for Écluse, so a row naming Écluse's index by URL counts too.
-}
dependencySources :: Value -> DependencySources
dependencySources = \case
    Object fields
        | Just (Array dependencies) <- KeyMap.lookup "deps" fields
        , Just (Object dependency) <- V.find fromOtherRegistry dependencies ->
            FetchesOutsideRegistry (renderOutside dependency)
    _ -> RegistryDependencies
  where
    -- Most rows name no registry, so only a named one pays for parsing. An unparseable one fails closed.
    fromOtherRegistry = \case
        Object dependency -> case KeyMap.lookup "registry" dependency of
            Nothing -> False
            Just Null -> False
            Just _ -> fromRight True (cratesIo >>= (`namesOtherRegistry` dependency))
        _ -> False

cratesIo :: Either ParseError (Set Text)
cratesIo = acceptedRegistries []
{-# NOINLINE cratesIo #-}

-- Name the crate and its manifest table, never the registry, which can carry a credential.
renderOutside :: KeyMap.KeyMap Value -> Text
renderOutside dependency = "dependency '" <> crate <> "' in " <> table <> " names a registry other than crates.io"
  where
    crate = fromMaybe "" (stringField "package" dependency <|> stringField "name" dependency)
    table = if KeyMap.lookup "kind" dependency == Just (String "build") then "build-dependencies" else "dependencies"
