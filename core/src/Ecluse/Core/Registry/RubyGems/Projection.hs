-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Whole-version RubyGems policy facts retain every supported platform artifact.
module Ecluse.Core.Registry.RubyGems.Projection (projectGemRows) where

import Control.Monad (foldM)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Ecluse.Core.Package (Artifact (..), Availability (Available), CodeExecSignal (CodeExecUnknown), PackageDetails (..), PackageInfo (..), PackageName, renderPackageName)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable))
import Ecluse.Core.Registry.RubyGems.Document (GemRow (..))
import Ecluse.Core.Registry.RubyGems.Project (coordinateFilename, coordinateName, coordinateVersion)
import Ecluse.Core.Registry.RubyGems.Request (gemArtifactUrl)
import Ecluse.Core.Registry.WireSupport (newestUpload)
import Ecluse.Core.Security (Limits, checkArtifactCount, checkVersionCountOf)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Version (renderVersion)

-- | Group exact raw versions and preserve unknown age whenever any platform time is missing.
projectGemRows :: Limits -> PackageName -> Text -> [GemRow] -> Either MetadataError PackageInfo
projectGemRows limits name base rows = do
    first MetadataBoundExceeded (checkArtifactCount limits (length rows))
    unless (Set.size (Set.fromList (map (unFilename . coordinateFilename . grCoordinate) rows)) == length rows && Set.size (Set.fromList (map grEntryKey rows)) == length rows) (Left MetadataUndecodable)
    versions <- foldM insert Map.empty rows
    first MetadataBoundExceeded (checkVersionCountOf limits (Map.size versions))
    let !settled = Map.map settle versions
    pure (PackageInfo name settled Map.empty [])
  where
    insert versions row = do
        let coordinate = grCoordinate row
            version = coordinateVersion coordinate
        unless (coordinateName coordinate == name) (Left (MetadataNameMismatch (renderPackageName (coordinateName coordinate))))
        url <- first (const MetadataUndecodable) (gemArtifactUrl base coordinate)
        let !artifact = Artifact{artEntryKey = grEntryKey row, artFilename = unFilename (coordinateFilename coordinate), artUrl = url, artHashes = strictElements [grChecksum row], artCoreMetadata = Nothing, artSize = Nothing}
            fresh = PackageDetails{pkgName = name, pkgVersion = version, pkgPublishedAt = grPublishedAt row, pkgInstallCode = CodeExecUnknown, pkgAvailability = Available, pkgArtifacts = artifact :| []}
            merge previous = previous{pkgPublishedAt = newestUpload (grPublishedAt row) (pkgPublishedAt previous), pkgArtifacts = artifact :| toList (pkgArtifacts previous)}
        pure (Map.alter (Just . maybe fresh merge) (renderVersion version) versions)

    settle details =
        let !artifacts = strictElements (NE.reverse (pkgArtifacts details))
            !published = force (pkgPublishedAt details)
         in details{pkgArtifacts = artifacts, pkgPublishedAt = published}
