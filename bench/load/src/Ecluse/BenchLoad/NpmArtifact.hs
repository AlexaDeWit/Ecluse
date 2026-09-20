-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pinned artifact coordinates projected from complete npm captures for metadata-gate replay.
module Ecluse.BenchLoad.NpmArtifact (SelectedArtifact (..), selectedNpmArtifact) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (Artifact (artFilename, artUrl), PackageDetails (pkgArtifacts), PackageInfo (infoVersions), PackageName)

import Ecluse.Core.Registry.Npm.Route (tarballPath)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)

-- | The proxy route and public location describe the same captured artifact.
data SelectedArtifact = SelectedArtifact
    { saProxyPath :: Text
    , saUpstreamUrl :: Text
    }
    deriving stock (Eq, Show)

-- | Refuse missing pins or unroutable filenames instead of constructing an unsupported metadata URL.
selectedNpmArtifact :: PackageName -> Text -> ByteString -> Either Text SelectedArtifact
selectedNpmArtifact name version bytes = do
    (info, _) <- first show (projectNpmManifest defaultLimits name bytes)
    details <- maybe (Left ("captured version is absent: " <> version)) Right (Map.lookup version (infoVersions info))
    let artifact :| _ = pkgArtifacts details
    path <- maybe (Left "captured artifact filename has no npm route") Right (tarballPath name (artFilename artifact))
    pure (SelectedArtifact path (artUrl artifact))
