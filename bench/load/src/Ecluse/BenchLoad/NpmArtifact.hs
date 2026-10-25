-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pinned artifact coordinates projected from complete npm captures for metadata-gate replay.
module Ecluse.BenchLoad.NpmArtifact (SelectedArtifact (..), selectedNpmArtifact) where

import Ecluse.BenchLoad.CapturedArtifact (SelectedArtifact (..), selectedArtifact)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Npm.Route (tarballPath)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)

-- | Refuse missing pins or unroutable filenames instead of constructing an unsupported metadata URL.
selectedNpmArtifact :: PackageName -> Text -> ByteString -> Either Text SelectedArtifact
selectedNpmArtifact name version bytes = do
    (info, _) <- first show (projectNpmManifest defaultLimits name bytes)
    selectedArtifact version ("captured version is absent: " <> version) route info
  where
    route = maybe (Left "captured artifact filename has no npm route") Right . tarballPath name
