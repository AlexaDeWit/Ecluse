-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Artifact coordinates selected from a projected capture.
module Ecluse.BenchLoad.CapturedArtifact (SelectedArtifact (..), selectedArtifact) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (Artifact (artFilename, artUrl), PackageDetails (pkgArtifacts), PackageInfo (infoVersions))

-- | The proxy route and public location describe the same captured artifact.
data SelectedArtifact = SelectedArtifact
    { saProxyPath :: Text
    , saUpstreamUrl :: Text
    }
    deriving stock (Eq, Show)

-- | Select the first artifact of a canonical release, using native routing and errors.
selectedArtifact :: Text -> Text -> (Text -> Either Text Text) -> PackageInfo -> Either Text SelectedArtifact
selectedArtifact version missingRelease route info = do
    details <- maybe (Left missingRelease) Right (Map.lookup version (infoVersions info))
    let artifact :| _ = pkgArtifacts details
    path <- route (artFilename artifact)
    pure (SelectedArtifact path (artUrl artifact))
