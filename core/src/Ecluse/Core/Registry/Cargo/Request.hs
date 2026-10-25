-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Sparse index paths and native request credentials over the shared sealed transport.
module Ecluse.Core.Registry.Cargo.Request (
    cargoArtifactHosts,
    cargoIndexPath,
    cargoPrefix,
    cargoIndexRequest,
    cargoConfigRequest,
    cargoArtifactRequest,
) where

import Data.Text qualified as T
import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (UrlFormationError (UnparseableUrl))
import Ecluse.Core.Registry.Cargo.Credential (cargoCredential)
import Ecluse.Core.Registry.Cargo.Project (cargoNameKey, projectName)
import Ecluse.Core.Registry.Request (BaseRequest, artifactRequestByUrl, metadataGet, requestAt)
import Ecluse.Core.Server.Path (encodeComponent)

-- | Public archive hosts are declared independently of discovery responses.
cargoArtifactHosts :: [Text]
cargoArtifactHosts = ["https://static.crates.io"]

-- | Prefix spelling follows Cargo's length rule and preserves its input's case.
cargoPrefix :: Text -> Text
cargoPrefix name = case T.length name of
    0 -> ""
    1 -> "1"
    2 -> "2"
    3 -> "3/" <> T.take 1 name
    _ -> T.take 2 name <> "/" <> T.take 2 (T.drop 2 name)

-- | Derive a sparse lookup path, folding ASCII case alone.
cargoIndexPath :: PackageName -> Text
cargoIndexPath name = cargoPrefix key <> "/" <> encodeComponent key
  where
    key = cargoNameKey (renderPackageName name)

-- | Fetch one native index file under the selected origin's credential posture.
cargoIndexRequest :: BaseRequest -> Maybe ClientCredential -> PackageName -> Either UrlFormationError Request
cargoIndexRequest base token name = do
    parsed <- first (const (UnparseableUrl "invalid Cargo index name")) (projectName (renderPackageName name))
    unless (parsed == name) (Left (UnparseableUrl "invalid Cargo index ecosystem"))
    metadataGet cargoCredential token "text/plain" <$> requestAt base (cargoIndexPath name)

-- | Fetch discovery only after the origin and its credentials exist.
cargoConfigRequest :: BaseRequest -> Maybe ClientCredential -> Either UrlFormationError Request
cargoConfigRequest base token = metadataGet cargoCredential token "application/json" <$> requestAt base "config.json"

-- | Fetch the metadata-bound archive URL without deriving identity from its terminal component.
cargoArtifactRequest :: Maybe ClientCredential -> Text -> Either UrlFormationError Request
cargoArtifactRequest = artifactRequestByUrl cargoCredential
