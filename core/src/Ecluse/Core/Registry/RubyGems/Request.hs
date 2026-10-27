-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | RubyGems requests use checked native coordinates and the selected origin's authority.
module Ecluse.Core.Registry.RubyGems.Request (
    gemInfoRequestUnder,
    gemInfoUrl,
    gemArtifactUrl,
    gemCompactSpecUrl,
    gemVersionJsonRequestUnder,
    gemVersionsJsonRequestUnder,
) where

import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (UrlFormationError)
import Ecluse.Core.Registry.Request (BaseRequest, joinPath, metadataGet, requestAt)
import Ecluse.Core.Registry.RubyGems.Credential (rubyReadCredential)
import Ecluse.Core.Registry.RubyGems.Project (GemCoordinate, coordinateFilename, coordinateName, coordinatePlatform, coordinateStem, coordinateVersion, unGemPlatform)
import Ecluse.Core.Server.Path (encodeComponent, unFilename)
import Ecluse.Core.Version (renderVersion)

-- | Fetch the named compact-index document under caller-selected credentials.
gemInfoRequestUnder :: BaseRequest -> Maybe ClientCredential -> PackageName -> Either UrlFormationError Request
gemInfoRequestUnder base credential name = metadataGet rubyReadCredential credential "text/plain" <$> requestAt base (infoPath name)

-- | Construct the named upstream path without acquiring a registry-wide catalogue.
gemInfoUrl :: Text -> PackageName -> Either UrlFormationError Text
gemInfoUrl base name = joinPath base (infoPath name)

infoPath :: PackageName -> Text
infoPath name = "info/" <> encodeComponent (renderPackageName name)

-- | Address an artifact by its exact checked native filename.
gemArtifactUrl :: Text -> GemCoordinate -> Either UrlFormationError Text
gemArtifactUrl base coordinate = joinPath base ("gems/" <> encodeComponent (unFilename (coordinateFilename coordinate)))

-- | Address the original zlib-compressed compact specification for one checked artifact.
gemCompactSpecUrl :: Text -> GemCoordinate -> Either UrlFormationError Text
gemCompactSpecUrl base coordinate = joinPath base ("quick/Marshal.4.8/" <> encodeComponent (coordinateStem coordinate) <> ".gemspec.rz")

-- | Select JSON facts for one version and platform without borrowing another artifact's identity.
gemVersionJsonRequestUnder :: BaseRequest -> Maybe ClientCredential -> GemCoordinate -> Either UrlFormationError Request
gemVersionJsonRequestUnder base credential coordinate = metadataGet rubyReadCredential credential "application/json" <$> requestAt base path
  where
    path = "api/v2/rubygems/" <> encodeComponent (renderPackageName (coordinateName coordinate)) <> "/versions/" <> encodeComponent (renderVersion (coordinateVersion coordinate)) <> ".json?platform=" <> encodeComponent (unGemPlatform (coordinatePlatform coordinate))

-- | Fetch bounded per-package version facts when an older native source omits a required field.
gemVersionsJsonRequestUnder :: BaseRequest -> Maybe ClientCredential -> PackageName -> Either UrlFormationError Request
gemVersionsJsonRequestUnder base credential name = metadataGet rubyReadCredential credential "application/json" <$> requestAt base ("api/v1/versions/" <> encodeComponent (renderPackageName name) <> ".json")
