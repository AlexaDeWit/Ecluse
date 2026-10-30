-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | RubyGems requests use checked native coordinates and the selected origin's authority.
module Ecluse.Core.Registry.RubyGems.Request (
    gemInfoRequestUnder,
    gemDirectoryRequestUnder,
    gemInfoUrl,
    gemArtifactUrl,
    gemCompactSpecUrl,
    gemPath,
    compactSpecPath,
) where

import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (UrlFormationError (UnparseableUrl))
import Ecluse.Core.Registry.Request (BaseRequest, joinPath, metadataGet, requestAt)
import Ecluse.Core.Registry.RubyGems.Credential (rubyReadCredential)
import Ecluse.Core.Registry.RubyGems.Project (GemCoordinate, coordinateFilename, coordinateStem, projectName, unscopedRubyGems)
import Ecluse.Core.Server.Path (encodeComponent, unFilename)

-- | Fetch the named compact-index document under caller-selected credentials.
gemInfoRequestUnder :: BaseRequest -> Maybe ClientCredential -> PackageName -> Either UrlFormationError Request
gemInfoRequestUnder base credential name = do
    path <- infoPath name
    metadataGet rubyReadCredential credential "text/plain" <$> requestAt base path

-- | Construct the named upstream path without acquiring a registry-wide catalogue.
gemInfoUrl :: Text -> PackageName -> Either UrlFormationError Text
gemInfoUrl base name = infoPath name >>= joinPath base

infoPath :: PackageName -> Either UrlFormationError Text
infoPath name = ("info/" <>) . encodeComponent <$> nativeName name

nativeName :: PackageName -> Either UrlFormationError Text
nativeName name = do
    unless (unscopedRubyGems name) (Left (UnparseableUrl "not an unscoped RubyGems package"))
    checked <- first (const (UnparseableUrl "invalid RubyGems package name")) (projectName (renderPackageName name))
    pure (renderPackageName checked)

-- | Address an artifact by its exact checked native filename.
gemArtifactUrl :: Text -> GemCoordinate -> Either UrlFormationError Text
gemArtifactUrl base coordinate = joinPath base (gemPath (unFilename (coordinateFilename coordinate)))

-- | The path of an archive by its native filename, under the registry root.
gemPath :: Text -> Text
gemPath filename = "gems/" <> encodeComponent filename

-- | Address the original zlib-compressed compact specification for one checked artifact.
gemCompactSpecUrl :: Text -> GemCoordinate -> Either UrlFormationError Text
gemCompactSpecUrl base coordinate = joinPath base (compactSpecPath (coordinateStem coordinate))

-- | The path of a compact specification by its archive's filename stem, under the registry root.
compactSpecPath :: Text -> Text
compactSpecPath stem = "quick/Marshal.4.8/" <> encodeComponent stem <> ".gemspec.rz"

-- | Directory reads retain the selected caller or anonymous public authority.
gemDirectoryRequestUnder :: BaseRequest -> Maybe ClientCredential -> Either UrlFormationError Request
gemDirectoryRequestUnder base credential = metadataGet rubyReadCredential credential "text/plain" <$> requestAt base "versions"
