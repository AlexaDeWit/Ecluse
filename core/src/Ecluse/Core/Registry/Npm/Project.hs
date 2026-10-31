-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Project npm metadata into the shared package model.
Version-map keys identify artifacts independently of their filenames.
-}
module Ecluse.Core.Registry.Npm.Project (
    -- * Projection
    projectVersionListEntry,
    projectVersionEntryResult,
    projectVersionManifest,
    installHooks,

    -- * Name validation
    projectName,
    projectScope,
    npmNameLeadChars,
    tarballCoordinate,
) where

import Data.Aeson (FromJSON (parseJSON), Value, withObject, (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (..),
    CodeExecSignal (NoCodeOnInstall, RunsCodeOnInstall),
    DependencySources (FetchesOutsideRegistry, RegistryDependencies),
    Hash,
    HashAlg (SHA1),
    PackageDetails (..),
    PackageName,
    Scope,
    mkHash,
    mkPackageName,
    mkScope,
    mkSriHashes,
    unscopedName,
 )
import Ecluse.Core.Package.Entry (EntryKey (ObjectEntry))
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Npm.DependencySource (renderExternalDependency)
import Ecluse.Core.Registry.Npm.Wire (
    Dist (..),
    VersionManifest (..),
 )
import Ecluse.Core.Registry.Npm.Wire qualified as Wire
import Ecluse.Core.Registry.VersionList (VersionListItem (..))
import Ecluse.Core.Registry.WireSupport (
    nameComponentWith,
    withinNameLimit,
 )
import Ecluse.Core.Server.Path (Filename, mkFilename)
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Text (urlFilename)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)

-- A decoded version object. A malformed @_npmUser@ drops the release, though nothing keeps it.
newtype VersionEntry = VersionEntry VersionManifest

instance FromJSON VersionEntry where
    parseJSON v =
        withObject "npm version object" (\o -> VersionEntry <$> parseJSON v <* (o .:? "_npmUser" :: Parser (Maybe Wire.Person))) v

-- | Project a compact release while retaining the decoder reason for the invalid-entry report.
projectVersionEntryResult :: PackageName -> Version -> Maybe UTCTime -> Value -> Either String PackageDetails
projectVersionEntryResult name version publishedAt value =
    projectDetails name version publishedAt <$> parseEither parseJSON value

-- | Recognise a compact release with the same optional-field checks as a complete release.
projectVersionListEntry :: Text -> Maybe Value -> VersionListItem
projectVersionListEntry key raw = VersionListEntry (mkVersion Npm key <$ (raw >>= rightToMaybe . (parseEither parseJSON :: Value -> Either String VersionEntry)))

-- Every field is evaluated here, so a retained release never keeps its decoded manifest alive.
projectDetails :: PackageName -> Version -> Maybe UTCTime -> VersionEntry -> PackageDetails
projectDetails name version publishedAt (VersionEntry vm) =
    projectVersionManifest name version publishedAt vm

-- | Project already decoded facts using the version-map key as the artifact identity.
projectVersionManifest :: PackageName -> Version -> Maybe UTCTime -> VersionManifest -> PackageDetails
projectVersionManifest name version publishedAt vm =
    PackageDetails
        { pkgName = name
        , pkgVersion = version
        , pkgPublishedAt = publishedAt
        , pkgInstallCode = installCode vm
        , pkgDependencySources = maybe RegistryDependencies (FetchesOutsideRegistry . renderExternalDependency) (vmExternalDependency vm)
        , pkgAvailability = vmAvailability vm
        , pkgArtifacts = (:| []) $! projectArtifact version (vmDist vm)
        }

-- A @false@ flag cannot hide a hook declared by the scripts map.
installCode :: VersionManifest -> CodeExecSignal
installCode vm
    | not (null hooks) =
        RunsCodeOnInstall ("declares install script(s): " <> T.intercalate ", " hooks)
    | vmHasInstallScript vm == Just True =
        RunsCodeOnInstall "declares an install script (hasInstallScript)"
    | otherwise = NoCodeOnInstall
  where
    hooks = filter (`Map.member` vmScripts vm) installHooks

-- | Hook presence declares install code, including an empty command.
installHooks :: [Text]
installHooks = ["preinstall", "install", "postinstall"]

-- The filter applies the https-only egress policy after projection.
projectArtifact :: Version -> Dist -> Artifact
projectArtifact version dist =
    Artifact
        { artEntryKey = ObjectEntry (renderVersion version)
        , artFilename = tarballFilename (distTarball dist) version
        , artUrl = distTarball dist
        , artHashes = strictElements (sriHashes <> maybeToList sha1Hash)
        , artCoreMetadata = Nothing
        , artSize = distUnpackedSize dist
        }
  where
    toHash :: HashAlg -> Text -> Maybe Hash
    toHash alg = rightToMaybe . mkHash alg
    sriHashes = maybe [] (either (const []) toList . mkSriHashes) (distIntegrity dist)
    sha1Hash = distShasum dist >>= toHash SHA1

-- Falls back to @\<version\>.tgz@ when the URL ends in a slash or names no file.
tarballFilename :: Text -> Version -> Text
tarballFilename url version =
    fromMaybe (renderVersion version <> ".tgz") (urlFilename url)

-- | Parse a package name. A bare @\@foo@ never falls back to an unscoped name.
projectName :: Text -> Either ParseError PackageName
projectName raw = do
    withinNpmNameLimit raw
    if T.isPrefixOf "@" raw
        then scopedName raw
        else mkPackageName Npm Nothing <$> nameComponent raw

-- A scope with no package component never falls back to an unscoped name.
scopedName :: Text -> Either ParseError PackageName
scopedName raw = case T.stripPrefix "/" afterScope of
    Nothing -> Left (ParseError ("scoped npm name with no package name: " <> show raw))
    Just base -> do
        scope <- projectScope scopeWire
        mkPackageName Npm (Just scope) <$> nameComponent base
  where
    (scopeWire, afterScope) = T.break (== '/') raw

-- | Parse a scope with or without its leading @\@@.
projectScope :: Text -> Either ParseError Scope
projectScope raw = do
    -- Measure after the strip, so @myorg and myorg stay one scope at the cap as well as below it.
    withinNpmNameLimit bare
    mkScope <$> nameComponent bare
  where
    bare = fromMaybe raw (T.stripPrefix "@" raw)

-- The package and scope parsers apply the length cap before parsing a component.
nameComponent :: Text -> Either ParseError Text
nameComponent = nameComponentWith "npm name component" usableComponent

usableComponent :: Text -> Bool
usableComponent component =
    T.all npmNameChar component
        && T.take 1 component `notElem` [".", "-", "_"]
        && T.toLower component `notElem` reservedNames

-- @ and / are scope structure, which 'projectName' and 'scopedName' read, so a part carries
-- neither.
npmNameChar :: Char -> Bool
npmNameChar ch = isAsciiUpper ch || isAsciiLower ch || isDigit ch || ch `elem` npmNameSpecials

-- The punctuation npm's own name grammar admits outside the alphanumerics.
npmNameSpecials :: [Char]
npmNameSpecials = "-_.!~*'()"

-- | The store walk's bucket alphabet follows the package-name grammar.
npmNameLeadChars :: [Char]
npmNameLeadChars = [ch | ch <- ['\0' .. '\127'], npmNameChar ch, usableComponent (T.singleton ch)]

-- The two names npm refuses outright, each because it collides with a path npm itself writes.
reservedNames :: [Text]
reservedNames = ["node_modules", "favicon.ico"]

-- The package cap includes its scope prefix. The scope cap measures the bare scope.
withinNpmNameLimit :: Text -> Either ParseError ()
withinNpmNameLimit = withinNameLimit "npm name" npmNameLimit

-- npm's own cap on a package name, the one its validator applies to a new package.
npmNameLimit :: Int
npmNameLimit = 214

-- | Parse an npm tarball-slot @file@ into the 'Version' and verbatim 'Filename' it names for @name@.
tarballCoordinate :: PackageName -> Text -> Maybe (Version, Filename)
tarballCoordinate name file =
    case T.stripSuffix ".tgz" file >>= T.stripPrefix (unscopedName name <> "-") of
        Just version
            | not (T.null version) -> (mkVersion Npm version,) <$> mkFilename file
        _ -> Nothing
