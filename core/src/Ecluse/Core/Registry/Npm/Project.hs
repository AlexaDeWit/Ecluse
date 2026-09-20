-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Project npm metadata into the shared package model.
Version-map keys identify artifacts independently of their filenames.
-}
module Ecluse.Core.Registry.Npm.Project (
    -- * Projection
    versionListParser,
    projectVersionEntryResult,

    -- * Name validation
    projectName,
    projectScope,
    npmNameLeadChars,
) where

import Data.Aeson (FromJSON (parseJSON), Value, withObject, (.:?))
import Data.Aeson.Types (parseEither)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.JsonStream.Parser qualified as J
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Availability (Available, Deprecated),
    CodeExecSignal (NoCodeOnInstall, RunsCodeOnInstall),
    Hash,
    HashAlg (SHA1),
    PackageDetails (..),
    PackageName,
    Person (..),
    Scope,
    Trust (TrustUnknown),
    mkHash,
    mkPackageName,
    mkScope,
    mkSriHashes,
 )
import Ecluse.Core.Package.Entry (EntryKey (ObjectEntry))
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Npm.Streaming (NpmContainer (VersionsContainer), NpmField (BeginContainer, VersionField), NpmRead (VersionListRead), npmFields)
import Ecluse.Core.Registry.Npm.Wire (
    Dist (..),
    License (LicenseObject, LicenseSpdx),
    VersionManifest (..),
 )
import Ecluse.Core.Registry.Npm.Wire qualified as Wire
import Ecluse.Core.Registry.VersionList (VersionListItem (..))
import Ecluse.Core.Registry.WireSupport (
    nameComponentWith,
    withinNameLimit,
 )
import Ecluse.Core.Security (Limits, maxNestingDepth)
import Ecluse.Core.Text (urlFilename)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)

-- A decoded version object: the wire 'VersionManifest' plus its @_npmUser@ publisher.
data VersionEntry = VersionEntry
    { veManifest :: VersionManifest
    , vePublisher :: Maybe Wire.Person
    }

instance FromJSON VersionEntry where
    parseJSON v =
        withObject "npm version object" (\o -> VersionEntry <$> parseJSON v <*> o .:? "_npmUser") v

-- | Project a compact release while retaining the decoder reason for the invalid-entry report.
projectVersionEntryResult :: PackageName -> Version -> Maybe UTCTime -> Value -> Either String PackageDetails
projectVersionEntryResult name version publishedAt value =
    projectDetails name version publishedAt <$> parseEither parseJSON value

-- | Recognise usable versions with only VersionEntry's discriminating fields and return sorted identifiers.
versionListParser :: Limits -> J.Parser VersionListItem
versionListParser limits = J.objectFound VersionListObject VersionListObject (J.catMaybeI (candidate <$> npmFields (maxNestingDepth limits) VersionListRead))
  where
    candidate (BeginContainer VersionsContainer) = Just VersionListContainer
    candidate (VersionField key raw) = Just (VersionListEntry (if usable raw then Just (mkVersion Npm key) else Nothing))
    candidate _ = Nothing
    usable raw = isJust (raw >>= rightToMaybe . (parseEither parseJSON :: Value -> Either String VersionEntry))

projectDetails :: PackageName -> Version -> Maybe UTCTime -> VersionEntry -> PackageDetails
projectDetails name version publishedAt entry =
    PackageDetails
        { pkgName = name
        , pkgVersion = version
        , pkgPublishedAt = publishedAt
        , pkgInstallCode = installCode vm
        , pkgTrust = TrustUnknown
        , pkgAvailability = availability vm
        , pkgArtifacts = projectArtifact version (vmDist vm) :| []
        , pkgLicenses = maybe [] (one . licenseText) (vmLicense vm)
        , pkgPublisher = projectPerson <$> vePublisher entry
        }
  where
    vm = veManifest entry

licenseText :: License -> Text
licenseText = \case
    LicenseSpdx spdx -> spdx
    LicenseObject name _url -> name

{- Fail closed across two independent wire signals: a @false@ @hasInstallScript@ cannot hide a
hook the @scripts@ map declares. -}
installCode :: VersionManifest -> CodeExecSignal
installCode vm
    | not (null hooks) =
        RunsCodeOnInstall ("declares install script(s): " <> T.intercalate ", " hooks)
    | vmHasInstallScript vm == Just True =
        RunsCodeOnInstall "declares an install script (hasInstallScript)"
    | otherwise = NoCodeOnInstall
  where
    hooks = filter (`Map.member` vmScripts vm) installHooks

-- The lifecycle script names whose presence means installation runs code.
installHooks :: [Text]
installHooks = ["preinstall", "install", "postinstall"]

availability :: VersionManifest -> Availability
availability vm = maybe Available Deprecated (vmDeprecated vm)

{- The @tarball@ URL stays verbatim: "Ecluse.Core.Package.Filter" folds its scheme against the
https-only egress policy afterward. -}
projectArtifact :: Version -> Dist -> Artifact
projectArtifact version dist =
    Artifact
        { artEntryKey = ObjectEntry (renderVersion version)
        , artFilename = tarballFilename (distTarball dist) version
        , artUrl = distTarball dist
        , artKind = Tarball
        , artHashes = sriHashes <> maybeToList sha1Hash
        , artSize = distUnpackedSize dist
        , artInterpreter = Nothing
        , artYanked = False
        , artProvenance = Nothing
        }
  where
    -- A malformed digest is absent, never degenerate: no bogus fingerprint may pass the
    -- public-integrity admission gate (security.md invariant 5).
    toHash :: HashAlg -> Text -> Maybe Hash
    toHash alg = rightToMaybe . mkHash alg
    -- One 'Hash' per @integrity@ component, so the admission floor and the worker's tamper
    -- gate rank and verify each digest exactly.
    sriHashes = maybe [] (either (const []) toList . mkSriHashes) (distIntegrity dist)
    sha1Hash = distShasum dist >>= toHash SHA1

-- Falls back to @\<version\>.tgz@ when the URL ends in a slash or names no file.
tarballFilename :: Text -> Version -> Text
tarballFilename url version =
    fromMaybe (renderVersion version <> ".tgz") (urlFilename url)

{- | Parse an npm package name into the domain 'PackageName': the one splitter every npm entry
point reads a name through. A bare @\@foo@ is a malformed scoped name, never an unscoped one.
-}
projectName :: Text -> Either ParseError PackageName
projectName raw = do
    withinNpmNameLimit raw
    if T.isPrefixOf "@" raw
        then scopedName raw
        else mkPackageName Npm Nothing <$> nameComponent raw

{- Split a scoped @\@scope\/name@ at its one separator. A scope with nothing after it is a
malformed scoped name, so the whole string never falls back to an unscoped reading. -}
scopedName :: Text -> Either ParseError PackageName
scopedName raw = case T.stripPrefix "/" afterScope of
    Nothing -> Left (ParseError ("scoped npm name with no package name: " <> show raw))
    Just base -> do
        scope <- projectScope scopeWire
        mkPackageName Npm (Just scope) <$> nameComponent base
  where
    (scopeWire, afterScope) = T.break (== '/') raw

{- | Parse an npm scope, with or without its leading @\@@ (@\@myorg@ and @myorg@ both give the
scope @myorg@).
-}
projectScope :: Text -> Either ParseError Scope
projectScope raw = do
    -- Measure after the strip, so @myorg and myorg stay one scope at the cap as well as below it.
    withinNpmNameLimit bare
    mkScope <$> nameComponent bare
  where
    bare = fromMaybe raw (T.stripPrefix "@" raw)

{- One component of an npm name, the scope or the bare name. It sits on the shared name floor
and adds npm's own grammar. 'projectName' and 'projectScope' own the length cap. -}
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

{- | Every character an npm package name may begin with, sieved out of ASCII by the grammar
above so the store walk's bucket alphabet cannot drift from what this module parses.
-}
npmNameLeadChars :: [Char]
npmNameLeadChars = [ch | ch <- ['\0' .. '\127'], npmNameChar ch, usableComponent (T.singleton ch)]

-- The two names npm refuses outright, each because it collides with a path npm itself writes.
reservedNames :: [Text]
reservedNames = ["node_modules", "favicon.ico"]

{- Refuse a name over npm's own cap. 'projectName' measures the whole name including any scope
prefix, and 'projectScope' measures a bare scope. -}
withinNpmNameLimit :: Text -> Either ParseError ()
withinNpmNameLimit = withinNameLimit "npm name" npmNameLimit

-- npm's own cap on a package name, the one its validator applies to a new package.
npmNameLimit :: Int
npmNameLimit = 214

projectPerson :: Wire.Person -> Person
projectPerson p =
    Person
        { personName = Wire.personName p
        , personEmail = Wire.personEmail p
        , personUrl = Wire.personUrl p
        }
