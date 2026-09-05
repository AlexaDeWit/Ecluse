-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Projection of npm wire JSON into the ecosystem-agnostic domain model, the second half of
the npm protocol boundary. "Ecluse.Core.Registry.Npm.Wire" captures what the registry said;
this module turns that into 'PackageInfo' and 'PackageDetails' and so realises the @parse*@
fields of the "Ecluse.Core.Registry" handle, and nothing above the adapter ever sees npm wire
data. The projection is pure and total: it returns 'Either' 'ParseError' and never throws.

== Per-version graceful degradation

The @versions@, @dist-tags@, and @time@ maps decode element-wise. A version whose manifest
lacks a required or security-decisive field is dropped rather than failing the packument, and
a dropped version is never served, so the degradation stays fail-closed for that one version
while every healthy version still resolves. A document is denied wholesale only when its
top-level structure is unusable: a @versions@ that is not an object, or an absent or empty
@name@. Every drop is recorded as an 'Ecluse.Core.Package.InvalidEntry' in
'Ecluse.Core.Package.infoInvalidEntries', so the serve path can log what arrived malformed.

== Signal mapping

Install-script presence maps to 'CodeExecSignal' __fail-closed__ across two independent wire
signals: the @scripts@ map is consulted even when @hasInstallScript@ is present and @false@,
so a hostile upstream cannot mask a declared hook behind the sibling flag. The @deprecated@
notice maps to 'Availability', and @dist@ to one 'Artifact' carrying both digests. Both
survive because a cross-upstream merge compares the same version's integrity across
registries to spot a divergence, and each is built through the validating 'mkHash', so a
malformed digest is absent rather than degenerate. Trust stays 'TrustUnknown', because
establishing it needs a signature fetch this pure projection does not perform.

== Name as a validation input

The requested 'PackageName' is the validation authority for the served packument's name,
never a rewrite of it. A document that self-reports a different name projects into the shared
'Ecluse.Core.Registry.WireSupport.Projection' mismatch and carries no packument, so
the caller can treat that origin as untrusted for this request, and an absent name is a
'ParseError' instead. 'projectName' is also the one splitter for npm identifiers: the route,
the URL rewrite, the publish guard, and the queue decode all read a name through it, so one
spelling has one verdict everywhere. @\@@ and @\/@ are scope structure, so a bare @\@foo@ is a
malformed scoped name rather than an unscoped one.

== The name grammar

Each part of a name parses against npm's own __error tier__, the rules invalid for every
package, legacy included: the allowlist that survives @encodeURIComponent@ unchanged (letters,
digits, and @-_.!~*'()@), no leading period, hyphen, or underscore, and neither reserved name
(@node_modules@, @favicon.ico@). It sits on
'Ecluse.Core.Registry.WireSupport.parseNameComponent', the non-empty, ASCII, path-safe floor
Écluse holds ecosystem-wide, so nothing invisible or path-unsafe enters by construction. npm's __warning
tier__ still parses, because real legacy names use it: capitals (@JSONStream@) and @~'!()*@. A
name over 214 characters never parses, counted whole including any scope prefix, as npm counts it.
-}
module Ecluse.Core.Registry.Npm.Project (
    -- * Projection
    parsePackageInfoFromValue,
    parseVersionList,
    projectVersionEntry,

    -- * Name validation
    projectName,
    projectScope,
    npmNameLeadChars,
) where

import Data.Aeson (FromJSON (parseJSON), Object, Value, eitherDecodeStrict, withObject, (.!=), (.:?))
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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
    InvalidEntry (invalidKey),
    InvalidEntryKind (InvalidDistTag, InvalidPublishTime, InvalidVersionManifest),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    Person (..),
    Scope,
    Trust (TrustUnknown),
    mkHash,
    mkPackageName,
    mkScope,
    mkSriHashes,
 )
import Ecluse.Core.Registry (ParseError (..), RegistryResponse (responseBody))
import Ecluse.Core.Registry.Npm.Wire (
    Dist (..),
    License (LicenseObject, LicenseSpdx),
    VersionManifest (..),
 )
import Ecluse.Core.Registry.Npm.Wire qualified as Wire
import Ecluse.Core.Registry.WireSupport (
    NameRefusal (NameEmpty, NameNotAscii, NameUnsafeComponent),
    Projection,
    checkNameAgreement,
    parseNameComponent,
    partitionLenient,
 )
import Ecluse.Core.Text (urlFilename)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)

{- The packument as this projection reads it: the wire fields plus the per-version @_npmUser@
that "Ecluse.Core.Registry.Npm.Wire" leaves off the manifest, so the publisher survives. -}
data WirePackument = WirePackument
    { wpName :: Text
    , wpDistTags :: Map Text Text
    , wpVersions :: Map Text VersionEntry
    , wpTime :: Map Text UTCTime
    , wpInvalidEntries :: [InvalidEntry]
    -- ^ The malformed @versions@\/@dist-tags@\/@time@ entries dropped during decode.
    }

instance FromJSON WirePackument where
    parseJSON = withObject "npm packument" $ \o -> do
        name <- o .:? "name" .!= ""
        (distTags, distTagDrops) <- lenientDistTags o
        (versions, versionDrops) <- lenientVersionMap o
        (time, timeDrops) <- lenientTimeMap (Map.keysSet versions) o
        pure
            WirePackument
                { wpName = name
                , wpDistTags = distTags
                , wpVersions = versions
                , wpTime = time
                , -- Deterministic order (versions, then dist-tags, then time), each
                  -- already in ascending-key order, so the dropped-entry list is stable.
                  wpInvalidEntries = versionDrops <> distTagDrops <> timeDrops
                }

{- Decode @versions@ element-wise, recording a manifest that lacks a required or security-decisive
field as an 'InvalidVersionManifest': it cannot be evaluated, so it must never be served. -}
lenientVersionMap :: Object -> Parser (Map Text VersionEntry, [InvalidEntry])
lenientVersionMap o = do
    raw <- o .:? "versions" .!= mempty -- Map Text Value: each version object kept raw
    pure (partitionLenient InvalidVersionManifest (parseEither parseJSON) raw)

{- Decode @dist-tags@ element-wise: a non-string value is dropped as an 'InvalidDistTag', so one
bad tag loses only that tag. 'mkVersion' is total, so the merge reconciles tag targeting later. -}
lenientDistTags :: Object -> Parser (Map Text Text, [InvalidEntry])
lenientDistTags o = do
    raw <- o .:? "dist-tags" .!= mempty
    pure (partitionLenient InvalidDistTag (parseEither parseJSON) raw)

{- Decode @time@ element-wise, dropping an entry that is not an instant. Only a key naming a present
version records an 'InvalidPublishTime': @created@ and @modified@ are package-level bookkeeping. -}
lenientTimeMap :: Set Text -> Object -> Parser (Map Text UTCTime, [InvalidEntry])
lenientTimeMap versionKeys o = do
    raw <- o .:? "time" .!= mempty
    let (kept, dropped) = partitionLenient InvalidPublishTime (parseEither parseJSON) raw
    pure (kept, filter ((`Set.member` versionKeys) . invalidKey) dropped)

{- A decoded version object: the wire 'VersionManifest' plus its @_npmUser@ publisher. -}
data VersionEntry = VersionEntry
    { veManifest :: VersionManifest
    , vePublisher :: Maybe Wire.Person
    }

instance FromJSON VersionEntry where
    parseJSON v =
        withObject "npm version object" (\o -> VersionEntry <$> parseJSON v <*> o .:? "_npmUser") v

{- | Project an already-decoded packument @Value@ into a 'Projection' for the requested package,
reusing that parse instead of the bytes. A @Value@ that is not a packument gives a 'ParseError'.
-}
parsePackageInfoFromValue :: PackageName -> Value -> Either ParseError (Projection PackageInfo)
parsePackageInfoFromValue requestedName value =
    decodePackumentValue value >>= projectValidated requestedName

{- Validate a decoded packument's self-reported name against the request. An absent or empty
upstream name fails as a 'ParseError'. -}
projectValidated :: PackageName -> WirePackument -> Either ParseError (Projection PackageInfo)
projectValidated requestedName pkmt = do
    info <- projectPackageInfo pkmt
    pure (checkNameAgreement requestedName (infoName info) info)

-- Project a 'WirePackument' into 'PackageInfo', taking the upstream's self-reported name.
-- 'projectValidated' owns checking that name against the request.
projectPackageInfo :: WirePackument -> Either ParseError PackageInfo
projectPackageInfo pkmt = do
    name <- projectName (wpName pkmt)
    pure
        PackageInfo
            { infoName = name
            , infoVersions = projectVersions name pkmt
            , infoDistTags = projectDistTags pkmt
            , infoInvalidEntries = wpInvalidEntries pkmt
            }

{- | Project one @versions@ entry into 'PackageDetails', or 'Nothing' on a missing required field.
"Ecluse.Core.Registry.Npm.SelectiveDecode" reuses this, so both decode paths project identically.
-}
projectVersionEntry :: PackageName -> Version -> Maybe UTCTime -> Value -> Maybe PackageDetails
projectVersionEntry name version publishedAt value =
    projectDetails name version publishedAt <$> parseMaybe parseJSON value

{- | The available versions of a fetched metadata response, in the packument's @versions@ key
order. Fails with a 'ParseError' only when the body does not decode.
-}
parseVersionList :: RegistryResponse -> Either ParseError [Version]
parseVersionList resp = do
    pkmt <- decodePackument resp
    pure (map (mkVersion Npm) (Map.keys (wpVersions pkmt)))

decodePackument :: RegistryResponse -> Either ParseError WirePackument
decodePackument =
    first (ParseError . toText) . eitherDecodeStrict . responseBody

{- Decode an already-parsed 'Value' into a 'WirePackument'. The result matches 'decodePackument'
on the bytes that produced it, because aeson runs the same 'FromJSON' instance either way. -}
decodePackumentValue :: Value -> Either ParseError WirePackument
decodePackumentValue =
    first (ParseError . toText) . parseEither parseJSON

projectVersions :: PackageName -> WirePackument -> Map Text PackageDetails
projectVersions name pkmt =
    Map.mapWithKey projectAt (wpVersions pkmt)
  where
    projectAt rawVersion =
        projectDetails
            name
            (mkVersion Npm rawVersion)
            (Map.lookup rawVersion (wpTime pkmt))

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

-- The SPDX expression or license name carried by a wire 'License'.
licenseText :: License -> Text
licenseText = \case
    LicenseSpdx spdx -> spdx
    LicenseObject name _url -> name

{- Map install-script presence onto 'CodeExecSignal', failing closed across two independent wire
signals: a @false@ @hasInstallScript@ cannot hide a hook the @scripts@ map declares. -}
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

-- Map an optional @deprecated@ notice onto 'Availability'.
availability :: VersionManifest -> Availability
availability vm = maybe Available Deprecated (vmDeprecated vm)

{- Project @dist@ into an 'Artifact' carrying both digests. The @tarball@ URL stays verbatim, and
"Ecluse.Core.Package.Filter" folds its scheme against the https-only egress policy afterward. -}
projectArtifact :: Version -> Dist -> Artifact
projectArtifact version dist =
    Artifact
        { artFilename = tarballFilename (distTarball dist) version
        , artUrl = distTarball dist
        , artKind = Tarball
        , artHashes = sriHashes <> maybeToList sha1Hash
        , artSize = distUnpackedSize dist
        , artInterpreter = Nothing
        , artYanked = False
        , artProvenance = Nothing
        }
  where
    -- The validating 'mkHash' makes a malformed digest absent, never degenerate: no bogus
    -- fingerprint may pass the public-integrity admission gate (security.md invariant 5).
    toHash :: HashAlg -> Text -> Maybe Hash
    toHash alg = rightToMaybe . mkHash alg
    -- 'mkSriHashes' splits a multi-component @integrity@ into one 'Hash' per component, so the
    -- admission floor and the worker's tamper gate rank and verify each digest exactly.
    sriHashes = maybe [] (either (const []) toList . mkSriHashes) (distIntegrity dist)
    sha1Hash = distShasum dist >>= toHash SHA1

{- The tarball's filename, falling back to @\<version\>.tgz@ when the URL ends in a
slash or names no file. -}
tarballFilename :: Text -> Version -> Text
tarballFilename url version =
    fromMaybe (renderVersion version <> ".tgz") (urlFilename url)

projectDistTags :: WirePackument -> Map Text Version
projectDistTags = Map.map (mkVersion Npm) . wpDistTags

{- | Parse an npm package name into the domain 'PackageName': the one splitter every npm entry
point reads a name through. A bare @\@foo@ is a malformed scoped name, never an unscoped one.
-}
projectName :: Text -> Either ParseError PackageName
projectName raw = do
    withinNameLimit raw
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
    withinNameLimit bare
    mkScope <$> nameComponent bare
  where
    bare = fromMaybe raw (T.stripPrefix "@" raw)

{- One component of an npm name, the scope or the bare name. It sits on the shared name floor
and adds npm's own grammar. 'projectName' and 'projectScope' own the length cap. -}
nameComponent :: Text -> Either ParseError Text
nameComponent component = do
    onFloor <- first (refusalText component) (parseNameComponent component)
    if usableComponent onFloor
        then Right onFloor
        else Left (ParseError ("unusable npm name component: " <> show component))

-- npm's own wording for each way the shared floor refuses a component.
refusalText :: Text -> NameRefusal -> ParseError
refusalText component = \case
    NameEmpty -> ParseError "empty npm name component"
    NameNotAscii -> ParseError ("non-ASCII npm name component: " <> show component)
    NameUnsafeComponent -> ParseError ("unusable npm name component: " <> show component)

{- npm's error tier for one name part on top of the floor, the rules invalid for every package,
legacy included: the allowlist @encodeURIComponent@ leaves unchanged, no leading @.@\/@-@\/@_@,
neither reserved name. -}
usableComponent :: Text -> Bool
usableComponent component =
    T.all npmNameChar component
        && T.take 1 component `notElem` [".", "-", "_"]
        && T.toLower component `notElem` reservedNames

-- The characters npm's validator admits in a name part. @ and / are scope structure, which
-- 'projectName' and 'scopedName' read, so a part carries neither.
npmNameChar :: Char -> Bool
npmNameChar ch = isAsciiUpper ch || isAsciiLower ch || isDigit ch || ch `elem` npmNameSpecials
  where
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
prefix, and 'projectScope' measures a bare scope. 'T.compareLength' stops at the cap. -}
withinNameLimit :: Text -> Either ParseError ()
withinNameLimit raw
    | T.compareLength raw npmNameLimit == GT = Left (ParseError overLong)
    | otherwise = Right ()
  where
    overLong :: Text
    overLong = "npm name over " <> show npmNameLimit <> " characters, starting " <> show (T.take 24 raw)

-- npm's own cap on a package name, the one its validator applies to a new package.
npmNameLimit :: Int
npmNameLimit = 214

-- Project a wire 'Wire.Person' into the domain 'Person' (a structural copy).
projectPerson :: Wire.Person -> Person
projectPerson p =
    Person
        { personName = Wire.personName p
        , personEmail = Wire.personEmail p
        , personUrl = Wire.personUrl p
        }
