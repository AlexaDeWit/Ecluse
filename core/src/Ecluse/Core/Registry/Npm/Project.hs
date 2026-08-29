-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Projection of npm wire JSON into the ecosystem-agnostic domain model.

This module is the second half of the npm protocol boundary.
"Ecluse.Core.Registry.Npm.Wire" captures /what the registry said/ as faithful wire
types. This module turns those into the domain vocabulary of "Ecluse.Core.Package":
'PackageInfo' (the packument-level view) and 'PackageDetails' (the per-version
snapshot the rules engine evaluates). Together they realise the @parse*@ fields
of the "Ecluse.Core.Registry" handle, so nothing above the adapter ever sees npm wire
data.

The projection is __pure and total__: it returns 'Either' 'ParseError' and never
throws. That is the execution half of /parse, don't validate/. Once the projection
runs, downstream code holds precise domain types and never re-inspects the wire shape.

== Per-version graceful degradation

The projection decodes the @versions@, @dist-tags@, and @time@ maps
__element-wise__. It __drops__ three kinds of entry rather than failing the whole
packument:

* A version whose manifest is missing or malformed in a required\/security-decisive
  field: no @dist@ or @tarball@, an unusable @version@.
* A @dist-tags@ entry whose value is not a string.
* A @time@ entry that is not a decodable instant.

Presence in the decision surface is what makes a version a serve-candidate, so a dropped
version is never served. That is fail-closed for that one version. A version that cannot
be decoded cannot be evaluated for integrity, CVEs, or rules, while every healthy
version still resolves. A dropped date is a version with no known publish time, and a
dropped tag loses only that one tag. The projection denies a document wholesale only
when its /top-level/ structure is unusable: a @versions@ that is not an object, an
absent\/empty @name@. A version's purely __advisory__ fields degrade in the wire layer
("Ecluse.Core.Registry.Npm.Wire") without dropping the version.

The projection __records__ every drop as an 'Ecluse.Core.Package.InvalidEntry' in
'Ecluse.Core.Package.infoInvalidEntries'. Each entry carries its key and reason, so the
serve path can log what an upstream served malformed rather than dropping it
silently.

== Signal mapping

The npm-specific fields collapse onto the normalised, ecosystem-blind signals:

* Install-script presence → 'CodeExecSignal', read __fail-closed__ across two
  independent wire signals. A version runs code on install when the abbreviated form's
  @hasInstallScript@ flag is @true@, or when the @scripts@ map declares any of
  @preinstall@\/@install@\/@postinstall@. That matches what npm itself sets the flag
  from. The two fields are independent on the wire, so the projection consults the
  @scripts@ map __even when @hasInstallScript@ is present and @false@__. A hostile
  upstream must not be able to mask a real install hook by lying in the sibling flag. A
  declared script is authoritative, and the signal is the union of the two, never the
  flag overriding a script. A version with neither signal maps to 'NoCodeOnInstall'.
  Both metadata forms always carry the @scripts@\/@hasInstallScript@ information, so its
  absence is a determination, not an unknown.
* The @deprecated@ notice → 'Availability'. A notice yields 'Deprecated' (carrying
  the message), its absence 'Available'. The npm protocol has no per-version yank, so
  @Yanked@ never arises here.
* The @dist@ object → a single-element 'NonEmpty' of 'Artifact', because npm
  publishes exactly one tarball per version. Both integrity digests survive when
  present and __well-formed__: @dist.shasum@ as a 'SHA1' 'Hash' /and/
  @dist.integrity@ as an 'SRI' 'Hash'. Carrying both is load-bearing. A cross-upstream
  merge compares the same version's integrity across the private and public registries
  to detect a supply-chain divergence. Dropping either digest would blind it. The
  projection builds each digest through the validating 'mkHash', so a __malformed__
  one is unconstructable and therefore __absent__, never a degenerate 'Hash'.
  Malformed covers empty (@"shasum":""@ \/ @"integrity":""@), truncated, non-hex, and
  bad-base64. A digest that ties the version to no tamper-evident fingerprint must not
  slip past the public-integrity admission gate.
* The @_npmUser@ field → 'pkgPublisher': who pushed this version, the provenance. It
  rides on the version object, but the wire manifest does not model it. The projection
  therefore reads it directly from the version object here.
* The @time[version]@ entry → 'pkgPublishedAt'. The publish timestamp lives in the
  packument's @time@ map, not the manifest. A version with no @time@ entry (or
  an abbreviated document, which omits @time@) projects to 'Nothing'.

Trust is left 'TrustUnknown': establishing it needs signature verification
against npm's published keys, a fetch this pure projection does not perform.

== Name as a validation input

The requested 'PackageName', the identity the proxy resolved from the route, is the
__validation authority__ for the served packument's name, never a rewrite of it. The
packument projection takes the requested name and checks the upstream's self-reported
top-level @name@ against it. A document whose self-report agrees is a 'Projected'
'PackageInfo' carrying the name the upstream genuinely reported. A document whose
self-report __disagrees__ is a 'NameMismatch', so the caller can treat that origin as
untrusted for this request and drop its contribution. The served name is therefore
always a value an upstream genuinely reported, never a substituted or manufactured
one. An /absent/ or otherwise undecodable name is a 'ParseError', distinct from a
present-but-different name.

'projectName' is also the __one splitter__ for npm identifiers. The route, the URL rewrite,
the publish guard, and the queue decode all read a name through it, so one spelling has one
verdict everywhere. A scope or bare name that is not a usable path component is a
'ParseError', and a bare @\@foo@ is a malformed scoped name, never an unscoped one.

The grammar carries two further refusals, both taken from npm's own validator, so a name this
splitter admits is a name a registry can serve. A Unicode __format__ character (U+200B,
U+202E, and the rest of the @Cf@ class) never parses. An invisible or direction-reversing
character makes two distinct names render identically, which is how a name-based rule gets
dodged and how a log line gets forged. A name over 214 characters never parses, counted over
the whole name including any scope prefix, which is what npm counts.
-}
module Ecluse.Core.Registry.Npm.Project (
    -- * Projection
    parsePackageInfoFromValue,
    parseVersionList,
    projectVersionEntry,

    -- * Name validation
    Projection (..),
    projectName,
    projectScope,
) where

import Data.Aeson (FromJSON (parseJSON), Object, Value, eitherDecodeStrict, withObject, (.!=), (.:?))
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Data.Char (GeneralCategory (Format), generalCategory, isSpace)
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
    InvalidEntry (..),
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
    NameAgreement (NameAgrees, NameDisagrees),
    checkNameAgreement,
    partitionLenient,
 )
import Ecluse.Core.Server.Path (isSafeComponent)
import Ecluse.Core.Text (lastPathSegment)
import Ecluse.Core.Version (Version, mkVersion, unVersion)

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

{- Decode @versions@ element-wise, dropping a version whose manifest lacks a required or
security-decisive field and recording it as an 'InvalidVersionManifest'. A version that does
not decode cannot be evaluated for integrity, CVEs, or rules, so it must never be served. -}
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

{- Decode @time@ element-wise, dropping an entry that is not an instant. Only a key naming a
present version records an 'InvalidPublishTime'. The @created@ and @modified@ keys are
package-level bookkeeping, not a version's publish time. -}
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

{- | The outcome of projecting an upstream packument against the requested package name.
The requested name validates the document and is never substituted into it.
-}
data Projection
    = -- | The document decoded and its self-reported name matched the request.
      Projected PackageInfo
    | -- | The document decoded but self-reported this /different/ name (carried verbatim for the audit log).
      NameMismatch Text
    deriving stock (Eq, Show)

{- | Project an already-decoded packument @Value@ into a 'Projection' for the requested package,
reusing that parse instead of the bytes. A @Value@ that is not a packument gives a 'ParseError'.
-}
parsePackageInfoFromValue :: PackageName -> Value -> Either ParseError Projection
parsePackageInfoFromValue requestedName value =
    decodePackumentValue value >>= projectValidated requestedName

{- Validate a decoded packument's self-reported name against the request. An absent or empty
upstream name fails as a 'ParseError'. -}
projectValidated :: PackageName -> WirePackument -> Either ParseError Projection
projectValidated requestedName pkmt = do
    info <- projectPackageInfo pkmt
    pure $ case checkNameAgreement requestedName (infoName info) of
        NameAgrees -> Projected info
        NameDisagrees reported -> NameMismatch reported

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

{- | Project one @versions@ entry into its 'PackageDetails', or 'Nothing' when the version
object lacks a required or security-decisive field. "Ecluse.Core.Registry.Npm.SelectiveDecode"
reuses this, so a single-version decode matches the whole-packument projection exactly.
-}
projectVersionEntry :: PackageName -> Version -> Maybe UTCTime -> Value -> Maybe PackageDetails
projectVersionEntry name version publishedAt value =
    projectDetails name version publishedAt <$> parseMaybe parseJSON value

{- | Extract the list of available versions from a fetched metadata response, in
the packument's @versions@ key order. Fails with a 'ParseError' only if the body
does not decode.
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
signals. The @scripts@ map is consulted even when @hasInstallScript@ is @false@, so a hostile
upstream cannot hide a declared install hook behind the flag. -}
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

{- Project @dist@ into an 'Artifact' carrying both digests, the SHA-1 @shasum@ and the SRI
@integrity@. The @tarball@ URL stays verbatim, and "Ecluse.Core.Package.Filter" folds its
scheme against the https-only egress policy afterward. -}
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
    -- Build each present digest through the validating 'mkHash', so a malformed value (empty,
    -- truncated, or non-hex) becomes absent, never a degenerate 'Hash'. A bogus or missing
    -- fingerprint must not pass the public-integrity admission gate (security.md invariant 5).
    toHash :: HashAlg -> Text -> Maybe Hash
    toHash alg = rightToMaybe . mkHash alg
    -- 'mkSriHashes' splits a multi-component @integrity@ into one 'Hash' per component, so the
    -- admission floor and the worker's tamper gate rank and verify each digest exactly.
    sriHashes = maybe [] (either (const []) toList . mkSriHashes) (distIntegrity dist)
    sha1Hash = distShasum dist >>= toHash SHA1

{- The tarball's filename: the URL's last path segment, falling back to
@\<version\>.tgz@ when the URL ends in a slash or has no segment. -}
tarballFilename :: Text -> Version -> Text
tarballFilename url version =
    fromMaybe (unVersion version <> ".tgz") (lastPathSegment url)

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
    withinNameLimit raw
    mkScope <$> nameComponent (fromMaybe raw (T.stripPrefix "@" raw))

{- One component of an npm name, the scope or the bare name. It reaches an interpolated upstream
URL, so an unsafe spelling must never parse. -}
nameComponent :: Text -> Either ParseError Text
nameComponent component
    | T.null component = Left (ParseError "empty npm name component")
    | isSafeComponent component && T.all usable component = Right component
    | otherwise = Left (ParseError ("unusable npm name component: " <> show component))
  where
    -- A format character is invisible or reverses how the text renders, so it makes two distinct
    -- names look like one in a log, a terminal, or a name-based rule.
    usable ch = ch /= '@' && not (isSpace ch) && generalCategory ch /= Format

{- Refuse a name over npm's own length cap. 'projectName' measures the whole name, scope prefix
included, because that is what npm's validator measures. -}
withinNameLimit :: Text -> Either ParseError ()
withinNameLimit raw
    | T.length raw > npmNameLimit =
        Left (ParseError ("npm name over " <> show npmNameLimit <> " characters: " <> show (T.length raw)))
    | otherwise = Right ()

-- npm's own cap on a package name, the one its validate-npm-package-name validator applies.
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
