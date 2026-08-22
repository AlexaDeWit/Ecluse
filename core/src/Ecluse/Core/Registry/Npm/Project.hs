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
-}
module Ecluse.Core.Registry.Npm.Project (
    -- * Projection
    parsePackageInfoFromValue,
    parseVersionList,
    projectVersionEntry,

    -- * Name validation
    Projection (..),
    projectName,
) where

import Data.Aeson (FromJSON (parseJSON), Object, Value, eitherDecodeStrict, withObject, (.!=), (.:?))
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
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
import Ecluse.Core.Text (lastPathSegment)
import Ecluse.Core.Version (Version, mkVersion, unVersion)

{- The packument as this projection needs to read it: the wire fields plus the
per-version @_npmUser@ that "Ecluse.Core.Registry.Npm.Wire" intentionally leaves off
the manifest. Decoding the version objects here (rather than through the wire
'VersionManifest' alone) is what lets the publisher survive, because the wire manifest
discards it.
-}
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

{- Decode the @versions@ map __element-wise leniently__. Read it as a raw map of
version key to 'Value', then keep only the entries that project to a 'VersionEntry'.
Drop any that do not, and record each as an 'InvalidVersionManifest'.

The decode __drops__ a version whose manifest is missing or malformed in a
required\/security-decisive field, rather than failing the whole packument. That covers
no @dist@\/@tarball@, or an unusable @version@. That is fail-closed for that version. A
version that cannot be decoded cannot be evaluated for integrity, CVEs, or rules, so it
must never be served. Every healthy version still decodes. An absent @versions@ is the
empty map. A @versions@ that is not an object at all still fails the decode, because
the document is not a usable packument. -}
lenientVersionMap :: Object -> Parser (Map Text VersionEntry, [InvalidEntry])
lenientVersionMap o = do
    raw <- o .:? "versions" .!= mempty -- Map Text Value: each version object kept raw
    pure (partitionLenient InvalidVersionManifest (parseEither parseJSON) raw)

{- Decode the @dist-tags@ map __element-wise leniently__. Read it as a raw map of tag
name to 'Value'. Keep each entry whose value is a JSON string, and drop any that is
not, recording it as an 'InvalidDistTag'. A single non-string tag value therefore
loses only that tag rather than failing the whole document. A string that is not a
valid version is still kept here. 'mkVersion' is total, so the merge reconciles
dist-tag /targeting/ later, never a decode failure. -}
lenientDistTags :: Object -> Parser (Map Text Text, [InvalidEntry])
lenientDistTags o = do
    raw <- o .:? "dist-tags" .!= mempty
    pure (partitionLenient InvalidDistTag (parseEither parseJSON) raw)

{- Decode the @time@ map __element-wise leniently__. Read it as a raw map of key to
'Value'. Keep each entry that decodes as an instant, and drop any that does not. With
the publish time folded onto each version, a malformed sibling date is simply a version
with no known publish time, never a document failure. The decode records only a drop
keyed by a __present version__, as an 'InvalidPublishTime'. The @created@\/@modified@
bookkeeping keys are package-level, not a version's publish time, so a malformed one is
not a per-version drop and stays untracked. -}
lenientTimeMap :: Set Text -> Object -> Parser (Map Text UTCTime, [InvalidEntry])
lenientTimeMap versionKeys o = do
    raw <- o .:? "time" .!= mempty
    let (kept, dropped) = partitionLenient InvalidPublishTime (parseEither parseJSON) raw
    pure (kept, filter ((`Set.member` versionKeys) . invalidKey) dropped)

{- A decoded version object: the wire 'VersionManifest' plus its @_npmUser@
publisher. One pass decodes both from the /same/ object, so there is a single
notion of what a version object is.
-}
data VersionEntry = VersionEntry
    { veManifest :: VersionManifest
    , vePublisher :: Maybe Wire.Person
    }

instance FromJSON VersionEntry where
    parseJSON v =
        withObject "npm version object" (\o -> VersionEntry <$> parseJSON v <*> o .:? "_npmUser") v

{- | The outcome of projecting an upstream packument against the requested package
name (see the module header, "Name as a validation input").

The requested name validates the document. It never rewrites it. A document whose
self-reported name agrees with the request is 'Projected'. One that disagrees is a
'NameMismatch'. The 'PackageInfo' of a 'Projected' carries the name the upstream
genuinely reported (which, having matched, equals the requested name), never a
substituted value.
-}
data Projection
    = -- | The document decoded and its self-reported name matched the request.
      Projected PackageInfo
    | -- | The document decoded but self-reported this /different/ name (carried verbatim for the audit log).
      NameMismatch Text
    deriving stock (Eq, Show)

{- | Project an __already-decoded__ packument @Value@ into a 'Projection' for the
requested package, without re-parsing any bytes. The serve layer uses this entry point
when it already decoded the upstream body to a raw @Value@. That @Value@ is the document
it edits in place to serve, and the layer wants the typed view of the /same/ document.
Projecting from the @Value@ reuses that one parse rather than tokenising the bytes a
second time. Pure and total: a @Value@ that is not a decodable npm packument comes back
as a 'ParseError', never thrown.

The requested name validates the self-reported @name@: a match is 'Projected', a
disagreement is 'NameMismatch'. The serve layer drops a 'NameMismatch' origin's
contribution (an untrusted, misreporting upstream) and keeps the served name a value
some upstream genuinely reported.
-}
parsePackageInfoFromValue :: PackageName -> Value -> Either ParseError Projection
parsePackageInfoFromValue requestedName value =
    decodePackumentValue value >>= projectValidated requestedName

{- Project + validate a decoded packument against the requested name. The shared
'checkNameAgreement' checks the genuine self-reported name against the request, taking
it from 'projectPackageInfo', which fails an absent\/empty name as a 'ParseError'.
Agreement yields 'Projected' carrying the genuine 'PackageInfo'. A disagreement yields
a 'NameMismatch' carrying what the upstream reported. The name is never substituted. -}
projectValidated :: PackageName -> WirePackument -> Either ParseError Projection
projectValidated requestedName pkmt = do
    info <- projectPackageInfo pkmt
    pure $ case checkNameAgreement requestedName (infoName info) of
        NameAgrees -> Projected info
        NameDisagrees reported -> NameMismatch reported

-- Project a decoded 'WirePackument' into the domain 'PackageInfo', taking the name
-- from the upstream's self-reported @name@ (validated against the request by
-- 'projectValidated'). Shared by the validating entry points and the version-detail
-- accessor so the projection lives in one place.
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

{- | Project a __single version object__ into its 'PackageDetails': one entry of a
packument's @versions@ map, as a raw 'Value'. Takes the requested package name, the
version key it sits under, and its publish time (the packument's @time[version]@, if
present). 'Nothing' when the version object does not decode in a required\/security-
decisive field, exactly the per-version drop the full packument projection applies.

This is the per-version projection step, factored out so a __selective__
single-version decode ("Ecluse.Core.Registry.Npm.SelectiveDecode") can reuse it. That
decode extracts only the one version object and its publish time from the packument
bytes. It projects them through the __same__ code the whole-packument path runs over
every version. The resulting 'PackageDetails' is identical to @'Map.lookup'@-ing the
version out of a full 'parsePackageInfoFromValue' projection. The element-wise leniency
is identical too: a version object missing its @dist@\/@tarball@ (or otherwise
unprojectable) yields 'Nothing', a genuine absence, never a half-built snapshot.
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

{- Decode a response body into a 'WirePackument', adapting aeson's 'String'
error into a domain 'ParseError'.
-}
decodePackument :: RegistryResponse -> Either ParseError WirePackument
decodePackument =
    first (ParseError . toText) . eitherDecodeStrict . responseBody

{- Project an already-decoded 'Value' into a 'WirePackument' via its 'FromJSON'
instance, adapting aeson's 'String' error into a domain 'ParseError'. The result is
identical to 'decodePackument' on the bytes that produced the @Value@. The @aeson@
decoder builds a 'Value' and then runs the same 'FromJSON' instance either way. This
reuses the one parse instead of tokenising the bytes again.
-}
decodePackumentValue :: Value -> Either ParseError WirePackument
decodePackumentValue =
    first (ParseError . toText) . parseEither parseJSON

{- Project every entry of the packument's @versions@ map into a
'PackageDetails', keyed by the raw version string (the packument's own key).
-}
projectVersions :: PackageName -> WirePackument -> Map Text PackageDetails
projectVersions name pkmt =
    Map.mapWithKey projectAt (wpVersions pkmt)
  where
    projectAt rawVersion =
        projectDetails
            name
            (mkVersion Npm rawVersion)
            (Map.lookup rawVersion (wpTime pkmt))

{- Build a 'PackageDetails' from one projected version entry and its publish
time (if the packument's @time@ map carried one).
-}
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

{- Map npm install-script presence onto 'CodeExecSignal', failing closed across
the two independent wire signals. A version runs code on install when the @scripts@ map
declares an install hook (@preinstall@\/@install@\/@postinstall@), or when the
abbreviated form's @hasInstallScript@ flag is @true@. This mapping consults the
@scripts@ map __even when the flag is present and @false@__. The two fields are
independent on the wire, so a hostile upstream cannot suppress a manifest's own declared
install hook by setting @hasInstallScript:false@ beside it. A declared script is
authoritative. The flag only contributes the abbreviated-form signal, where the wire
form strips @scripts@, and never overrides a script the manifest itself carries.
-}
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

{- Project the @dist@ object into an 'Artifact', carrying __both__ integrity
digests: the legacy SHA-1 @shasum@ and the modern @integrity@ SRI string. Each
present, non-empty digest becomes an algorithm-tagged 'Hash'. A content-empty digest
counts as absent, so this projection drops no real digest and fabricates no empty one.
It carries the @dist.tarball@ URL verbatim. The egress-scheme fold in
"Ecluse.Core.Package.Filter" normalises its scheme against the https-only egress
policy afterward.
-}
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
    -- Build each present digest through the validating 'mkHash'. A malformed value is
    -- unconstructable, so it becomes absent rather than a degenerate 'Hash'. Malformed
    -- covers the empty string (@"shasum":""@ / @"integrity":""@), and equally a
    -- truncated or non-hex one. A digest that ties the version to no tamper-evident
    -- fingerprint must not slip past the public-integrity admission gate (security.md
    -- invariant 5). It must not feed a bogus fingerprint to the cross-upstream
    -- divergence check either. Dropping it here leaves the now-hashless version for
    -- Ecluse.Core.Package.Integrity to classify NoIntegrity.
    toHash :: HashAlg -> Text -> Maybe Hash
    toHash alg = rightToMaybe . mkHash alg
    -- 'mkSriHashes' splits a multi-component @integrity@ (rare on npm, legal SRI) into
    -- one 'Hash' per component. The strongest-digest selection at the admission floor
    -- and the worker's tamper gate then rank and verify each component exactly. Neither
    -- reads a joined string two different ways.
    sriHashes = maybe [] (either (const []) toList . mkSriHashes) (distIntegrity dist)
    sha1Hash = distShasum dist >>= toHash SHA1

{- The artifact filename for a tarball: the path segment after the URL's last
@\'\/\'@, or the whole string when it has none. When that segment is empty (a URL
ending in a slash), fall back to the conventional @\<version\>.tgz@ form.
-}
tarballFilename :: Text -> Version -> Text
tarballFilename url version =
    fromMaybe (unVersion version <> ".tgz") (lastPathSegment url)

{- Project the @dist-tags@ map (tag to raw version string) into a map of tag
to parsed 'Version'.
-}
projectDistTags :: WirePackument -> Map Text Version
projectDistTags = Map.map (mkVersion Npm) . wpDistTags

{- | Parse an npm package name into the domain 'PackageName', splitting a scoped
@\@scope\/name@ into its 'Scope' and bare name. Fails with a 'ParseError' on an
empty name. A non-scoped or well-formed scoped name always succeeds.

This is the npm name canonicaliser. Equality on the resulting 'PackageName' is
ecosystem-aware, because npm is case-sensitive. It is the agreement test both the read
path and the publish path compare against. The read path checks an upstream's
self-reported @name@ against the request, and the publish path checks a document body's
declared @_id@\/@name@\/@versions[].name@ against the URL-path name. Neither compares
byte-for-byte strings, so an encoding variant of the same name cannot disagree
silently.
-}
projectName :: Text -> Either ParseError PackageName
projectName raw
    | T.null raw = Left (ParseError "empty package name")
    | otherwise = case scopeOf raw of
        Just (scope, base) -> Right (mkPackageName Npm (Just scope) base)
        Nothing -> Right (mkPackageName Npm Nothing raw)

{- Split a scoped npm name @\@scope\/name@ into its 'Scope' and bare name, or
'Nothing' for an unscoped name. An @\'\@\'@-prefixed name with no @\'\/\'@, an
empty scope, or an empty bare name are all malformed and yield 'Nothing'. The
caller then treats the whole string as an unscoped name.
-}
scopeOf :: Text -> Maybe (Scope, Text)
scopeOf raw = do
    afterAt <- T.stripPrefix "@" raw
    let (scopeText, rest) = T.break (== '/') afterAt
        base = T.drop 1 rest
    guard (not (T.null scopeText))
    guard (not (T.null base))
    pure (mkScope scopeText, base)

-- Project a wire 'Wire.Person' into the domain 'Person' (a structural copy).
projectPerson :: Wire.Person -> Person
projectPerson p =
    Person
        { personName = Wire.personName p
        , personEmail = Wire.personEmail p
        , personUrl = Wire.personUrl p
        }
