-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Projection of npm wire JSON into the ecosystem-agnostic domain model.

This module is the second half of the npm protocol boundary. Where
"Ecluse.Core.Registry.Npm.Wire" captures /what the registry said/ as faithful wire
types, this module turns those into the domain vocabulary of "Ecluse.Core.Package" --
'PackageInfo' (the packument-level view) and 'PackageDetails' (the per-version
snapshot the rules engine evaluates). Together they realise the @parse*@ fields
of the "Ecluse.Core.Registry" handle: nothing above the adapter ever sees npm wire
data.

The projection is __pure and total__ (it returns 'Either' 'ParseError', never
throws), the execution half of /parse, don't validate/ -- once a response has
been projected, downstream code holds precise domain types and never re-inspects
the wire shape.

== Per-version graceful degradation

The @versions@, @dist-tags@, and @time@ maps are decoded __element-wise__: a
version whose manifest is missing or malformed in a required\/security-decisive
field (no @dist@ or @tarball@, an unusable @version@), a @dist-tags@ entry whose
value is not a string, or a @time@ entry that is not a decodable instant is
__dropped__ rather than failing the whole packument. Because presence in the
decision surface is what makes a version a serve-candidate, a dropped version is
automatically never served -- fail-closed for that one version (a version that
cannot be decoded cannot be evaluated for integrity, CVEs, or rules) while every
healthy version still resolves; a dropped date is simply a version with no known
publish time, and a dropped tag loses only that one tag. Only a document whose
/top-level/ structure is unusable (a @versions@ that is not an object, an
absent\/empty @name@) is denied wholesale. A version's purely __advisory__ fields
degrade in the wire layer ("Ecluse.Core.Registry.Npm.Wire") without dropping the
version. Every drop is __recorded__ as an 'Ecluse.Core.Package.InvalidEntry' in
'Ecluse.Core.Package.infoInvalidEntries' (a version-manifest, dist-tag, or
publish-time drop, each carrying its key and reason), so the serve path can log
what an upstream served malformed rather than dropping it silently.

== Signal mapping

The npm-specific fields collapse onto the normalised, ecosystem-blind signals:

* install-script presence → 'CodeExecSignal', read __fail-closed__ across two
  independent wire signals. A version runs code on install when /either/ the
  abbreviated form's @hasInstallScript@ flag is @true@ /or/ the @scripts@ map
  declares any of @preinstall@\/@install@\/@postinstall@ (matching what npm
  itself sets the flag from). The two fields are independent on the wire, so the
  @scripts@ map is consulted __even when @hasInstallScript@ is present and
  @false@__: a hostile upstream must not be able to mask a real install hook by
  lying in the sibling flag, so a declared script is authoritative and the
  signal is the union of the two, never the flag overriding a script. A version
  with neither signal maps to 'NoCodeOnInstall' (both metadata forms always
  carry the @scripts@\/@hasInstallScript@ information, so its absence is a
  determination, not an unknown).
* @deprecated@ → 'Availability': a notice yields 'Deprecated' (carrying the
  message), its absence 'Available'. npm has no per-version yank, so @Yanked@
  never arises here.
* @dist@ → a single-element 'NonEmpty' of 'Artifact' (npm publishes exactly one
  tarball per version). __Both__ integrity digests survive when present and
  __well-formed__: @dist.shasum@ as a 'SHA1' 'Hash' /and/ @dist.integrity@ as an
  'SRI' 'Hash'. Carrying both is load-bearing -- a cross-upstream merge compares the
  same version's integrity across the private and public registries to detect a
  supply-chain divergence, which dropping either digest would blind. Each digest is
  built through the validating 'mkHash', so a __malformed__ one -- empty
  (@"shasum":""@ \/ @"integrity":""@), truncated, non-hex, or bad-base64 -- is
  unconstructable and so treated as __absent__, never as a degenerate 'Hash': a
  digest that ties the version to no tamper-evident fingerprint must not slip past
  the public-integrity admission gate.
* @_npmUser@ → 'pkgPublisher' (who pushed this version -- provenance). It rides
  on the version object but is not modelled by the wire manifest, so the
  projection reads it directly from the version object here.
* @time[version]@ → 'pkgPublishedAt'. The publish timestamp lives in the
  packument's @time@ map, not the manifest; a version with no @time@ entry (or
  an abbreviated document, which omits @time@) projects to 'Nothing'.

Trust is left 'TrustUnknown': establishing it needs signature verification
against npm's published keys, a fetch this pure projection does not perform.

== Name as a validation input

The requested 'PackageName' -- the identity the proxy resolved from the route -- is
the __validation authority__ for the served packument's name, never a rewrite of
it. The packument projection takes the requested name and checks the upstream's
self-reported top-level @name@ against it: a document whose self-report agrees is a
'Projected' 'PackageInfo' carrying the name the upstream genuinely reported; a
document whose self-report __disagrees__ is a 'NameMismatch', so the caller can
treat that origin as untrusted for this request and drop its contribution. The
served name is therefore always a value an upstream genuinely reported, never a
substituted or manufactured one. An /absent/ or otherwise undecodable name remains
a 'ParseError', as before -- distinct from a present-but-different name.
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
    renderPackageName,
 )
import Ecluse.Core.Registry (ParseError (..), RegistryResponse (responseBody))
import Ecluse.Core.Registry.Npm.Wire (
    Dist (..),
    License (LicenseObject, LicenseSpdx),
    VersionManifest (..),
 )
import Ecluse.Core.Registry.Npm.Wire qualified as Wire
import Ecluse.Core.Version (Version, mkVersion, unVersion)

{- The packument as this projection needs to read it: the wire fields plus the
per-version @_npmUser@ that "Ecluse.Core.Registry.Npm.Wire" intentionally leaves off
the manifest. Decoding the version objects here (rather than reusing the wire
'Wire.Packument') is what lets the publisher survive, since the wire manifest has
already discarded it.
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

{- Partition a raw @key -> 'Value'@ map into the entries that decode and the ones that
do not: each undecodable entry is dropped and recorded as an 'InvalidEntry' of the given
'InvalidEntryKind', carrying its key, the __raw offending 'Value'__ (verbatim, for
diagnostics), and the aeson decode error as the reason. The dropped list is in
ascending-key order ('Map.foldrWithKey' visits keys ascending and each step prepends), so
it is deterministic. This is the one place per-entry leniency and drop-tracking are
realised, shared by the @dist-tags@\/@time@ axes (the @versions@ axis layers a domain
decode on top). -}
partitionLenient :: InvalidEntryKind -> (Value -> Either String a) -> Map Text Value -> (Map Text a, [InvalidEntry])
partitionLenient kind decode =
    Map.foldrWithKey step (Map.empty, [])
  where
    step key value (kept, dropped) = case decode value of
        Right a -> (Map.insert key a kept, dropped)
        Left err -> (kept, InvalidEntry kind key value (toText err) : dropped)

{- Decode the @versions@ map __element-wise leniently__: read it as a raw map of
version key to 'Value', then keep only the entries that project to a 'VersionEntry',
dropping any that do not and recording each as an 'InvalidVersionManifest'. A version
whose manifest is missing or malformed in a required\/security-decisive field (no
@dist@\/@tarball@, an unusable @version@) is __dropped from the decision surface__
rather than failing the whole packument: fail-closed for that version (a version that
cannot be decoded cannot be evaluated for integrity, CVEs, or rules, so it must never
be served) while every healthy version still decodes. An absent @versions@ is the empty
map; a @versions@ that is not an object at all still fails the decode (the document is
not a usable packument). -}
lenientVersionMap :: Object -> Parser (Map Text VersionEntry, [InvalidEntry])
lenientVersionMap o = do
    raw <- o .:? "versions" .!= mempty -- Map Text Value: each version object kept raw
    pure (partitionLenient InvalidVersionManifest (parseEither parseJSON) raw)

{- Decode the @dist-tags@ map __element-wise leniently__: read it as a raw map of tag
name to 'Value', keeping each entry whose value is a JSON string and dropping any that
is not (recording it as an 'InvalidDistTag'). A single non-string tag value therefore
loses only that tag rather than failing the whole document. A string that is not a
valid version is still kept here ('mkVersion' is total, so dist-tag /targeting/ is
reconciled later, never a decode failure). -}
lenientDistTags :: Object -> Parser (Map Text Text, [InvalidEntry])
lenientDistTags o = do
    raw <- o .:? "dist-tags" .!= mempty
    pure (partitionLenient InvalidDistTag (parseEither parseJSON) raw)

{- Decode the @time@ map __element-wise leniently__: read it as a raw map of key to
'Value', keeping each entry that decodes as an instant and dropping any that does not.
With the publish time folded onto each version, a malformed sibling date is simply a
version with no known publish time, never a document failure. Only a drop keyed by a
__present version__ is recorded (as an 'InvalidPublishTime'); the @created@\/@modified@
bookkeeping keys are package-level, not a version's publish time, so a malformed one is
not a per-version drop and is left untracked. -}
lenientTimeMap :: Set Text -> Object -> Parser (Map Text UTCTime, [InvalidEntry])
lenientTimeMap versionKeys o = do
    raw <- o .:? "time" .!= mempty
    let (kept, dropped) = partitionLenient InvalidPublishTime (parseEither parseJSON) raw
    pure (kept, filter ((`Set.member` versionKeys) . invalidKey) dropped)

{- A decoded version object: the wire 'VersionManifest' plus its @_npmUser@
publisher. Both are decoded from the /same/ object in one pass, so there is a
single notion of what a version object is.
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

The requested name validates the document; it never rewrites it. A document whose
self-reported name agrees with the request is 'Projected'; one that disagrees is a
'NameMismatch'. The 'PackageInfo' of a 'Projected' carries the name the upstream
genuinely reported (which, having matched, equals the requested name) -- never a
substituted value.
-}
data Projection
    = -- | The document decoded and its self-reported name matched the request.
      Projected PackageInfo
    | -- | The document decoded but self-reported this /different/ name (carried verbatim for the audit log).
      NameMismatch Text
    deriving stock (Eq, Show)

{- | Project an __already-decoded__ packument @Value@ into a 'Projection' for the
requested package, without re-parsing any bytes. This is the entry point the serve
layer uses when it has already decoded the upstream body to a raw @Value@ (the
document it edits in place to serve) and wants the typed view of the /same/
document: projecting from the @Value@ reuses that one parse rather than tokenising
the bytes a second time. Pure and total -- a @Value@ that is not a decodable npm
packument is reported as a 'ParseError', never thrown.

The requested name validates the self-reported @name@: a match is 'Projected', a
disagreement is 'NameMismatch'. The serve layer drops a 'NameMismatch' origin's
contribution (an untrusted, misreporting upstream) and keeps the served name a value
some upstream genuinely reported.
-}
parsePackageInfoFromValue :: PackageName -> Value -> Either ParseError Projection
parsePackageInfoFromValue requestedName value =
    decodePackumentValue value >>= projectValidated requestedName

{- Project + validate a decoded packument against the requested name. The genuine
self-reported name (from 'projectPackageInfo', which fails an absent\/empty name as
a 'ParseError') is compared to the request via 'PackageName' equality -- ecosystem-
aware, so npm's case sensitivity is honoured. Equal yields 'Projected' carrying the
genuine 'PackageInfo'; unequal yields 'NameMismatch' carrying what the upstream
reported. The name is never substituted. -}
projectValidated :: PackageName -> WirePackument -> Either ParseError Projection
projectValidated requestedName pkmt = do
    info <- projectPackageInfo pkmt
    pure $
        if infoName info == requestedName
            then Projected info
            else NameMismatch (renderPackageName (infoName info))

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

{- | Project a __single version object__ -- one entry of a packument's @versions@ map,
as a raw 'Value' -- into its 'PackageDetails', given the requested package name, the
version key it sits under, and its publish time (the packument's @time[version]@, if
present). 'Nothing' when the version object does not decode in a required\/security-
decisive field, exactly the per-version drop the full packument projection applies.

This is the per-version projection step factored out so a __selective__ single-version
decode (see "Ecluse.Core.Registry.Npm.SelectiveDecode"), which extracts only the one
version object and its publish time from the packument bytes, projects it through the
__same__ code the whole-packument path runs over every version -- so the resulting
'PackageDetails' is identical to @'Map.lookup'@-ing the version out of a full
'parsePackageInfoFromValue' projection. The element-wise leniency is identical too: a version object missing
its @dist@\/@tarball@ (or otherwise unprojectable) yields 'Nothing', i.e. a genuine
absence, never a half-built snapshot.
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
identical to 'decodePackument' on the bytes that produced the @Value@: aeson decodes
to a 'Value' and then runs the same 'FromJSON' instance either way, so this reuses
the one parse instead of tokenising the bytes again.
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
the two independent wire signals: a version runs code on install when /either/
the @scripts@ map declares an install hook
(@preinstall@\/@install@\/@postinstall@) /or/ the abbreviated form's
@hasInstallScript@ flag is @true@. The @scripts@ map is consulted __even when
the flag is present and @false@__ -- the two fields are independent on the wire,
so a hostile upstream cannot suppress a manifest's own declared install hook by
setting @hasInstallScript:false@ beside it. A declared script is authoritative;
the flag only contributes the abbreviated-form signal (where @scripts@ is
stripped), it never overrides a script the manifest itself carries.
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
present, non-empty digest becomes an algorithm-tagged 'Hash'; a content-empty
digest is treated as absent, so neither a real digest is dropped nor an empty one
fabricated. The @dist.tarball@ URL is carried verbatim here; its scheme is normalised
against the https-only egress policy afterward by
'Ecluse.Core.Registry.Egress.enforceArtifactScheme'.
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
    -- Build each present digest through the validating 'mkHash'; a malformed value (the
    -- empty string @"shasum":""@ / @"integrity":""@, but equally a truncated or non-hex
    -- one) is unconstructable, so it becomes absent rather than a degenerate 'Hash'. A
    -- digest that ties the version to no tamper-evident fingerprint must not slip past the
    -- public-integrity admission gate (security.md invariant 5) or feed a bogus fingerprint
    -- to the cross-upstream divergence check; dropping it here leaves the now-hashless
    -- version to be classified NoIntegrity by Ecluse.Core.Package.Integrity.
    toHash :: HashAlg -> Text -> Maybe Hash
    toHash alg = rightToMaybe . mkHash alg
    -- A multi-component @integrity@ (rare on npm, legal SRI) is split into one
    -- 'Hash' per component ('mkSriHashes'), so the strongest-digest selection at
    -- the admission floor and the worker's tamper gate rank and verify each
    -- component exactly -- never a joined string read two different ways.
    sriHashes = maybe [] (either (const []) toList . mkSriHashes) (distIntegrity dist)
    sha1Hash = distShasum dist >>= toHash SHA1

{- The artifact filename for a tarball: the path segment after the URL's last
@\'\/\'@ (the whole string when it has none), or the conventional
@\<version\>.tgz@ form as a fallback when that segment is empty (a URL ending in
a slash).
-}
tarballFilename :: Text -> Version -> Text
tarballFilename url version =
    let afterLastSlash = snd (T.breakOnEnd "/" url)
     in if T.null afterLastSlash then unVersion version <> ".tgz" else afterLastSlash

{- Project the @dist-tags@ map (tag to raw version string) into a map of tag
to parsed 'Version'.
-}
projectDistTags :: WirePackument -> Map Text Version
projectDistTags = Map.map (mkVersion Npm) . wpDistTags

{- | Parse an npm package name into the domain 'PackageName', splitting a scoped
@\@scope\/name@ into its 'Scope' and bare name. Fails with a 'ParseError' on an
empty name; a non-scoped or well-formed scoped name always succeeds.

This is the npm name canonicaliser: equality on the resulting 'PackageName' is
ecosystem-aware (npm is case-sensitive), so it is the agreement test both the read
path (an upstream's self-reported @name@ against the request) and the publish path (a
document body's declared @_id@\/@name@\/@versions[].name@ against the URL-path name)
compare against -- never a byte-for-byte string compare, so an encoding variant of the
same name cannot disagree silently.
-}
projectName :: Text -> Either ParseError PackageName
projectName raw
    | T.null raw = Left (ParseError "empty package name")
    | otherwise = case scopeOf raw of
        Just (scope, base) -> Right (mkPackageName Npm (Just scope) base)
        Nothing -> Right (mkPackageName Npm Nothing raw)

{- Split a scoped npm name @\@scope\/name@ into its 'Scope' and bare name, or
'Nothing' for an unscoped name. An @\'\@\'@-prefixed name with no @\'\/\'@, an
empty scope, or an empty bare name are all malformed and yield 'Nothing' (the
caller then treats the whole string as an unscoped name).
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
