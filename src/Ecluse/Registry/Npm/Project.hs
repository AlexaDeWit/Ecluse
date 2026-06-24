{- | Projection of npm wire JSON into the ecosystem-agnostic domain model.

This module is the second half of the npm protocol boundary. Where
"Ecluse.Registry.Npm.Wire" captures /what the registry said/ as faithful wire
types, this module turns those into the domain vocabulary of "Ecluse.Package" —
'PackageInfo' (the packument-level view) and 'PackageDetails' (the per-version
snapshot the rules engine evaluates). Together they realise the @parse*@ fields
of the "Ecluse.Registry" handle: nothing above the adapter ever sees npm wire
data.

The projection is __pure and total__ (it returns 'Either' 'ParseError', never
throws), the execution half of /parse, don't validate/ — once a response has
been projected, downstream code holds precise domain types and never re-inspects
the wire shape.

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
  tarball per version). __Both__ integrity digests survive: @dist.shasum@ as a
  'SHA1' 'Hash' /and/ @dist.integrity@ as an 'SRI' 'Hash'. Carrying both is
  load-bearing — a cross-upstream merge compares the same version's integrity
  across the private and public registries to detect a supply-chain divergence,
  which dropping either digest would blind.
* @_npmUser@ → 'pkgPublisher' (who pushed this version — provenance). It rides
  on the version object but is not modelled by the wire manifest, so the
  projection reads it directly from the version object here.
* @time[version]@ → 'pkgPublishedAt'. The publish timestamp lives in the
  packument's @time@ map, not the manifest; a version with no @time@ entry (or
  an abbreviated document, which omits @time@) projects to 'Nothing'.

Trust is left 'TrustUnknown': establishing it needs signature verification
against npm's published keys, a fetch this pure projection does not perform.
-}
module Ecluse.Registry.Npm.Project (
    -- * Projection
    parsePackageInfo,
    parsePackageInfoFromValue,
    parseVersionDetails,
    parseVersionList,
) where

import Data.Aeson (FromJSON (parseJSON), Value, eitherDecodeStrict, withObject, (.!=), (.:?))
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (
    Artifact (..),
    ArtifactKind (Tarball),
    Availability (Available, Deprecated),
    CodeExecSignal (NoCodeOnInstall, RunsCodeOnInstall),
    DepKind (Dev, Optional, Peer, Runtime),
    Dependency (..),
    Hash (..),
    HashAlg (SHA1, SRI),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    Person (..),
    Scope,
    Trust (TrustUnknown),
    mkPackageName,
    mkScope,
 )
import Ecluse.Registry (ParseError (..), RegistryResponse (responseBody))
import Ecluse.Registry.Npm.Wire (
    Dist (..),
    License (LicenseObject, LicenseSpdx),
    VersionManifest (..),
 )
import Ecluse.Registry.Npm.Wire qualified as Wire
import Ecluse.Version (Version, mkVersion, renderVersion, unVersion)

{- The packument as this projection needs to read it: the wire fields plus the
per-version @_npmUser@ that "Ecluse.Registry.Npm.Wire" intentionally leaves off
the manifest. Decoding the version objects here (rather than reusing the wire
'Wire.Packument') is what lets the publisher survive, since the wire manifest has
already discarded it.
-}
data WirePackument = WirePackument
    { wpName :: Text
    , wpDistTags :: Map Text Text
    , wpVersions :: Map Text VersionEntry
    , wpTime :: Map Text UTCTime
    }

instance FromJSON WirePackument where
    parseJSON = withObject "npm packument" $ \o ->
        WirePackument
            <$> o .:? "name" .!= ""
            <*> o .:? "dist-tags" .!= mempty
            <*> o .:? "versions" .!= mempty
            <*> o .:? "time" .!= mempty

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

{- | Project a fetched metadata response into the packument-level 'PackageInfo'.
Pure and total: a body that is not a decodable npm packument is reported as a
'ParseError', never thrown.
-}
parsePackageInfo :: RegistryResponse -> Either ParseError PackageInfo
parsePackageInfo resp = decodePackument resp >>= projectPackageInfo

{- | Project an __already-decoded__ packument @Value@ into the packument-level
'PackageInfo', without re-parsing any bytes. This is the entry point the serve
layer uses when it has already decoded the upstream body to a raw @Value@ (the
document it edits in place to serve) and wants the typed view of the /same/
document: projecting from the @Value@ reuses that one parse rather than tokenising
the bytes a second time. Pure and total — a @Value@ that is not a decodable npm
packument is reported as a 'ParseError', never thrown.
-}
parsePackageInfoFromValue :: Value -> Either ParseError PackageInfo
parsePackageInfoFromValue value = decodePackumentValue value >>= projectPackageInfo

-- Project a decoded 'WirePackument' into the domain 'PackageInfo'. Shared by both
-- the byte- and 'Value'-decoding entry points so the projection lives in one place.
projectPackageInfo :: WirePackument -> Either ParseError PackageInfo
projectPackageInfo pkmt =
    let name = projectName (wpName pkmt)
     in Right
            PackageInfo
                { infoName = name
                , infoVersions = projectVersions name pkmt
                , infoDistTags = projectDistTags pkmt
                , infoPublishedAt = projectPublishTimes pkmt
                }

{- | Project a fetched metadata response into the 'PackageDetails' for a single
version. Fails with a 'ParseError' if the body does not decode or the requested
version is absent from the packument.
-}
parseVersionDetails :: RegistryResponse -> Version -> Either ParseError PackageDetails
parseVersionDetails resp version = do
    info <- parsePackageInfo resp
    case Map.lookup (renderVersion version) (infoVersions info) of
        Just details -> Right details
        Nothing ->
            Left (ParseError ("version not present in packument: " <> renderVersion version))

{- | Extract the list of available versions from a fetched metadata response, in
the packument's @versions@ key order. Fails with a 'ParseError' only if the body
does not decode.
-}
parseVersionList :: RegistryResponse -> Either ParseError [Version]
parseVersionList resp = do
    pkmt <- decodePackument resp
    pure (map (mkVersion Npm) (Map.keys (wpVersions pkmt)))

-- ── packument decoding ───────────────────────────────────────────────────────

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

-- ── per-version projection ───────────────────────────────────────────────────

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
        , pkgMaintainers = map projectPerson (vmMaintainers vm)
        , pkgDependencies = projectDependencies vm
        }
  where
    vm = veManifest entry

-- The SPDX expression or license name carried by a wire 'License'.
licenseText :: License -> Text
licenseText = \case
    LicenseSpdx spdx -> spdx
    LicenseObject name _url -> name

{- Project the four npm dependency maps into a flat list of 'Dependency',
tagging each with its 'DepKind'. The constraint strings are kept __raw__ (npm
never resolves ranges server-side), and npm carries no PEP 508 environment
markers, so 'depMarker' is always 'Nothing'.
-}
projectDependencies :: VersionManifest -> [Dependency]
projectDependencies vm =
    concatMap
        depsOfKind
        [ (Runtime, vmDependencies vm)
        , (Dev, vmDevDependencies vm)
        , (Peer, vmPeerDependencies vm)
        , (Optional, vmOptionalDependencies vm)
        ]
  where
    depsOfKind (kind, deps) =
        [ Dependency
            { depName = name
            , depConstraint = constraint
            , depKind = kind
            , depMarker = Nothing
            }
        | (name, constraint) <- Map.toList deps
        ]

{- Map npm install-script presence onto 'CodeExecSignal', failing closed across
the two independent wire signals: a version runs code on install when /either/
the @scripts@ map declares an install hook
(@preinstall@\/@install@\/@postinstall@) /or/ the abbreviated form's
@hasInstallScript@ flag is @true@. The @scripts@ map is consulted __even when
the flag is present and @false@__ — the two fields are independent on the wire,
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
present digest becomes an algorithm-tagged 'Hash'; neither is dropped.
-}
projectArtifact :: Version -> Dist -> Artifact
projectArtifact version dist =
    Artifact
        { artFilename = tarballFilename (distTarball dist) version
        , artUrl = distTarball dist
        , artKind = Tarball
        , artHashes = catMaybes [sriHash, sha1Hash]
        , artSize = distUnpackedSize dist
        , artInterpreter = Nothing
        , artYanked = False
        , artProvenance = Nothing
        }
  where
    sriHash = Hash SRI <$> distIntegrity dist
    sha1Hash = Hash SHA1 <$> distShasum dist

{- The artifact filename for a tarball: the path segment after the URL's last
@\'\/\'@ (the whole string when it has none), or the conventional
@\<version\>.tgz@ form as a fallback when that segment is empty (a URL ending in
a slash).
-}
tarballFilename :: Text -> Version -> Text
tarballFilename url version =
    let afterLastSlash = snd (T.breakOnEnd "/" url)
     in if T.null afterLastSlash then unVersion version <> ".tgz" else afterLastSlash

-- ── packument-level projection ───────────────────────────────────────────────

{- Project the @dist-tags@ map (tag to raw version string) into a map of tag
to parsed 'Version'.
-}
projectDistTags :: WirePackument -> Map Text Version
projectDistTags = Map.map (mkVersion Npm) . wpDistTags

{- Project the per-version publish timestamps from the packument's @time@ map,
keeping only the entries keyed by a version present in @versions@ (dropping the
@created@\/@modified@ bookkeeping keys).
-}
projectPublishTimes :: WirePackument -> Map Text UTCTime
projectPublishTimes pkmt =
    Map.restrictKeys (wpTime pkmt) (Map.keysSet (wpVersions pkmt))

-- ── name and person projection ───────────────────────────────────────────────

{- Project an npm package name into the domain 'PackageName', splitting a scoped
@\@scope\/name@ into its 'Scope' and bare name. __Total__: an empty name (the
default the wire decoder already supplies when the packument omits @name@) projects
to an empty-display unscoped name rather than aborting. A name-less-but-version-bearing
document must still contribute its versions — the registry model serves the
best-effort union and errors only when /nothing/ resolves, and the requested name is
known from the route — so the name is never the projection's gate; the serve layer's
own tarball rewrite likewise skips a missing name rather than failing on it.
-}
projectName :: Text -> PackageName
projectName raw = case scopeOf raw of
    Just (scope, base) -> mkPackageName Npm (Just scope) base
    Nothing -> mkPackageName Npm Nothing raw

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
