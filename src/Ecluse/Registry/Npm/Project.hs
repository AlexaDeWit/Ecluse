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

* install-script presence → 'CodeExecSignal'. The abbreviated form's
  @hasInstallScript@ flag is authoritative when present; otherwise it is
  /derived/ from the @scripts@ map, which runs code on install exactly when it
  declares any of @preinstall@\/@install@\/@postinstall@, matching what npm
  itself sets the flag from. A version with neither signal maps to
  'NoCodeOnInstall' (both metadata forms always carry the @scripts@\/
  @hasInstallScript@ information, so its absence is a determination, not an
  unknown).
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
    parseVersionDetails,
    parseVersionList,
) where

import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict, withObject, (.!=), (.:?))
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

{- | The packument as this projection needs to read it: the wire fields plus the
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

{- | A decoded version object: the wire 'VersionManifest' plus its @_npmUser@
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
parsePackageInfo resp = do
    pkmt <- decodePackument resp
    name <- projectName (wpName pkmt)
    pure
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

{- | Decode a response body into a 'WirePackument', adapting aeson's 'String'
error into a domain 'ParseError'.
-}
decodePackument :: RegistryResponse -> Either ParseError WirePackument
decodePackument =
    first (ParseError . toText) . eitherDecodeStrict . responseBody

-- ── per-version projection ───────────────────────────────────────────────────

{- | Project every entry of the packument's @versions@ map into a
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

{- | Build a 'PackageDetails' from one projected version entry and its publish
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

-- | The SPDX expression or license name carried by a wire 'License'.
licenseText :: License -> Text
licenseText = \case
    LicenseSpdx spdx -> spdx
    LicenseObject name _url -> name

{- | Project the four npm dependency maps into a flat list of 'Dependency',
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

{- | Map npm install-script presence onto 'CodeExecSignal'. The abbreviated
form's @hasInstallScript@ flag wins when present; otherwise presence is derived
from the @scripts@ map (any of @preinstall@\/@install@\/@postinstall@), matching
what npm itself sets the flag from.
-}
installCode :: VersionManifest -> CodeExecSignal
installCode vm = case vmHasInstallScript vm of
    Just True -> RunsCodeOnInstall "declares an install script (hasInstallScript)"
    Just False -> NoCodeOnInstall
    Nothing
        | not (null hooks) ->
            RunsCodeOnInstall ("declares install script(s): " <> T.intercalate ", " hooks)
        | otherwise -> NoCodeOnInstall
  where
    hooks = filter (`Map.member` vmScripts vm) installHooks

-- | The lifecycle script names whose presence means installation runs code.
installHooks :: [Text]
installHooks = ["preinstall", "install", "postinstall"]

-- | Map an optional @deprecated@ notice onto 'Availability'.
availability :: VersionManifest -> Availability
availability vm = maybe Available Deprecated (vmDeprecated vm)

{- | Project the @dist@ object into an 'Artifact', carrying __both__ integrity
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

{- | The artifact filename for a tarball: the path segment after the URL's last
@\'\/\'@ (the whole string when it has none), or the conventional
@\<version\>.tgz@ form as a fallback when that segment is empty (a URL ending in
a slash).
-}
tarballFilename :: Text -> Version -> Text
tarballFilename url version =
    let afterLastSlash = snd (T.breakOnEnd "/" url)
     in if T.null afterLastSlash then unVersion version <> ".tgz" else afterLastSlash

-- ── packument-level projection ───────────────────────────────────────────────

{- | Project the @dist-tags@ map (tag to raw version string) into a map of tag
to parsed 'Version'.
-}
projectDistTags :: WirePackument -> Map Text Version
projectDistTags = Map.map (mkVersion Npm) . wpDistTags

{- | Project the per-version publish timestamps from the packument's @time@ map,
keeping only the entries keyed by a version present in @versions@ (dropping the
@created@\/@modified@ bookkeeping keys).
-}
projectPublishTimes :: WirePackument -> Map Text UTCTime
projectPublishTimes pkmt =
    Map.restrictKeys (wpTime pkmt) (Map.keysSet (wpVersions pkmt))

-- ── name and person projection ───────────────────────────────────────────────

{- | Parse an npm package name into the domain 'PackageName', splitting a scoped
@\@scope\/name@ into its 'Scope' and bare name. Fails with a 'ParseError' on an
empty name; a non-scoped or well-formed scoped name always succeeds.
-}
projectName :: Text -> Either ParseError PackageName
projectName raw
    | T.null raw = Left (ParseError "empty package name")
    | otherwise = case scopeOf raw of
        Just (scope, base) -> Right (mkPackageName Npm (Just scope) base)
        Nothing -> Right (mkPackageName Npm Nothing raw)

{- | Split a scoped npm name @\@scope\/name@ into its 'Scope' and bare name, or
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

-- | Project a wire 'Wire.Person' into the domain 'Person' (a structural copy).
projectPerson :: Wire.Person -> Person
projectPerson p =
    Person
        { personName = Wire.personName p
        , personEmail = Wire.personEmail p
        , personUrl = Wire.personUrl p
        }
