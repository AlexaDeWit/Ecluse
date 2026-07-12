-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm realization of the serve-path read operations: fetch a package's full
packument and project it into the domain manifest, reporting every failure as a typed
'MetadataError'.

npm satisfies both serve-path needs from the /same/ full-packument endpoint: the
publish-age rules require the packument's @time@ map, which npm exposes only in the
full form, so even the single-version need fetches the full bytes. This module owns the
npm side of both serve-path operations -- the fetch and the projection -- and the
constructor ('newNpmMetadataClient') that leads them into the serve layer's agnostic
caching, metrics, and failure-log policy ("Ecluse.Core.Server.Metadata"), which is
where the cross-cutting caching policy belongs:

  * 'fetchNpmManifest' \/ 'projectNpmManifest' back the full-manifest operation. The
    projection is the sequence the serve path has always applied to a fetched packument --
    decode, bound the nesting depth, project and validate the self-reported name, bound the
    version count -- re-expressed as a total 'Either' so the serve path maps each cause onto
    a response rather than catching a typed throw.

  * 'fetchNpmVersion' \/ 'projectNpmVersion' back the single-version operation. The full
    bytes are still fetched (npm carries @time@ only in the full form), but they are parsed
    __selectively__ ("Ecluse.Core.Registry.Npm.SelectiveDecode"): only the requested
    version's object and @time@ entry are materialised, the others skipped unallocated, so
    a cold tarball gate no longer pays a whole-packument decode to consult one version. The
    selected version is projected through the /same/ per-version code the full path runs, so
    its 'Ecluse.Core.Package.PackageDetails' is identical to selecting it out of a full
    projection -- the optimization the stable boundary was always meant to admit.
-}
module Ecluse.Core.Registry.Npm.Metadata (
    -- * Per-request read handle
    newNpmMetadataClient,

    -- * npm full-manifest fetch
    fetchNpmManifest,

    -- * npm single-version fetch
    fetchNpmVersion,

    -- * Pure projection
    projectNpmManifest,
    projectNpmVersion,
) where

import Data.Aeson (Value, eitherDecodeStrict, parseJSON)
import Data.Aeson.Types (parseMaybe)
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    InvalidEntry,
    PackageDetails,
    PackageInfo,
    PackageName,
    renderPackageName,
 )
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    RegistryResponse (responseBody),
 )
import Ecluse.Core.Registry.Egress (enforceArtifactScheme, enforceArtifactSchemeDetails)
import Ecluse.Core.Registry.Metadata (
    Manifest (Manifest, manifestDigest, manifestInfo, manifestRaw),
    MetadataClient,
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable, MetadataUnreachable, MetadataUrlUnformable),
    digestOf,
 )
import Ecluse.Core.Registry.Npm (
    NpmClientConfig (npmBaseUrl, npmLimits),
    fetchMetadataFormBounded,
 )
import Ecluse.Core.Registry.Npm.Project (
    Projection (NameMismatch, Projected),
    parsePackageInfoFromValue,
    projectName,
    projectVersionEntry,
 )
import Ecluse.Core.Registry.Npm.Request (
    MetadataForm (Full),
    noValidators,
 )
import Ecluse.Core.Registry.Npm.SelectiveDecode (
    SelectedVersion (svName, svTime, svVersion, svVersionCount),
    SelectiveError (SelectiveTooDeeplyNested, SelectiveUndecodable),
    selectVersionFromPackument,
 )
import Ecluse.Core.Security (
    LimitError (TooDeeplyNested, TooManyVersions),
    Limits,
    checkNestingDepth,
    checkVersionCount,
    maxNestingDepth,
    maxVersionCount,
 )
import Ecluse.Core.Server.Metadata (ManifestCaching, newMetadataClient)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Version (Version, mkVersion, renderVersion)

{- | Build a per-request read handle for the npm protocol over one origin's fetch
configuration: the npm full-manifest and single-version fetches as the raw primitives, with
the serve-path caching, metrics, and the failure and dropped-entry logs wired by
'Ecluse.Core.Server.Metadata.newMetadataClient'.
-}
newNpmMetadataClient ::
    TracingPort ->
    MetricsPort ->
    Metric.Upstream ->
    ManifestCaching ->
    (PackageName -> MetadataError -> IO ()) ->
    (PackageName -> [InvalidEntry] -> IO ()) ->
    (PackageName -> IO ()) ->
    NpmClientConfig ->
    MetadataClient
newNpmMetadataClient tracing metrics upstream caching logFailure logInvalid logFetch config =
    newMetadataClient metrics upstream caching logFailure logInvalid logFetch (fetchNpmManifest tracing config) (fetchNpmVersion tracing config)

{- | Fetch a package's full packument and project it into a 'Manifest' (typed view,
raw document, and the wire bytes' 'ContentDigest'), or the typed 'MetadataError' for
why it could not.

The body is read bounded against the config's response budget (so an oversized upstream
is refused fail-closed before it is buffered whole); a breach surfaces as
'MetadataBoundExceeded', an unformable upstream URL as 'MetadataUrlUnformable', and an
unreachable upstream as 'MetadataUnreachable', each threaded straight from the bounded
fetch's 'FetchFault' value. Total by type: this 'Either' carries every outcome the
serve path renders, the transport channel included.

The digest is computed here, over the strict body the bounded read already produced:
the one place the wire bytes exist, so no later stage re-encodes the document just to
fingerprint it.
-}
fetchNpmManifest :: TracingPort -> NpmClientConfig -> PackageName -> IO (Either MetadataError Manifest)
fetchNpmManifest tracing config name =
    spanMetadataFetch tracing name (fetchMetadataFormBounded config Full noValidators name) >>= \case
        Left fault -> pure (Left (fetchFaultError fault))
        Right response ->
            let body = responseBody response
             in spanMetadataDecode tracing name $
                    pure
                        ( manifestOf (digestOf body) . first (enforceArtifactScheme (npmBaseUrl config))
                            <$> projectNpmManifest (npmLimits config) name body
                        )
  where
    manifestOf digest (info, raw) = Manifest{manifestInfo = info, manifestRaw = raw, manifestDigest = digest}

-- Map the bounded fetch's 'FetchFault' onto the serve path's 'MetadataError': a
-- response-bound breach is 'MetadataBoundExceeded', an unformable upstream URL is
-- 'MetadataUrlUnformable' (a config fault held distinct from a decode or an outage),
-- and a transport fault is 'MetadataUnreachable' (the outage, kept transient).
fetchFaultError :: FetchFault -> MetadataError
fetchFaultError = \case
    FetchBoundExceeded err -> MetadataBoundExceeded err
    FetchUrlUnformable urlErr -> MetadataUrlUnformable urlErr
    FetchTransport transport -> MetadataUnreachable transport

{- | Project a fetched packument's bytes into @(manifest, raw document)@, applying the
serve path's response bounds and name validation. Pure and total.

The sequence -- decode to a 'Value', bound its nesting depth, project the typed
'PackageInfo' and validate its self-reported name against the request, then bound the
version count -- is the one the serve path has always run; the raw 'Value' returned is
the nesting-checked document the serve path edits in place, so the typed view and the
served bytes describe the same parse. Each refusal maps to the constructor the serve
path renders: a decode failure or an absent\/undecodable name is 'MetadataUndecodable';
a self-reported /different/ name is 'MetadataNameMismatch'; a nesting-depth or
version-count breach is 'MetadataBoundExceeded'.
-}
projectNpmManifest :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, Value)
projectNpmManifest limits name body = do
    value <- first (const MetadataUndecodable) (eitherDecodeStrict body)
    bounded <- first MetadataBoundExceeded (checkNestingDepth limits value)
    info <- case parsePackageInfoFromValue name bounded of
        Left _ -> Left MetadataUndecodable
        Right (NameMismatch reported) -> Left (MetadataNameMismatch reported)
        Right (Projected projected) -> Right projected
    boundedInfo <- first MetadataBoundExceeded (checkVersionCount limits info)
    pure (boundedInfo, bounded)

{- | Fetch a package's full packument and project __only the requested version__ into its
'PackageDetails', or the typed 'MetadataError' for why it could not -- the cheap counterpart
to 'fetchNpmManifest' for the single-version serve operation.

npm carries the @time@ map only in the full document, so the __full bytes are still
fetched__ (bounded against the config's budget, exactly as 'fetchNpmManifest'); the win is
that they are parsed __selectively__ ('projectNpmVersion'), materialising the one requested
version rather than every version. A 'Nothing' is a version genuinely absent from a sound
document (a forwarded miss); a 'MetadataError' is metadata that could not be obtained at
all, an unreachable upstream included, exactly as 'fetchNpmManifest'.
-}
fetchNpmVersion :: TracingPort -> NpmClientConfig -> PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
fetchNpmVersion tracing config name version =
    spanMetadataFetch tracing name (fetchMetadataFormBounded config Full noValidators name) >>= \case
        Left fault -> pure (Left (fetchFaultError fault))
        Right response ->
            spanMetadataDecode tracing name $
                pure ((>>= enforceArtifactSchemeDetails (npmBaseUrl config)) <$> projectNpmVersion (npmLimits config) name version (responseBody response))

{- | Project a fetched packument's bytes into __one version's__ 'PackageDetails' (or the
typed 'MetadataError'), without decoding the other versions. Pure and total.

The outcome is the same the whole-document path would reach for that one version, computed
selectively: 'Ecluse.Core.Registry.Npm.SelectiveDecode.selectVersionFromPackument' walks the
token stream -- depth-bounding every value (the 'maxNestingDepth' ceiling
'projectNpmManifest' applies through 'checkNestingDepth') and reporting malformed JSON as
'MetadataUndecodable' -- and materialises only the document @name@, the requested version's
object, and its @time@ entry. Those are then validated and projected exactly as
'projectNpmManifest' would:

  * the self-reported @name@ is validated against the request -- an absent\/undecodable name
    is 'MetadataUndecodable', a self-reported /different/ name is 'MetadataNameMismatch' (the
    anti-shadowing distinction);
  * the @versions@ count is bounded against 'maxVersionCount' (the raw entry count -- a
    fail-closed defence-in-depth backstop on this path, which evaluates only the one version
    regardless, so it never needs the projected count the full path bounds);
  * the requested version's object is projected through
    'Ecluse.Core.Registry.Npm.Project.projectVersionEntry' -- the same per-version projection
    the full path runs -- so a present version yields a 'PackageDetails' identical to
    @'Data.Map.Strict.lookup'@-ing it out of a full 'projectNpmManifest', and an
    absent\/unprojectable version yields 'Nothing'.
-}
projectNpmVersion :: Limits -> PackageName -> Version -> ByteString -> Either MetadataError (Maybe PackageDetails)
projectNpmVersion limits name version body = do
    selected <- first (selectiveError limits) (selectVersionFromPackument (maxNestingDepth limits) version body)
    -- The self-reported name is the validation authority (anti-shadowing), checked before
    -- the version-count backstop -- the same order 'projectNpmManifest' validates the name
    -- before bounding the count.
    reported <- validateReportedName (svName selected)
    when (reported /= name) (Left (MetadataNameMismatch (renderPackageName reported)))
    when
        (svVersionCount selected > maxVersionCount limits)
        (Left (MetadataBoundExceeded (TooManyVersions (svVersionCount selected) (maxVersionCount limits))))
    publishedAt <- parsePublishTime (svTime selected)
    -- 'mkVersion' over the requested version's rendered key matches the whole-document path,
    -- which keys 'projectVersions' by that same string and so projects the version under it.
    pure (svVersion selected >>= projectVersionEntry name (mkVersion Npm (renderVersion version)) publishedAt)

-- The document's self-reported name, validated as the whole-document decode does: an
-- absent name defaults to the empty string and so fails 'projectName' (undecodable), a
-- present non-string fails the @Text@ decode (undecodable), and a well-formed name is
-- the 'PackageName' the request is matched against.
validateReportedName :: Maybe Value -> Either MetadataError PackageName
validateReportedName = \case
    Nothing -> Left MetadataUndecodable
    Just nameValue -> case parseMaybe parseJSON nameValue of
        Nothing -> Left MetadataUndecodable
        Just raw -> first (const MetadataUndecodable) (projectName raw)

-- The requested version's publish stamp, folded leniently to match the whole-document
-- path: absent is no stamp ('Nothing'), and a present-but-un-decodable stamp is also
-- 'Nothing' (the version has no known publish time) rather than a document failure:
-- the full path drops a malformed @time@ entry per-entry, so the requested version it
-- would project there carries no time, and the selective projection must agree. (A
-- structurally-malformed-JSON stamp is still a 'SelectiveUndecodable' from the walk, as
-- it is an 'eitherDecodeStrict' failure on the full path.)
parsePublishTime :: Maybe Value -> Either MetadataError (Maybe UTCTime)
parsePublishTime = \case
    Nothing -> Right Nothing
    Just timeValue -> Right (parseMaybe parseJSON timeValue)

-- Map a selective-decode refusal onto the 'MetadataError' the whole-document path raises
-- for the same cause: malformed\/non-object bytes are 'MetadataUndecodable', a depth breach
-- is the 'maxNestingDepth' bound 'checkNestingDepth' reports.
selectiveError :: Limits -> SelectiveError -> MetadataError
selectiveError limits = \case
    SelectiveUndecodable -> MetadataUndecodable
    SelectiveTooDeeplyNested -> MetadataBoundExceeded (TooDeeplyNested (maxNestingDepth limits))
