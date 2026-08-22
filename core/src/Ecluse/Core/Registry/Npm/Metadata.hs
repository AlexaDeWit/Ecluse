-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm realisation of the serve-path read operations: fetch a package's full
packument and project it into the domain manifest. Every failure comes back as a typed
'MetadataError'.

npm satisfies both serve-path needs from the /same/ full-packument endpoint. The
publish-age rules require the packument's @time@ map, which npm exposes only in the
full form, so even the single-version need fetches the full bytes. This module owns the
npm side of both serve-path operations, the fetch and the projection. It also owns the
constructor ('newNpmMetadataClient') that leads them into the serve layer's agnostic
caching, metrics, and failure-log policy ("Ecluse.Core.Server.Metadata"). The
cross-cutting caching policy belongs there.

  * 'fetchNpmManifest' \/ 'projectNpmManifest' back the full-manifest operation. The
    projection runs one sequence over a fetched packument: decode, bound the nesting
    depth, project and validate the self-reported name, then bound the version count.
    It is a total 'Either', so the serve path maps each cause onto a response rather
    than catching a typed throw.

  * 'fetchNpmVersion' \/ 'projectNpmVersion' back the single-version operation. It still
    fetches the full bytes, because npm carries @time@ only in the full form, but it
    parses them __selectively__ ("Ecluse.Core.Registry.Npm.SelectiveDecode"). It
    materialises only the requested version's object and @time@ entry, and skips the
    others unallocated. A cold tarball gate therefore does not pay a whole-packument
    decode to consult one version. It projects the selected version through the /same/
    per-version code the full path runs. Its 'Ecluse.Core.Package.PackageDetails' is
    identical to selecting it out of a full projection.
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
import Ecluse.Core.Package.Filter (enforceArtifactScheme, enforceArtifactSchemeDetails)
import Ecluse.Core.Registry (RegistryResponse (responseBody))
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.Metadata (
    Manifest (Manifest, manifestDigest, manifestInfo, manifestRaw),
    MetadataClient,
    MetadataError (MetadataBoundExceeded, MetadataNameMismatch, MetadataUndecodable),
    digestOf,
    fetchFaultError,
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
configuration. The npm full-manifest and single-version fetches are the raw primitives.
'Ecluse.Core.Server.Metadata.newMetadataClient' wires the serve-path caching, metrics,
and the failure and dropped-entry logs.
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

{- | Fetch a package's full packument and project it into a 'Manifest': the typed view,
the raw document, and the wire bytes' 'ContentDigest'. A failure is the typed
'MetadataError' for why it could not.

The fetch reads the body bounded against the config's response budget, so it refuses an
oversized upstream fail-closed before anything buffers it whole. The shared
'Ecluse.Core.Registry.Metadata.fetchFaultError' fold threads any fetch fault straight
onto its 'MetadataError': a response-bound breach, an unformable upstream URL, or an
unreachable upstream. Total by type: this 'Either' carries every outcome the serve path
renders, the transport channel included.

This fetch computes the digest here, over the strict body the bounded read already
produced. That is the one place the wire bytes exist, so no later stage re-encodes the
document just to fingerprint it.
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
    -- Inject npm's raw packument 'Value' into the opaque served-document carrier at the
    -- fetch boundary: the neutral pipeline and cache thread it without reading it.
    manifestOf digest (info, raw) = Manifest{manifestInfo = info, manifestRaw = fst npmCached raw, manifestDigest = digest}

{- | Project a fetched packument's bytes into @(manifest, raw document)@, applying the
serve path's response bounds and name validation. Pure and total.

The sequence is: decode to a 'Value', bound its nesting depth, then project the typed
'PackageInfo'. That projection validates the self-reported name against the request, and
the last step bounds the version count. The returned raw 'Value' is the nesting-checked
document the serve path edits in place. The typed view and the served bytes therefore
describe the same parse. Each refusal maps to the constructor the serve path renders. A
decode failure or an absent\/undecodable name is 'MetadataUndecodable'. A self-reported
/different/ name is 'MetadataNameMismatch'. A nesting-depth or version-count breach is
'MetadataBoundExceeded'.
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
'PackageDetails', or the typed 'MetadataError' for why it could not. This is the cheap
counterpart to 'fetchNpmManifest' for the single-version serve operation.

npm carries the @time@ map only in the full document, so it __still fetches the full
bytes__, bounded against the config's budget exactly as 'fetchNpmManifest'. The win is
that it parses them __selectively__ ('projectNpmVersion'), materialising the one
requested version rather than every version. A 'Nothing' is a version genuinely absent
from a sound document, a forwarded miss. A 'MetadataError' is metadata that could not be
obtained at all, an unreachable upstream included, exactly as 'fetchNpmManifest'.
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

The outcome is the same one the whole-document path would reach for that version,
computed selectively.
'Ecluse.Core.Registry.Npm.SelectiveDecode.selectVersionFromPackument' walks the token
stream and materialises only the document @name@, the requested version's object, and
its @time@ entry. The walk depth-bounds every value at the 'maxNestingDepth' ceiling
'projectNpmManifest' applies through 'checkNestingDepth', and reports malformed JSON as
'MetadataUndecodable'. It then validates and projects those three pieces exactly as
'projectNpmManifest' would:

  * The projection validates the self-reported @name@ against the request. An
    absent\/undecodable name is 'MetadataUndecodable', and a self-reported /different/
    name is 'MetadataNameMismatch' (the anti-shadowing distinction).
  * It bounds the @versions@ count against 'maxVersionCount', over the raw entry count.
    That is a fail-closed defence-in-depth backstop on this path. This path evaluates
    only the one version regardless, so it never needs the projected count the full path
    bounds.
  * It projects the requested version's object through
    'Ecluse.Core.Registry.Npm.Project.projectVersionEntry', the same per-version
    projection the full path runs. A present version yields a 'PackageDetails' identical
    to @'Data.Map.Strict.lookup'@-ing it out of a full 'projectNpmManifest', and an
    absent\/unprojectable version yields 'Nothing'.
-}
projectNpmVersion :: Limits -> PackageName -> Version -> ByteString -> Either MetadataError (Maybe PackageDetails)
projectNpmVersion limits name version body = do
    selected <- first (selectiveError limits) (selectVersionFromPackument (maxNestingDepth limits) version body)
    -- The self-reported name is the validation authority (anti-shadowing), checked before
    -- the version-count backstop, the same order 'projectNpmManifest' validates the name
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

-- The document's self-reported name, validated as the whole-document decode does. An
-- absent name defaults to the empty string and so fails 'projectName' (undecodable). A
-- present non-string fails the @Text@ decode (undecodable). A well-formed name is the
-- 'PackageName' this check compares with the request.
validateReportedName :: Maybe Value -> Either MetadataError PackageName
validateReportedName = \case
    Nothing -> Left MetadataUndecodable
    Just nameValue -> case parseMaybe parseJSON nameValue of
        Nothing -> Left MetadataUndecodable
        Just raw -> first (const MetadataUndecodable) (projectName raw)

-- The requested version's publish stamp, folded leniently to match the whole-document
-- path. An absent stamp is no stamp ('Nothing'). A present but undecodable stamp is
-- also 'Nothing': the version has no known publish time, never a document failure. The
-- full path drops a malformed @time@ entry per-entry, so the version it would project
-- there carries no time. The selective projection must agree. (A
-- structurally-malformed-JSON stamp is still a 'SelectiveUndecodable' from the walk, as
-- it is an 'eitherDecodeStrict' failure on the full path.)
parsePublishTime :: Maybe Value -> Either MetadataError (Maybe UTCTime)
parsePublishTime = \case
    Nothing -> Right Nothing
    Just timeValue -> Right (parseMaybe parseJSON timeValue)

-- Map a selective-decode refusal onto the 'MetadataError' the whole-document path raises
-- for the same cause. Malformed\/non-object bytes are 'MetadataUndecodable'. A depth
-- breach is the 'maxNestingDepth' bound 'checkNestingDepth' reports.
selectiveError :: Limits -> SelectiveError -> MetadataError
selectiveError limits = \case
    SelectiveUndecodable -> MetadataUndecodable
    SelectiveTooDeeplyNested -> MetadataBoundExceeded (TooDeeplyNested (maxNestingDepth limits))
