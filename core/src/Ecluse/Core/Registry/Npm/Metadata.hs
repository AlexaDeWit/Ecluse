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
    MetadataError (MetadataBoundExceeded, MetadataFetch, MetadataNameMismatch, MetadataUndecodable),
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
configuration. 'Ecluse.Core.Server.Metadata.newMetadataClient' wires the serve-path caching,
metrics, and logs around these raw fetches.
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

{- | Fetch a package's full packument and project it into a 'Manifest': the typed view, the
raw document, and the wire bytes' 'ContentDigest'.

The read is bounded against the config's response budget, so an oversized upstream is
refused fail-closed before anything buffers it whole. The digest is computed here, over the
strict body that read produced, the one place the wire bytes exist.
-}
fetchNpmManifest :: TracingPort -> NpmClientConfig -> PackageName -> IO (Either MetadataError Manifest)
fetchNpmManifest tracing config name =
    spanMetadataFetch tracing name (fetchMetadataFormBounded config Full noValidators name) >>= \case
        Left fault -> pure (Left (MetadataFetch fault))
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

{- | Project a fetched packument's bytes into @(manifest, raw document)@, applying the serve
path's response bounds and name validation. Pure and total.

The raw 'Value' is the nesting-checked document the typed view was projected from, so both
describe one parse. A decode failure or an absent\/undecodable name is 'MetadataUndecodable'.
A self-reported /different/ name is 'MetadataNameMismatch'. A nesting-depth or version-count
breach is 'MetadataBoundExceeded'.
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
'PackageDetails'.

npm carries the @time@ map only in the full document, so this still fetches the full bytes.
The win is that 'projectNpmVersion' parses them selectively, materialising one version
rather than every version. A 'Nothing' is a version genuinely absent from a sound document,
a forwarded miss.
-}
fetchNpmVersion :: TracingPort -> NpmClientConfig -> PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
fetchNpmVersion tracing config name version =
    spanMetadataFetch tracing name (fetchMetadataFormBounded config Full noValidators name) >>= \case
        Left fault -> pure (Left (MetadataFetch fault))
        Right response ->
            spanMetadataDecode tracing name $
                pure ((>>= enforceArtifactSchemeDetails (npmBaseUrl config)) <$> projectNpmVersion (npmLimits config) name version (responseBody response))

{- | Project a fetched packument's bytes into __one version's__ 'PackageDetails', without
decoding the other versions. Pure and total.

The outcome matches the one the whole-document path reaches for that version. An
absent\/undecodable @name@ is 'MetadataUndecodable' and a self-reported /different/ name is
'MetadataNameMismatch', the anti-shadowing distinction. A breach of 'maxNestingDepth' or of
the 'maxVersionCount' backstop is 'MetadataBoundExceeded'. An absent or unprojectable
version yields 'Nothing'.
-}
projectNpmVersion :: Limits -> PackageName -> Version -> ByteString -> Either MetadataError (Maybe PackageDetails)
projectNpmVersion limits name version body = do
    selected <- first (selectiveError limits) (selectVersionFromPackument (maxNestingDepth limits) version body)
    -- The self-reported name is the validation authority (anti-shadowing), checked before the
    -- version-count backstop, as 'projectNpmManifest' does.
    reported <- validateReportedName (svName selected)
    when (reported /= name) (Left (MetadataNameMismatch (renderPackageName reported)))
    when
        (svVersionCount selected > maxVersionCount limits)
        (Left (MetadataBoundExceeded (TooManyVersions (svVersionCount selected) (maxVersionCount limits))))
    publishedAt <- parsePublishTime (svTime selected)
    -- 'mkVersion' over the requested version's rendered key matches the whole-document path,
    -- which keys 'projectVersions' by that same string and so projects the version under it.
    pure (svVersion selected >>= projectVersionEntry name (mkVersion Npm (renderVersion version)) publishedAt)

-- The document's self-reported name, folded to the same 'MetadataUndecodable' the
-- whole-document decode reaches for an absent, non-string, or malformed name.
validateReportedName :: Maybe Value -> Either MetadataError PackageName
validateReportedName = \case
    Nothing -> Left MetadataUndecodable
    Just nameValue -> case parseMaybe parseJSON nameValue of
        Nothing -> Left MetadataUndecodable
        Just raw -> first (const MetadataUndecodable) (projectName raw)

-- An absent or undecodable stamp means no known publish time, never a document failure.
-- The whole-document path drops a malformed @time@ entry the same way.
parsePublishTime :: Maybe Value -> Either MetadataError (Maybe UTCTime)
parsePublishTime = \case
    Nothing -> Right Nothing
    Just timeValue -> Right (parseMaybe parseJSON timeValue)

-- Map a selective-decode refusal onto the 'MetadataError' the whole-document path raises
-- for the same cause.
selectiveError :: Limits -> SelectiveError -> MetadataError
selectiveError limits = \case
    SelectiveUndecodable -> MetadataUndecodable
    SelectiveTooDeeplyNested -> MetadataBoundExceeded (TooDeeplyNested (maxNestingDepth limits))
