-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm metadata reads for full manifests and selected versions.
Both fetch the full packument because publish-age rules need its @time@ map.
Selective reads materialise only the requested version and timestamp.
-}
module Ecluse.Core.Registry.Npm.Metadata (
    -- * Per-request read handle
    newNpmMetadataClient,

    -- * npm full-manifest fetch
    fetchNpmManifest,

    -- * Pure projection
    projectNpmManifest,
    projectNpmVersion,
) where

import Data.Aeson (Value (String), parseJSON)
import Data.Aeson.Types (parseMaybe)
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    InvalidEntry,
    PackageInfo,
    PackageName,
 )
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry (FetchFault, RegistryResponse)
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.Metadata (
    Manifest (Manifest, manifestDigest, manifestInfo, manifestRaw),
    MetadataClient,
    MetadataError (MetadataBoundExceeded),
    VersionRead (VersionRead, vrDetails, vrUpstreamLatest),
    digestOf,
    fetchThenProject,
 )
import Ecluse.Core.Registry.Metadata.Projection (projectMetadata, projectionResult, selectiveError, validateReportedName)
import Ecluse.Core.Registry.Npm (fetchMetadataFormBounded)
import Ecluse.Core.Registry.Npm.Project (
    parsePackageInfoFromValue,
    projectName,
    projectVersionEntry,
 )
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Full), npmArtifactHosts)
import Ecluse.Core.Registry.Npm.SelectiveDecode (
    SelectedVersion (svDistTagLatest, svName, svTime, svVersion, svVersionCount),
    selectVersionFromPackument,
 )
import Ecluse.Core.Registry.Origin (OriginClient (ocBaseUrl, ocLimits))
import Ecluse.Core.Registry.Request (noValidators)
import Ecluse.Core.Registry.WireSupport (checkNameAgreement)
import Ecluse.Core.Security (
    AllowedHostPorts,
    Limits,
    checkVersionCountOf,
    ecosystemArtifactAuthorities,
    maxNestingDepth,
 )
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Metadata (ManifestCaching, newMetadataClient)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)

-- | Build an origin read handle with shared caching and telemetry.
newNpmMetadataClient ::
    TracingPort ->
    MetricsPort ->
    Metric.Upstream ->
    ManifestCaching ->
    (PackageName -> MetadataError -> IO ()) ->
    (PackageName -> [InvalidEntry] -> IO ()) ->
    (PackageName -> IO ()) ->
    OriginClient ->
    MetadataClient
newNpmMetadataClient tracing metrics upstream caching logFailure logInvalid logFetch origin =
    newMetadataClient metrics upstream caching logFailure logInvalid logFetch (fetchNpmManifest tracing origin) (fetchNpmVersion tracing origin)

fetchNpmPackument :: OriginClient -> PackageName -> IO (Either FetchFault RegistryResponse)
fetchNpmPackument origin = fetchMetadataFormBounded origin Full noValidators

-- | Fetch a bounded full packument with the digest that scopes its cached document.
fetchNpmManifest :: TracingPort -> OriginClient -> PackageName -> IO (Either MetadataError Manifest)
fetchNpmManifest tracing origin name =
    fetchThenProject tracing (fetchNpmPackument origin) name $ \body ->
        manifestOf (digestOf body) . first (enforceArtifactLocations npmArtifactAuthorities (originBaseUrl origin))
            <$> projectNpmManifest (ocLimits origin) name body
  where
    manifestOf digest (info, raw) = Manifest{manifestInfo = info, manifestRaw = fst npmCached raw, manifestDigest = digest}

-- | Project a nesting-checked packument and retain its raw document for assembly.
projectNpmManifest :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, Value)
projectNpmManifest limits name = projectMetadata (parsePackageInfoFromValue name) limits

fetchNpmVersion :: TracingPort -> OriginClient -> PackageName -> Version -> IO (Either MetadataError VersionRead)
fetchNpmVersion tracing origin name version =
    fetchThenProject tracing (fetchNpmPackument origin) name $
        fmap locationChecked . projectNpmVersion (ocLimits origin) name version
  where
    locationChecked versionRead =
        versionRead{vrDetails = vrDetails versionRead >>= enforceArtifactLocationsOf npmArtifactAuthorities (originBaseUrl origin)}

-- npm artifacts must use the authority that served the packument.
npmArtifactAuthorities :: AllowedHostPorts
npmArtifactAuthorities = ecosystemArtifactAuthorities npmArtifactHosts

originBaseUrl :: OriginClient -> Text
originBaseUrl = registryUrlText . ocBaseUrl

-- | Project one version without decoding its siblings. Absent or unprojectable versions yield 'Nothing'.
projectNpmVersion :: Limits -> PackageName -> Version -> ByteString -> Either MetadataError VersionRead
projectNpmVersion limits name version body = do
    decoded <- first (selectiveError limits) (selectVersionFromPackument (maxNestingDepth limits) version body)
    reported <- validateReportedName projectName (svName decoded)
    selected <- projectionResult (checkNameAgreement name reported decoded)
    first MetadataBoundExceeded (checkVersionCountOf limits (svVersionCount selected))
    publishedAt <- parsePublishTime (svTime selected)
    pure
        VersionRead
            { -- Use the same rendered version key as the full-document projection.
              vrDetails = svVersion selected >>= projectVersionEntry name (mkVersion Npm (renderVersion version)) publishedAt
            , vrUpstreamLatest = latestTarget (svDistTagLatest selected)
            }

-- A non-string @latest@ is no known tag, matching the whole-document projection's per-entry drop.
latestTarget :: Maybe Value -> Maybe Version
latestTarget = \case
    Just (String raw) -> Just (mkVersion Npm raw)
    _ -> Nothing

-- An absent or undecodable stamp means no known publish time, never a document failure.
-- The whole-document path drops a malformed @time@ entry the same way.
parsePublishTime :: Maybe Value -> Either MetadataError (Maybe UTCTime)
parsePublishTime = \case
    Nothing -> Right Nothing
    Just timeValue -> Right (parseMaybe parseJSON timeValue)
