-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Read full PyPI indexes or selected releases through one file projection.
The fetch digest scopes full-document assembly, while selective reads retain original entry positions.
-}
module Ecluse.Core.Registry.PyPI.Metadata (
    -- * Per-request read handle
    newPyPIMetadataClient,

    -- * PyPI index fetch
    fetchPyPIManifest,

    -- * Pure projection
    projectPyPIIndex,
    projectPyPIVersion,
) where

import Data.Aeson (Value)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (
    InvalidEntry,
    PackageDetails,
    PackageInfo (infoVersions),
    PackageName,
 )
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable), RegistryResponse)
import Ecluse.Core.Registry.CachedDocument (pypiSimpleCached)
import Ecluse.Core.Registry.Exchange (boundedFetch, formThen)
import Ecluse.Core.Registry.Metadata (
    Manifest (Manifest, manifestDigest, manifestInfo, manifestRaw),
    MetadataClient,
    MetadataError (MetadataBoundExceeded, MetadataUndecodable),
    digestOf,
    fetchThenProject,
 )
import Ecluse.Core.Registry.Metadata.Projection (projectMetadata, projectionResult, selectiveError, validateReportedName)
import Ecluse.Core.Registry.Origin (OriginClient (ocBaseUrl, ocLimits, ocManager, ocToken))
import Ecluse.Core.Registry.PyPI.Project (
    fileVersionKey,
    projectName,
    projectSimpleFiles,
    projectSimpleIndexFromValue,
 )
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts, simpleIndexRequest)
import Ecluse.Core.Registry.PyPI.SelectiveDecode (
    SelectedFiles (sfFileCount, sfFiles, sfMeta, sfName),
    selectFilesFromIndex,
 )
import Ecluse.Core.Registry.PyPI.Wire (checkApiVersion)
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
import Ecluse.Core.Version (Version, renderVersion)

-- | Build an origin read handle with shared caching and telemetry.
newPyPIMetadataClient ::
    TracingPort ->
    MetricsPort ->
    Metric.Upstream ->
    ManifestCaching ->
    (PackageName -> MetadataError -> IO ()) ->
    (PackageName -> [InvalidEntry] -> IO ()) ->
    (PackageName -> IO ()) ->
    OriginClient ->
    MetadataClient
newPyPIMetadataClient tracing metrics upstream caching logFailure logInvalid logFetch origin =
    newMetadataClient metrics upstream caching logFailure logInvalid logFetch (fetchPyPIManifest tracing origin) (fetchPyPIVersion tracing origin)

fetchSimpleIndex :: OriginClient -> PackageName -> IO (Either FetchFault RegistryResponse)
fetchSimpleIndex origin name =
    formThen
        FetchUrlUnformable
        (boundedFetch (ocManager origin) (ocLimits origin))
        (simpleIndexRequest (registryUrlText (ocBaseUrl origin)) (ocToken origin) noValidators name)

-- | Fetch a bounded Simple index with the digest that scopes its cached document.
fetchPyPIManifest :: TracingPort -> OriginClient -> PackageName -> IO (Either MetadataError Manifest)
fetchPyPIManifest tracing origin name =
    fetchThenProject tracing (fetchSimpleIndex origin) name $ \body ->
        manifestOf (digestOf body) . first (enforceArtifactLocations pypiArtifactAuthorities (originBaseUrl origin))
            <$> projectPyPIIndex (ocLimits origin) name body
  where
    manifestOf digest (info, raw) =
        Manifest
            { manifestInfo = info
            , manifestRaw = fst pypiSimpleCached raw
            , manifestDigest = digest
            }

-- | Project a nesting-checked index and retain its raw document for assembly.
projectPyPIIndex :: Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, Value)
projectPyPIIndex limits name = projectMetadata (projectSimpleIndexFromValue name) limits

-- 'Nothing' is a release genuinely absent from a sound index, a forwarded miss.
fetchPyPIVersion :: TracingPort -> OriginClient -> PackageName -> Version -> IO (Either MetadataError (Maybe PackageDetails))
fetchPyPIVersion tracing origin name version =
    fetchThenProject tracing (fetchSimpleIndex origin) name $
        fmap (>>= enforceArtifactLocationsOf pypiArtifactAuthorities (originBaseUrl origin)) . projectPyPIVersion (ocLimits origin) name version

-- | Project one release after the full path's protocol check, retaining original file positions.
projectPyPIVersion :: Limits -> PackageName -> Version -> ByteString -> Either MetadataError (Maybe PackageDetails)
projectPyPIVersion limits name version body = do
    decoded <- first (selectiveError limits) (selectFilesFromIndex (maxNestingDepth limits) belongsToRelease body)
    first (const MetadataUndecodable) (parseEither checkApiVersion (maybe mempty (KeyMap.singleton "meta") (sfMeta decoded)))
    reported <- validateReportedName projectName (sfName decoded)
    selected <- projectionResult (checkNameAgreement name reported decoded)
    first MetadataBoundExceeded (checkVersionCountOf limits (sfFileCount selected))
    pure (Map.lookup wanted (infoVersions (projectSimpleFiles reported (sfFiles selected))))
  where
    belongsToRelease filename = fileVersionKey name filename == Just wanted
    wanted = renderVersion version

pypiArtifactAuthorities :: AllowedHostPorts
pypiArtifactAuthorities = ecosystemArtifactAuthorities pypiArtifactHosts

originBaseUrl :: OriginClient -> Text
originBaseUrl = registryUrlText . ocBaseUrl
