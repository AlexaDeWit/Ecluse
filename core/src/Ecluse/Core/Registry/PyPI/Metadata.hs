-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Full and selected Simple-index reads share incremental extraction and source identity.
module Ecluse.Core.Registry.PyPI.Metadata (
    newPyPIMetadataReads,
    fetchPyPIManifest,
    projectPyPIStream,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (InvalidEntry, PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable), isAuthorisationFailure)
import Ecluse.Core.Registry.CachedDocument (pypiSimpleCached)
import Ecluse.Core.Registry.Exchange (boundedJsonFetchWith, formThen)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError (..), VersionDoc (..), VersionRead (..), metadataFetchError)
import Ecluse.Core.Registry.Metadata.Projection (streamError)
import Ecluse.Core.Registry.Origin (OriginClient (ocLimits, ocManager, ocToken), OriginFor, originBaseUrl)
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts, simpleIndexRequest)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIRead (..), pypiFields)
import Ecluse.Core.Registry.PyPI.StreamingProjection (PyPIProjection, collectField, emptyProjection, finishProjection)
import Ecluse.Core.Security (AllowedHostPorts, BodyLimit (MetadataBodyLimit), Limits, ecosystemArtifactAuthorities, maxMetadataBytes, maxNestingDepth)
import Ecluse.Core.Server.Metadata (MetadataReads, newMetadataReads)
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Version (Version, renderVersion)

-- | Bind one origin's reads to observers. PyPI retains no publication object for selected releases.
newPyPIMetadataReads ::
    TracingPort ->
    MetricsPort ->
    (PackageName -> MetadataError -> IO ()) ->
    (PackageName -> [InvalidEntry] -> IO ()) ->
    (PackageName -> IO ()) ->
    OriginFor posture ->
    MetadataReads posture
newPyPIMetadataReads tracing metrics logFailure logInvalid logFetch =
    newMetadataReads metrics logFailure logInvalid logFetch (fetchPyPIManifest tracing) (fetchPyPIVersion tracing)

-- | Fetch compact files and hash the complete decompressed source inside the response lifetime.
fetchPyPIManifest :: TracingPort -> OriginClient -> PackageName -> IO (Either MetadataError Manifest)
fetchPyPIManifest tracing origin name = do
    result <- fetchPyPIStream tracing origin name FullRead
    pure $ do
        streamed <- result
        (info, document) <- projectPyPIStream (ocLimits origin) name streamed
        pure
            Manifest
                { manifestInfo = enforceArtifactLocations pypiArtifactAuthorities (originBaseUrl origin) info
                , manifestRaw = fst pypiSimpleCached document
                , manifestBodyBytes = streamBytes streamed
                , manifestDigest = streamDigest streamed
                }

fetchPyPIStream :: TracingPort -> OriginClient -> PackageName -> PyPIRead -> IO (Either MetadataError (StreamResult PyPIProjection))
fetchPyPIStream tracing origin name mode =
    spanMetadataFetch tracing name fetch <&> \case
        Left fault -> Left (metadataFetchError fault)
        Right (404, _) -> Left MetadataAbsent
        Right (code, result)
            | isAuthorisationFailure code -> Left (MetadataAuthorisationFailure code)
            | Just streamed <- result -> Right streamed
            | otherwise -> Left (MetadataHttpFailure code)
  where
    limits = ocLimits origin
    fetch =
        formThen
            FetchUrlUnformable
            ( boundedJsonFetchWith
                (spanMetadataDecode tracing name)
                (ocManager origin)
                (MetadataBodyLimit (maxMetadataBytes limits))
                (pypiFields (maxNestingDepth limits) mode)
                (collectField limits name mode)
                emptyProjection
            )
            (simpleIndexRequest (originBaseUrl origin) (ocToken origin) name)

fetchPyPIVersion :: TracingPort -> OriginClient -> PackageName -> Version -> IO (Either MetadataError VersionRead)
fetchPyPIVersion tracing origin name version = do
    result <- fetchPyPIStream tracing origin name (SelectedRead name (renderVersion version))
    pure $ do
        streamed <- result
        (info, _) <- projectPyPIStream (ocLimits origin) name streamed
        pure
            VersionRead
                { vrVersion = do
                    details <- Map.lookup (renderVersion version) (infoVersions info)
                    located <- enforceArtifactLocationsOf pypiArtifactAuthorities (originBaseUrl origin) details
                    pure VersionDoc{vdDetails = located, vdRaw = Nothing}
                , vrBodyBytes = streamBytes streamed
                , vrUpstreamLatest = Nothing
                }

-- | Finish both read modes without separating typed files from their source coordinates.
projectPyPIStream :: Limits -> PackageName -> StreamResult PyPIProjection -> Either MetadataError (PackageInfo, SimpleDocument)
projectPyPIStream limits name streamed =
    first (streamError limits) (streamValue streamed) >>= finishProjection name

pypiArtifactAuthorities :: AllowedHostPorts
pypiArtifactAuthorities = ecosystemArtifactAuthorities pypiArtifactHosts
