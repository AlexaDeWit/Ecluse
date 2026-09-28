-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Full and selected Simple-index reads share incremental extraction. Only full reads hash the source.
module Ecluse.Core.Registry.PyPI.Metadata (
    newPyPIMetadataReads,
    fetchPyPIManifest,
    pypiChargeFactors,
    projectPyPIStream,
    packedWalk,
    projectPyPIPacked,
) where

import Data.Map.Strict qualified as Map

import Data.JsonStream.TokenParser (TokenResult)
import Ecluse.Core.Package (InvalidEntry, PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable))
import Ecluse.Core.Registry.CachedDocument (pypiPacked)
import Ecluse.Core.Registry.Exchange (chargedRead, digestingRead, formThen, withSuccessBody)
import Ecluse.Core.Registry.Json.Intern (InternTable, newInternTable, newTableKey)
import Ecluse.Core.Registry.Json.Pack (sealTable)
import Ecluse.Core.Registry.Json.Packed (Packed)
import Ecluse.Core.Registry.Json.Walk (Step, readJsonWalk)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError (..), VersionDoc (..), VersionRead (..), metadataResponse)
import Ecluse.Core.Registry.Metadata.Projection (streamError)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocLimits, ocManager, ocToken), OriginFor, originBaseUrl)
import Ecluse.Core.Registry.PyPI.Document (PackedSimple, SimpleDocument, packedSimple)
import Ecluse.Core.Registry.PyPI.Reader (fileUniqueFields, pypiWalk, pypiWalkTable)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts, simpleIndexRequest)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIRead (..))
import Ecluse.Core.Registry.PyPI.StreamingProjection (PyPIProjection, PyPIProjectionOf, collectField, collectFieldWith, emptyProjection, finishParts, finishProjection, keepsFile, packedFile)
import Ecluse.Core.Security (AllowedHostPorts, BodyLimit (MetadataBodyLimit), LimitError, Limits (progressFloor), ecosystemArtifactAuthorities, maxMetadataBytes, maxNestingDepth)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (..))
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

{- | PyPI's memory charges per source byte, which the residency tier holds above its maxima: a full
read's retention (3.13, requests) and a listing's encoding with its strict copy (1.57, requests).
-}
pypiChargeFactors :: ChargeFactors
pypiChargeFactors = ChargeFactors{cfFullReadPermille = 4500, cfOutputPermille = 1600}

-- | Fetch compact files and hash the complete decompressed source inside the response lifetime.
fetchPyPIManifest :: TracingPort -> OriginClient -> PackageName -> IO (Either MetadataError Manifest)
fetchPyPIManifest tracing origin name = do
    result <- fetchPyPIBody tracing origin name (digestingRead (decodePacked tracing origin name) . chargedRead (ocChargeFullRead origin))
    pure $ do
        (streamed, digest) <- result
        (info, packed) <- projectPyPIPacked (ocLimits origin) name streamed
        pure
            Manifest
                { manifestInfo = enforceArtifactLocations pypiArtifactAuthorities (originBaseUrl origin) info
                , manifestRaw = fst pypiPacked packed
                , manifestBodyBytes = streamBytes streamed
                , manifestDigest = digest
                }

fetchPyPIBody :: TracingPort -> OriginClient -> PackageName -> (IO ByteString -> IO (Either LimitError r)) -> IO (Either MetadataError r)
fetchPyPIBody tracing origin name consume =
    metadataResponse
        <$> spanMetadataFetch
            tracing
            name
            (formThen FetchUrlUnformable (withSuccessBody (ocManager origin) (progressFloor (ocLimits origin)) consume) (simpleIndexRequest (originBaseUrl origin) (ocToken origin) name))

decodePyPI :: TracingPort -> OriginClient -> PackageName -> PyPIRead -> IO ByteString -> IO (Either LimitError (StreamResult PyPIProjection))
decodePyPI tracing origin name mode readChunk = do
    table <- newInternTable <$> newTableKey <*> pure fileUniqueFields
    spanMetadataDecode tracing name $
        readJsonWalk (MetadataBodyLimit (maxMetadataBytes limits)) (pypiWalk (maxNestingDepth limits) mode (collectField limits mode) keepsFile table (emptyProjection name)) readChunk
  where
    limits = ocLimits origin

-- A full read packs each kept file against a table keyed for this read.
decodePacked :: TracingPort -> OriginClient -> PackageName -> IO ByteString -> IO (Either LimitError (StreamResult (InternTable, PyPIProjectionOf Packed)))
decodePacked tracing origin name readChunk = do
    table <- newInternTable <$> newTableKey <*> pure fileUniqueFields
    spanMetadataDecode tracing name $
        readJsonWalk (MetadataBodyLimit (maxMetadataBytes limits)) (packedWalk limits name table) readChunk
  where
    limits = ocLimits origin

-- | The full-read walk: each kept file packs against the read's table.
packedWalk :: Limits -> PackageName -> InternTable -> TokenResult -> Step (InternTable, PyPIProjectionOf Packed)
packedWalk limits name table =
    pypiWalkTable (maxNestingDepth limits) FullRead (collectFieldWith limits FullRead packedFile) keepsFile table (emptyProjection name)

-- | Finish a packed full read over its sealed table, keeping its typed error classification.
projectPyPIPacked :: Limits -> PackageName -> StreamResult (InternTable, PyPIProjectionOf Packed) -> Either MetadataError (PackageInfo, PackedSimple)
projectPyPIPacked limits name streamed = do
    (table, acc) <- first (streamError limits) (streamValue streamed)
    (\(info, envelope, files) -> (info, packedSimple envelope (sealTable table) files)) <$> finishParts name acc

fetchPyPIVersion :: TracingPort -> OriginClient -> PackageName -> Version -> IO (Either MetadataError VersionRead)
fetchPyPIVersion tracing origin name version = do
    result <- fetchPyPIBody tracing origin name (decodePyPI tracing origin name (SelectedRead name (renderVersion version)))
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
