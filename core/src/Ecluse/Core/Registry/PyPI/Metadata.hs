-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- The reads specialise here. Full laziness would float each member's rarely taken continuation out
-- of the element's continuation, and every member of a read would allocate it.
{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Full and selected Simple-index reads share incremental extraction. Only full reads hash the source.
module Ecluse.Core.Registry.PyPI.Metadata (
    newPyPIMetadataReads,
    fetchPyPIManifest,
    pypiChargeFactors,
    readPyPIIndex,
    pypiIndexWalk,
    projectPyPIStream,
) where

import Data.JsonStream.TokenParser (TokenResult)
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (InvalidEntry, PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable))
import Ecluse.Core.Registry.CachedDocument (pypiSimpleCached)
import Ecluse.Core.Registry.Exchange (chargedRead, digestingRead, formThen, withSuccessBody)
import Ecluse.Core.Registry.Json.Intern (InternTable, newInternTable, newTableKey)
import Ecluse.Core.Registry.Json.Walk (Step, readJsonWalk)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError (..), VersionDoc (..), VersionRead (..), metadataResponse)
import Ecluse.Core.Registry.Metadata.Projection (streamError)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocLimits, ocManager, ocToken), OriginFor, originBaseUrl)
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument)
import Ecluse.Core.Registry.PyPI.Reader (fileUniqueFields, pypiWalk)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts, simpleIndexRequest)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIRead (..))
import Ecluse.Core.Registry.PyPI.StreamingProjection (PyPIProjection, collectField, emptyProjection, finishProjection, keepsFile)
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

{- | PyPI's memory charges per source byte, above the largest read peak from one meter step of source
up (3.29, boto3) and a listing's encoding with its strict copy (1.57, requests).
-}
pypiChargeFactors :: ChargeFactors
pypiChargeFactors = ChargeFactors{cfFullReadPermille = 4200, cfOutputPermille = 1600}

-- | Fetch compact files and hash the complete decompressed source inside the response lifetime.
fetchPyPIManifest :: TracingPort -> OriginClient -> PackageName -> IO (Either MetadataError Manifest)
fetchPyPIManifest tracing origin name = do
    result <- fetchPyPIBody tracing origin name (digestingRead (decodePyPI tracing origin name FullRead) . chargedRead (ocChargeFullRead origin))
    pure $ do
        (streamed, digest) <- result
        (info, document) <- projectPyPIStream (ocLimits origin) name streamed
        pure
            Manifest
                { manifestInfo = enforceArtifactLocations pypiArtifactAuthorities (originBaseUrl origin) info
                , manifestRaw = fst pypiSimpleCached document
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
decodePyPI tracing origin name mode = spanMetadataDecode tracing name . readPyPIIndex (ocLimits origin) name mode

-- | Walk an index's chunks with the production field policy, over a table keyed afresh for the read.
readPyPIIndex :: Limits -> PackageName -> PyPIRead -> IO ByteString -> IO (Either LimitError (StreamResult PyPIProjection))
readPyPIIndex limits name mode readChunk = do
    table <- newInternTable <$> newTableKey <*> pure fileUniqueFields
    readJsonWalk (MetadataBodyLimit (maxMetadataBytes limits)) (pypiIndexWalk limits name mode table) readChunk

-- | The production Simple-index walk over a caller's intern table.
pypiIndexWalk :: Limits -> PackageName -> PyPIRead -> InternTable -> TokenResult -> Step PyPIProjection
pypiIndexWalk limits name mode table = pypiWalk (maxNestingDepth limits) mode (collectField limits mode) keepsFile table (emptyProjection name)

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
