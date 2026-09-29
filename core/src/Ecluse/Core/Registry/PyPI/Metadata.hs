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
    readPyPIFull,
    PyPIFullRead,
    pypiFullWalk,
    projectPyPIPacked,
) where

import Control.Monad.ST (ST, stToIO)
import Data.JsonStream.TokenParser (TokenResult)
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (InvalidEntry, PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable))
import Ecluse.Core.Registry.CachedDocument (pypiPacked)
import Ecluse.Core.Registry.Exchange (chargedRead, digestingRead, formThen, withSuccessBody)
import Ecluse.Core.Registry.Json.Intern (InternTable, newInternTable, newTableKey, tableTexts)
import Ecluse.Core.Registry.Json.Packed (docTable)
import Ecluse.Core.Registry.Json.Shape (Trees (..))
import Ecluse.Core.Registry.Json.Walk (Step, Steps, Walked (..), pureStep, readJsonWalk, readJsonWalkST)
import Ecluse.Core.Registry.Json.Writer (Writer, newWriter)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError (..), VersionDoc (..), VersionRead (..), metadataResponse)
import Ecluse.Core.Registry.Metadata.Projection (streamError)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocLimits, ocManager, ocToken), OriginFor, originBaseUrl)
import Ecluse.Core.Registry.PyPI.Document (PackedSimple, SimpleDocument)
import Ecluse.Core.Registry.PyPI.Reader (fileUniqueFields, pypiWalk)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts, simpleIndexRequest)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIRead (..))
import Ecluse.Core.Registry.PyPI.StreamingProjection (PackedRead, TreeRead, emptyPackedRead, emptyTreeRead, finishPacked, finishTree, keepsPackedFile, keepsTreeFile, packedStep, treeStep)
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
    result <- fetchPyPIBody tracing origin name (digestingRead (spanMetadataDecode tracing name . readPyPIFull (ocLimits origin) name) . chargedRead (ocChargeFullRead origin))
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

decodePyPI :: TracingPort -> OriginClient -> PackageName -> PyPIRead -> IO ByteString -> IO (Either LimitError (StreamResult (Walked TreeRead)))
decodePyPI tracing origin name mode = spanMetadataDecode tracing name . readPyPIIndex (ocLimits origin) name mode

-- | Walk an index's chunks into aeson's trees with the production field policy, over a table keyed afresh for the read.
readPyPIIndex :: Limits -> PackageName -> PyPIRead -> IO ByteString -> IO (Either LimitError (StreamResult (Walked TreeRead)))
readPyPIIndex limits name mode readChunk = do
    table <- newInternTable <$> newTableKey <*> pure fileUniqueFields
    readJsonWalk (MetadataBodyLimit (maxMetadataBytes limits)) (pypiIndexWalk limits name mode table) readChunk

-- | The production Simple-index walk into aeson's trees over a caller's intern table.
pypiIndexWalk :: Limits -> PackageName -> PyPIRead -> InternTable -> TokenResult -> Step (Walked TreeRead)
pypiIndexWalk limits name mode table = pypiWalk Trees (maxNestingDepth limits) mode (pureStep (treeStep limits mode)) keepsTreeFile table (emptyTreeRead name)

-- | What a full read finishes with: the read's table, and its typed facts and packed files.
type PyPIFullRead = Walked PackedRead

-- | Walk a whole index's chunks into its packed form, over a table keyed afresh for the read.
readPyPIFull :: Limits -> PackageName -> IO ByteString -> IO (Either LimitError (StreamResult PyPIFullRead))
readPyPIFull limits name readChunk = do
    table <- newInternTable <$> newTableKey <*> pure fileUniqueFields
    writer <- stToIO (newWriter Nothing)
    readJsonWalkST stToIO (MetadataBodyLimit (maxMetadataBytes limits)) (pypiFullWalk writer limits name table) readChunk

-- | The production full-read walk: each kept file packed by the writer against the caller's table.
pypiFullWalk :: Writer st -> Limits -> PackageName -> InternTable -> TokenResult -> ST st (Steps (ST st) PyPIFullRead)
pypiFullWalk writer limits name table = pypiWalk writer (maxNestingDepth limits) FullRead (packedStep writer limits) keepsPackedFile table (emptyPackedRead name)
{-# INLINE pypiFullWalk #-}

-- | Finish a full read over its sealed table, keeping its typed error classification.
projectPyPIPacked :: Limits -> PackageName -> StreamResult PyPIFullRead -> Either MetadataError (PackageInfo, PackedSimple)
projectPyPIPacked limits name streamed = do
    Walked table acc <- first (streamError limits) (streamValue streamed)
    finishPacked name (docTable (tableTexts table)) acc

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
projectPyPIStream :: Limits -> PackageName -> StreamResult (Walked TreeRead) -> Either MetadataError (PackageInfo, SimpleDocument)
projectPyPIStream limits name streamed = do
    Walked _ acc <- first (streamError limits) (streamValue streamed)
    finishTree name acc

pypiArtifactAuthorities :: AllowedHostPorts
pypiArtifactAuthorities = ecosystemArtifactAuthorities pypiArtifactHosts
