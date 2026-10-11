-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- Full laziness allocates rarely used continuations for each member, so reads specialise here.
{-# OPTIONS_GHC -fno-full-laziness #-}

-- | PyPI full and selected Simple-index reads for "Ecluse.Core.Registry.Metadata.Fetch".
module Ecluse.Core.Registry.PyPI.Metadata (
    pypiRead,
    pypiChargeFactors,
    pypiIndexWalk,
    projectPyPIStream,
    PyPIFullRead,
    pypiPackedWalk,
    projectPyPIPacked,
) where

import Control.Monad.ST (ST, stToIO)
import Data.JsonStream.Lexer (Cursor)
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, pypiPacked)
import Ecluse.Core.Registry.Json.Intern (InternTable, tableTexts)
import Ecluse.Core.Registry.Json.Packed (docTable)
import Ecluse.Core.Registry.Json.Walk (Step, Steps, Walked (..), pureStep, readJsonWalk, readJsonWalkST)
import Ecluse.Core.Registry.Json.Writer (Writer, newWriter)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (MetadataError, VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.Fetch.Types (DocumentWalk, EcosystemRead (..))
import Ecluse.Core.Registry.Metadata.Projection (streamError)
import Ecluse.Core.Registry.PyPI.Document (PackedSimple, SimpleDocument)
import Ecluse.Core.Registry.PyPI.FileWriter (FileWriter, fileWriter)
import Ecluse.Core.Registry.PyPI.Reader (fileUniqueFields, pypiFullWalk, pypiWalk)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts, simpleIndexRequest)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIRead (..))
import Ecluse.Core.Registry.PyPI.StreamingProjection (PyPIProjection, collectField, emptyProjection, finishPackedProjection, finishProjection, keepsFile, packedFileStep)
import Ecluse.Core.Security (AllowedHostPorts, Limits, ecosystemArtifactAuthorities, maxNestingDepth)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (..))
import Ecluse.Core.Version (Version, renderVersion)

-- | Full and selected reads share the Simple-index request and field policy.
pypiRead :: EcosystemRead
pypiRead =
    EcosystemRead
        { erRequest = simpleIndexRequest
        , erUniqueFields = fileUniqueFields
        , erWalkFull = \limits name _ -> readPyPIFull limits name
        , erFinishFull = finishPyPIFull
        , erWalkSelected = \limits name version -> readPyPIIndex limits name (SelectedRead name (renderVersion version))
        , erFinishSelected = finishPyPIVersion
        }

-- | Charges per 1000 source or output-basis bytes, calibrated with native memory probes. See @docs/testing.md@.
pypiChargeFactors :: ChargeFactors
pypiChargeFactors = ChargeFactors{cfFullReadPermille = 3300, cfOutputPermille = 1600}

readPyPIIndex :: Limits -> PackageName -> PyPIRead -> DocumentWalk PyPIProjection
readPyPIIndex limits name mode bound table = readJsonWalk bound (pypiIndexWalk limits name mode table)

-- | The production Simple-index walk over a caller's intern table.
pypiIndexWalk :: Limits -> PackageName -> PyPIRead -> InternTable -> Cursor -> Step PyPIProjection
pypiIndexWalk limits name mode table = pypiWalk (maxNestingDepth limits) mode (collectField limits mode) keepsFile table (emptyProjection name)

-- | The packed read returns its final table beside the typed projection and supported files.
type PyPIFullRead = Walked PyPIProjection

readPyPIFull :: Limits -> PackageName -> DocumentWalk PyPIFullRead
readPyPIFull limits name bound table readChunk = do
    writer <- stToIO (newWriter Nothing)
    build <- stToIO (fileWriter writer)
    readJsonWalkST stToIO bound (pypiPackedWalk writer build limits name table) readChunk

-- | The production full-read walk over the caller's table and scratch writer.
pypiPackedWalk :: Writer st -> FileWriter st -> Limits -> PackageName -> InternTable -> Cursor -> ST st (Steps (ST st) PyPIFullRead)
pypiPackedWalk writer build limits name table =
    pypiFullWalk build (maxNestingDepth limits) (pureStep (collectField limits FullRead)) keepsFile (packedFileStep writer limits) table (emptyProjection name)
{-# INLINE pypiPackedWalk #-}

-- | Finish packed full reads without rebuilding file trees for their typed facts.
projectPyPIPacked :: Limits -> PackageName -> StreamResult PyPIFullRead -> Either MetadataError (PackageInfo, PackedSimple)
projectPyPIPacked limits name streamed = do
    Walked table acc <- first (streamError limits) (streamValue streamed)
    finishPackedProjection name (docTable (tableTexts table)) acc

finishPyPIFull :: Limits -> PackageName -> Text -> StreamResult PyPIFullRead -> Either MetadataError (PackageInfo, CachedDoc)
finishPyPIFull limits name base streamed =
    bimap (enforceArtifactLocations pypiArtifactAuthorities base) (fst pypiPacked) <$> projectPyPIPacked limits name streamed

-- PyPI retains no publication object for a selected release, and its documents declare no latest tag.
finishPyPIVersion :: Limits -> PackageName -> Text -> Version -> StreamResult PyPIProjection -> Either MetadataError VersionRead
finishPyPIVersion limits name base version streamed = do
    (info, _) <- projectPyPIStream limits name streamed
    pure
        VersionRead
            { vrVersion = do
                details <- Map.lookup (renderVersion version) (infoVersions info)
                located <- enforceArtifactLocationsOf pypiArtifactAuthorities base details
                pure VersionDoc{vdDetails = located, vdRaw = Nothing}
            , vrBodyBytes = streamBytes streamed
            , vrUpstreamLatest = Nothing
            }

-- | Finish a tree read, keeping files linked to their source coordinates.
projectPyPIStream :: Limits -> PackageName -> StreamResult PyPIProjection -> Either MetadataError (PackageInfo, SimpleDocument)
projectPyPIStream limits name streamed =
    first (streamError limits) (streamValue streamed) >>= finishProjection name

pypiArtifactAuthorities :: AllowedHostPorts
pypiArtifactAuthorities = ecosystemArtifactAuthorities pypiArtifactHosts
