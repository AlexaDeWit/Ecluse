-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- The reads specialise here. Full laziness would float each member's rarely taken continuation out
-- of the element's continuation, and every member of a read would allocate it.
{-# OPTIONS_GHC -fno-full-laziness #-}

{- | PyPI's part of a metadata read, which "Ecluse.Core.Registry.Metadata.Fetch" drives. Full and
selected Simple-index reads share incremental extraction.
-}
module Ecluse.Core.Registry.PyPI.Metadata (
    pypiRead,
    pypiChargeFactors,
    pypiIndexWalk,
    projectPyPIStream,
) where

import Data.JsonStream.Lexer.Internal (Cursor)
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, pypiSimpleCached)
import Ecluse.Core.Registry.Json.Intern (InternTable)
import Ecluse.Core.Registry.Json.Walk (Step, readJsonWalk)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (MetadataError, VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.Fetch.Types (DocumentWalk, EcosystemRead (..))
import Ecluse.Core.Registry.Metadata.Projection (streamError)
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument)
import Ecluse.Core.Registry.PyPI.Reader (fileUniqueFields, pypiWalk)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts, simpleIndexRequest)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIRead (..))
import Ecluse.Core.Registry.PyPI.StreamingProjection (PyPIProjection, collectField, emptyProjection, finishProjection, keepsFile)
import Ecluse.Core.Security (AllowedHostPorts, Limits, ecosystemArtifactAuthorities, maxNestingDepth)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (..))
import Ecluse.Core.Version (Version, renderVersion)

-- | What the read driver needs to read a Simple index: the request, the walk in both modes, and their finishes.
pypiRead :: EcosystemRead
pypiRead =
    EcosystemRead
        { erRequest = simpleIndexRequest
        , erUniqueFields = fileUniqueFields
        , erWalkFull = \limits name _ -> readPyPIIndex limits name FullRead
        , erFinishFull = finishPyPIFull
        , erWalkSelected = \limits name version -> readPyPIIndex limits name (SelectedRead name (renderVersion version))
        , erFinishSelected = finishPyPIVersion
        }

{- | PyPI's memory charges, above the largest read peak per source byte (3.09, boto3) and output
working set per basis byte of a realistic merge (1.24, boto3) from one meter step up.
-}
pypiChargeFactors :: ChargeFactors
pypiChargeFactors = ChargeFactors{cfFullReadPermille = 3900, cfOutputPermille = 1600}

-- Walk an index's chunks with the production field policy.
readPyPIIndex :: Limits -> PackageName -> PyPIRead -> DocumentWalk PyPIProjection
readPyPIIndex limits name mode bound table = readJsonWalk bound (pypiIndexWalk limits name mode table)

-- | The production Simple-index walk over a caller's intern table.
pypiIndexWalk :: Limits -> PackageName -> PyPIRead -> InternTable -> Cursor -> Step PyPIProjection
pypiIndexWalk limits name mode table = pypiWalk (maxNestingDepth limits) mode (collectField limits mode) keepsFile table (emptyProjection name)

finishPyPIFull :: Limits -> PackageName -> Text -> StreamResult PyPIProjection -> Either MetadataError (PackageInfo, CachedDoc)
finishPyPIFull limits name base streamed =
    bimap (enforceArtifactLocations pypiArtifactAuthorities base) (fst pypiSimpleCached) <$> projectPyPIStream limits name streamed

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

-- | Finish both read modes without separating typed files from their source coordinates.
projectPyPIStream :: Limits -> PackageName -> StreamResult PyPIProjection -> Either MetadataError (PackageInfo, SimpleDocument)
projectPyPIStream limits name streamed =
    first (streamError limits) (streamValue streamed) >>= finishProjection name

pypiArtifactAuthorities :: AllowedHostPorts
pypiArtifactAuthorities = ecosystemArtifactAuthorities pypiArtifactHosts
