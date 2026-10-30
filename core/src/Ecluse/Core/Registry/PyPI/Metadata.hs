-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- Full laziness allocates rarely used continuations for each member, so reads specialise here.
{-# OPTIONS_GHC -fno-full-laziness #-}

-- | PyPI full and selected Simple-index reads for "Ecluse.Core.Registry.Metadata.Fetch".
module Ecluse.Core.Registry.PyPI.Metadata (
    pypiRead,
    preparePyPIRead,
    pypiMirrorRead,
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
import Data.Primitive.PrimVar (PrimVar)
import Data.Set qualified as Set

import Ecluse.Core.Package (Artifact (artEntryKey), PackageDetails (pkgArtifacts), PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Package.Filter (enforceArtifactLocations, enforceArtifactLocationsOf)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, pypiPacked, pypiSimpleCached)
import Ecluse.Core.Registry.Json.Intern (InternTable, tableTexts)
import Ecluse.Core.Registry.Json.Packed (docTable)
import Ecluse.Core.Registry.Json.Walk (Step, Steps, Walked (..), readJsonWalk)
import Ecluse.Core.Registry.Json.Writer (Writer, newWriter)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Metadata (MetadataError, VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.EntryFloor (countingEntries, readFlooredWalk)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (PrepareRead))
import Ecluse.Core.Registry.Metadata.Fetch.Types (DocumentWalk, EcosystemRead (..))
import Ecluse.Core.Registry.Metadata.Projection (streamError)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullReadWorkspace))
import Ecluse.Core.Registry.PyPI.Document (PackedSimple, SimpleDocument, restrictSimpleFiles)
import Ecluse.Core.Registry.PyPI.FileWriter (FileWriter, fileWriter)
import Ecluse.Core.Registry.PyPI.Reader (fileUniqueFields, pypiFullWalk, pypiWalk)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts, simpleIndexRequestUnder)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIRead (..))
import Ecluse.Core.Registry.PyPI.StreamingProjection (PyPIProjection, collectField, emptyProjection, finishPackedProjection, finishProjection, keepsFile, packedFileStep, retainedEntries)
import Ecluse.Core.Security (AllowedHostPorts, Limits, ecosystemArtifactAuthorities, maxNestingDepth)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (..))
import Ecluse.Core.Version (Version, renderVersion)

-- | Full and selected reads share the Simple-index request and field policy. Full reads charge no workspace.
pypiRead :: EcosystemRead
pypiRead = pypiReadWith (const pass) SelectedRead (const (const Nothing))

-- | 'pypiRead' with the selected origin's full-read workspace bound, which pays the per-entry floor.
preparePyPIRead :: PrepareRead
preparePyPIRead = PrepareRead (\origin -> pure (Right (pypiReadWith (ocChargeFullReadWorkspace origin) SelectedRead (const (const Nothing)))))

-- | Workers retain the supported envelope beside selected files without changing ordinary serve reads.
pypiMirrorRead :: EcosystemRead
pypiMirrorRead = pypiReadWith (const pass) MirrorRead (\details -> Just . mirrorDocument details)

pypiReadWith :: (Int -> IO ()) -> (PackageName -> Text -> PyPIRead) -> (PackageDetails -> SimpleDocument -> Maybe CachedDoc) -> EcosystemRead
pypiReadWith pay mode retain =
    EcosystemRead
        { erRequest = simpleIndexRequestUnder
        , erUniqueFields = fileUniqueFields
        , erWalkFull = \limits name _ -> readPyPIFull pay limits name
        , erFinishFull = finishPyPIFull
        , erWalkSelected = \limits name version -> readPyPIIndex limits name (mode name (renderVersion version))
        , erFinishSelected = finishPyPIVersion retain
        }

-- | Charges per 1000 source or output-basis bytes, calibrated with native memory probes. See @docs/testing.md@.
pypiChargeFactors :: ChargeFactors
pypiChargeFactors = ChargeFactors{cfFullReadPermille = 3500, cfKeptPermille = 3300, cfOutputPermille = 1900}

readPyPIIndex :: Limits -> PackageName -> PyPIRead -> DocumentWalk PyPIProjection
readPyPIIndex limits name mode bound table = readJsonWalk bound (pypiIndexWalk limits name mode table)

-- | The production Simple-index walk over a caller's intern table.
pypiIndexWalk :: Limits -> PackageName -> PyPIRead -> InternTable -> Cursor -> Step PyPIProjection
pypiIndexWalk limits name mode table = pypiWalk (maxNestingDepth limits) mode (collectField limits) keepsFile table (emptyProjection name)

-- | The packed read returns its final table beside the typed projection and supported files.
type PyPIFullRead = Walked PyPIProjection

readPyPIFull :: (Int -> IO ()) -> Limits -> PackageName -> DocumentWalk PyPIFullRead
readPyPIFull pay limits name bound table readChunk = do
    writer <- stToIO (newWriter Nothing)
    build <- stToIO (fileWriter writer)
    readFlooredWalk pay (cfFullReadPermille pypiChargeFactors) bound (\entries -> pypiPackedWalk entries writer build limits name table) readChunk

-- | Full reads use the supplied table and writer without materialising file trees, publishing the entries retained.
pypiPackedWalk :: PrimVar st Int -> Writer st -> FileWriter st -> Limits -> PackageName -> InternTable -> Cursor -> ST st (Steps (ST st) PyPIFullRead)
pypiPackedWalk entries writer build limits name table =
    pypiFullWalk build (maxNestingDepth limits) step keepsFile fileStep table (emptyProjection name)
  where
    counted = countingEntries entries retainedEntries
    step acc field next = counted next (collectField limits acc field)
    fileStep acc position value next = packedFileStep writer limits acc position value (counted next)
{-# INLINE pypiPackedWalk #-}

-- | Finish packed full reads without rebuilding file trees for their typed facts.
projectPyPIPacked :: Limits -> PackageName -> StreamResult PyPIFullRead -> Either MetadataError (PackageInfo, PackedSimple)
projectPyPIPacked limits name streamed = do
    Walked table acc <- first (streamError limits) (streamValue streamed)
    finishPackedProjection name (docTable (tableTexts table)) acc

finishPyPIFull :: Limits -> PackageName -> Text -> StreamResult PyPIFullRead -> Either MetadataError (PackageInfo, CachedDoc)
finishPyPIFull limits name base streamed =
    bimap (enforceArtifactLocations pypiArtifactAuthorities base) (fst pypiPacked) <$> projectPyPIPacked limits name streamed

finishPyPIVersion :: (PackageDetails -> SimpleDocument -> Maybe CachedDoc) -> Limits -> PackageName -> Text -> Version -> StreamResult PyPIProjection -> Either MetadataError VersionRead
finishPyPIVersion retain limits name base version streamed = do
    (info, document) <- projectPyPIStream limits name streamed
    pure
        VersionRead
            { vrVersion = do
                details <- Map.lookup (renderVersion version) (infoVersions info)
                located <- enforceArtifactLocationsOf pypiArtifactAuthorities base details
                pure VersionDoc{vdDetails = located, vdRaw = retain located document}
            , vrBodyBytes = streamBytes streamed
            , vrUpstreamLatest = Nothing
            }

mirrorDocument :: PackageDetails -> SimpleDocument -> CachedDoc
mirrorDocument details document = fst pypiSimpleCached retained
  where
    keys = Set.fromList (map artEntryKey (toList (pkgArtifacts details)))
    retained = restrictSimpleFiles keys document

-- | Finish a tree read, keeping files linked to their source coordinates.
projectPyPIStream :: Limits -> PackageName -> StreamResult PyPIProjection -> Either MetadataError (PackageInfo, SimpleDocument)
projectPyPIStream limits name streamed =
    first (streamError limits) (streamValue streamed) >>= finishProjection name

pypiArtifactAuthorities :: AllowedHostPorts
pypiArtifactAuthorities = ecosystemArtifactAuthorities pypiArtifactHosts
