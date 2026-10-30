-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Join supported PyPI fields while preserving source positions and existing read limits.
module Ecluse.Core.Registry.PyPI.StreamingProjection (
    PyPIProjection,
    emptyProjection,
    collectField,
    keepsFile,
    retainedEntries,
    finishProjection,
    packedFileStep,
    finishPackedProjection,
) where

import Control.Monad.ST (ST)
import Data.Aeson (Value)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)

import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind (InvalidVersionListing), PackageInfo, PackageName, mkInvalidEntry)
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed)
import Ecluse.Core.Registry.Json.Writer (Writer, decodeWhole, discard, sealValue)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataUndecodable))
import Ecluse.Core.Registry.Metadata.Projection (validateNameAgreement)
import Ecluse.Core.Registry.PyPI.Document (PackedSimple, SimpleDocument, packedSimple, simpleDocument, urlHole)
import Ecluse.Core.Registry.PyPI.FileWriter (FileValue (..))
import Ecluse.Core.Registry.PyPI.Project (FilenameMemo, ProjectedFile, filenameMemo, projectFile, projectName, projectSimpleIndex, projectedEntryKey, readCoordinate)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField (..))
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (ifEntryKey, ifFilename, ifUrl), checkApiVersion, decodeIndexFiles)
import Ecluse.Core.Registry.ServedDocument (artifactUrlPacked)
import Ecluse.Core.Security (LimitError, Limits, checkVersionCountOf)

-- | Pending files wait for the reported name so member order cannot change the package identity.
data PyPIProjection = PyPIProjection
    { projectedEnvelope :: KeyMap.KeyMap Value
    , projectedFiles :: [(ProjectedFile, FilePayload)]
    , projectedMemo :: FilenameMemo
    , projectedFileDrops :: [InvalidEntry]
    , projectedVersionDrops :: [InvalidEntry]
    , projectedEntries :: Int
    , projectedShape :: Bool
    , projectedFilesSeen :: Bool
    , projectedFilesActive :: Bool
    , projectedVersionsSeen :: Bool
    , projectedVersionsActive :: Bool
    }

data FilePayload = TreeFile Value | PackedFile Packed

-- | Start one project's source without retaining any input chunks.
emptyProjection :: PackageName -> PyPIProjection
emptyProjection name = PyPIProjection mempty [] (filenameMemo name) [] [] 0 True False False False False

-- | Whether a file read now would be retained: files in the first files array.
keepsFile :: PyPIProjection -> Bool
keepsFile = projectedFilesActive

-- | The files, file and version drops, and envelope array items the projection holds.
retainedEntries :: PyPIProjection -> Int
retainedEntries = projectedEntries

-- | Take one field. A file refuses the read at the first file position past the version ceiling.
collectField :: Limits -> PyPIProjection -> PyPIField -> Either LimitError PyPIProjection
collectField limits acc = \case
    IgnoredField -> Right acc
    EnvelopeField key value
        | KeyMap.member (Key.fromText key) (projectedEnvelope acc) -> Right acc
        | otherwise -> Right acc{projectedEnvelope = KeyMap.insert (Key.fromText key) value (projectedEnvelope acc)}
    EnvelopeItem key
        | KeyMap.member (Key.fromText key) (projectedEnvelope acc) -> Right acc
        | otherwise -> Right acc{projectedEntries = projectedEntries acc + 1}
    FilesShape valid -> Right acc{projectedFilesSeen = True, projectedFilesActive = not (projectedFilesSeen acc), projectedShape = projectedShape acc && (projectedFilesSeen acc || valid)}
    VersionsShape valid -> Right acc{projectedVersionsSeen = True, projectedVersionsActive = not (projectedVersionsSeen acc), projectedShape = projectedShape acc && (projectedVersionsSeen acc || valid)}
    InvalidVersionField position value
        | projectedVersionsActive acc ->
            Right acc{projectedVersionDrops = mkInvalidEntry InvalidVersionListing (show position) value "expected a version string" : projectedVersionDrops acc, projectedEntries = projectedEntries acc + 1}
        | otherwise -> Right acc
    FileField position raw
        | projectedFilesActive acc -> do
            checkVersionCountOf limits (position + 1)
            Right (maybe acc (\value -> retainFile position value acc) raw)
        | otherwise -> Right acc

-- The served file, or its invalid-entry diagnostic, under its source position.
retainFile :: Int -> Value -> PyPIProjection -> PyPIProjection
retainFile position value current = foldl' (retainFileFact (TreeFile value)) withDrops files
  where
    (files, drops) = decodeIndexFiles [(position, value)]
    withDrops = current{projectedFileDrops = reverse drops <> projectedFileDrops current, projectedEntries = projectedEntries current + length drops}

retainFileFact :: FilePayload -> PyPIProjection -> IndexFile -> PyPIProjection
retainFileFact served current file =
    let (coordinate, memo) = readCoordinate (projectedMemo current) (ifFilename file)
        !projected = projectFile file coordinate
     in current{projectedFiles = (projected, served) : projectedFiles current, projectedMemo = memo, projectedEntries = projectedEntries current + 1}

-- | Refuse at the first file position past the version ceiling. Decode a whole file only for its invalid-entry diagnostic.
packedFileStep :: Writer st -> Limits -> PyPIProjection -> Int -> FileValue -> (Either LimitError PyPIProjection -> ST st r) -> ST st r
packedFileStep writer limits acc position value next
    | not (projectedFilesActive acc) = discard writer >> next (Right acc)
    | Left fault <- checkVersionCountOf limits (position + 1) = discard writer >> next (Left fault)
    | otherwise = do
        packed <- sealValue writer urlHole
        case value of
            FileValue (Just file) -> do
                let !indexed = file{ifEntryKey = ArrayEntry position}
                    !served = artifactUrlPacked (Just (ifUrl file)) packed
                next (Right $! retainFileFact (PackedFile served) acc indexed)
            _ -> do
                diagnostic <- decodeWhole writer packed
                next (Right $! retainFile position diagnostic acc)

-- | Check protocol, name and shape, then project the source's retained files.
finishProjection :: PackageName -> PyPIProjection -> Either MetadataError (PackageInfo, SimpleDocument)
finishProjection requested acc = do
    info <- finishPackage requested acc
    pure (info, simpleDocument (projectedEnvelope acc) (servedFiles (projectedFiles acc)))

-- | Bind the reported name after reading, and retain serving files against the sealed table.
finishPackedProjection :: PackageName -> DocTable -> PyPIProjection -> Either MetadataError (PackageInfo, PackedSimple)
finishPackedProjection requested table acc = do
    info <- finishPackage requested acc
    pure (info, packedSimple (projectedEnvelope acc) table (foldl' serve [] (projectedFiles acc)))
  where
    serve served (file, PackedFile payload) = let !key = projectedEntryKey file in (key, payload) : served
    serve served _ = served

finishPackage :: PackageName -> PyPIProjection -> Either MetadataError PackageInfo
finishPackage requested acc = do
    first (const MetadataUndecodable) (parseEither checkApiVersion (projectedEnvelope acc))
    reported <- validateNameAgreement projectName requested (KeyMap.lookup "name" (projectedEnvelope acc))
    unless (projectedShape acc) (Left MetadataUndecodable)
    let invalid = reverse (projectedFileDrops acc) <> reverse (projectedVersionDrops acc)
    pure (projectSimpleIndex reported invalid (map fst (projectedFiles acc)))

-- Restore source order with every key evaluated, so the served list holds no decoded file.
servedFiles :: [(ProjectedFile, FilePayload)] -> [(EntryKey, Value)]
servedFiles = foldl' serve []
  where
    serve served (file, TreeFile value) = let !key = projectedEntryKey file in (key, value) : served
    serve served _ = served
