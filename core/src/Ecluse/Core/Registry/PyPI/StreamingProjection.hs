-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Join supported PyPI fields while preserving source positions and existing read limits.
module Ecluse.Core.Registry.PyPI.StreamingProjection (
    PyPIProjection,
    emptyProjection,
    collectField,
    keepsFile,
    finishProjection,
    packedFileStep,
    finishPackedProjection,
) where

import Control.Monad.ST (ST)
import Data.Aeson (Value)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Set qualified as Set

import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind (InvalidVersionListing), PackageInfo, PackageName, mkInvalidEntry)
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, withoutHole)
import Ecluse.Core.Registry.Json.Writer (Writer, decodeWhole, discard, sealValue)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataUndecodable))
import Ecluse.Core.Registry.Metadata.Projection (validateNameAgreement)
import Ecluse.Core.Registry.PyPI.Document (PackedSimple, SimpleDocument, packedSimple, simpleDocument, urlHole)
import Ecluse.Core.Registry.PyPI.FileWriter (FileValue (..))
import Ecluse.Core.Registry.PyPI.Project (FilenameMemo, ProjectedFile, fcVersionKey, filenameMemo, projectFile, projectName, projectSimpleIndex, projectedEntryKey, readCoordinate)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField (..), PyPIRead (..))
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (ifEntryKey, ifFilename, ifUrl), checkApiVersion, decodeIndexFiles)
import Ecluse.Core.Registry.ServedDocument (rebaseArtifactUrl)
import Ecluse.Core.Security (LimitError, Limits, checkArtifactCount, checkVersionCountOf)

-- | Pending files wait for the reported name so member order cannot change the package identity.
data PyPIProjection = PyPIProjection
    { projectedEnvelope :: KeyMap.KeyMap Value
    , projectedFiles :: [(ProjectedFile, FilePayload)]
    , projectedMemo :: FilenameMemo
    , projectedFileDrops :: [InvalidEntry]
    , projectedVersionDrops :: [InvalidEntry]
    , projectedVersions :: Set Text
    , projectedArtifactCount :: Int
    , projectedBound :: Maybe LimitError
    , projectedShape :: Bool
    , projectedFilesSeen :: Bool
    , projectedFilesActive :: Bool
    , projectedVersionsSeen :: Bool
    , projectedVersionsActive :: Bool
    }

data FilePayload = TreeFile Value | PackedFile Packed

-- | Start one project's source without retaining any input chunks.
emptyProjection :: PackageName -> PyPIProjection
emptyProjection name = PyPIProjection mempty [] (filenameMemo name) [] [] mempty 0 Nothing True False False False False

-- | Whether a file read now would be retained: files in the first files array, until a limit trips.
keepsFile :: PyPIProjection -> Bool
keepsFile acc = projectedFilesActive acc && isNothing (projectedBound acc)

-- | Decode one compact file and stop retaining payloads after an existing structural limit trips.
collectField :: Limits -> PyPIRead -> PyPIProjection -> PyPIField -> Either LimitError PyPIProjection
collectField limits mode acc =
    Right . \case
        IgnoredField -> acc
        EnvelopeField key value
            | KeyMap.member (Key.fromText key) (projectedEnvelope acc) -> acc
            | otherwise -> acc{projectedEnvelope = KeyMap.insert (Key.fromText key) value (projectedEnvelope acc)}
        FilesShape valid -> acc{projectedFilesSeen = True, projectedFilesActive = not (projectedFilesSeen acc), projectedShape = projectedShape acc && (projectedFilesSeen acc || valid)}
        VersionsShape valid -> acc{projectedVersionsSeen = True, projectedVersionsActive = not (projectedVersionsSeen acc), projectedShape = projectedShape acc && (projectedVersionsSeen acc || valid)}
        InvalidVersionField position value
            | projectedVersionsActive acc && isNothing (projectedBound acc) -> acc{projectedVersionDrops = mkInvalidEntry InvalidVersionListing (show position) value "expected a version string" : projectedVersionDrops acc}
            | otherwise -> acc
        FileField position raw
            | projectedFilesActive acc -> collectFile position raw
            | otherwise -> acc
  where
    collectFile position raw =
        let count = position + 1
            bounded = case mode of
                FullRead -> acc
                SelectedRead{} -> withBound (checkVersionCountOf limits count) acc
                MirrorRead{} -> withBound (checkVersionCountOf limits count) acc
         in case raw of
                Just value | isNothing (projectedBound bounded) || mode == FullRead -> retainFile position value bounded
                _ -> bounded
    retainFile position value current = foldl' (retainFileFact limits mode (Just (TreeFile value))) withDrops files
      where
        (files, drops) = decodeIndexFiles [(position, value)]
        withDrops
            | isNothing (projectedBound current) = current{projectedFileDrops = reverse drops <> projectedFileDrops current}
            | otherwise = current

retainFileFact :: Limits -> PyPIRead -> Maybe FilePayload -> PyPIProjection -> IndexFile -> PyPIProjection
retainFileFact limits mode payload current file =
    let (coordinate, memo) = readCoordinate (projectedMemo current) (ifFilename file)
        next = case payload of
            Just served
                | isNothing (projectedBound current) ->
                    let !projected = projectFile file coordinate
                     in current{projectedFiles = (projected, served) : projectedFiles current, projectedMemo = memo}
            _ -> current
     in case (mode, fcVersionKey <$> coordinate) of
            (FullRead, Just version) ->
                let versions = Set.insert version (projectedVersions current)
                    count = projectedArtifactCount current + 1
                    bounded = case checkVersionCountOf limits (Set.size versions) of
                        Left fault -> next{projectedBound = Just fault}
                        Right () -> next{projectedVersions = versions, projectedArtifactCount = count}
                 in withBound (checkArtifactCount limits count) bounded
            _ -> next

withBound :: Either LimitError () -> PyPIProjection -> PyPIProjection
withBound result current = current{projectedBound = projectedBound current <|> either Just (const Nothing) result}

-- | Continue typed counts after retention stops so later version faults retain priority. Decode whole files only for recorded invalid-entry diagnostics.
packedFileStep :: Writer st -> Limits -> PyPIProjection -> Int -> FileValue -> (Either LimitError PyPIProjection -> ST st r) -> ST st r
packedFileStep writer limits acc position value next
    | not (projectedFilesActive acc) = discard writer >> next (Right acc)
    | isJust (projectedBound acc) = do
        discard writer
        next $! Right $! case value of
            FileValue (Just file) -> retainFileFact limits FullRead Nothing acc file
            _ -> acc
    | otherwise = do
        packed <- sealValue writer urlHole
        case value of
            FileValue (Just file) -> do
                let !indexed = file{ifEntryKey = ArrayEntry position}
                    !served = if isJust (rebaseArtifactUrl Just (ifUrl file)) then packed else withoutHole packed
                next (Right $! retainFileFact limits FullRead (Just (PackedFile served)) acc indexed)
            _ -> do
                diagnostic <- decodeWhole writer packed
                next $! collectField limits FullRead acc (FileField position (Just diagnostic))

-- | Check protocol and name before structural counts, then project the source's retained files.
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
    traverse_ (Left . MetadataBoundExceeded) (projectedBound acc)
    let invalid = reverse (projectedFileDrops acc) <> reverse (projectedVersionDrops acc)
    pure (projectSimpleIndex reported invalid (map fst (projectedFiles acc)))

-- Restore source order with every key evaluated, so the served list holds no decoded file.
servedFiles :: [(ProjectedFile, FilePayload)] -> [(EntryKey, Value)]
servedFiles = foldl' serve []
  where
    serve served (file, TreeFile value) = let !key = projectedEntryKey file in (key, value) : served
    serve served _ = served
