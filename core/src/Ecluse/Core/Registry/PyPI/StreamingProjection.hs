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
) where

import Data.Aeson (Value)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Set qualified as Set

import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind (InvalidVersionListing), PackageInfo, PackageName, mkInvalidEntry)
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataUndecodable))
import Ecluse.Core.Registry.Metadata.Projection (projectionResult, validateReportedName)
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, simpleDocument)
import Ecluse.Core.Registry.PyPI.Project (FileCoordinate, FilenameMemo, fcVersionKey, filenameMemo, projectName, projectSimpleIndex, readCoordinate)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField (..), PyPIRead (..))
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (ifEntryKey, ifFilename), checkApiVersion, decodeIndexFiles)
import Ecluse.Core.Registry.WireSupport (checkNameAgreement)
import Ecluse.Core.Security (LimitError (TooManyArtifacts), Limits (maxArtifactCount), checkVersionCountOf)

-- | Parsed files keep their read coordinate and share retained scalars with the compact serving records.
data PyPIProjection = PyPIProjection
    { projectedEnvelope :: KeyMap.KeyMap Value
    , projectedFiles :: [(IndexFile, Maybe FileCoordinate, Value)]
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
         in case raw of
                Just value | isNothing (projectedBound bounded) || mode == FullRead -> retainFile position value bounded
                _ -> bounded
    retainFile position value current = foldl' (retain value) withDrops files
      where
        (files, drops) = decodeIndexFiles [(position, value)]
        withDrops
            | isNothing (projectedBound current) = current{projectedFileDrops = reverse drops <> projectedFileDrops current}
            | otherwise = current
    retain value current file =
        let (coordinate, memo) = readCoordinate (projectedMemo current) (ifFilename file)
            next
                | isNothing (projectedBound current) = current{projectedFiles = (file, coordinate, value) : projectedFiles current, projectedMemo = memo}
                | otherwise = current
         in case (mode, fcVersionKey <$> coordinate) of
                (FullRead, Just version) ->
                    let versions = Set.insert version (projectedVersions current)
                        count = projectedArtifactCount current + 1
                        bounded = case checkVersionCountOf limits (Set.size versions) of
                            Left fault -> next{projectedBound = Just fault}
                            Right () -> next{projectedVersions = versions, projectedArtifactCount = count}
                     in withBound (if count > maxArtifactCount limits then Left (TooManyArtifacts count (maxArtifactCount limits)) else Right ()) bounded
                _ -> next
    withBound result current = current{projectedBound = projectedBound current <|> either Just (const Nothing) result}

-- | Check protocol and name before structural counts, then project the source's retained files.
finishProjection :: PackageName -> PyPIProjection -> Either MetadataError (PackageInfo, SimpleDocument)
finishProjection requested acc = do
    first (const MetadataUndecodable) (parseEither checkApiVersion (projectedEnvelope acc))
    reported <- validateReportedName projectName (KeyMap.lookup "name" (projectedEnvelope acc))
    _ <- projectionResult (checkNameAgreement requested reported ())
    unless (projectedShape acc) (Left MetadataUndecodable)
    traverse_ (Left . MetadataBoundExceeded) (projectedBound acc)
    let invalid = reverse (projectedFileDrops acc) <> reverse (projectedVersionDrops acc)
    pure
        ( projectSimpleIndex reported invalid [(file, coordinate) | (file, coordinate, _) <- reverse (projectedFiles acc)]
        , simpleDocument (projectedEnvelope acc) (servedFiles (projectedFiles acc))
        )

-- Restore source order with every key evaluated, so the served list holds no decoded file.
servedFiles :: [(IndexFile, Maybe FileCoordinate, Value)] -> [(EntryKey, Value)]
servedFiles = foldl' (\served (file, _, value) -> let !key = ifEntryKey file in (key, value) : served) []
