-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Join supported PyPI fields while preserving source positions and existing read limits.
module Ecluse.Core.Registry.PyPI.StreamingProjection (
    PyPIProjectionOf,
    PyPIProjection,
    emptyProjection,
    collectField,
    collectFieldWith,
    packedFile,
    keepsFile,
    finishParts,
    finishProjection,
) where

import Data.Aeson (Value)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Set qualified as Set

import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind (InvalidVersionListing), PackageInfo, PackageName, mkInvalidEntry)
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry.Json.Pack (Tree, packTree, treeValue)
import Ecluse.Core.Registry.Json.Packed (Packed)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataUndecodable))
import Ecluse.Core.Registry.Metadata.Projection (projectionResult, validateReportedName)
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, simpleDocument, urlHole)
import Ecluse.Core.Registry.PyPI.Project (FileCoordinate (fcVersionKey), FilenameMemo, filenameMemo, projectName, projectSimpleIndex, readCoordinate)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField, PyPIFieldOf (..), PyPIRead (..))
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (ifEntryKey, ifFilename), checkApiVersion, decodeIndexFiles)
import Ecluse.Core.Registry.WireSupport (checkNameAgreement)
import Ecluse.Core.Security (LimitError (TooManyArtifacts), Limits (maxArtifactCount), checkVersionCountOf)

-- | Parsed files keep their read coordinate and share retained scalars with the compact serving records.
data PyPIProjectionOf doc = PyPIProjection
    { projectedEnvelope :: KeyMap.KeyMap Value
    , projectedFiles :: [(IndexFile, Maybe FileCoordinate, doc)]
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

-- | A projection whose served files are aeson's trees.
type PyPIProjection = PyPIProjectionOf Value

-- | Start one project's source without retaining any input chunks.
emptyProjection :: PackageName -> PyPIProjectionOf doc
emptyProjection name = PyPIProjection mempty [] (filenameMemo name) [] [] mempty 0 Nothing True False False False False

-- | Whether a file read now would be retained: files in the first files array, until a limit trips.
keepsFile :: PyPIProjectionOf doc -> Bool
keepsFile acc = projectedFilesActive acc && isNothing (projectedBound acc)

-- | Decode one compact file and stop retaining payloads after an existing structural limit trips.
collectField :: Limits -> PyPIRead -> PyPIProjection -> PyPIField -> Either LimitError PyPIProjection
collectField limits mode = collectFieldWith limits mode (\value -> (value, value))

-- | Keep each file packed: its typed facts decode from its tree, and the served file packs with a @url@ hole.
packedFile :: Tree -> (Value, Packed)
packedFile tree = (treeValue tree, packTree urlHole tree)

-- | 'collectField' for any file form: the pair is the tree its typed facts decode and the form served.
collectFieldWith :: Limits -> PyPIRead -> (file -> (Value, doc)) -> PyPIProjectionOf doc -> PyPIFieldOf file -> Either LimitError (PyPIProjectionOf doc)
collectFieldWith limits mode keep acc =
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
    retainFile position raw current = foldl' (retain served) withDrops files
      where
        (value, served) = keep raw
        (files, drops) = decodeIndexFiles [(position, value)]
        withDrops
            | isNothing (projectedBound current) = current{projectedFileDrops = reverse drops <> projectedFileDrops current}
            | otherwise = current
    retain served current file =
        let (coordinate, memo) = readCoordinate (projectedMemo current) (ifFilename file)
            next
                | isNothing (projectedBound current) = served `seq` current{projectedFiles = (file, coordinate, served) : projectedFiles current, projectedMemo = memo}
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
finishProjection requested acc = (\(info, envelope, files) -> (info, simpleDocument envelope files)) <$> finishParts requested acc

-- | 'finishProjection' for any file form: the typed view, the envelope, and the served files in source order.
finishParts :: PackageName -> PyPIProjectionOf doc -> Either MetadataError (PackageInfo, KeyMap.KeyMap Value, [(EntryKey, doc)])
finishParts requested acc = do
    first (const MetadataUndecodable) (parseEither checkApiVersion (projectedEnvelope acc))
    reported <- validateReportedName projectName (KeyMap.lookup "name" (projectedEnvelope acc))
    _ <- projectionResult (checkNameAgreement requested reported ())
    unless (projectedShape acc) (Left MetadataUndecodable)
    traverse_ (Left . MetadataBoundExceeded) (projectedBound acc)
    let invalid = reverse (projectedFileDrops acc) <> reverse (projectedVersionDrops acc)
    pure
        ( projectSimpleIndex reported invalid [(file, coordinate) | (file, coordinate, _) <- reverse (projectedFiles acc)]
        , projectedEnvelope acc
        , servedFiles (projectedFiles acc)
        )

-- Restore source order with every key evaluated, so the served list holds no decoded file.
servedFiles :: [(IndexFile, Maybe FileCoordinate, doc)] -> [(EntryKey, doc)]
servedFiles = foldl' (\served (file, _, value) -> let !key = ifEntryKey file in (key, value) : served) []
