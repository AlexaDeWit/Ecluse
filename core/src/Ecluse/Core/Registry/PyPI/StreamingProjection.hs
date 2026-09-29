-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Join supported PyPI fields while preserving source positions and existing read limits. The typed
projection keeps each file's typed facts, and each read keeps the served files in its own form:
aeson's tree, or packed against the read's table.
-}
module Ecluse.Core.Registry.PyPI.StreamingProjection (
    -- * Typed facts
    PyPIProjection,
    emptyProjection,
    collectField,
    FileFacts,
    fileFacts,
    keepsFile,

    -- * Reading files as aeson's tree
    TreeRead,
    emptyTreeRead,
    keepsTreeFile,
    treeStep,
    finishTree,

    -- * Reading files packed
    PackedRead,
    emptyPackedRead,
    keepsPackedFile,
    packedStep,
    finishPacked,
) where

import Control.Monad.ST (ST)
import Data.Aeson (Value (Object))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Set qualified as Set

import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind (InvalidVersionListing), PackageInfo, PackageName, mkInvalidEntry)
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, withoutHole)
import Ecluse.Core.Registry.Json.Writer (Writer, decodeWhole, discard, sealValue)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataUndecodable))
import Ecluse.Core.Registry.Metadata.Projection (projectionResult, validateReportedName)
import Ecluse.Core.Registry.PyPI.Document (PackedSimple, SimpleDocument, packedSimple, simpleDocument, urlHole)
import Ecluse.Core.Registry.PyPI.Project (FileCoordinate (fcVersionKey), FilenameMemo, filenameMemo, projectName, projectSimpleIndex, readCoordinate)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIFieldOf (..), PyPIRead (..))
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (ifEntryKey, ifFilename), checkApiVersion, decodeIndexFiles)
import Ecluse.Core.Registry.ServedDocument (rebaseArtifactUrl, stringField)
import Ecluse.Core.Registry.WireSupport (checkNameAgreement)
import Ecluse.Core.Security (LimitError (TooManyArtifacts), Limits (maxArtifactCount), checkVersionCountOf)

-- | Parsed files keep their read coordinate, and the count of files kept so far.
data PyPIProjection = PyPIProjection
    { projectedEnvelope :: KeyMap.KeyMap Value
    , projectedFiles :: [(IndexFile, Maybe FileCoordinate)]
    , projectedKept :: Int
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
emptyProjection name = PyPIProjection mempty [] 0 (filenameMemo name) [] [] mempty 0 Nothing True False False False False

-- | Whether a file read now would be retained: files in the first files array, until a limit trips.
keepsFile :: PyPIProjection -> Bool
keepsFile acc = projectedFilesActive acc && isNothing (projectedBound acc)

{- | Decode one compact file with the read's decoder, and stop retaining payloads after an existing
structural limit trips.
-}
collectField :: Limits -> PyPIRead -> (Int -> file -> FileFacts) -> PyPIProjection -> PyPIFieldOf file -> Either LimitError PyPIProjection
collectField limits mode decode acc =
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
    retainFile position value current = foldl' retain withDrops files
      where
        (files, drops) = decode position value
        withDrops
            | null drops || isJust (projectedBound current) = current
            | otherwise = current{projectedFileDrops = reverse drops <> projectedFileDrops current}
    retain current file =
        let (coordinate, memo) = readCoordinate (projectedMemo current) (ifFilename file)
            next
                | isNothing (projectedBound current) = current{projectedFiles = (file, coordinate) : projectedFiles current, projectedKept = projectedKept current + 1, projectedMemo = memo}
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
    -- The first limit a read trips stays its bound. A check that passes leaves the projection as it is.
    withBound result current = case result of
        Left fault | isNothing (projectedBound current) -> current{projectedBound = Just fault}
        _ -> current

-- | A file's typed facts, or the entry that records why it was dropped.
type FileFacts = ([IndexFile], [InvalidEntry])

-- | The typed facts of a file read as aeson's tree.
fileFacts :: Int -> Value -> FileFacts
fileFacts position value = decodeIndexFiles [(position, value)]

-- The entry key of the file the projection kept last, when the field kept one.
keptFile :: PyPIProjection -> PyPIProjection -> Maybe EntryKey
keptFile before after = case projectedFiles after of
    (file, _) : _ | projectedKept after > projectedKept before -> Just (ifEntryKey file)
    _ -> Nothing

-- Check protocol and name before structural counts, then project the source's retained files.
finishParts :: PackageName -> PyPIProjection -> Either MetadataError (PackageInfo, KeyMap.KeyMap Value)
finishParts requested acc = do
    first (const MetadataUndecodable) (parseEither checkApiVersion (projectedEnvelope acc))
    reported <- validateReportedName projectName (KeyMap.lookup "name" (projectedEnvelope acc))
    _ <- projectionResult (checkNameAgreement requested reported ())
    unless (projectedShape acc) (Left MetadataUndecodable)
    traverse_ (Left . MetadataBoundExceeded) (projectedBound acc)
    let invalid = reverse (projectedFileDrops acc) <> reverse (projectedVersionDrops acc)
    pure (projectSimpleIndex reported invalid (reverse (projectedFiles acc)), projectedEnvelope acc)

-- | A read that keeps each file as aeson's tree: the typed facts, and the served files, latest first.
data TreeRead = TreeRead PyPIProjection [(EntryKey, Value)]

-- | Start a tree read of one project.
emptyTreeRead :: PackageName -> TreeRead
emptyTreeRead name = TreeRead (emptyProjection name) []

-- | Whether the tree read keeps a file read now.
keepsTreeFile :: TreeRead -> Bool
keepsTreeFile (TreeRead acc _) = keepsFile acc

-- | Project each file from its tree, and serve the tree as read when the projection keeps it.
treeStep :: Limits -> PyPIRead -> TreeRead -> PyPIFieldOf Value -> Either LimitError TreeRead
treeStep limits mode (TreeRead acc served) field = do
    kept <- collectField limits mode fileFacts acc field
    pure $ TreeRead kept $ case (keptFile acc kept, field) of
        (Just key, FileField _ (Just value)) -> let !entry = key in (entry, value) : served
        _ -> served

-- | Check protocol and name before structural counts, and serve the kept files in source order.
finishTree :: PackageName -> TreeRead -> Either MetadataError (PackageInfo, SimpleDocument)
finishTree requested (TreeRead acc served) = (\(info, envelope) -> (info, simpleDocument envelope (reverse served))) <$> finishParts requested acc

-- | A read that keeps each file packed against its table: the typed facts, and the served files, latest first.
data PackedRead = PackedRead PyPIProjection [(EntryKey, Packed)]

-- | Start a packed read of one project.
emptyPackedRead :: PackageName -> PackedRead
emptyPackedRead name = PackedRead (emptyProjection name) []

-- | Whether the packed read keeps a file read now.
keepsPackedFile :: PackedRead -> Bool
keepsPackedFile (PackedRead acc _) = keepsFile acc

{- | Seal each file the writer finished in the active files array, and project its typed facts from
the file decoded whole. A file the projection does not keep is not served.
-}
packedStep :: Writer st -> Limits -> PackedRead -> PyPIFieldOf () -> (Either LimitError PackedRead -> ST st r) -> ST st r
packedStep writer limits (PackedRead acc served) field next = case field of
    FileField position (Just ())
        | projectedFilesActive acc -> do
            sealed <- sealValue writer urlHole
            value <- decodeWhole writer sealed
            let facts = fileFacts position value
                !file = if rebases value then sealed else withoutHole sealed
            next $! do
                kept <- collectField limits FullRead (\_ () -> facts) acc field
                pure $ PackedRead kept $ case keptFile acc kept of
                    Just key -> (key, file) : served
                    Nothing -> served
        | otherwise -> discard writer >> unchanged
    _ -> unchanged
  where
    unchanged = next $! (`PackedRead` served) <$> collectField limits FullRead (\_ () -> ([], [])) acc field

-- Whether today's rebase rule rewrites the file's URL, so a render may rebase its hole.
rebases :: Value -> Bool
rebases = \case
    Object file | Just url <- stringField "url" file -> isJust (rebaseArtifactUrl Just url)
    _ -> False

-- | Check protocol and name before structural counts, and serve the kept files packed over the read's sealed table.
finishPacked :: PackageName -> DocTable -> PackedRead -> Either MetadataError (PackageInfo, PackedSimple)
finishPacked requested table (PackedRead acc served) = (\(info, envelope) -> (info, packedSimple envelope table (reverse served))) <$> finishParts requested acc
