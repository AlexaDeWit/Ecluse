-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Join supported PyPI fields while preserving source positions and existing read limits.
module Ecluse.Core.Registry.PyPI.StreamingProjection (
    PyPIProjection,
    emptyProjection,
    collectField,
    finishProjection,
) where

import Data.Aeson (Value)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Set qualified as Set

import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind (InvalidVersionListing), PackageInfo, PackageName, mkInvalidEntry, renderPackageName)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataUndecodable))
import Ecluse.Core.Registry.Metadata.Projection (projectionResult, validateReportedName)
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, simpleDocument)
import Ecluse.Core.Registry.PyPI.Project (fileVersionKey, projectName, projectSimpleIndex)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField (..), PyPIRead (..))
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (ifEntryKey, ifFilename), SimpleIndex (..), checkApiVersion, decodeIndexFiles)
import Ecluse.Core.Registry.WireSupport (checkNameAgreement)
import Ecluse.Core.Security (LimitError (TooManyArtifacts), Limits (maxArtifactCount), checkVersionCountOf)

-- | Parsed files share their retained scalar values with the compact serving records.
data PyPIProjection = PyPIProjection
    { projectedEnvelope :: KeyMap.KeyMap Value
    , projectedFiles :: [(IndexFile, Value)]
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

-- | Start one source without retaining any input chunks.
emptyProjection :: PyPIProjection
emptyProjection = PyPIProjection mempty [] [] [] mempty 0 Nothing True False False False False

-- | Decode one compact file and stop retaining payloads after an existing structural limit trips.
collectField :: Limits -> PackageName -> PyPIRead -> PyPIProjection -> PyPIField -> Either LimitError PyPIProjection
collectField limits name mode acc =
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
        let next
                | isNothing (projectedBound current) = current{projectedFiles = (file, value) : projectedFiles current}
                | otherwise = current
         in case (mode, fileVersionKey name (ifFilename file)) of
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
    let files = reverse (projectedFiles acc)
        index = SimpleIndex (renderPackageName reported) (map fst files) (reverse (projectedFileDrops acc) <> reverse (projectedVersionDrops acc))
    pure
        ( projectSimpleIndex reported index
        , simpleDocument (projectedEnvelope acc) [(ifEntryKey file, value) | (file, value) <- files]
        )
