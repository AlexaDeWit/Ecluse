-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Join streamed npm versions, tags and timestamps without retaining the source document.
module Ecluse.Core.Registry.Npm.StreamingProjection (
    NpmProjectionOf,
    NpmProjection,
    emptyProjection,
    KeepRelease,
    valueRelease,
    projectRelease,
    collectField,
    collectFieldWith,
    keepsRelease,
    NpmParts (..),
    finishParts,
    finishProjection,

    -- * Packed full reads
    packedRelease,
    packedDocument,
) where

import Data.Aeson (Value (..), parseJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind (..), PackageDetails (..), PackageInfo (..), PackageName, mkInvalidEntry)
import Ecluse.Core.Registry.Json.Intern (Entry)
import Ecluse.Core.Registry.Json.Pack (Tree, packTree, treeMembers, treeValue, withMember)
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed)
import Ecluse.Core.Registry.Metadata (MetadataError (..))
import Ecluse.Core.Registry.Metadata.Projection (projectionResult, validateReportedName)
import Ecluse.Core.Registry.Npm.Document (PackedPackument (..), tarballHole)
import Ecluse.Core.Registry.Npm.Project (projectName, projectVersionEntryResult)
import Ecluse.Core.Registry.Npm.Streaming (NpmContainer (..), NpmField, NpmFieldOf (..), versionListFields)
import Ecluse.Core.Registry.WireSupport (checkNameAgreement)
import Ecluse.Core.Security (LimitError, Limits, checkArtifactCount, checkVersionCountOf)
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Version (Version, mkVersion)

-- | Independent source maps. Typed releases are built as each retained version finishes.
data NpmProjectionOf doc = NpmProjection
    { projectedName :: Maybe Value
    , projectedVersions :: Map Text (Either InvalidEntry PackageDetails, doc)
    , projectedTimes :: Map Text (Either InvalidEntry UTCTime)
    , projectedTags :: Map Text (Either InvalidEntry Version)
    , projectedBookkeeping :: Map Text Value
    , projectedCount :: Int
    , projectedContainers :: Set NpmContainer
    , projectedActiveContainer :: Maybe NpmContainer
    , projectedInvalidContainer :: Bool
    }

-- | A projection whose served releases are aeson's tree.
type NpmProjection = NpmProjectionOf Value

-- | How a read keeps a release: its typed projection, and the form the served document holds.
type KeepRelease release doc = Text -> release -> (Either InvalidEntry PackageDetails, doc)

-- | Keep each release as aeson's tree.
valueRelease :: PackageName -> KeepRelease Value Value
valueRelease name key value = let !typed = projectRelease name key value value in (typed, value)

{- | Project a release's typed facts from the members they read. A release that does not project is
recorded with its whole value.
-}
projectRelease :: PackageName -> Text -> Value -> Value -> Either InvalidEntry PackageDetails
projectRelease name key projected whole = case projectVersionEntryResult name (mkVersion Npm key) Nothing projected of
    Left err -> Left $! mkInvalidEntry InvalidVersionManifest key whole (toText err)
    Right details -> Right $! details

-- | Start one source projection. No document or input chunk is retained here.
emptyProjection :: NpmProjectionOf doc
emptyProjection =
    NpmProjection
        { projectedName = Nothing
        , projectedVersions = mempty
        , projectedTimes = mempty
        , projectedTags = mempty
        , projectedBookkeeping = mempty
        , projectedCount = 0
        , projectedContainers = mempty
        , projectedActiveContainer = Nothing
        , projectedInvalidContainer = False
        }

-- | Whether a release read now under the key would be kept: the key's first in the first versions object.
keepsRelease :: NpmProjectionOf doc -> Text -> Bool
keepsRelease acc key = projectedActiveContainer acc == Just VersionsContainer && Map.notMember key (projectedVersions acc)

-- | Project each release once and enforce the version ceiling while receiving source fields.
collectField :: Limits -> PackageName -> NpmProjection -> NpmField -> Either LimitError NpmProjection
collectField limits name = collectFieldWith limits (valueRelease name)

-- | 'collectField' for any release form, kept as the given function keeps it.
collectFieldWith :: Limits -> KeepRelease release doc -> NpmProjectionOf doc -> NpmFieldOf release -> Either LimitError (NpmProjectionOf doc)
collectFieldWith limits keep acc = \case
    IgnoredField -> Right acc{projectedActiveContainer = Nothing}
    BeginContainer container ->
        Right
            acc
                { projectedContainers = Set.insert container (projectedContainers acc)
                , projectedActiveContainer = if Set.member container (projectedContainers acc) then Nothing else Just container
                }
    InvalidContainer container ->
        Right
            acc
                { projectedContainers = Set.insert container (projectedContainers acc)
                , projectedActiveContainer = Nothing
                , projectedInvalidContainer = projectedInvalidContainer acc || Set.notMember container (projectedContainers acc)
                }
    NameField value -> Right acc{projectedName = projectedName acc <|> Just value}
    VersionField _ _ | projectedActiveContainer acc /= Just VersionsContainer -> Right acc
    VersionField key raw -> do
        let count = projectedCount acc + 1
            counted = acc{projectedCount = count}
        checkVersionCountOf limits count
        pure $ case raw of
            Just value | Map.notMember key (projectedVersions acc) -> retain key value counted
            _ -> counted
    TimeField _ _ | projectedActiveContainer acc /= Just TimeContainer -> Right acc
    TimeField key _ | Map.member key (projectedTimes acc) -> Right acc
    TimeField key value ->
        Right
            acc
                { projectedTimes = firstInsert key (decode force InvalidPublishTime key value) (projectedTimes acc)
                , projectedBookkeeping =
                    if key == "created" || key == "modified"
                        then firstInsert key value (projectedBookkeeping acc)
                        else projectedBookkeeping acc
                }
    TagField _ _ | projectedActiveContainer acc /= Just TagsContainer -> Right acc
    TagField key _ | Map.member key (projectedTags acc) -> Right acc
    TagField key value -> Right acc{projectedTags = firstInsert key (decode (mkVersion Npm) InvalidDistTag key value) (projectedTags acc)}
  where
    -- The walk interned the key and release, so the typed release reads the served document's texts.
    retain key value current = case keep key value of
        (!typed, !doc) -> current{projectedVersions = Map.insert key (typed, doc) (projectedVersions current)}
    -- Force the decoded payload so a successful entry cannot retain its source Value.
    decode convert kind key value = case parseEither parseJSON value of
        Left err -> Left $! mkInvalidEntry kind key value (toText err)
        Right typed -> Right $! convert typed

firstInsert :: (Ord k) => k -> a -> Map k a -> Map k a
firstInsert = Map.insertWith (\_ old -> old)

-- | A finished read's served parts: its name, its releases in the read's form, and its bookkeeping times.
data NpmParts doc = NpmParts
    { partName :: Value
    , partVersions :: Map Text doc
    , partTime :: Map Text Value
    }

-- | Bind the reported name and join policy timestamps with their same-source release objects.
finishProjection :: Limits -> PackageName -> Text -> NpmProjection -> Either MetadataError (PackageInfo, Value)
finishProjection limits requested authorPointer acc = second (valueDocument authorPointer) <$> finishParts limits requested acc

-- | 'finishProjection' for any release form, leaving the served document to its builder.
finishParts :: Limits -> PackageName -> NpmProjectionOf doc -> Either MetadataError (PackageInfo, NpmParts doc)
finishParts limits requested acc = do
    when (projectedInvalidContainer acc) (Left MetadataUndecodable)
    reported <- validateReportedName projectName (projectedName acc)
    _ <- projectionResult (checkNameAgreement requested reported ())
    info <- first MetadataBoundExceeded (checkArtifactCount limits package)
    pure (info, NpmParts (fromMaybe Null (projectedName acc)) (Map.map snd (projectedVersions acc)) (projectedBookkeeping acc))
  where
    versions = Map.mapMaybe (rightToMaybe . fst) (projectedVersions acc)
    times = Map.mapMaybe rightToMaybe (projectedTimes acc)
    tags = Map.mapMaybe rightToMaybe (projectedTags acc)
    stamp key details = details{pkgPublishedAt = Map.lookup key times}
    drops = lefts . Map.elems
    package =
        PackageInfo
            { infoName = requested
            , infoVersions = Map.mapWithKey stamp versions
            , infoDistTags = tags
            , infoInvalidEntries =
                strictElements
                    ( lefts (map fst (Map.elems (projectedVersions acc)))
                        <> drops (projectedTags acc)
                        <> drops (Map.restrictKeys (projectedTimes acc) (Map.keysSet versions))
                    )
            }

valueDocument :: Text -> NpmParts Value -> Value
valueDocument authorPointer parts =
    Object
        ( KeyMap.fromList
            [ ("name", partName parts)
            , ("author", String authorPointer)
            , ("versions", Object (KeyMap.fromList [(Key.fromText key, withPointer raw) | (key, raw) <- Map.toList (partVersions parts)]))
            , ("time", Object (KeyMap.fromList [(Key.fromText key, raw) | (key, raw) <- Map.toList (partTime parts)]))
            ]
        )
  where
    withPointer = \case
        Object fields -> Object (KeyMap.insert "author" (String authorPointer) fields)
        other -> other

{- | Keep each release packed, with the source author pointer under its @author@ key. Its typed facts
read only the members 'versionListFields' names.
-}
packedRelease :: PackageName -> Entry -> Entry -> KeepRelease Tree Packed
packedRelease name authorKey pointer key tree =
    let !typed = projectRelease name key (treeMembers typedKeys tree) (treeValue tree)
        !packed = packTree tarballHole (withMember authorKey pointer tree)
     in (typed, packed)

typedKeys :: [Key.Key]
typedKeys = map Key.fromText versionListFields

-- | The packed document of a finished read, over the read's sealed table.
packedDocument :: Text -> DocTable -> NpmParts Packed -> PackedPackument
packedDocument authorPointer table parts =
    PackedPackument
        { packumentTop =
            KeyMap.fromList
                [ ("name", partName parts)
                , ("author", String authorPointer)
                , ("time", Object (KeyMap.fromList [(Key.fromText key, raw) | (key, raw) <- Map.toList (partTime parts)]))
                ]
        , packumentTable = table
        , packumentVersions = KeyMap.fromList [(Key.fromText key, packed) | (key, packed) <- Map.toList (partVersions parts)]
        }
