-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Join streamed npm versions, tags and timestamps without retaining the source document. The typed
projection keeps each release's typed facts, and each read keeps the served releases in its own form:
aeson's tree, or packed against the read's table.
-}
module Ecluse.Core.Registry.Npm.StreamingProjection (
    -- * Typed facts
    NpmProjection,
    TypedRelease,
    emptyProjection,
    keepsRelease,
    collectField,

    -- * Reading releases as aeson's tree
    TreeRead,
    emptyTreeRead,
    keepsTreeRelease,
    treeStep,
    finishTree,

    -- * Reading releases packed
    PackedRead,
    emptyPackedRead,
    keepsPackedRelease,
    packedStep,
    finishPacked,
) where

import Control.Monad.ST (ST)
import Data.Aeson (Value (..), parseJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind (..), PackageDetails (..), PackageInfo (..), PackageName, mkInvalidEntry)
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, withoutHole)
import Ecluse.Core.Registry.Json.Writer (Pick (..), Writer, decodePicked, decodeWhole, discard, replacedMember, sealValue)
import Ecluse.Core.Registry.Metadata (MetadataError (..))
import Ecluse.Core.Registry.Metadata.Projection (projectionResult, validateReportedName)
import Ecluse.Core.Registry.Npm.Document (PackedPackument (..), tarballHole, tarballUrl)
import Ecluse.Core.Registry.Npm.Project (projectName, projectVersionEntryResult)
import Ecluse.Core.Registry.Npm.Streaming (NpmContainer (..), NpmFieldOf (..), versionListFields)
import Ecluse.Core.Registry.Npm.Wire (distFields)
import Ecluse.Core.Registry.ServedDocument (rebaseArtifactUrl)
import Ecluse.Core.Registry.WireSupport (checkNameAgreement, parsePublishTime)
import Ecluse.Core.Security (LimitError, Limits, checkArtifactCount, checkVersionCountOf)
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Version (Version, mkVersion)

-- | A kept release's typed facts, or the entry that records why it was dropped.
type TypedRelease = Either InvalidEntry PackageDetails

-- | Independent source maps. Typed releases are built as each retained version finishes.
data NpmProjection = NpmProjection
    { projectedName :: Maybe Value
    , projectedVersions :: Map Text TypedRelease
    , projectedTimes :: Map Text (Either InvalidEntry UTCTime)
    , projectedTags :: Map Text (Either InvalidEntry Version)
    , projectedBookkeeping :: Map Text Value
    , projectedCount :: Int
    , projectedContainers :: Set NpmContainer
    , projectedActiveContainer :: Maybe NpmContainer
    , projectedInvalidContainer :: Bool
    }

-- | Start one source projection. No document or input chunk is retained here.
emptyProjection :: NpmProjection
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
keepsRelease :: NpmProjection -> Text -> Bool
keepsRelease acc key = projectedActiveContainer acc == Just VersionsContainer && Map.notMember key (projectedVersions acc)

-- Receive a release the projection keeps, with its typed facts, and enforce the version ceiling.
collectRelease :: Limits -> NpmProjection -> NpmFieldOf release -> Text -> TypedRelease -> Either LimitError NpmProjection
collectRelease limits acc field key typed = (\counted -> counted{projectedVersions = Map.insert key typed (projectedVersions counted)}) <$> collectField limits acc field

-- | Enforce the version ceiling while receiving source fields. A release's own payload is not read here.
collectField :: Limits -> NpmProjection -> NpmFieldOf release -> Either LimitError NpmProjection
collectField limits acc = \case
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
    VersionField _ _ -> do
        let count = projectedCount acc + 1
        checkVersionCountOf limits count
        pure acc{projectedCount = count}
    TimeField _ _ | projectedActiveContainer acc /= Just TimeContainer -> Right acc
    TimeField key _ | Map.member key (projectedTimes acc) -> Right acc
    TimeField key value ->
        Right
            acc
                { projectedTimes = firstInsert key (decode parsePublishTime force InvalidPublishTime key value) (projectedTimes acc)
                , projectedBookkeeping =
                    if key == "created" || key == "modified"
                        then firstInsert key value (projectedBookkeeping acc)
                        else projectedBookkeeping acc
                }
    TagField _ _ | projectedActiveContainer acc /= Just TagsContainer -> Right acc
    TagField key _ | Map.member key (projectedTags acc) -> Right acc
    TagField key value -> Right acc{projectedTags = firstInsert key (decode parseJSON (mkVersion Npm) InvalidDistTag key value) (projectedTags acc)}
  where
    -- Force the decoded payload so a successful entry cannot retain its source Value.
    decode parse convert kind key value = case parseEither parse value of
        Left err -> Left $! mkInvalidEntry kind key value (toText err)
        Right typed -> Right $! convert typed

firstInsert :: (Ord k) => k -> a -> Map k a -> Map k a
firstInsert = Map.insertWith (\_ old -> old)

-- A release's typed facts from the members they read, or why it does not project.
projectRelease :: PackageName -> Text -> Value -> Either Text PackageDetails
projectRelease name key projected = case projectVersionEntryResult name (mkVersion Npm key) Nothing projected of
    Left err -> Left (toText err)
    Right details -> Right $! details

-- A dropped release, recorded with the whole release as read.
invalidRelease :: Text -> Value -> Text -> TypedRelease
invalidRelease key whole reason = Left $! mkInvalidEntry InvalidVersionManifest key whole reason

-- The parts a finished read serves beside its releases: the reported name and the bookkeeping times.
data NpmParts = NpmParts Value (Map Text Value)

-- Bind the reported name and join policy timestamps with their same-source releases.
finishParts :: Limits -> PackageName -> NpmProjection -> Either MetadataError (PackageInfo, NpmParts)
finishParts limits requested acc = do
    when (projectedInvalidContainer acc) (Left MetadataUndecodable)
    reported <- validateReportedName projectName (projectedName acc)
    _ <- projectionResult (checkNameAgreement requested reported ())
    info <- first MetadataBoundExceeded (checkArtifactCount limits package)
    pure (info, NpmParts (fromMaybe Null (projectedName acc)) (projectedBookkeeping acc))
  where
    versions = Map.mapMaybe rightToMaybe (projectedVersions acc)
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
                    ( drops (projectedVersions acc)
                        <> drops (projectedTags acc)
                        <> drops (Map.restrictKeys (projectedTimes acc) (Map.keysSet versions))
                    )
            }

-- | A read that keeps each release as aeson's tree: the typed facts, and the served releases.
data TreeRead = TreeRead NpmProjection (Map Text Value)

-- | Start a tree read.
emptyTreeRead :: TreeRead
emptyTreeRead = TreeRead emptyProjection mempty

-- | Whether the tree read keeps a release read now under the key.
keepsTreeRelease :: TreeRead -> Text -> Bool
keepsTreeRelease (TreeRead acc _) = keepsRelease acc

-- | Project each kept release from its tree, and serve the tree as read.
treeStep :: Limits -> PackageName -> TreeRead -> NpmFieldOf Value -> Either LimitError TreeRead
treeStep limits name (TreeRead acc served) field = case field of
    VersionField key (Just value) | keepsRelease acc key -> do
        let !typed = either (invalidRelease key value) Right (projectRelease name key value)
        kept <- collectRelease limits acc field key typed
        pure (TreeRead kept (Map.insert key value served))
    _ -> (`TreeRead` served) <$> collectField limits acc field

-- | Bind the reported name, and serve each kept release with the source author pointer.
finishTree :: Limits -> PackageName -> Text -> TreeRead -> Either MetadataError (PackageInfo, Value)
finishTree limits requested authorPointer (TreeRead acc served) = second document <$> finishParts limits requested acc
  where
    document (NpmParts name time) =
        Object
            ( KeyMap.fromList
                [ ("name", name)
                , ("author", String authorPointer)
                , ("versions", Object (KeyMap.fromList [(Key.fromText key, withPointer raw) | (key, raw) <- Map.toList served]))
                , ("time", Object (KeyMap.fromList [(Key.fromText key, raw) | (key, raw) <- Map.toList time]))
                ]
            )
    withPointer = \case
        Object fields -> Object (KeyMap.insert "author" (String authorPointer) fields)
        other -> other

-- | A read that keeps each release packed against its table: the typed facts, and the served releases, latest first.
data PackedRead = PackedRead NpmProjection [(Text, Packed)]

-- | Start a packed read.
emptyPackedRead :: PackedRead
emptyPackedRead = PackedRead emptyProjection []

-- | Whether the packed read keeps a release read now under the key.
keepsPackedRelease :: PackedRead -> Text -> Bool
keepsPackedRelease (PackedRead acc _) = keepsRelease acc

{- | Seal each kept release the writer finished, and project its typed facts from the members they
read. A release the read does not keep is discarded.
-}
packedStep :: Writer st -> Limits -> PackageName -> PackedRead -> NpmFieldOf () -> (Either LimitError PackedRead -> ST st r) -> ST st r
packedStep writer limits name (PackedRead acc served) field next = case field of
    VersionField key (Just ())
        | keepsRelease acc key -> do
            sealed <- sealValue writer tarballHole
            picked <- decodePicked writer typedMembers sealed
            typed <- case projectRelease name key picked of
                Right details -> pure (Right details)
                Left reason -> (\whole original -> invalidRelease key (asRead original whole) reason) <$> decodeWhole writer sealed <*> replacedMember writer
            let !release = if rebases picked then sealed else withoutHole sealed
            next $! do
                kept <- collectRelease limits acc field key typed
                pure (PackedRead kept ((key, release) : served))
        | otherwise -> discard writer >> unchanged
    _ -> unchanged
  where
    unchanged = next $! (`PackedRead` served) <$> collectField limits acc field
    -- The release as the source wrote it: its own author member, or none, in place of the pointer.
    asRead original = \case
        Object fields -> Object (maybe (KeyMap.delete "author") (KeyMap.insert "author") original fields)
        other -> other

-- Whether the rebase rule rewrites the release's tarball URL, so a render may rebase its hole.
rebases :: Value -> Bool
rebases release = isJust (tarballUrl release >>= rebaseArtifactUrl Just)

-- The members a release's typed facts read: the version list's fields, and of @dist@ what 'Dist' reads.
typedMembers :: Pick
typedMembers = Only [(field, if field == "dist" then Only [(member, Whole) | member <- distFields] else Whole) | field <- versionListFields]

-- | Bind the reported name, and serve each kept release packed over the read's sealed table.
finishPacked :: Limits -> PackageName -> Text -> DocTable -> PackedRead -> Either MetadataError (PackageInfo, PackedPackument)
finishPacked limits requested authorPointer table (PackedRead acc served) = second document <$> finishParts limits requested acc
  where
    document (NpmParts name time) =
        PackedPackument
            { packumentTop =
                KeyMap.fromList
                    [ ("name", name)
                    , ("author", String authorPointer)
                    , ("time", Object (KeyMap.fromList [(Key.fromText key, raw) | (key, raw) <- Map.toList time]))
                    ]
            , packumentTable = table
            , packumentVersions = KeyMap.fromMap (Map.fromDistinctAscList [(Key.fromText key, release) | (key, release) <- sortBy (comparing fst) served])
            }
