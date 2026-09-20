-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Join streamed npm versions, tags and timestamps without retaining the source document.
module Ecluse.Core.Registry.Npm.StreamingProjection (
    NpmProjection,
    emptyProjection,
    collectField,
    finishProjection,
) where

import Data.Aeson (Value (..), parseJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (InvalidEntry, InvalidEntryKind (..), PackageDetails (..), PackageInfo (..), PackageName, mkInvalidEntry)
import Ecluse.Core.Registry.Metadata (MetadataError (..))
import Ecluse.Core.Registry.Metadata.Projection (projectionResult, validateReportedName)
import Ecluse.Core.Registry.Npm.Project (projectName, projectVersionEntryResult)
import Ecluse.Core.Registry.Npm.Streaming (NpmField (..))
import Ecluse.Core.Registry.WireSupport (checkNameAgreement)
import Ecluse.Core.Security (LimitError, Limits, checkArtifactCount, checkVersionCountOf)
import Ecluse.Core.Version (Version, mkVersion)

-- | Independent source maps. Typed releases are built as each retained version finishes.
data NpmProjection = NpmProjection
    { projectedName :: Maybe Value
    , projectedVersions :: Map Text (Either InvalidEntry PackageDetails, Value)
    , projectedTimes :: Map Text (Either InvalidEntry UTCTime, Value)
    , projectedTags :: Map Text (Either InvalidEntry Version, Value)
    , projectedCount :: Int
    }

-- | Start one source projection. No document or input chunk is retained here.
emptyProjection :: NpmProjection
emptyProjection = NpmProjection Nothing mempty mempty mempty 0

-- | Project each release once and enforce the version ceiling while receiving source fields.
collectField :: Limits -> PackageName -> NpmProjection -> NpmField -> Either LimitError NpmProjection
collectField limits name acc = \case
    IgnoredField -> Right acc
    NameField value -> Right acc{projectedName = projectedName acc <|> Just value}
    VersionField key raw -> do
        let count = projectedCount acc + 1
        checkVersionCountOf limits count
        pure
            acc
                { projectedCount = count
                , projectedVersions = maybe (projectedVersions acc) (\value -> let !typed = release key value in firstInsert key (typed, value) (projectedVersions acc)) raw
                }
    TimeField key value -> Right acc{projectedTimes = firstInsert key (decode InvalidPublishTime key value, value) (projectedTimes acc)}
    TagField key value -> Right acc{projectedTags = firstInsert key (mkVersion Npm <$> decode InvalidDistTag key value, value) (projectedTags acc)}
  where
    release key value = case projectVersionEntryResult name (mkVersion Npm key) Nothing value of
        Left err -> Left $! mkInvalidEntry InvalidVersionManifest key value (toText err)
        Right details -> Right $! details
    decode kind key value = first (mkInvalidEntry kind key value . toText) (parseEither parseJSON value)

firstInsert :: (Ord k) => k -> a -> Map k a -> Map k a
firstInsert = Map.insertWith (\_ old -> old)

-- | Bind the reported name and join policy timestamps with their same-source release objects.
finishProjection :: Limits -> PackageName -> Text -> NpmProjection -> Either MetadataError (PackageInfo, Value)
finishProjection limits requested authorPointer acc = do
    reported <- validateReportedName projectName (projectedName acc)
    _ <- projectionResult (checkNameAgreement requested reported ())
    info <- first MetadataBoundExceeded (checkArtifactCount limits package)
    pure (info, document)
  where
    versions = Map.mapMaybe (rightToMaybe . fst) (projectedVersions acc)
    times = Map.mapMaybe (rightToMaybe . fst) (projectedTimes acc)
    tags = Map.mapMaybe (rightToMaybe . fst) (projectedTags acc)
    stamp key details = details{pkgPublishedAt = Map.lookup key times}
    drops entries = lefts (map (fst . snd) (Map.toAscList entries))
    package =
        PackageInfo
            { infoName = requested
            , infoVersions = Map.mapWithKey stamp versions
            , infoDistTags = tags
            , infoInvalidEntries =
                drops (projectedVersions acc)
                    <> drops (projectedTags acc)
                    <> drops (Map.restrictKeys (projectedTimes acc) (Map.keysSet versions))
            }
    document =
        Object
            ( KeyMap.fromList
                [ ("name", fromMaybe Null (projectedName acc))
                , ("author", String authorPointer)
                , ("versions", Object (KeyMap.fromList [(Key.fromText key, withPointer raw) | (key, (_, raw)) <- Map.toList (projectedVersions acc)]))
                , ("time", rawMap (projectedTimes acc))
                , ("dist-tags", rawMap (projectedTags acc))
                ]
            )
    withPointer = \case
        Object fields -> Object (KeyMap.insert "author" (String authorPointer) fields)
        other -> other
    rawMap entries = Object (KeyMap.fromList [(Key.fromText key, raw) | (key, (_, raw)) <- Map.toList entries])
