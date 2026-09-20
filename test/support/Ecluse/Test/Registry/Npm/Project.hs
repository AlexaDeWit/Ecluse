-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Whole-tree reference projection and held-byte adapters for npm parser comparisons.
module Ecluse.Test.Registry.Npm.Project (parsePackageInfoFromValue, parseVersionList) where

import Data.Aeson (Value, parseJSON, withObject, (.!=), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (InvalidEntryKind (..), PackageDetails (..), PackageInfo (..), PackageName, invalidKey, mkInvalidEntry)
import Ecluse.Core.Registry (ParseError (ParseError), RegistryResponse (responseBody))
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue))
import Ecluse.Core.Registry.Npm.Project (projectName, projectVersionEntryResult, versionListParser)
import Ecluse.Core.Registry.VersionList (collectVersionList, emptyVersionList, finishVersionList)
import Ecluse.Core.Registry.WireSupport (Projection, checkNameAgreement)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), defaultLimits)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)
import Ecluse.Test.Registry.WireSupport (partitionLenient)

-- | Keep the prior whole-tree policy projection as an independent comparison for streamed captures.
parsePackageInfoFromValue :: PackageName -> Value -> Either ParseError (Projection PackageInfo)
parsePackageInfoFromValue requested value = do
    (reported, rawVersions, rawTimes, rawTags) <- first (ParseError . toText) (parseEither fields value)
    name <- projectName reported
    let projected = Map.mapWithKey (release name) rawVersions
        versions = Map.mapMaybe rightToMaybe projected
        (times, timeDrops) = partitionLenient InvalidPublishTime (parseEither parseJSON) rawTimes
        (tags, tagDrops) = partitionLenient InvalidDistTag (parseEither parseJSON) rawTags
        stamp key details = details{pkgPublishedAt = Map.lookup key (times :: Map Text UTCTime)}
        info =
            PackageInfo
                { infoName = name
                , infoVersions = Map.mapWithKey stamp versions
                , infoDistTags = Map.map (mkVersion Npm) (tags :: Map Text Text)
                , infoInvalidEntries = lefts (Map.elems projected) <> tagDrops <> filter ((`Set.member` Map.keysSet versions) . invalidKey) timeDrops
                }
    pure (checkNameAgreement requested name info)
  where
    release name key raw = first (mkInvalidEntry InvalidVersionManifest key raw . toText) (projectVersionEntryResult name (mkVersion Npm key) Nothing raw)
    fields :: Value -> Parser (Text, Map Text Value, Map Text Value, Map Text Value)
    fields = withObject "npm packument" $ \o ->
        (,,,) <$> o .:? "name" .!= "" <*> o .:? "versions" .!= mempty <*> o .:? "time" .!= mempty <*> o .:? "dist-tags" .!= mempty

-- | Parse caller-owned bytes through the production selective inventory parser.
parseVersionList :: RegistryResponse -> Either ParseError [Version]
parseVersionList response = do
    let body = responseBody response
    streamed <- first (ParseError . show) (parseJsonChunks (MetadataBodyLimit (BS.length body)) (versionListParser defaultLimits) (collectVersionList defaultLimits) emptyVersionList [body])
    streamValue streamed >>= finishVersionList
