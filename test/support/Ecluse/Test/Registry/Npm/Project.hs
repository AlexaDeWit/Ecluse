-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Whole-tree reference projection and held-byte adapters for npm parser comparisons.
module Ecluse.Test.Registry.Npm.Project (parsePackageInfoFromValue, parseVersionList, parseVersionBytes, referenceVersionListParser) where

import Data.Aeson (Value, parseJSON, withObject, (.!=), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (InvalidEntryKind (..), PackageDetails (..), PackageInfo (..), PackageName, invalidKey, mkInvalidEntry)
import Ecluse.Core.Registry (ParseError (ParseError), RegistryResponse (responseBody))
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue))
import Ecluse.Core.Registry.Npm.Project (projectName, projectVersionEntryResult, projectVersionListEntry)
import Ecluse.Core.Registry.Npm.Streaming (NpmContainer (VersionsContainer), NpmFieldOf (BeginContainer, InvalidContainer, VersionField))
import Ecluse.Core.Registry.Npm.VersionListReader (npmVersionListWalk)
import Ecluse.Core.Registry.VersionList (VersionListItem (..), collectVersionList, emptyVersionList, finishVersionList)
import Ecluse.Core.Registry.WireSupport (Projection, checkNameAgreement)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits, defaultLimits, maxNestingDepth)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Registry.JsonStream (walkJsonChunks)
import Ecluse.Test.Registry.Npm.Streaming (NpmRead (VersionListRead), npmFields)
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
parseVersionList = parseVersionBytes . responseBody

-- | Read caller-owned bytes without manufacturing an HTTP response.
parseVersionBytes :: ByteString -> Either ParseError [Version]
parseVersionBytes body = do
    streamed <- first (ParseError . show) (walkJsonChunks (MetadataBodyLimit (BS.length body)) (npmVersionListWalk defaultLimits (collectVersionList defaultLimits) emptyVersionList) [body])
    streamValue streamed >>= finishVersionList

-- | Preserve the former production parser as an independent oracle for the native inventory walk.
referenceVersionListParser :: Limits -> J.Parser VersionListItem
referenceVersionListParser limits = J.objectFound VersionListObject VersionListObject (J.catMaybeI (candidate <$> npmFields (maxNestingDepth limits) VersionListRead))
  where
    candidate (BeginContainer VersionsContainer) = Just VersionListContainer
    candidate (InvalidContainer VersionsContainer) = Just VersionListInvalidContainer
    candidate (VersionField key raw) = Just (projectVersionListEntry key raw)
    candidate _ = Nothing
