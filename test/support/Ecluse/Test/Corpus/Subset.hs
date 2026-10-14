-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Registry documents cut to part of their versions, as a private registry that holds only the
versions a deployment consumed serves them. A cut document keeps its other fields as captured.
-}
module Ecluse.Test.Corpus.Subset (
    -- * Choosing versions
    byPublishTime,
    newestVersions,
    oldestVersions,

    -- * Cutting documents
    keepNpmVersions,
    keepPyPIVersions,
    heavyNpmBase,
    heavyPyPIBase,

    -- * The newest share of a capture
    newestNpmShare,
    newestPyPIShare,
) where

import Data.Aeson (Value (Array, Object, String), eitherDecodeStrict, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Ecluse.Core.Package (Artifact (artFilename), PackageDetails (pkgArtifacts, pkgPublishedAt), PackageInfo (infoVersions), PackageName)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Version (canonicalPep440, renderVersion)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)

-- | A projection's version keys, oldest first by publish time. A version with no time counts as oldest.
byPublishTime :: PackageInfo -> [Text]
byPublishTime info = map fst (sortOn (\(key, details) -> (pkgPublishedAt details, key)) (Map.toList (infoVersions info)))

-- | The newest share of a projection's versions by publish time, rounded up to at least one version.
newestVersions :: Rational -> PackageInfo -> Set Text
newestVersions share info = Set.fromList (drop (length published - shareOf share published) published)
  where
    published = byPublishTime info

-- | The oldest share of a projection's versions by publish time, at least one version.
oldestVersions :: Rational -> PackageInfo -> Set Text
oldestVersions share info = Set.fromList (take (shareOf share published) published)
  where
    published = byPublishTime info

shareOf :: Rational -> [a] -> Int
shareOf share items = max 1 (min count (ceiling (share * fromIntegral count)))
  where
    count = length items

-- | Keep selected npm releases, their times, and tags that point at a kept release.
keepNpmVersions :: Set Text -> Value -> Value
keepNpmVersions kept = onFields $ \field value -> case (field, value) of
    ("versions", Object releases) -> Object (KeyMap.filterWithKey (\key _ -> Key.toText key `Set.member` kept) releases)
    ("time", Object times) -> Object (KeyMap.filterWithKey (\key _ -> Key.toText key `elem` ["created", "modified"] || Key.toText key `Set.member` kept) times)
    ("dist-tags", Object tags) -> Object (KeyMap.filter keptTarget tags)
    _ -> value
  where
    keptTarget = \case
        String target -> target `Set.member` kept
        _ -> False

-- | Keep the files and version entries that belong to the selected PyPI releases.
keepPyPIVersions :: PackageInfo -> Set Text -> Value -> Value
keepPyPIVersions info kept = onFields $ \field value -> case (field, value) of
    ("files", Array entries) -> toJSON (filter keptFile (toList entries))
    ("versions", Array entries) -> toJSON (filter keptVersion (toList entries))
    _ -> value
  where
    files = Set.fromList [artFilename artifact | details <- Map.elems (Map.restrictKeys (infoVersions info) kept), artifact <- toList (pkgArtifacts details)]
    keptFile = \case
        Object entry | Just (String filename) <- KeyMap.lookup "filename" entry -> filename `Set.member` files
        _ -> False
    keptVersion = \case
        String version -> maybe False ((`Set.member` kept) . renderVersion) (canonicalPep440 version)
        _ -> False

-- | The newest share of an npm capture's versions by publish time, cut as 'keepNpmVersions' cuts.
newestNpmShare :: Rational -> PackageName -> ByteString -> Either String Value
newestNpmShare share name bytes = do
    (info, _) <- first show (projectNpmManifest defaultLimits name bytes)
    keepNpmVersions (newestVersions share info) <$> eitherDecodeStrict bytes

-- | The newest share of a PyPI capture's versions by upload time, cut as 'keepPyPIVersions' cuts.
newestPyPIShare :: Rational -> PackageName -> ByteString -> Either String Value
newestPyPIShare share name bytes = do
    (info, _) <- first show (projectPyPIIndex defaultLimits name bytes)
    keepPyPIVersions info (newestVersions share info) <$> eitherDecodeStrict bytes

onFields :: (Text -> Value -> Value) -> Value -> Value
onFields change = \case
    Object fields -> Object (KeyMap.mapWithKey (change . Key.toText) fields)
    other -> other

-- | Add release deprecation text whose total width follows the requested byte count.
heavyNpmBase :: Int -> Value -> Value
heavyNpmBase size = \case
    Object fields -> Object $ case KeyMap.lookup "versions" fields of
        Just (Object releases) -> KeyMap.insert "versions" (Object (deprecated releases)) fields
        _ -> fields
    other -> other
  where
    deprecated releases = KeyMap.mapWithKey (\key -> deprecate (padded (Key.toText key) (size `div` max 1 (KeyMap.size releases)))) releases
    deprecate notice = \case
        Object release -> Object (KeyMap.insert "deprecated" (String notice) release)
        other -> other

-- | Add project status text with the requested width.
heavyPyPIBase :: Int -> Value -> Value
heavyPyPIBase size = \case
    Object fields -> Object (KeyMap.insert "project-status" (object ["status" .= ("active" :: Text), "reason" .= padded "heavy base" size]) fields)
    other -> other

padded :: Text -> Int -> Text
padded label width = label <> " " <> T.replicate (max 0 (width - T.length label - 1)) "x"
