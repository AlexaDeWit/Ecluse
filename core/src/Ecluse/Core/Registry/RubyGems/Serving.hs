-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Preserve exact admitted Ruby rows before encoding the native Dependencies API.
module Ecluse.Core.Registry.RubyGems.Serving (
    assembleRubyGemsDocument,
    serialiseRubyGemsDocument,
    marshalDependencyRows,
) where

import Data.Aeson (Value (Null))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Core.Package (renderPackageName)
import Ecluse.Core.Package.Merge (MergePlan, SourceId)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, rubyGemsCached)
import Ecluse.Core.Registry.RubyGems.Document (GemDependency (..), GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Marshal (DependencyRow (..), encodeDependencies)
import Ecluse.Core.Registry.RubyGems.Project (coordinateName, coordinatePlatform, coordinateVersion, unGemPlatform)
import Ecluse.Core.Registry.ServedDocument (RenderRefused (RenderRefused), overlaySurvivors)
import Ecluse.Core.Snapshot (Snapshot, snapshotValue)
import Ecluse.Core.Version (renderVersion)

-- | Native order and physical coordinates follow the winning source snapshot.
assembleRubyGemsDocument :: Text -> Map SourceId (Snapshot CachedDoc) -> MergePlan -> Maybe CachedDoc -> CachedDoc
assembleRubyGemsDocument _ bySource plan base = case anchor of
    Nothing -> fst npmCached Null
    Just document -> fst rubyGemsCached document{gemRows = map snd (overlaySurvivors entries native plan)}
  where
    native = Map.mapMaybe (traverse (snd rubyGemsCached)) bySource
    anchor = (base >>= snd rubyGemsCached) <|> (snapshotValue . snd <$> Map.lookupMin native)
    entries document = [(grEntryKey row, row) | row <- gemRows document]

-- | Encoding refuses foreign documents rather than emitting a non-native response.
serialiseRubyGemsDocument :: CachedDoc -> Either RenderRefused LByteString
serialiseRubyGemsDocument cached = do
    document <- maybeToRight RenderRefused (snd rubyGemsCached cached)
    first (const RenderRefused) (fromStrict <$> encodeDependencies maxBound (marshalDependencyRows document))

-- | Retain runtime dependency conjunctions in the syntax consumed by Bundler.
marshalDependencyRows :: GemDocument -> [DependencyRow]
marshalDependencyRows document = map convert (gemRows document)
  where
    convert row =
        DependencyRow
            { dependencyName = renderPackageName (coordinateName (grCoordinate row))
            , dependencyVersion = renderVersion (coordinateVersion (grCoordinate row))
            , dependencyPlatform = unGemPlatform (coordinatePlatform (grCoordinate row))
            , dependencyRequirements = [(renderPackageName (gdName dep), T.intercalate ", " (toList (gdRequirement dep))) | dep <- grDependencies row]
            }
