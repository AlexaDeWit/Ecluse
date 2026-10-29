-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Preserve exact admitted installation rows in Compact Index responses.
module Ecluse.Core.Registry.RubyGems.Serving (
    assembleRubyGemsDocument,
    serialiseRubyGemsDocument,
) where

import Data.Aeson (Value (Null))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package.Merge (MergePlan, SourceId)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, rubyGemsCached)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.ServedDocument (RenderRefused (RenderRefused), overlaySurvivors)
import Ecluse.Core.Snapshot (Snapshot, snapshotValue)

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
    pure (fromStrict (gemHeader document <> BS.concat [grSource row <> "\n" | row <- gemRows document]))
