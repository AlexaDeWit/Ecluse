-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Assemble admitted native rows from exact winning snapshots in their source order.
module Ecluse.Core.Registry.Cargo.Serving (
    assembleCargoDocument,
    serialiseCargoDocument,
) where

import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray (smallArrayFromList)

import Ecluse.Core.Package.Merge (MergePlan, SourceId)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, cargoCached, cargoPacked, cargoRendered)
import Ecluse.Core.Registry.Cargo.Document (PackedIndex (..), packCargoDocument, packedIndexPlan)
import Ecluse.Core.Registry.Json.Packed (LinePlan (..), Piece (..), renderPackedLines)
import Ecluse.Core.Registry.ServedDocument (RenderRefused (RenderRefused), overlaySurvivors)
import Ecluse.Core.Snapshot (Snapshot, snapshotValue)
import Ecluse.Core.Strict (strictElements)

-- | Only exact admitted coordinates enter the LF output. Index records contain no download URL to rebase.
assembleCargoDocument :: Text -> Map SourceId (Snapshot CachedDoc) -> MergePlan -> Maybe CachedDoc -> CachedDoc
assembleCargoDocument _ bySource plan _ =
    fst
        cargoRendered
        LinePlan
            { lineTables = smallArrayFromList [indexTable (snapshotValue source) | source <- Map.elems packed]
            , linePieces = strictElements [piece | (_, piece) <- overlaySurvivors entriesOf indexed plan]
            }
  where
    packed = Map.mapMaybe (traverse asPacked) bySource
    asPacked document = snd cargoPacked document <|> (packCargoDocument <$> snd cargoCached document)
    indexed = Map.fromDistinctAscList (zipWith (\index (sid, source) -> (sid, (index,) <$> source)) [0 ..] (Map.toAscList packed))
    entriesOf (index, source) = [(key, Piece index record) | (key, record) <- indexRecords source]

-- | Refuse damaged or foreign documents rather than emit a non-native empty JSON object.
serialiseCargoDocument :: CachedDoc -> Either RenderRefused LByteString
serialiseCargoDocument document = do
    plan <- maybeToRight RenderRefused (snd cargoRendered document <|> (packedIndexPlan <$> snd cargoPacked document) <|> (packedIndexPlan . packCargoDocument <$> snd cargoCached document))
    maybeToRight RenderRefused (fromStrict <$> renderPackedLines plan)
