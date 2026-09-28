-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pack a whole JSON value through the production reader, and render one packed value alone.
module Ecluse.Test.Registry.Packed (
    packValue,
    renderAlone,
) where

import Data.Aeson (Value, encode)
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS

import Ecluse.Core.Registry.Json.Pack (Tree, packTree, sealTable)
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, Piece (..), Pieces (ArrayPieces), RenderPlan (..), Replacement, renderPlan)
import Ecluse.Core.Registry.Json.Shape (Mode (Share), Shape (Generic), readShape)
import Ecluse.Core.Registry.Json.Walk (Step (Finished), withElement)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit))
import Ecluse.Test.Registry.JsonStream (testTable, walkJsonChunks)

{- | Read a value's encoding with every key and string shared, and pack it with a hole at the path. A
trailing space ends a scalar that the lexer would otherwise wait on.
-}
packValue :: [Key.Key] -> Value -> Maybe (DocTable, Packed)
packValue hole value = case walkJsonChunks (MetadataBodyLimit (BS.length body)) walk [body] of
    Right result -> join (rightToMaybe (streamValue result))
    Left _ -> Nothing
  where
    body = toStrict (encode value) <> " "
    walk tokens = withElement tokens $ \element rest ->
        readShape (Generic 64) Share (testTable []) element rest (\(tree :: Tree) table _ -> Finished (Just (sealTable table, packTree hole tree)))

-- | One packed value rendered alone, as the one item of an array under the key @k@.
renderAlone :: DocTable -> Packed -> Maybe Replacement -> ByteString
renderAlone table packed substitute = renderPlan (RenderPlan mempty "k" (ArrayPieces [Piece table packed substitute]))
