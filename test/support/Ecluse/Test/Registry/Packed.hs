-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pack JSON through the production reader and writer, as a full read packs a retained value.
module Ecluse.Test.Registry.Packed (
    packBytes,
    packValue,
    renderAlone,
) where

import Control.Monad.ST (ST)
import Data.Aeson (Value, encode)
import Data.ByteString qualified as BS
import Data.JsonStream.TokenReader (Tokens)

import Ecluse.Core.Registry.Json.Intern (tableTexts)
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, Piece (..), Pieces (ArrayPieces), RenderPlan (..), UrlPrefix, docTable, renderPlan)
import Ecluse.Core.Registry.Json.Shape (Mode (Share), Shape, readShape)
import Ecluse.Core.Registry.Json.Walk (Steps (Finished), withElement)
import Ecluse.Core.Registry.Json.Writer (newWriter, sealValue)
import Ecluse.Core.Registry.JsonStream (StreamResult)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError)
import Ecluse.Test.Registry.JsonStream (testTable, walkWritingChunks)

{- | Read chunks under the shape with every key and string shared except the values of @url@ members,
and pack the value with its hole at the path.
-}
packBytes :: Shape -> [Text] -> [ByteString] -> Either LimitError (StreamResult (DocTable, Packed))
packBytes shape hole chunks = walkWritingChunks (MetadataBodyLimit (sum (map BS.length chunks))) setup chunks
  where
    setup :: ST st (Tokens -> ST st (Steps (ST st) (DocTable, Packed)))
    setup =
        newWriter Nothing <&> \writer tokens -> withElement tokens $ \element rest ->
            readShape writer shape Share (testTable ["url"]) element rest $ \() table _ -> do
                value <- sealValue writer hole
                pure (Finished (docTable (tableTexts table), value))

-- | 'packBytes' over a value's encoding. A trailing space ends a scalar the lexer would otherwise wait on.
packValue :: Shape -> [Text] -> Value -> Either LimitError (StreamResult (DocTable, Packed))
packValue shape hole value = packBytes shape hole [toStrict (encode value) <> " "]

-- | One packed value rendered alone: the one item of an array under the key @k@, without the wrapping.
renderAlone :: DocTable -> Maybe UrlPrefix -> Packed -> Maybe ByteString
renderAlone table prefix value = BS.dropEnd 2 . BS.drop 6 <$> renderPlan plan
  where
    plan = RenderPlan{planMembers = mempty, planSlot = "k", planTables = fromList [table], planPieces = ArrayPieces [Piece 0 value], planPrefix = prefix}
