-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE UnboxedTuples #-}

{- | Pack JSON through the production reader and writer, as a full read packs a retained value, and
measure a packed value's encoding by a reference walk of its opcodes.
-}
module Ecluse.Test.Registry.Packed (
    packBytes,
    packTable,
    packValue,
    renderAlone,
    walkedLength,
) where

import Control.Monad.ST (ST)
import Data.Aeson (Value, encode)
import Data.Array.Byte (ByteArray)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.JsonStream.TokenReader (Tokens)
import Data.Vector qualified as V

import Ecluse.Core.Registry.Json.Intern (InternTable, tableTexts)
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, Piece (..), Pieces (ArrayPieces), RenderPlan (..), UrlPrefix, docTable, encodedLength, opArray, opFalse, opNull, opObject, opShared, opTrue, packedBlob, readVarint, renderPlan, separators)
import Ecluse.Core.Registry.Json.Shape (Mode (Share), Shape, readShape)
import Ecluse.Core.Registry.Json.Walk (Steps (Finished), withElement)
import Ecluse.Core.Registry.Json.Writer (newWriter, sealValue)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError)
import Ecluse.Test.Registry.JsonStream (testTable, walkWritingChunks)

{- | Read chunks under the shape with every key and string shared except the values of @url@ members,
and pack the value with its hole at the path.
-}
packBytes :: Shape -> [Text] -> [ByteString] -> Either LimitError (StreamResult (DocTable, Packed))
packBytes shape hole chunks = sealed <$> packTable shape hole chunks
  where
    sealed result = result{streamValue = first (docTable . tableTexts) <$> streamValue result}

-- | 'packBytes' with the read's table as the read ends, before 'docTable' lays its strings out.
packTable :: Shape -> [Text] -> [ByteString] -> Either LimitError (StreamResult (InternTable, Packed))
packTable shape hole chunks = walkWritingChunks (MetadataBodyLimit (sum (map BS.length chunks))) setup chunks
  where
    setup :: ST st (Tokens st -> ST st (Steps (ST st) (InternTable, Packed)))
    setup =
        newWriter Nothing <&> \writer tokens -> withElement tokens $ \element rest ->
            readShape writer shape Share (testTable ["url"]) element rest $ \() table _ -> do
                value <- sealValue writer hole
                pure (Finished (table, value))

-- | 'packBytes' over a value's encoding. A trailing space ends a scalar the lexer would otherwise wait on.
packValue :: Shape -> [Text] -> Value -> Either LimitError (StreamResult (DocTable, Packed))
packValue shape hole value = packBytes shape hole [toStrict (encode value) <> " "]

-- | One packed value rendered alone: the one item of an array under the key @k@, without the wrapping.
renderAlone :: DocTable -> Maybe UrlPrefix -> Packed -> Maybe ByteString
renderAlone table prefix value = BS.dropEnd 2 . BS.drop 6 <$> renderPlan plan
  where
    plan = RenderPlan{planMembers = mempty, planSlot = "k", planTables = fromList [table], planPieces = ArrayPieces [Piece 0 value], planPrefix = prefix}

{- | Reference encoded length from the value's opcodes and read table, or -1 for a missing string.
Partially applying the table reads its strings once.
-}
walkedLength :: InternTable -> Packed -> Int
walkedLength = lengthOver . V.fromList . toList . tableTexts

lengthOver :: V.Vector Text -> Packed -> Int
lengthOver texts value = case encodedAt texts (packedBlob value) 0 of (# len, _ #) -> len

-- The encoded length of the value at a position, or -1 when it names a string the texts lack, and
-- the position after it.
encodedAt :: V.Vector Text -> ByteArray -> Int -> (# Int, Int #)
encodedAt texts blob position = case SBS.index (SBS.ShortByteString blob) position of
    byte
        | byte == opNull -> (# 4, position + 1 #)
        | byte == opFalse -> (# 5, position + 1 #)
        | byte == opTrue -> (# 4, position + 1 #)
        | byte == opShared -> case readVarint blob (position + 1) of
            (# index, next #) -> (# textLength texts index, next #)
        | byte == opObject -> case readVarint blob (position + 1) of
            (# count, next #) -> members count next (separators count)
        | byte == opArray -> case readVarint blob (position + 1) of
            (# count, next #) -> items count next (separators count)
        | otherwise -> case readVarint blob (position + 1) of (# len, next #) -> (# len, next + len #)
  where
    members !count !at !total
        | count <= 0 = (# total, at #)
        | otherwise = case readVarint blob at of
            (# tagged, next #)
                | even tagged -> case textLength texts (tagged `div` 2) of
                    keyLen
                        | keyLen < 0 -> (# -1, next #)
                        | otherwise -> member count next (keyLen + 1) total
                | otherwise -> member count (next + tagged `div` 2) (tagged `div` 2 + 1) total
    member !count !at !keyed !total = case encodedAt texts blob at of
        (# len, after #)
            | len < 0 -> (# -1, after #)
            | otherwise -> members (count - 1) after (total + keyed + len)
    items !count !at !total
        | count <= 0 = (# total, at #)
        | otherwise = case encodedAt texts blob at of
            (# len, after #)
                | len < 0 -> (# -1, after #)
                | otherwise -> items (count - 1) after (total + len)

-- A table string's encoded length as the sealed table lays it out, or -1 past the table.
textLength :: V.Vector Text -> Int -> Int
textLength texts index = maybe (-1) encodedLength (texts V.!? index)
