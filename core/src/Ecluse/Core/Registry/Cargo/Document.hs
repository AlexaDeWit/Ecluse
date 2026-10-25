-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native source records retain physical coordinates and never acquire a synthetic JSON envelope.
module Ecluse.Core.Registry.Cargo.Document (
    CargoDocument,
    cargoDocument,
    cargoRecords,
    PackedIndex (..),
    packCargoDocument,
    packedIndexDocument,
    packedIndexPlan,
    packedIndexResident,
) where

import Data.Aeson (Value)
import Data.Primitive.SmallArray (smallArrayFromList)

import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry.Json.Packed (DocTable, LinePlan (..), Packed, Piece (..), docTable, packedResident, packedValue, tableResident)
import Ecluse.Core.Registry.Json.Writer (packTree)
import Ecluse.Core.Strict (strictElements)

-- | Original records keep their zero-based physical line positions across selection.
newtype CargoDocument = CargoDocument {cargoRecords :: [(EntryKey, Value)]}
    deriving stock (Eq, Show)

-- | Construct a source document without sorting, renumbering or changing optional fields.
cargoDocument :: [(EntryKey, Value)] -> CargoDocument
cargoDocument = CargoDocument . strictElements

-- | One source owns one table and the native order of its packed records.
data PackedIndex = PackedIndex
    { indexTable :: DocTable
    , indexRecords :: [(EntryKey, Packed)]
    }
    deriving stock (Eq, Show)

-- | Pack bounded trees without keeping an input buffer or deferred JSON decoding.
packCargoDocument :: CargoDocument -> PackedIndex
packCargoDocument document = PackedIndex (docTable (smallArrayFromList [])) (strictElements [let !record = packTree [] value in (key, record) | (key, value) <- cargoRecords document])

-- | Recover native records only when every table reference remains valid.
packedIndexDocument :: PackedIndex -> Maybe CargoDocument
packedIndexDocument document = cargoDocument <$> traverse (\(key, value) -> (key,) <$> packedValue (indexTable document) value) (indexRecords document)

-- | Render one source without imposing version order on its records.
packedIndexPlan :: PackedIndex -> LinePlan
packedIndexPlan document = LinePlan (smallArrayFromList [indexTable document]) [Piece 0 value | (_, value) <- indexRecords document]

-- | Retained bytes include source coordinates, list cells, blobs and the source table once.
packedIndexResident :: PackedIndex -> Int
packedIndexResident document = tableResident (indexTable document) + sum [64 + packedResident value | (_, value) <- indexRecords document]
