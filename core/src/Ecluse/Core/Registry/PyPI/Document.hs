-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Compact Simple indexes keep original file coordinates independently of retained order.
module Ecluse.Core.Registry.PyPI.Document (
    SimpleDocument,
    simpleDocument,
    simpleEnvelope,
    simpleFiles,
    simpleEncoding,

    -- * Packed full reads
    PackedSimple (..),
    packedSimple,
    packedSimpleDocument,
    packedSimplePlan,
    packedSimpleResident,
    urlHole,
) where

import Data.Aeson (Encoding, Object, Value, toEncoding)
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Primitive.SmallArray (smallArrayFromList)

import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, Piece (..), Pieces (ArrayPieces), RenderPlan (..), packedResident, packedValue, tableResident)

-- | Supported envelope fields and files associated with their original source positions.
data SimpleDocument = SimpleDocument
    { simpleEnvelope :: Object
    -- ^ Supported top-level fields, excluding the file array.
    , simpleFiles :: [(EntryKey, Value)]
    -- ^ Original keys in source order, including gaps left by discarded files.
    }
    deriving stock (Eq, Show)

-- | Bind compact files to their source coordinates before admission or assembly.
simpleDocument :: Object -> [(EntryKey, Value)] -> SimpleDocument
simpleDocument envelope = SimpleDocument (KeyMap.delete "files" envelope)

-- | Encode the envelope with the retained files in source order. Source coordinates stay internal.
simpleEncoding :: SimpleDocument -> Encoding
simpleEncoding document =
    Encoding.pairs (KeyMap.foldMapWithKey Encoding.pair (KeyMap.insert "files" files (toEncoding <$> simpleEnvelope document)))
  where
    files = Encoding.list (toEncoding . snd) (simpleFiles document)

-- | A full read's envelope and packed files, with their original coordinates and shared string table.
data PackedSimple = PackedSimple
    { packedEnvelope :: Object
    -- ^ Supported fields outside the file array.
    , packedTable :: DocTable
    -- ^ The read's sealed strings, referenced by every file blob.
    , packedFiles :: [(EntryKey, Packed)]
    -- ^ Source coordinates in source order, including gaps from invalid files.
    }
    deriving stock (Eq, Show)

-- | Bind packed files to source coordinates, excluding any envelope file member.
packedSimple :: Object -> DocTable -> [(EntryKey, Packed)] -> PackedSimple
packedSimple envelope = PackedSimple (KeyMap.delete "files" envelope)

-- | Materialise the serving tree for tree-boundary callers, or refuse missing table strings.
packedSimpleDocument :: PackedSimple -> Maybe SimpleDocument
packedSimpleDocument document =
    simpleDocument (packedEnvelope document) <$> traverse (\(key, file) -> (key,) <$> packedValue (packedTable document) file) (packedFiles document)

-- | Render a source document in source order without reconstructing file trees or rebasing URLs.
packedSimplePlan :: PackedSimple -> RenderPlan
packedSimplePlan document =
    RenderPlan
        { planMembers = packedEnvelope document
        , planSlot = "files"
        , planTables = smallArrayFromList [packedTable document]
        , planPieces = ArrayPieces [Piece 0 file | (_, file) <- packedFiles document]
        , planPrefix = Nothing
        }

-- | Account for table and file storage, including each file's list cell, pair and coordinate.
packedSimpleResident :: PackedSimple -> Int
packedSimpleResident document = tableResident (packedTable document) + sum [64 + packedResident file | (_, file) <- packedFiles document]

-- | The URL string a listing rebases onto its mount's distribution prefix.
urlHole :: [Text]
urlHole = ["url"]
