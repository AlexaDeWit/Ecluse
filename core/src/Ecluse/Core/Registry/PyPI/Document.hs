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

    -- * The packed form of a full read
    PackedSimple (..),
    packedSimple,
    packedSimpleDocument,
    packedSimpleBytes,
    urlHole,
) where

import Data.Aeson (Encoding, Object, Value, toEncoding)
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap

import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, packedBytes, packedValue, tableBytes)

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

-- | A full read's envelope, its table, and each retained file packed, with its source coordinate.
data PackedSimple = PackedSimple
    { packedEnvelope :: Object
    , packedTable :: DocTable
    , packedFiles :: [(EntryKey, Packed)]
    }
    deriving stock (Eq, Show)

-- | Bind packed files to their source coordinates, as 'simpleDocument' binds decoded ones.
packedSimple :: Object -> DocTable -> [(EntryKey, Packed)] -> PackedSimple
packedSimple envelope = PackedSimple (KeyMap.delete "files" envelope)

-- | The document as aeson's trees, as the read would have built it.
packedSimpleDocument :: PackedSimple -> SimpleDocument
packedSimpleDocument packed =
    SimpleDocument (packedEnvelope packed) [(key, packedValue (packedTable packed) file Nothing) | (key, file) <- packedFiles packed]

-- | The bytes the packed files and the table hold.
packedSimpleBytes :: PackedSimple -> Int
packedSimpleBytes packed = tableBytes (packedTable packed) + sum (map (packedBytes . snd) (packedFiles packed))

-- | The member path of the string a served file rebases.
urlHole :: [Key.Key]
urlHole = ["url"]
