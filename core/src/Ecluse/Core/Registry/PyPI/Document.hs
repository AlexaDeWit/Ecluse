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
) where

import Data.Aeson (Encoding, Object, Value, toEncoding)
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.KeyMap qualified as KeyMap

import Ecluse.Core.Package.Entry (EntryKey)

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

-- | Preserve envelope key order and file source order. Source coordinates stay internal.
simpleEncoding :: SimpleDocument -> Encoding
simpleEncoding document =
    Encoding.pairs (fields (< "files") <> Encoding.pair "files" files <> fields (> "files"))
  where
    -- The pinned KeyMap uses ascending key order, including the inserted files field.
    fields include = KeyMap.foldMapWithKey (\key value -> if include key then Encoding.pair key (toEncoding value) else mempty) (simpleEnvelope document)
    files = Encoding.list (toEncoding . snd) (simpleFiles document)
