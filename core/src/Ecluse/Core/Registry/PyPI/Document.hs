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
import Data.Aeson.Key (Key)
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
    Encoding.pairs (KeyMap.foldrWithKey envelopePair id (simpleEnvelope document) files)
  where
    files = Encoding.pair "files" (Encoding.list (toEncoding . snd) (simpleFiles document))

-- The pending files field keeps its former position in the pinned ascending KeyMap fold.
envelopePair :: Key -> Value -> (Encoding.Series -> Encoding.Series) -> Encoding.Series -> Encoding.Series
envelopePair key value next pending
    | key < "files" = Encoding.pair key (toEncoding value) <> next pending
    | otherwise = pending <> Encoding.pair key (toEncoding value) <> next mempty
