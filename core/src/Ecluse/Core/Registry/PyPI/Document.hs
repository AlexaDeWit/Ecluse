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

import Data.Aeson (Encoding, Object, Value (Null), toEncoding)
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

-- | Preserve envelope key order and file source order, overwriting any envelope files key.
simpleEncoding :: SimpleDocument -> Encoding
simpleEncoding document
    | KeyMap.null envelope = Encoding.pairs files
    | otherwise = Encoding.pairs (KeyMap.foldMapWithKey (envelopePair files) (KeyMap.insert "files" Null envelope))
  where
    envelope = simpleEnvelope document
    files = Encoding.pair "files" (Encoding.list (toEncoding . snd) (simpleFiles document))

-- The sentinel's value never reaches the output. Only its key selects the retained files.
envelopePair :: Encoding.Series -> Key -> Value -> Encoding.Series
envelopePair files key value
    | key == "files" = files
    | otherwise = Encoding.pair key (toEncoding value)
