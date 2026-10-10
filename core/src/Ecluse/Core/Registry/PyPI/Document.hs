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
import Data.Map.Strict qualified as Map

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
simpleEncoding document =
    case Map.split "files" (KeyMap.toMap (simpleEnvelope document)) of
        (before, after) ->
            Encoding.pairs
                ( Map.foldMapWithKey envelopePair before
                    -- A non-empty Series appends a builder even when its right operand is empty.
                    <> if Map.null after then files else files <> Map.foldMapWithKey envelopePair after
                )
  where
    files = Encoding.pair "files" (Encoding.list (toEncoding . snd) (simpleFiles document))

envelopePair :: Key -> Value -> Encoding.Series
envelopePair key value = Encoding.pair key (toEncoding value)
