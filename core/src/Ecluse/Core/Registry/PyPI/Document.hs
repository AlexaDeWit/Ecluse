-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Compact Simple indexes keep original file coordinates independently of retained order.
module Ecluse.Core.Registry.PyPI.Document (
    SimpleDocument,
    simpleDocument,
    simpleEnvelope,
    simpleFiles,
    simpleValue,
) where

import Data.Aeson (Object, Value (Array, Object))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Vector qualified as V

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

-- | Render the retained fields. Source coordinates remain internal to admission and assembly.
simpleValue :: SimpleDocument -> Value
simpleValue document =
    Object (KeyMap.insert "files" (Array (V.fromList (map snd (simpleFiles document)))) (simpleEnvelope document))
