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

-- The empty state avoids a trailing Series append. Strict payloads keep the fold eager.
data EnvelopePairs
    = NoEnvelopePairs
    | PendingFiles !Encoding.Series
    | CompletePairs !Encoding.Series

-- The ordered fold inserts files once and discards an envelope value at the same key.
envelopePair :: Encoding.Series -> Key -> Value -> EnvelopePairs -> EnvelopePairs
envelopePair files key value !rest = case compare key "files" of
    GT -> PendingFiles (prependPair (encodeField key value) rest)
    EQ -> CompletePairs (prependPair files rest)
    LT ->
        let !suffix = finishEnvelope files rest
         in CompletePairs (encodeField key value <> suffix)

finishEnvelope :: Encoding.Series -> EnvelopePairs -> Encoding.Series
finishEnvelope files = \case
    NoEnvelopePairs -> files
    PendingFiles rest -> files <> rest
    CompletePairs pairs -> pairs

prependPair :: Encoding.Series -> EnvelopePairs -> Encoding.Series
prependPair !pair = \case
    NoEnvelopePairs -> pair
    PendingFiles rest -> pair <> rest
    CompletePairs rest -> pair <> rest

encodeField :: Key -> Value -> Encoding.Series
encodeField key value =
    let !encoding = toEncoding value
     in Encoding.pair key encoding

-- | Preserve envelope key order and file source order, overwriting any envelope files key.
simpleEncoding :: SimpleDocument -> Encoding
simpleEncoding document
    | KeyMap.null envelope = Encoding.pairs files
    | otherwise = Encoding.pairs (finishEnvelope files (KeyMap.foldrWithKey (envelopePair files) NoEnvelopePairs envelope))
  where
    envelope = simpleEnvelope document
    -- An unforced files encoder can retain the assembled document through a Series payload.
    !filesEncoding = Encoding.list (toEncoding . snd) (simpleFiles document)
    files = Encoding.pair "files" filesEncoding
