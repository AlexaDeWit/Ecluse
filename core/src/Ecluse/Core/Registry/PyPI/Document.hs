-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Compact Simple indexes keep original file coordinates independently of retained order.
module Ecluse.Core.Registry.PyPI.Document (
    SimpleDocument,
    simpleDocument,
    simpleEnvelope,
    simpleFiles,
    restrictSimpleFiles,
    simpleSourceCompanions,
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
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Set qualified as Set

import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry (CompanionRequirement (DeclaredCompanions))
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

-- | Keep exact source coordinates, preserving the envelope, order and repeated matching entries.
restrictSimpleFiles :: Set EntryKey -> SimpleDocument -> SimpleDocument
restrictSimpleFiles keys document =
    simpleDocument (simpleEnvelope document) (filter ((`Set.member` keys) . fst) (simpleFiles document))

-- | A retained file needs a companion only where its entry declares core metadata.
simpleSourceCompanions :: CompanionRequirement
simpleSourceCompanions = DeclaredCompanions

-- The empty state avoids a trailing Series append. Strict payloads keep the fold eager.
data EnvelopePairs
    = NoEnvelopePairs
    | PendingFiles !Encoding.Series
    | CompletePairs !Encoding.Series

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
