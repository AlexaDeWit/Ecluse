-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The tree a full read builds for each retained release or file, and its packing into
"Ecluse.Core.Registry.Json.Packed". The tree holds the table's entries, so a release packs by
index and its typed facts read the table's shared texts. Each container carries its blob size and
encoded length from the moment it is built, so a release packs in one pass. It lives only until
its release packs.
-}
module Ecluse.Core.Registry.Json.Pack (
    Tree,
    packTree,
    treeValue,
    treeMembers,
    withMember,
    sealTable,
) where

import Control.Monad.ST (ST, runST)
import Data.Aeson (Value (..), toEncoding)
import Data.Aeson.Encoding (fromEncoding)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Builder.Extra qualified as Builder
import Data.ByteString.Short qualified as SBS
import Data.Map.Internal (Map (Bin, Tip))
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Vector qualified as V

import Data.JsonStream.TokenParser (Element, TokenResult)
import Ecluse.Core.Registry.Json.Intern (Entry (..), InternTable, tableStrings)
import Ecluse.Core.Registry.Json.Packed (Blob, DocTable, Opcode (..), Packed, blobOffset, docTable, encodedLength, finishBlob, newBlob, putOpcode, putRaw, putString, putVarint, varintSize)

import Ecluse.Core.Registry.Json.Shape (Mode, Retained (..), Shape, readShape)
import Ecluse.Core.Registry.Json.Walk (Step)

-- | One retained value as a full read builds it. An object member under a table key is a 'Keyed'.
data Tree
    = Shared !Entry
    | Own !Text !Int
    | Literal !Value
    | Number' !Value !SBS.ShortByteString
    | Members !Int !Int !(KeyMap.KeyMap Tree)
    | Items !Int !Int !Int ![Tree]
    | Keyed !Entry !Tree

instance Retained Tree where
    sharedString = Shared
    ownString text = Own text (encodedLength text)
    whole = wholeTree
    emptyContainer = emptyItems
    sharedMember = Keyed
    object fields =
        let count = KeyMap.size fields
            members = KeyMap.toMap fields
         in Members (1 + varintSize count + mapBytes members 0) (separators count + mapEncoded members 0) fields
    array count items = Items count (1 + varintSize count + sumSizes nodeBytes items) (separators count + sumSizes nodeEncoded items) items

{-# SPECIALIZE readShape :: Shape -> Mode -> InternTable -> Element -> TokenResult -> (Tree -> InternTable -> TokenResult -> Step s) -> Step s #-}

-- The empty array, built once.
emptyItems :: Tree
emptyItems = Items 0 2 2 []

-- Two brackets and a comma between each pair of members or items.
separators :: Int -> Int
separators count = 2 + max 0 (count - 1)

-- A value taken whole, as a tree with its own keys and strings.
wholeTree :: Value -> Tree
wholeTree = \case
    String text -> ownString text
    Object fields -> object (KeyMap.map wholeTree fields)
    Array values -> array (V.length values) (reverse (map wholeTree (toList values)))
    value@(Number _) -> Number' value (SBS.toShort (toStrict (Builder.toLazyByteStringWith (Builder.untrimmedStrategy 32 32) mempty (fromEncoding (toEncoding value)))))
    value -> Literal value

-- The blob bytes of a value.
nodeBytes :: Tree -> Int
nodeBytes = \case
    Shared entry -> 1 + varintSize (entryIndex entry)
    Own _ len -> 1 + varintSize len + len
    Literal _ -> 1
    Number' _ bytes -> 1 + varintSize (SBS.length bytes) + SBS.length bytes
    Members bytes _ _ -> bytes
    Items _ bytes _ _ -> bytes
    Keyed _ tree -> nodeBytes tree

-- The encoded length of a value.
nodeEncoded :: Tree -> Int
nodeEncoded = \case
    Shared entry -> entryLength entry
    Own _ len -> len
    Literal (Bool False) -> 5
    Literal _ -> 4
    Number' _ bytes -> SBS.length bytes
    Members _ encoded _ -> encoded
    Items _ _ encoded _ -> encoded
    Keyed _ tree -> nodeEncoded tree

-- Each member's key and value, in blob bytes and in encoded length.
mapBytes :: Map Key.Key Tree -> Int -> Int
mapBytes Tip !total = total
mapBytes (Bin _ key tree left right) total = mapBytes right (mapBytes left total + keyBytes + nodeBytes tree)
  where
    keyBytes = case tree of
        Keyed entry _ -> varintSize (2 * entryIndex entry)
        _ -> let len = encodedLength (Key.toText key) in varintSize (2 * len + 1) + len

mapEncoded :: Map Key.Key Tree -> Int -> Int
mapEncoded Tip !total = total
mapEncoded (Bin _ key tree left right) total = mapEncoded right (mapEncoded left total + keyLength + 1 + nodeEncoded tree)
  where
    keyLength = case tree of
        Keyed entry _ -> entryLength entry
        _ -> encodedLength (Key.toText key)

sumSizes :: (Tree -> Int) -> [Tree] -> Int
sumSizes size = foldl' (\total item -> total + size item) 0

-- | The tree as aeson's tree, sharing the table's strings.
treeValue :: Tree -> Value
treeValue = \case
    Shared entry -> entryString entry
    Own text _ -> String text
    Literal value -> value
    Number' value _ -> value
    Members _ _ fields -> Object (KeyMap.map treeValue fields)
    Items count _ _ items -> Array (V.fromListN count (reverse (map treeValue items)))
    Keyed _ tree -> treeValue tree

-- | An object's named members as aeson's tree. Any other value converts whole.
treeMembers :: [Key.Key] -> Tree -> Value
treeMembers keys = \case
    Members _ _ fields -> Object (KeyMap.fromList [(key, treeValue tree) | key <- keys, Just tree <- [KeyMap.lookup key fields]])
    tree -> treeValue tree

-- | Add a member under a table key to an object, replacing any member already under that key.
withMember :: Entry -> Entry -> Tree -> Tree
withMember key value = \case
    Members _ _ fields -> object (KeyMap.insert (Key.fromText (entryText key)) (Keyed key (Shared value)) fields)
    tree -> tree

-- | The table a document's packed values index, laid out once the read ends.
sealTable :: InternTable -> DocTable
sealTable = docTable . tableStrings

-- | Pack a tree. The hole is the string at the path of member keys, when one is there.
packTree :: [Key.Key] -> Tree -> Packed
packTree path tree = runST $ do
    blob <- newBlob (nodeBytes tree)
    hole <- newPrimVar (-1)
    write blob hole (Just path) tree
    readPrimVar hole >>= finishBlob blob (nodeEncoded tree)

write :: Blob s -> PrimVar s Int -> Maybe [Key.Key] -> Tree -> ST s ()
write blob hole path = \case
    Shared entry -> do
        markHole blob hole path
        putOpcode blob OpShared
        putVarint blob (entryIndex entry)
    Own text len -> do
        markHole blob hole path
        putOpcode blob OpInline
        putVarint blob len
        putString blob text
    Literal value -> putOpcode blob $ case value of
        Bool False -> OpFalse
        Bool True -> OpTrue
        _ -> OpNull
    Number' _ bytes -> do
        putOpcode blob OpInline
        putVarint blob (SBS.length bytes)
        putRaw blob bytes
    Members _ _ fields -> do
        putOpcode blob OpObject
        putVarint blob (KeyMap.size fields)
        writeMap blob hole path (KeyMap.toMap fields)
    Items count _ _ items -> do
        putOpcode blob OpArray
        putVarint blob count
        writeItems blob hole items
    Keyed _ tree -> write blob hole path tree

-- The string about to be written is the hole when the path has run out.
markHole :: Blob s -> PrimVar s Int -> Maybe [Key.Key] -> ST s ()
markHole blob hole = \case
    Just [] -> blobOffset blob >>= writePrimVar hole
    _ -> pass

-- The items are held in reverse, so the earlier items go first.
writeItems :: Blob s -> PrimVar s Int -> [Tree] -> ST s ()
writeItems blob hole = \case
    [] -> pass
    item : rest -> writeItems blob hole rest >> write blob hole Nothing item

-- Write an object's members in key order, the order aeson writes them.
writeMap :: Blob s -> PrimVar s Int -> Maybe [Key.Key] -> Map Key.Key Tree -> ST s ()
writeMap _ _ _ Tip = pass
writeMap blob hole path (Bin _ key value left right) = do
    writeMap blob hole path left
    case value of
        Keyed entry tree -> do
            putVarint blob (2 * entryIndex entry)
            write blob hole (below path key) tree
        tree -> do
            let text = Key.toText key
            putVarint blob (2 * encodedLength text + 1)
            putString blob text
            write blob hole (below path key) tree
    writeMap blob hole path right

-- The rest of the path under a member, when the path runs through it.
below :: Maybe [Key.Key] -> Key.Key -> Maybe [Key.Key]
below path key = case path of
    Just (next : rest) | next == key -> Just rest
    _ -> Nothing
{-# INLINE below #-}
