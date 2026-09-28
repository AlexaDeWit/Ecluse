-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Retained-value shapes for the token walk, and the reader that builds each value once. A shape
reads what the matching "Ecluse.Core.Registry.JsonStream" combinator reads, and interns keys and
strings by the bytes the lexer read unless its mode keeps them as read.
-}
module Ecluse.Core.Registry.Json.Shape (
    Shape (..),
    Members,
    namedMembers,
    everyMember,
    knownMembers,
    Mode (..),
    readShape,
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.JsonStream.CLexer (unescapeText)
import Data.JsonStream.TokenParser (Element (..), TokenResult (..))
import Data.Vector qualified as V

import Ecluse.Core.Registry.Json.Intern (Entry (..), InternTable, Interned (..), Name (..), internName, nameBytes, nameText)
import Ecluse.Core.Registry.Json.Walk (Step (..), isString, memberName, nestingLimit, readString, skipFrom, tooDeep, withElement)

{- | What to retain of one value. Each budget is the structural depth left, and a value read with
none left is skipped and fails the read.
-}
data Shape
    = -- | A scalar as read, or an empty array for a container.
      Scalar !Int
    | -- | The whole value.
      Generic !Int
    | -- | An object's members, or the fallback shape for any other value.
      ObjectWith !Int Members Shape
    | -- | An array's items, or the fallback shape for any other value.
      ArrayWith !Int Shape Shape
    | -- | A string, or the other shape for any other value.
      StringOr !Int Shape
    | -- | An object's members, or the given value for any other value.
      ObjectOr Value Members
    | -- | The shape, charged against a budget of its own.
      Checked !Int Shape

-- | Which members of an object are retained, and with what shape.
data Members = Members (HashMap.HashMap ByteString (Key.Key, Shape)) (Maybe Shape)

-- | Retain only the named members. The first entry for a name wins.
namedMembers :: [(Text, Shape)] -> Members
namedMembers entries = Members (HashMap.fromListWith (\_ earlier -> earlier) [(encodeUtf8 name, (Key.fromText name, shape)) | (name, shape) <- entries]) Nothing

-- | Retain every member with one shape.
everyMember :: Shape -> Members
everyMember = Members mempty . Just

-- | Retain every member with one shape, sharing the key of each listed name.
knownMembers :: [Text] -> Shape -> Members
knownMembers names shape = Members (HashMap.fromList [(encodeUtf8 name, (Key.fromText name, shape)) | name <- names]) (Just shape)

-- | Whether a value's keys and strings go through the document's table or keep their own copies.
data Mode = Share | Keep

-- | Read the value starting at the element and build it once.
readShape :: Shape -> Mode -> InternTable -> Element -> TokenResult -> (Value -> InternTable -> TokenResult -> Step s) -> Step s
readShape = readAt 0 False

-- json-stream races a skip of a container an alternative rejects, whose lexer failure beats a nesting
-- failure. open counts levels inside the outermost raced container, and raced marks a raced value.
readAt :: Int -> Bool -> Shape -> Mode -> InternTable -> Element -> TokenResult -> (Value -> InternTable -> TokenResult -> Step s) -> Step s
readAt open raced shape mode table element rest next = case shape of
    Scalar budget
        | budget <= 0 -> tooDeepAt open element rest
        | otherwise -> readScalar mode table element rest next (next (Array mempty) table)
    Generic budget
        | budget <= 0 -> tooDeepAt open element rest
        | otherwise -> case element of
            ObjectBegin -> readObject (entering raced) (everyMember (Generic (budget - 1))) mode table rest next
            ArrayBegin -> readArray (entering True) (Generic (budget - 1)) mode table rest next
            _ -> readScalar mode table element rest next (\_ -> Failed "unexpected container")
    ObjectWith budget members fallback
        | budget <= 0 -> tooDeepAt open element rest
        | ObjectBegin <- element -> readObject (entering raced) members mode table rest next
        | otherwise -> readAt open True fallback mode table element rest next
    ArrayWith budget item fallback
        | budget <= 0 -> tooDeepAt open element rest
        | ArrayBegin <- element -> readArray (entering raced) item mode table rest next
        | otherwise -> readAt open True fallback mode table element rest next
    StringOr budget other
        | budget <= 0 -> tooDeepAt open element rest
        | isString element -> readString element rest (uncurry next . stringValue mode table)
        | otherwise -> readAt open True other mode table element rest next
    ObjectOr fallback members
        | ObjectBegin <- element -> readObject (entering raced) members mode table rest next
        | otherwise -> skipFrom element rest (next fallback table)
    Checked budget inner
        | budget <= 0 -> tooDeepAt open element rest
        | otherwise -> readAt open raced inner mode table element rest next
  where
    entering racing
        | open > 0 = open + 1
        | racing = 1
        | otherwise = 0

-- Skip the value, then fail on the nesting limit, unless a parallel skip has already failed further on.
tooDeepAt :: Int -> Element -> TokenResult -> Step s
tooDeepAt open element rest
    | open > 0 = skipFrom element rest (raceFailure open)
    | otherwise = tooDeep element rest

raceFailure :: Int -> TokenResult -> Step s
raceFailure !level tokens = case tokens of
    TokFailed -> Failed "the JSON lexer failed"
    TokMoreData _ -> Failed nestingLimit
    PartialResult element rest -> case element of
        ArrayEnd _ -> closed rest
        ObjectEnd _ -> closed rest
        ArrayBegin -> raceFailure (level + 1) rest
        ObjectBegin -> raceFailure (level + 1) rest
        StringContent _ -> longString rest
        StringEnd _ -> Failed "unexpected end of string"
        _ -> raceFailure level rest
  where
    closed rest
        | level <= 1 = Failed nestingLimit
        | otherwise = raceFailure (level - 1) rest
    longString = \case
        TokFailed -> Failed "the JSON lexer failed"
        TokMoreData _ -> Failed nestingLimit
        PartialResult (StringContent _) rest -> longString rest
        PartialResult (StringEnd _) rest -> raceFailure level rest
        PartialResult _ _ -> Failed "unexpected token in a string"

-- json-stream's scalar parsers: a container is skipped and handed to the last continuation.
readScalar :: Mode -> InternTable -> Element -> TokenResult -> (Value -> InternTable -> TokenResult -> Step s) -> (TokenResult -> Step s) -> Step s
readScalar mode table element rest next container = case element of
    JInteger number -> next (Number (fromIntegral number)) table rest
    JValue (String text) -> uncurry next (stringValue mode table (Decoded text)) rest
    JValue value -> next value table rest
    ObjectBegin -> skipFrom element rest container
    ArrayBegin -> skipFrom element rest container
    _
        | isString element -> readString element rest (uncurry next . stringValue mode table)
        | otherwise -> Failed "unexpected token where a value belongs"

-- The table's shared copy of a string, or a copy of its own when the mode keeps it.
stringValue :: Mode -> InternTable -> Name -> (Value, InternTable)
stringValue mode table name = case mode of
    Keep -> (String (nameText name), table)
    Share -> case internName name table of
        Interned entry held -> (entryString entry, held)

readObject :: Int -> Members -> Mode -> InternTable -> TokenResult -> (Value -> InternTable -> TokenResult -> Step s) -> Step s
readObject open (Members named other) mode table0 tokens0 next = loop table0 [] tokens0
  where
    -- The last pair of a reversed member list is the first in the source, so it wins.
    loop table fields tokens = case tokens of
        PartialResult (ObjectEnd _) rest -> next (Object (KeyMap.fromList fields)) table rest
        PartialResult (StringRaw bytes True _) rest -> member table fields (Plain bytes) rest
        _ -> withElement tokens $ \element rest -> case element of
            ObjectEnd _ -> next (Object (KeyMap.fromList fields)) table rest
            _ -> memberName element rest (member table fields) (loop table fields)
    member table fields name rest = case HashMap.lookup (nameBytes name) named of
        Just (shared, shape) -> value table fields name (Just shared) shape rest
        Nothing -> case other of
            Just shape -> value table fields name Nothing shape rest
            Nothing -> withElement rest $ \element afterKey -> skipFrom element afterKey (loop table fields)
    value table fields name shared shape rest = case memberKey mode table name shared of
        Keyed key valueMode held -> withElement rest $ \element afterKey -> case direct shape valueMode held element of
            Direct field table' -> loop table' ((key, field) : fields) afterKey
            Indirect -> readAt open False shape valueMode held element afterKey $ \field table' afterValue -> loop table' ((key, field) : fields) afterValue

data Keyed = Keyed !Key.Key !Mode !InternTable

-- A shared key goes through the table, and a key the table keeps holds its value as read.
memberKey :: Mode -> InternTable -> Name -> Maybe Key.Key -> Keyed
memberKey mode table name shared = case mode of
    Keep -> Keyed (fromMaybe (Key.fromText (nameText name)) shared) Keep table
    Share -> case internName name table of
        Interned entry held -> Keyed (Key.fromText (entryText entry)) (if entryKeeps entry then Keep else Share) held
{-# INLINE memberKey #-}

readArray :: Int -> Shape -> Mode -> InternTable -> TokenResult -> (Value -> InternTable -> TokenResult -> Step s) -> Step s
readArray open item mode table0 tokens0 next = loop table0 0 [] tokens0
  where
    loop table !count values tokens = withElement tokens $ \element rest -> case element of
        ArrayEnd _ -> next (Array (V.fromListN count (reverse values))) table rest
        _ -> case direct item mode table element of
            Direct field table' -> loop table' (count + 1) (field : values) rest
            Indirect -> readAt open False item mode table element rest $ \field table' afterValue -> loop table' (count + 1) (field : values) afterValue

-- A complete scalar token's value under a shape, when the shape takes it without reading further.
data Direct = Direct !Value !InternTable | Indirect

direct :: Shape -> Mode -> InternTable -> Element -> Direct
direct shape mode table element = case shape of
    Scalar budget | budget > 0 -> scalarToken mode table element
    Generic budget | budget > 0 -> scalarToken mode table element
    ObjectWith budget _ fallback | budget > 0 -> direct fallback mode table element
    ArrayWith budget _ fallback | budget > 0 -> direct fallback mode table element
    StringOr budget other
        | budget > 0 -> case element of
            StringRaw{} -> scalarToken mode table element
            JValue (String _) -> scalarToken mode table element
            _ -> direct other mode table element
    ObjectOr fallback _ -> case element of
        StringRaw{} -> Direct fallback table
        JValue _ -> Direct fallback table
        JInteger _ -> Direct fallback table
        _ -> Indirect
    Checked budget inner | budget > 0 -> direct inner mode table element
    _ -> Indirect

scalarToken :: Mode -> InternTable -> Element -> Direct
scalarToken mode table = \case
    StringRaw bytes True _ -> uncurry Direct (stringValue mode table (Plain bytes))
    StringRaw bytes False _ -> either (const Indirect) (uncurry Direct . stringValue mode table . Decoded) (unescapeText bytes)
    JValue (String text) -> uncurry Direct (stringValue mode table (Decoded text))
    JValue scalar -> Direct scalar table
    JInteger number -> Direct (Number (fromIntegral number)) table
    _ -> Indirect
