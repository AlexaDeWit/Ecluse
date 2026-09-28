-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Retained-value shapes for the token walk, and the reader that builds each value once. A shape
reads what the matching "Ecluse.Core.Registry.JsonStream" combinator reads, and interns keys and
strings by the bytes the lexer read unless its mode keeps them as read. The reader builds any
'Retained' form: aeson's tree for selected reads, the packed tree for full reads.
-}
module Ecluse.Core.Registry.Json.Shape (
    Shape (..),
    Members,
    namedMembers,
    everyMember,
    knownMembers,
    Mode (..),
    Retained (..),
    readShape,
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.JsonStream.CLexer (unescapeText)
import Data.JsonStream.TokenParser (Element (..), TokenResult (..))
import Data.Vector qualified as V

import Ecluse.Core.Registry.Json.Intern (Entry (..), InternTable, Interned (..), Name (Plain), decodedName, internName, nameBytes, nameText)

import Ecluse.Core.Registry.Json.Walk (Step (..), isString, memberName, nestingLimit, readString, skipFrom, tooDeep, withElement)

-- | What a read builds for each retained value.
class Retained v where
    -- | A string the document's table shares.
    sharedString :: Entry -> v

    -- | A string with a copy of its own.
    ownString :: Text -> v

    -- | A number, boolean, null or fallback value, taken whole.
    whole :: Value -> v

    -- | The empty array a scalar shape keeps for a container it skips.
    emptyContainer :: v

    -- | A member's value under a key the table shares.
    sharedMember :: Entry -> v -> v

    -- | An object from its members.
    object :: KeyMap.KeyMap v -> v

    -- | An array from its item count and its items in reverse.
    array :: Int -> [v] -> v

instance Retained Value where
    sharedString = entryString
    ownString = String
    whole = id
    emptyContainer = emptyArray
    sharedMember _ value = value
    object = Object
    array count values = Array (V.fromListN count (reverse values))

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
readShape :: (Retained v) => Shape -> Mode -> InternTable -> Element -> TokenResult -> (v -> InternTable -> TokenResult -> Step s) -> Step s
readShape = readAt 0 False
{-# INLINEABLE readShape #-}
{-# SPECIALIZE readShape :: Shape -> Mode -> InternTable -> Element -> TokenResult -> (Value -> InternTable -> TokenResult -> Step s) -> Step s #-}

-- json-stream races a skip of a container an alternative rejects, whose lexer failure beats a nesting
-- failure. open counts levels inside the outermost raced container, and raced marks a raced value.
{-# INLINEABLE readAt #-}
readAt :: (Retained v) => Int -> Bool -> Shape -> Mode -> InternTable -> Element -> TokenResult -> (v -> InternTable -> TokenResult -> Step s) -> Step s
readAt open raced shape mode table element rest next = case shape of
    Scalar budget
        | budget <= 0 -> tooDeepAt open element rest
        | otherwise -> readScalar mode table element rest next (next emptyContainer table)
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
        | isString element -> readString element rest (string mode table next)
        | otherwise -> readAt open True other mode table element rest next
    ObjectOr fallback members
        | ObjectBegin <- element -> readObject (entering raced) members mode table rest next
        | otherwise -> let !value = whole fallback in skipFrom element rest (next value table)
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
{-# INLINEABLE readScalar #-}
readScalar :: (Retained v) => Mode -> InternTable -> Element -> TokenResult -> (v -> InternTable -> TokenResult -> Step s) -> (TokenResult -> Step s) -> Step s
readScalar mode table element rest next container = case element of
    JInteger number -> let !value = whole (Number (fromIntegral number)) in next value table rest
    JValue (String text) -> string mode table next (decodedName text) rest
    JValue value -> let !built = whole value in next built table rest
    ObjectBegin -> skipFrom element rest container
    ArrayBegin -> skipFrom element rest container
    _
        | isString element -> readString element rest (string mode table next)
        | otherwise -> Failed "unexpected token where a value belongs"

-- The table's shared copy of a string, or a copy of its own when the mode keeps it.
{-# INLINEABLE stringValue #-}
stringValue :: (Retained v) => Mode -> InternTable -> Name -> Built v
stringValue mode table name = case mode of
    Keep -> Built (ownString (nameText name)) table
    Share -> case internName name table of
        Interned entry held -> Built (sharedString entry) held

-- A value built in full, so no retained value holds a thunk or a slice of an input chunk.
data Built v = Built !v !InternTable

{-# INLINEABLE string #-}
string :: (Retained v) => Mode -> InternTable -> (v -> InternTable -> TokenResult -> Step s) -> Name -> TokenResult -> Step s
string mode table next name after = case stringValue mode table name of
    Built value held -> next value held after

-- The first member under a key wins, as in json-stream. A repeat is read where json-stream reads it,
-- then dropped, and nothing it holds enters the table.
{-# INLINEABLE readObject #-}
readObject :: (Retained v) => Int -> Members -> Mode -> InternTable -> TokenResult -> (v -> InternTable -> TokenResult -> Step s) -> Step s
readObject open (Members named other) mode table0 tokens0 next = loop table0 KeyMap.empty tokens0
  where
    loop table !fields tokens = case tokens of
        PartialResult (ObjectEnd _) rest -> let !built = object fields in next built table rest
        PartialResult (StringRaw bytes True _) rest -> field table fields (Plain bytes) rest
        _ -> withElement tokens $ \element rest -> case element of
            ObjectEnd _ -> let !built = object fields in next built table rest
            _ -> memberName element rest (field table fields) (loop table fields)
    field table fields name rest = case HashMap.lookup (nameBytes name) named of
        Just (shared, shape) -> value table fields name (Just shared) shape rest
        Nothing -> case other of
            Just shape -> value table fields name Nothing shape rest
            Nothing -> withElement rest $ \element afterKey -> skipFrom element afterKey (loop table fields)
    value table fields name shared shape rest = case memberKey mode table name shared of
        SharedKey key entry valueMode held -> field' key (sharedMember entry) valueMode held
        OwnKey key held -> field' key id Keep held
      where
        field' key keyed valueMode held
            | KeyMap.member key fields = withElement rest $ \element afterKey -> case sameForm fields (direct shape Keep table element) of
                Direct _ _ -> loop table fields afterKey
                Indirect -> readAt open False shape Keep table element afterKey (\ignored _ -> dropped fields ignored `seq` loop table fields)
            | otherwise = withElement rest $ \element afterKey -> case direct shape valueMode held element of
                Direct built table' -> loop table' (KeyMap.insert key (keyed built) fields) afterKey
                Indirect -> readAt open False shape valueMode held element afterKey $ \built table' -> loop table' (KeyMap.insert key (keyed built) fields)
        {-# INLINE field' #-}

-- A member's key: shared through the table with the mode for its value, or kept as read.
data Keyed = SharedKey !Key.Key !Entry !Mode !InternTable | OwnKey !Key.Key !InternTable

-- A repeated member is read in the form its object builds, then dropped.
sameForm :: KeyMap.KeyMap v -> Direct v -> Direct v
sameForm _ built = built

dropped :: KeyMap.KeyMap v -> v -> ()
dropped _ _ = ()

-- A shared key goes through the table, and a key the table keeps holds its value as read.
memberKey :: Mode -> InternTable -> Name -> Maybe Key.Key -> Keyed
memberKey mode table name shared = case mode of
    Keep -> OwnKey (fromMaybe (Key.fromText (nameText name)) shared) table
    Share -> case internName name table of
        Interned entry held -> SharedKey (Key.fromText (entryText entry)) entry (if entryKeeps entry then Keep else Share) held
{-# INLINE memberKey #-}

{-# INLINEABLE readArray #-}
readArray :: (Retained v) => Int -> Shape -> Mode -> InternTable -> TokenResult -> (v -> InternTable -> TokenResult -> Step s) -> Step s
readArray open item mode table0 tokens0 next = loop table0 0 [] tokens0
  where
    loop table !count values tokens = withElement tokens $ \element rest -> case element of
        ArrayEnd _ -> let !items = array count values in next items table rest
        _ -> case direct item mode table element of
            Direct field table' -> loop table' (count + 1) (field : values) rest
            Indirect -> readAt open False item mode table element rest $ \field table' afterValue -> loop table' (count + 1) (field : values) afterValue

-- A complete scalar token's value under a shape, when the shape takes it without reading further.
data Direct v = Direct !v !InternTable | Indirect

{-# INLINEABLE direct #-}
direct :: (Retained v) => Shape -> Mode -> InternTable -> Element -> Direct v
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
        StringRaw{} -> Direct (whole fallback) table
        JValue _ -> Direct (whole fallback) table
        JInteger _ -> Direct (whole fallback) table
        _ -> Indirect
    Checked budget inner | budget > 0 -> direct inner mode table element
    _ -> Indirect

{-# INLINEABLE scalarToken #-}
scalarToken :: (Retained v) => Mode -> InternTable -> Element -> Direct v
scalarToken mode table = \case
    StringRaw bytes True _ -> direct' (Plain bytes)
    StringRaw bytes False _ -> either (const Indirect) (direct' . decodedName) (unescapeText bytes)
    JValue (String text) -> direct' (decodedName text)
    JValue scalar -> Direct (whole scalar) table
    JInteger number -> Direct (whole (Number (fromIntegral number))) table
    _ -> Indirect
  where
    direct' name = case stringValue mode table name of
        Built value held -> Direct value held

emptyArray :: Value
emptyArray = Array mempty
