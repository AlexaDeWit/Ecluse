-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TypeFamilies #-}

{- | Retained-value shapes for the token walk, and the reader that builds each value once. A shape
reads what the matching "Ecluse.Core.Registry.JsonStream" combinator reads, and interns keys and
strings by the bytes the lexer read unless its mode keeps them as read. A 'Build' says what the
reader builds: aeson's tree, or the packed form a full read writes.
-}
module Ecluse.Core.Registry.Json.Shape (
    Shape (..),
    Members,
    namedMembers,
    everyMember,
    knownMembers,
    prepareMembers,
    listedMember,
    Mode (..),
    MemberKey (..),
    Build (..),
    Trees (..),
    readShape,
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.JsonStream.CLexer (unescapeText)
import Data.JsonStream.TokenReader (Element (..), Next (..), Tokens, nextToken)
import Data.Vector qualified as V

import Ecluse.Core.Registry.Json.Intern (Entry, InternTable, Interned (..), Name (Plain), PreparedName, decodedName, entryKeeps, entryString, entryText, internName, internPreparedName, nameBytes, nameText, prepareName)
import Ecluse.Core.Registry.Json.Walk (Walk (..), isString, memberName, nestingLimit, readString, skipFrom, tooDeep, withElement)

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
data Members = Members (HashMap.HashMap ByteString Member) (Maybe Shape)

data Member = Member (Key.Key, Shape) (Maybe PreparedName)

-- | Retain only the named members. The first entry for a name wins.
namedMembers :: [(Text, Shape)] -> Members
namedMembers entries = Members (HashMap.fromListWith (\_ earlier -> earlier) [(encodeUtf8 name, Member (Key.fromText name, shape) Nothing) | (name, shape) <- entries]) Nothing

-- | Retain every member with one shape.
everyMember :: Shape -> Members
everyMember = Members mempty . Just

-- | Retain every member with one shape, sharing the key of each listed name.
knownMembers :: [Text] -> Shape -> Members
knownMembers names shape = Members (HashMap.fromList [(encodeUtf8 name, Member (Key.fromText name, shape) Nothing) | name <- names]) (Just shape)

-- | Cache the listed names' hashes for this read, leaving table insertion and nested shapes unchanged.
prepareMembers :: InternTable -> Members -> Members
prepareMembers table (Members named other) = Members (HashMap.map prepare named) other
  where
    prepare (Member schema@(key, _) _) =
        let !prepared = prepareName table (Key.toText key)
         in Member schema (Just prepared)

-- | A listed name's key and shape. Every member a read keeps as read under that name holds this one key.
listedMember :: Members -> Name -> Maybe (Key.Key, Shape)
listedMember members name = case findMember members name of
    Just (Member schema _) -> Just schema
    Nothing -> Nothing
{-# INLINE listedMember #-}

findMember :: Members -> Name -> Maybe Member
findMember (Members named _) name = HashMap.lookup (nameBytes name) named
{-# INLINE findMember #-}

-- | Whether a value's keys and strings go through the document's table or keep their own copies.
data Mode = Share | Keep

-- | A member's key: the table's entry, or a key with its own copy.
data MemberKey = SharedKey !Entry | OwnKey !Key.Key

-- The key's text.
memberText :: MemberKey -> Text
memberText = \case
    SharedKey entry -> entryText entry
    OwnKey key -> Key.toText key
{-# INLINE memberText #-}

{- | What a read builds, one value at a time, each step handing its result to a continuation. An
object's members arrive in source order, and a repeated key's value is read and then dropped.
-}
class (Walk r) => Build b r where
    -- | A finished value.
    type Built b

    -- | An object being read.
    type Fields b

    -- | An array being read.
    type Items b

    -- | A string the document's table shares.
    sharedString :: b -> Entry -> (Built b -> r) -> r

    -- | A string with a copy of its own.
    ownString :: b -> Name -> (Built b -> r) -> r

    -- | An integer the lexer read whole.
    integer :: b -> Int -> (Built b -> r) -> r

    -- | A number, boolean, null or fallback value, taken whole.
    whole :: b -> Value -> (Built b -> r) -> r

    -- | The empty array a scalar shape keeps for a container it skips.
    emptyContainer :: b -> (Built b -> r) -> r

    -- | An object begins.
    openObject :: b -> (Fields b -> r) -> r

    -- | A member under the key begins, and whether the object already holds the key.
    beginMember :: b -> MemberKey -> Fields b -> (Bool -> r) -> r

    -- | The member begun last ends with its value.
    addMember :: b -> MemberKey -> Built b -> Fields b -> (Fields b -> r) -> r

    -- | Forget the member begun last, whose key the object already holds.
    dropValue :: b -> Built b -> r -> r

    -- | Finish an object.
    closeObject :: b -> Fields b -> (Built b -> r) -> r

    -- | An array begins.
    openArray :: b -> (Items b -> r) -> r

    -- | An item of the array ends.
    addItem :: b -> Built b -> Items b -> (Items b -> r) -> r

    -- | Finish an array of the given item count.
    closeArray :: b -> Int -> Items b -> (Built b -> r) -> r

-- | Build aeson's tree.
data Trees = Trees

instance (Walk r) => Build Trees r where
    type Built Trees = Value
    type Fields Trees = KeyMap.KeyMap Value
    type Items Trees = [Value]
    sharedString _ entry next = next (entryString entry)
    ownString _ name next = next (String (nameText name))
    integer _ number next = let !value = Number (fromIntegral number) in next value
    whole _ value next = next value
    emptyContainer _ next = next emptyArray
    openObject _ next = next KeyMap.empty
    beginMember _ key fields next = next (KeyMap.member (Key.fromText (memberText key)) fields)
    addMember _ key value fields next = next (KeyMap.insert (Key.fromText (memberText key)) value fields)
    dropValue _ _ next = next
    closeObject _ fields next = next (Object fields)
    openArray _ next = next []
    addItem _ value values next = next (value : values)
    closeArray _ count values next = let !array = Array (V.fromListN count (reverse values)) in next array
    {-# INLINE sharedString #-}
    {-# INLINE ownString #-}
    {-# INLINE integer #-}
    {-# INLINE whole #-}
    {-# INLINE emptyContainer #-}
    {-# INLINE openObject #-}
    {-# INLINE beginMember #-}
    {-# INLINE addMember #-}
    {-# INLINE dropValue #-}
    {-# INLINE closeObject #-}
    {-# INLINE openArray #-}
    {-# INLINE addItem #-}
    {-# INLINE closeArray #-}

-- | Read the value starting at the element and build it once.
readShape :: (Build b r) => b -> Shape -> Mode -> InternTable -> Element -> Tokens (TokenState r) -> (Built b -> InternTable -> Tokens (TokenState r) -> r) -> r
readShape build = readAt build 0 False
{-# INLINEABLE readShape #-}

-- json-stream races a skip of a container an alternative rejects, whose lexer failure beats a nesting
-- failure. open counts levels inside the outermost raced container, and raced marks a raced value.
readAt :: (Build b r) => b -> Int -> Bool -> Shape -> Mode -> InternTable -> Element -> Tokens (TokenState r) -> (Built b -> InternTable -> Tokens (TokenState r) -> r) -> r
readAt build open raced shape mode table element rest next = case shape of
    Scalar budget
        | budget <= 0 -> tooDeepAt open element rest
        | otherwise -> readScalar build mode table element rest next (\after -> emptyContainer build (\value -> next value table after))
    Generic budget
        | budget <= 0 -> tooDeepAt open element rest
        | otherwise -> case element of
            ObjectBegin -> readObject build (entering raced) (everyMember (Generic (budget - 1))) mode table rest next
            ArrayBegin -> readArray build (entering True) (Generic (budget - 1)) mode table rest next
            _ -> readScalar build mode table element rest next (\_ -> failWith "unexpected container")
    ObjectWith budget members fallback
        | budget <= 0 -> tooDeepAt open element rest
        | ObjectBegin <- element -> readObject build (entering raced) members mode table rest next
        | otherwise -> readAt build open True fallback mode table element rest next
    ArrayWith budget item fallback
        | budget <= 0 -> tooDeepAt open element rest
        | ArrayBegin <- element -> readArray build (entering raced) item mode table rest next
        | otherwise -> readAt build open True fallback mode table element rest next
    StringOr budget other
        | budget <= 0 -> tooDeepAt open element rest
        | isString element -> readString element rest (string build mode table next)
        | otherwise -> readAt build open True other mode table element rest next
    ObjectOr fallback members
        | ObjectBegin <- element -> readObject build (entering raced) members mode table rest next
        | otherwise -> skipFrom element rest (\after -> whole build fallback (\value -> next value table after))
    Checked budget inner
        | budget <= 0 -> tooDeepAt open element rest
        | otherwise -> readAt build open raced inner mode table element rest next
  where
    entering racing
        | open > 0 = open + 1
        | racing = 1
        | otherwise = 0
{-# INLINEABLE readAt #-}

-- Skip the value, then fail on the nesting limit, unless a parallel skip has already failed further on.
tooDeepAt :: (Walk r) => Int -> Element -> Tokens (TokenState r) -> r
tooDeepAt open element rest
    | open > 0 = skipFrom element rest (raceFailure open)
    | otherwise = tooDeep element rest
{-# INLINEABLE tooDeepAt #-}

raceFailure :: (Walk r) => Int -> Tokens (TokenState r) -> r
raceFailure !level tokens = reading (nextToken tokens) $ \case
    TokFailed -> failWith "the JSON lexer failed"
    TokMoreData -> failWith nestingLimit
    PartialResult element ->
        let rest = tokens
         in case element of
                ArrayEnd -> closed rest
                ObjectEnd -> closed rest
                ArrayBegin -> raceFailure (level + 1) rest
                ObjectBegin -> raceFailure (level + 1) rest
                StringContent _ -> longString rest
                StringEnd -> failWith "unexpected end of string"
                _ -> raceFailure level rest
  where
    closed rest
        | level <= 1 = failWith nestingLimit
        | otherwise = raceFailure (level - 1) rest
    longString rest = reading (nextToken rest) $ \case
        TokFailed -> failWith "the JSON lexer failed"
        TokMoreData -> failWith nestingLimit
        PartialResult (StringContent _) -> longString rest
        PartialResult StringEnd -> raceFailure level rest
        PartialResult _ -> failWith "unexpected token in a string"
{-# INLINEABLE raceFailure #-}

-- json-stream's scalar parsers: a container is skipped and handed to the last continuation.
readScalar :: (Build b r) => b -> Mode -> InternTable -> Element -> Tokens (TokenState r) -> (Built b -> InternTable -> Tokens (TokenState r) -> r) -> (Tokens (TokenState r) -> r) -> r
readScalar build mode table element rest next container = case element of
    JInteger number -> integer build (fromIntegral number) (\value -> next value table rest)
    JValue (String text) -> string build mode table next (decodedName text) rest
    JValue value -> whole build value (\built -> next built table rest)
    ObjectBegin -> skipFrom element rest container
    ArrayBegin -> skipFrom element rest container
    _
        | isString element -> readString element rest (string build mode table next)
        | otherwise -> failWith "unexpected token where a value belongs"
{-# INLINEABLE readScalar #-}

-- The table's shared copy of a string, or a copy of its own when the mode keeps it.
string :: (Build b r) => b -> Mode -> InternTable -> (Built b -> InternTable -> Tokens (TokenState r) -> r) -> Name -> Tokens (TokenState r) -> r
string build mode table next name after = case mode of
    Keep -> ownString build name (\value -> next value table after)
    Share -> case internName name table of
        Interned entry held -> sharedString build entry (\value -> next value held after)
{-# INLINE string #-}

-- The first member under a key wins, as in json-stream. A repeat is read where json-stream reads it,
-- then dropped, and nothing it holds enters the table.
readObject :: (Build b r) => b -> Int -> Members -> Mode -> InternTable -> Tokens (TokenState r) -> (Built b -> InternTable -> Tokens (TokenState r) -> r) -> r
readObject build open members@(Members _ other) mode table0 tokens0 next = openObject build (\fields0 -> loop table0 fields0 tokens0)
  where
    loop table !fields tokens = withElement tokens $ \element rest -> case element of
        ObjectEnd -> closeObject build fields (\built -> next built table rest)
        StringRaw bytes True -> member table fields (Plain bytes) rest
        _ -> memberName element rest (member table fields) (loop table fields)
    member table fields name rest = case findMember members name of
        Just (Member (shared, shape) prepared) -> value table fields name (Just shared) prepared shape rest
        Nothing -> case other of
            Just shape -> value table fields name Nothing Nothing shape rest
            Nothing -> withElement rest $ \element afterKey -> skipFrom element afterKey (loop table fields)
    -- A shared key goes through the table, and a key the table keeps holds its value as read.
    value table fields name shared prepared shape rest = case mode of
        Keep -> keyed (OwnKey (fromMaybe (Key.fromText (nameText name)) shared)) Keep table
        Share -> case maybe (internName name table) (`internPreparedName` table) prepared of
            Interned entry held -> keyed (SharedKey entry) (if entryKeeps entry then Keep else Share) held
      where
        keyed key !valueMode held = beginMember build key fields $ \ !repeated ->
            if repeated
                then withElement rest $ \element afterKey ->
                    withDirect build (direct shape Keep table element) table (\ignored _ -> dropValue build ignored (loop table fields afterKey)) $
                        readAt build open False shape Keep table element afterKey (\ignored _ -> dropValue build ignored . loop table fields)
                else withElement rest $ \element afterKey ->
                    withDirect build (direct shape valueMode held element) held (\field table' -> addMember build key field fields (\fields' -> loop table' fields' afterKey)) $
                        readAt build open False shape valueMode held element afterKey $ \field table' afterValue ->
                            addMember build key field fields (\fields' -> loop table' fields' afterValue)
        {-# INLINE keyed #-}
{-# INLINEABLE readObject #-}

readArray :: (Build b r) => b -> Int -> Shape -> Mode -> InternTable -> Tokens (TokenState r) -> (Built b -> InternTable -> Tokens (TokenState r) -> r) -> r
readArray build open item mode table0 tokens0 next = openArray build (\items0 -> loop table0 0 items0 tokens0)
  where
    loop table !count items tokens = withElement tokens $ \element rest -> case element of
        ArrayEnd -> closeArray build count items (\array -> next array table rest)
        _ ->
            withDirect build (direct item mode table element) table (\field table' -> addItem build field items (\items' -> loop table' (count + 1) items' rest)) $
                readAt build open False item mode table element rest $ \field table' afterValue ->
                    addItem build field items (\items' -> loop table' (count + 1) items' afterValue)
{-# INLINEABLE readArray #-}

-- A complete scalar token's value under a shape, when the shape takes it without reading further.
data Direct
    = DirectShared !Entry !InternTable
    | DirectOwn !Name
    | DirectInteger !Int
    | DirectWhole !Value
    | Indirect

-- Build a complete scalar token's value, or read the value on when the token does not complete it.
withDirect :: (Build b r) => b -> Direct -> InternTable -> (Built b -> InternTable -> r) -> r -> r
withDirect build scalar table next indirect = case scalar of
    DirectShared entry held -> sharedString build entry (`next` held)
    DirectOwn name -> ownString build name (`next` table)
    DirectInteger number -> integer build number (`next` table)
    DirectWhole value -> whole build value (`next` table)
    Indirect -> indirect
{-# INLINE withDirect #-}

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
        StringRaw{} -> DirectWhole fallback
        JValue _ -> DirectWhole fallback
        JInteger _ -> DirectWhole fallback
        _ -> Indirect
    Checked budget inner | budget > 0 -> direct inner mode table element
    _ -> Indirect

scalarToken :: Mode -> InternTable -> Element -> Direct
scalarToken mode table = \case
    StringRaw bytes True -> direct' (Plain bytes)
    StringRaw bytes False -> either (const Indirect) (direct' . decodedName) (unescapeText bytes)
    JValue (String text) -> direct' (decodedName text)
    JValue scalar -> DirectWhole scalar
    JInteger number -> DirectInteger (fromIntegral number)
    _ -> Indirect
  where
    direct' name = case mode of
        Keep -> DirectOwn name
        Share -> case internName name table of
            Interned entry held -> DirectShared entry held

emptyArray :: Value
emptyArray = Array mempty
