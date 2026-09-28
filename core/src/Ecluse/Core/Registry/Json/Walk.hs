-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A resumable walk over the vendored json-stream lexer's tokens, and its body-bounded driver.
Each primitive keeps the acceptance of the json-stream combinator it stands in for: which strings
and keys are decoded, which malformed input is skipped, and where a read fails.
-}
module Ecluse.Core.Registry.Json.Walk (
    -- * Driving a walk
    Step (..),
    readJsonWalk,
    nestingLimit,
    Walked (..),
    emit,

    -- * Tokens
    withElement,
    skipFrom,
    skipRest,
    tooDeep,
    isString,
    readString,
    eachMember,
    memberName,
    eachItem,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.JsonStream.CLexer (tokenParser, unescapeText)
import Data.JsonStream.TokenParser (Element (..), TokenResult (..))

import Ecluse.Core.Registry.Json.Intern (InternTable, Name (Plain), decodedName)
import Ecluse.Core.Registry.JsonStream (Step (..), StreamResult, readSteps)
import Ecluse.Core.Security (BodyLimit, LimitError)

-- | The parse error that marks a retained value past its structural budget.
nestingLimit :: Text
nestingLimit = "retained JSON nesting limit"

-- | Walk a body as 'Ecluse.Core.Registry.JsonStream.readJsonStream' reads one, from the lexer's first token.
readJsonWalk :: (Monad m) => BodyLimit -> (TokenResult -> Step s) -> m ByteString -> m (Either LimitError (StreamResult s))
readJsonWalk bound walk = readSteps bound (walk (tokenParser BS.empty))

-- | A walk's state between tokens: the read's table and the consumer's accumulator.
data Walked s = Walked !InternTable s

-- | Pass a field to the consumer's step. A refused field ends the walk.
emit :: (s -> field -> Either LimitError s) -> s -> field -> (s -> Step r) -> Step r
emit step acc field next = either Refused next (step acc field)
{-# INLINE emit #-}

-- | The next element, suspending for input at a chunk boundary.
withElement :: TokenResult -> (Element -> TokenResult -> Step s) -> Step s
withElement tokens next = case tokens of
    PartialResult element rest -> next element rest
    _ -> awaitElement tokens next
{-# INLINE withElement #-}

awaitElement :: TokenResult -> (Element -> TokenResult -> Step s) -> Step s
awaitElement tokens next = case tokens of
    PartialResult element rest -> next element rest
    TokMoreData more -> NeedData (\chunk -> awaitElement (more chunk) next)
    TokFailed -> Failed "the JSON lexer failed"
{-# NOINLINE awaitElement #-}

-- | Skip the value starting at the element without decoding it, as json-stream's @ignoreVal@ does.
skipFrom :: Element -> TokenResult -> (TokenResult -> Step s) -> Step s
skipFrom element rest next = case element of
    JValue _ -> next rest
    JInteger _ -> next rest
    StringRaw{} -> next rest
    StringContent _ -> skipStringThen rest next
    ArrayBegin -> skipRest 1 rest next
    ObjectBegin -> skipRest 1 rest next
    ArrayEnd _ -> Failed "unexpected end of array"
    ObjectEnd _ -> Failed "unexpected end of object"
    StringEnd _ -> Failed "unexpected end of string"

-- | Skip to the end of the container the given number of levels up. Any closing token closes a level.
skipRest :: Int -> TokenResult -> (TokenResult -> Step s) -> Step s
skipRest !level tokens next = case tokens of
    PartialResult element rest -> case element of
        ArrayEnd _ -> closed rest
        ObjectEnd _ -> closed rest
        ArrayBegin -> skipRest (level + 1) rest next
        ObjectBegin -> skipRest (level + 1) rest next
        StringContent _ -> skipStringThen rest (\after -> skipRest level after next)
        StringEnd _ -> Failed "unexpected end of string"
        _ -> skipRest level rest next
    TokMoreData more -> NeedData (\chunk -> skipRest level (more chunk) next)
    TokFailed -> Failed "the JSON lexer failed"
  where
    closed rest
        | level <= 1 = next rest
        | otherwise = skipRest (level - 1) rest next

skipStringThen :: TokenResult -> (TokenResult -> Step s) -> Step s
skipStringThen tokens next = withElement tokens $ \element rest -> case element of
    StringContent _ -> skipStringThen rest next
    StringEnd _ -> next rest
    _ -> Failed "unexpected token in a string"

-- | Skip the value, then fail: a retained value with no structural budget left.
tooDeep :: Element -> TokenResult -> Step s
tooDeep element rest = skipFrom element rest (const (Failed nestingLimit))

-- | Whether the element starts a string.
isString :: Element -> Bool
isString = \case
    StringRaw{} -> True
    StringContent _ -> True
    JValue (Aeson.String _) -> True
    _ -> False

-- | Decode the string starting at the element, failing where json-stream's @string@ fails.
readString :: Element -> TokenResult -> (Name -> TokenResult -> Step s) -> Step s
readString element rest next = case element of
    StringRaw bytes True _ -> next (Plain bytes) rest
    StringRaw bytes False _ -> case unescapeText bytes of
        Right text -> next (decodedName text) rest
        Left err -> Failed (show err)
    StringContent part -> longString [part] rest next
    JValue (Aeson.String text) -> next (decodedName text) rest
    _ -> Failed "expected a string"

longString :: [ByteString] -> TokenResult -> (Name -> TokenResult -> Step s) -> Step s
longString parts tokens next = withElement tokens $ \element rest -> case element of
    StringContent part -> longString (part : parts) rest next
    StringEnd _ -> case unescapeText (BS.concat (reverse parts)) of
        Right text -> next (decodedName text) rest
        Left _ -> Failed "Error decoding UTF8"
    _ -> Failed "unexpected token in a string"

{- | Visit each member of an object whose opening brace was read, as json-stream's @objectKeyValues@
does: every key is decoded, and a key longer than 64 KiB across pieces drops its member unread.
-}
eachMember :: (st -> Name -> TokenResult -> (st -> TokenResult -> Step s) -> Step s) -> (st -> TokenResult -> Step s) -> st -> TokenResult -> Step s
eachMember visit done = loop
  where
    loop acc tokens = withElement tokens $ \element rest -> case element of
        ObjectEnd _ -> done acc rest
        _ -> memberName element rest (\key after -> visit acc key after loop) (loop acc)
{-# INLINE eachMember #-}

-- | Decode the object key at the element, or skip its member when json-stream drops it unread.
memberName :: Element -> TokenResult -> (Name -> TokenResult -> Step s) -> (TokenResult -> Step s) -> Step s
memberName element rest named dropped = case element of
    JValue (Aeson.String key) -> named (decodedName key) rest
    StringRaw bytes True _ -> named (Plain bytes) rest
    StringRaw bytes False _ -> case unescapeText bytes of
        Right key -> named (decodedName key) rest
        Left err -> Failed (show err)
    StringContent part -> longKey [part] (BS.length part) rest named dropped
    _ -> Failed "unexpected token where an object key belongs"

-- json-stream's getLongKey: the limit applies from the third piece, and a dropped key skips its value.
longKey :: [ByteString] -> Int -> TokenResult -> (Name -> TokenResult -> Step s) -> (TokenResult -> Step s) -> Step s
longKey parts !size tokens next dropped = withElement tokens $ \element rest -> case element of
    StringEnd _ -> case unescapeText (BS.concat (reverse parts)) of
        Right key -> next (decodedName key) rest
        Left _ -> Failed "Error decoding UTF8"
    StringContent part
        | size > 65536 -> skipStringThen rest (\after -> withElement after (\value afterValue -> skipFrom value afterValue dropped))
        | otherwise -> longKey (part : parts) (size + BS.length part) rest next dropped
    _ -> Failed "unexpected token in an object key"

-- | Visit each item of an array whose opening bracket was read, with its position.
eachItem :: (st -> Int -> Element -> TokenResult -> (st -> TokenResult -> Step s) -> Step s) -> (st -> TokenResult -> Step s) -> st -> TokenResult -> Step s
eachItem visit done = loop 0
  where
    loop !position acc tokens = withElement tokens $ \element rest -> case element of
        ArrayEnd _ -> done acc rest
        _ -> visit acc position element rest (loop (position + 1))
{-# INLINE eachItem #-}
