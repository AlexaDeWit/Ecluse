-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TypeFamilies #-}

{- | A resumable walk over json-stream tokens with a reader owned by its state thread.
The primitives retain the upstream combinators' acceptance rules, including skipped malformed
input and lenient numbers. The driver bounds input pieces and drains successful responses.
-}
module Ecluse.Core.Registry.Json.Walk (
    -- * Driving a walk
    Steps (..),
    Step,
    Walk (..),
    readJsonWalk,
    readJsonWalkST,
    nestingLimit,
    Walked (..),
    FieldStep,
    pureStep,
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

import Control.Monad.ST (RealWorld, ST, stToIO)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.JsonStream.CLexer (unescapeText)
import Data.JsonStream.TokenReader (Element (..), Next (..), Tokens, newTokenReader, nextToken, supplyTokens)
import GHC.Exts (oneShot)

import Ecluse.Core.Registry.Json.Intern (InternTable, Name (Plain), decodedName)
import Ecluse.Core.Registry.JsonStream (Steps (..), StreamResult, readSteps)
import Ecluse.Core.Security (BodyLimit, LimitError)

-- | The parse error that marks a retained value past its structural budget.
nestingLimit :: Text
nestingLimit = "retained JSON nesting limit"

-- | What a walk's continuations return: a read that needs input, has failed or been refused, or is done.
class Walk r where
    -- | What a finished walk returns.
    type Result r

    -- | The state thread that owns the token reader.
    type TokenState r

    -- | Sequence an operation on the owned reader before continuing the walk.
    reading :: ST (TokenState r) a -> (a -> r) -> r

    -- | Suspend until the next chunk of input.
    needData :: (ByteString -> r) -> r

    -- | Stop on a parse error.
    failWith :: Text -> r

    -- | Stop on a refused field.
    refuse :: LimitError -> r

    -- | Finish with the walk's result.
    finish :: Result r -> r

-- | A walk that advances its reader and writes its result in the same state thread.
type Step st s = ST st (Steps (ST st) s)

instance Walk (ST st (Steps (ST st) s)) where
    type Result (ST st (Steps (ST st) s)) = s
    type TokenState (ST st (Steps (ST st) s)) = st
    reading = (>>=)
    needData = pure . NeedData
    failWith = pure . Failed
    refuse = pure . Refused
    finish = pure . Finished

-- | Walk a response's chunks with one reader owned by this read.
readJsonWalk :: BodyLimit -> (Tokens RealWorld -> Step RealWorld s) -> IO ByteString -> IO (Either LimitError (StreamResult s))
readJsonWalk = readJsonWalkST stToIO

-- | Run a walk's reader and writer in the same state thread, lifting only at chunk boundaries.
readJsonWalkST :: (Monad m) => (forall a. ST st a -> m a) -> BodyLimit -> (Tokens st -> Step st s) -> m ByteString -> m (Either LimitError (StreamResult s))
readJsonWalkST run bound walk readChunk = do
    start <- run (newTokenReader >>= walk)
    readSteps run bound start readChunk

-- | A walk's state between tokens: the read's table and the consumer's accumulator.
data Walked s = Walked !InternTable s

-- | How a consumer takes each field: it continues with its next state, or refuses the field.
type FieldStep s field r = s -> field -> (Either LimitError s -> r) -> r

-- | A consumer that takes each field without an effect of its own.
pureStep :: (s -> field -> Either LimitError s) -> FieldStep s field r
pureStep step acc field next = next (step acc field)
{-# INLINE pureStep #-}

-- | Pass a field to the consumer's step. A refused field ends the walk.
emit :: (Walk r) => FieldStep s field r -> s -> field -> (s -> r) -> r
emit step acc field next = step acc field (either refuse next)
{-# INLINE emit #-}

-- | The next element, suspending for input at a chunk boundary. The continuation runs once.
withElement :: (Walk r) => Tokens (TokenState r) -> (Element -> Tokens (TokenState r) -> r) -> r
withElement tokens next = reading (nextToken tokens) $ \found -> awaitElement tokens found (oneShot (oneShot . next))
{-# INLINE withElement #-}

awaitElement :: (Walk r) => Tokens (TokenState r) -> Next -> (Element -> Tokens (TokenState r) -> r) -> r
awaitElement tokens found next = case found of
    PartialResult element -> next element tokens
    TokMoreData -> needData (\chunk -> reading (supplyTokens tokens chunk) (\() -> withElement tokens next))
    TokFailed -> failWith "the JSON lexer failed"
{-# INLINEABLE awaitElement #-}

-- | Skip the value starting at the element without decoding it, as json-stream's @ignoreVal@ does.
skipFrom :: (Walk r) => Element -> Tokens (TokenState r) -> (Tokens (TokenState r) -> r) -> r
skipFrom element rest next = case element of
    JValue _ -> next rest
    JInteger _ -> next rest
    StringRaw{} -> next rest
    StringContent _ -> skipStringThen rest next
    ArrayBegin -> skipRest 1 rest next
    ObjectBegin -> skipRest 1 rest next
    ArrayEnd -> failWith "unexpected end of array"
    ObjectEnd -> failWith "unexpected end of object"
    StringEnd -> failWith "unexpected end of string"
{-# INLINEABLE skipFrom #-}

-- | Skip to the end of the container the given number of levels up. Any closing token closes a level.
skipRest :: (Walk r) => Int -> Tokens (TokenState r) -> (Tokens (TokenState r) -> r) -> r
skipRest !level tokens next = withElement tokens $ \element rest -> case element of
    ArrayEnd -> closed rest
    ObjectEnd -> closed rest
    ArrayBegin -> skipRest (level + 1) rest next
    ObjectBegin -> skipRest (level + 1) rest next
    StringContent _ -> skipStringThen rest (\after -> skipRest level after next)
    StringEnd -> failWith "unexpected end of string"
    _ -> skipRest level rest next
  where
    closed rest
        | level <= 1 = next rest
        | otherwise = skipRest (level - 1) rest next
{-# INLINEABLE skipRest #-}

skipStringThen :: (Walk r) => Tokens (TokenState r) -> (Tokens (TokenState r) -> r) -> r
skipStringThen tokens next = withElement tokens $ \element rest -> case element of
    StringContent _ -> skipStringThen rest next
    StringEnd -> next rest
    _ -> failWith "unexpected token in a string"
{-# INLINEABLE skipStringThen #-}

-- | Skip the value, then fail: a retained value with no structural budget left.
tooDeep :: (Walk r) => Element -> Tokens (TokenState r) -> r
tooDeep element rest = skipFrom element rest (const (failWith nestingLimit))
{-# INLINEABLE tooDeep #-}

-- | Whether the element starts a string.
isString :: Element -> Bool
isString = \case
    StringRaw{} -> True
    StringContent _ -> True
    JValue (Aeson.String _) -> True
    _ -> False

-- | Decode the string starting at the element, failing where json-stream's @string@ fails.
readString :: (Walk r) => Element -> Tokens (TokenState r) -> (Name -> Tokens (TokenState r) -> r) -> r
readString element rest next = case element of
    StringRaw bytes True -> next (Plain bytes) rest
    StringRaw bytes False -> case unescapeText bytes of
        Right text -> next (decodedName text) rest
        Left err -> failWith (show err)
    StringContent part -> longString [part] rest next
    JValue (Aeson.String text) -> next (decodedName text) rest
    _ -> failWith "expected a string"
{-# INLINEABLE readString #-}

longString :: (Walk r) => [ByteString] -> Tokens (TokenState r) -> (Name -> Tokens (TokenState r) -> r) -> r
longString parts tokens next = withElement tokens $ \element rest -> case element of
    StringContent part -> longString (part : parts) rest next
    StringEnd -> case unescapeText (BS.concat (reverse parts)) of
        Right text -> next (decodedName text) rest
        Left _ -> failWith "Error decoding UTF8"
    _ -> failWith "unexpected token in a string"
{-# INLINEABLE longString #-}

{- | Visit each member of an object whose opening brace was read, as json-stream's @objectKeyValues@
does: every key is decoded, and a key longer than 64 KiB across pieces drops its member unread.
-}
eachMember :: (Walk r) => (st -> Name -> Tokens (TokenState r) -> (st -> Tokens (TokenState r) -> r) -> r) -> (st -> Tokens (TokenState r) -> r) -> st -> Tokens (TokenState r) -> r
eachMember visit done = loop
  where
    loop acc tokens = withElement tokens $ \element rest -> case element of
        ObjectEnd -> done acc rest
        _ -> memberName element rest (\key after -> visit acc key after loop) (loop acc)
{-# INLINE eachMember #-}

-- | Decode the object key at the element, or skip its member when json-stream drops it unread.
memberName :: (Walk r) => Element -> Tokens (TokenState r) -> (Name -> Tokens (TokenState r) -> r) -> (Tokens (TokenState r) -> r) -> r
memberName element rest named dropped = case element of
    JValue (Aeson.String key) -> named (decodedName key) rest
    StringRaw bytes True -> named (Plain bytes) rest
    StringRaw bytes False -> case unescapeText bytes of
        Right key -> named (decodedName key) rest
        Left err -> failWith (show err)
    StringContent part -> longKey [part] (BS.length part) rest named dropped
    _ -> failWith "unexpected token where an object key belongs"
{-# INLINEABLE memberName #-}

-- json-stream's getLongKey: the limit applies from the third piece, and a dropped key skips its value.
longKey :: (Walk r) => [ByteString] -> Int -> Tokens (TokenState r) -> (Name -> Tokens (TokenState r) -> r) -> (Tokens (TokenState r) -> r) -> r
longKey parts !size tokens next dropped = withElement tokens $ \element rest -> case element of
    StringEnd -> case unescapeText (BS.concat (reverse parts)) of
        Right key -> next (decodedName key) rest
        Left _ -> failWith "Error decoding UTF8"
    StringContent part
        | size > 65536 -> skipStringThen rest (\after -> withElement after (\value afterValue -> skipFrom value afterValue dropped))
        | otherwise -> longKey (part : parts) (size + BS.length part) rest next dropped
    _ -> failWith "unexpected token in an object key"
{-# INLINEABLE longKey #-}

-- | Visit each item of an array whose opening bracket was read, with its position.
eachItem :: (Walk r) => (st -> Int -> Element -> Tokens (TokenState r) -> (st -> Tokens (TokenState r) -> r) -> r) -> (st -> Tokens (TokenState r) -> r) -> st -> Tokens (TokenState r) -> r
eachItem visit done = loop 0
  where
    loop !position acc tokens = withElement tokens $ \element rest -> case element of
        ArrayEnd -> done acc rest
        _ -> visit acc position element rest (loop (position + 1))
{-# INLINE eachItem #-}
