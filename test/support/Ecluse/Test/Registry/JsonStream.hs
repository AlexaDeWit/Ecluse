-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure chunk inputs for the production registry stream drivers, and checks for shared keys and texts.
module Ecluse.Test.Registry.JsonStream (parseJsonChunks, walkJsonChunks, walkWritingChunks, readStepsChunks, heldChunks, sliceChunks, byteChunks, testTable, readOutcome, sharesKey, sharesString, sameTexts, namedMembers, retainedObjectOr) where

import Control.Monad.ST (ST, runST)
import Data.Aeson (Value (Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.JsonStream.Lexer (Cursor)
import Data.JsonStream.Parser qualified as J
import System.Mem.StableName (makeStableName)
import UnliftIO.Exception (evaluate)

import Ecluse.Core.Registry.Json.Intern (InternTable, SipKey (SipKey), newInternTable)
import Ecluse.Core.Registry.Json.Walk (Step, Steps, readJsonWalk, readJsonWalkST)
import Ecluse.Core.Registry.JsonStream (StreamResult (..), isNestingLimit, readJsonStream, readStepsWithEnd, retainedObjectWith)
import Ecluse.Core.Registry.JsonStream.Internal (Members (NamedMembers))
import Ecluse.Core.Security (BodyLimit, LimitError)

-- | Run the same incremental driver against explicit chunks for pure callers and boundary tests.
parseJsonChunks :: BodyLimit -> J.Parser a -> (s -> a -> Either LimitError s) -> s -> [ByteString] -> Either LimitError (StreamResult s)
parseJsonChunks bound parser step initial = evalState (readJsonStream bound parser step initial nextChunk)

-- | Run the production walk driver against explicit chunks.
walkJsonChunks :: BodyLimit -> (Cursor -> Step s) -> [ByteString] -> Either LimitError (StreamResult s)
walkJsonChunks bound walk = evalState (readJsonWalk bound walk nextChunk)

-- | Run a walk that writes as it reads against explicit chunks, from a setup that makes its writer.
walkWritingChunks :: BodyLimit -> (forall st. ST st (Cursor -> ST st (Steps (ST st) s))) -> [ByteString] -> Either LimitError (StreamResult s)
walkWritingChunks bound setup chunks = runST $ do
    walk <- setup
    evalStateT (readJsonWalkST lift bound walk nextChunk) chunks

-- | Run native steps and their own end-of-input hook against explicit pure chunks.
readStepsChunks :: (Step s -> Identity (Step s)) -> BodyLimit -> Step s -> [ByteString] -> Either LimitError (StreamResult s)
readStepsChunks finish bound steps = evalState (readStepsWithEnd (pure . runIdentity) (pure . runIdentity . finish) bound steps nextChunk)

-- | A chunk source for a reader that runs in IO: the chunks in order, then the empty chunk that ends the body.
heldChunks :: [ByteString] -> IO (IO ByteString)
heldChunks chunks = do
    remaining <- newIORef chunks
    pure $ atomicModifyIORef' remaining $ \case
        [] -> ([], BS.empty)
        chunk : rest -> (rest, chunk)

-- | A body cut into slices of the given size, the last one shorter when the size does not divide the body.
sliceChunks :: Int -> ByteString -> [ByteString]
sliceChunks size bytes
    | BS.null bytes = []
    | otherwise = case BS.splitAt (max 1 size) bytes of (!piece, rest) -> piece : sliceChunks size rest

-- | One-byte chunks that do not share the body's buffer.
byteChunks :: ByteString -> [ByteString]
byteChunks = map BS.singleton . BS.unpack

nextChunk :: (MonadState [ByteString] m) => m ByteString
nextChunk = state $ \case
    [] -> (BS.empty, [])
    chunk : rest -> (chunk, rest)

-- | A document table with the production hash under a fixed key, for reads that must repeat exactly.
testTable :: [Text] -> InternTable
testTable = newInternTable (SipKey 0x0706050403020100 0x0f0e0d0c0b0a0908)

-- | Compare refusals, byte counts, values and the retained-depth failure class.
readOutcome :: Either LimitError (StreamResult a) -> Either LimitError (Int, Either Bool a)
readOutcome = fmap (\result -> (streamBytes result, first isNestingLimit (streamValue result)))

-- | Whether at least two objects hold the member and all of them hold its key as one heap object.
sharesKey :: Key.Key -> [Value] -> IO Bool
sharesKey key objects = oneObject [Key.toText held | Object fields <- objects, held <- KeyMap.keys fields, held == key]

-- | Whether at least two of the values are the string and all of them hold its text as one heap object.
sharesString :: Text -> [Value] -> IO Bool
sharesString text values = oneObject [held | String held <- values, held == text]

oneObject :: [Text] -> IO Bool
oneObject texts = do
    names <- traverse (makeStableName <=< evaluate) texts
    pure $ case names of
        stable : rest@(_ : _) -> all (== stable) rest
        _ -> False

-- | Whether each pair holds one heap object twice, so the second holder adds only a pointer.
sameTexts :: [(Text, Text)] -> IO Bool
sameTexts pairs = and <$> traverse (\(held, other) -> oneObject [held, other]) pairs

-- | Retain only the named members. The first entry for a name wins.
namedMembers :: [(Text, J.Parser Value)] -> Members
namedMembers entries = NamedMembers (HashMap.fromListWith (\_ earlier -> earlier) [(name, (Key.fromText name, parser)) | (name, parser) <- entries])

-- | Retain an object or the supplied witness while preserving the combinator skip rules.
retainedObjectOr :: Value -> Members -> J.Parser Value
retainedObjectOr fallback = retainedObjectWith (pure fallback)
