-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Bounded batch records for the temporary tasty-bench diagnostic overlay.
module Test.Tasty.Bench.Diagnostic (Recorder, withRecorder, recordBatch) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.DeepSeq (force)
import Control.Exception (Exception, bracket, evaluate, finally, onException, throwIO)
import Control.Monad (unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Word (Word64)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import System.Environment (lookupEnv)
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)

-- | A row owns fixed storage and the identity of the thread that executes its batches.
data Recorder = Recorder FilePath ThreadId String (IORef Int) (Ptr Word64) (Ptr Int64)

data DiagnosticFault = InvalidMode | MissingOutput | BufferFull | DifferentThread
    deriving (Show)

instance Exception DiagnosticFault

capacity :: Int
capacity = 256

-- | Allocate before the row and flush after it, including partial records when the row fails.
withRecorder :: (Maybe Recorder -> Bool -> IO a) -> IO a
withRecorder action = do
    mode <- lookupEnv "ECLUSE_BATCH_MODE"
    case mode of
        Just "record" -> recorded False
        Just "empty-record" -> recorded True
        Just "empty-native" -> action Nothing True
        _ -> throwIO InvalidMode
  where
    recorded empty = bracket acquire release (\recorder -> action (Just recorder) empty)

acquire :: IO Recorder
acquire = do
    path <- lookupEnv "ECLUSE_BATCH_OUTPUT" >>= maybe (throwIO MissingOutput) pure
    thread <- myThreadId
    label <- evaluate (force (show thread))
    count <- newIORef 0
    -- Nested allocation releases the first buffer if allocation of the second fails.
    bracketOnAllocation path thread label count

bracketOnAllocation :: FilePath -> ThreadId -> String -> IORef Int -> IO Recorder
bracketOnAllocation path thread label count = do
    unsigned <- mallocArray (capacity * 5)
    signed <- mallocArray (capacity * 2) `onException` free unsigned
    pure (Recorder path thread label count unsigned signed)

-- | Save after the final native counter read. Counter endpoints retain their original integer types.
recordBatch :: Recorder -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Int64 -> Int64 -> IO ()
recordBatch (Recorder _ expected _ count unsigned signed) n startTime startAllocs endTime endAllocs threadStart threadEnd = do
    actual <- myThreadId
    unless (actual == expected) (throwIO DifferentThread)
    index <- readIORef count
    when (index >= capacity) (throwIO BufferFull)
    pokeElemOff unsigned (index * 5) n
    pokeElemOff unsigned (index * 5 + 1) startTime
    pokeElemOff unsigned (index * 5 + 2) startAllocs
    pokeElemOff unsigned (index * 5 + 3) endTime
    pokeElemOff unsigned (index * 5 + 4) endAllocs
    pokeElemOff signed (index * 2) threadStart
    pokeElemOff signed (index * 2 + 1) threadEnd
    writeIORef count (index + 1)

release :: Recorder -> IO ()
release recorder@(Recorder _ _ _ _ unsigned signed) =
    flush recorder `finally` (free signed >> free unsigned)

flush :: Recorder -> IO ()
flush (Recorder path _ label count unsigned signed) = do
    size <- readIORef count
    withFile path WriteMode $ \handle -> do
        hPutStrLn handle "sequence,n,startTimePs,startGlobalBytes,endTimePs,endGlobalBytes,startThreadBytes,endThreadBytes,thread"
        mapM_ (writeRow handle) [0 .. size - 1]
  where
    writeRow handle index = do
        values <- mapM (peekElemOff unsigned . (index * 5 +)) [0 .. 4]
        start <- peekElemOff signed (index * 2)
        end <- peekElemOff signed (index * 2 + 1)
        hPutStrLn handle (intercalate "," (show index : map show values ++ [show start, show end, label]))
