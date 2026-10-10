-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

-- | Source ownership and cancellation checks shared by the registry readers.
module Ecluse.Test.Registry.Source (trackedSource, assertSourceHeld, awaitSourceRelease, assertCancelledRead) where

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Foreign.Concurrent qualified as Foreign
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import System.Mem (performMajorGC)
import System.Timeout (timeout)
import Test.Hspec (Expectation, shouldReturn)
import UnliftIO.Async (AsyncCancelled (AsyncCancelled), cancel, waitCatch, withAsync)
import UnliftIO.Exception (finally)

-- | Attach the finaliser to the backing allocation, which every retained slice keeps alive.
trackedSource :: ByteString -> MVar () -> IO ByteString
trackedSource prefix released = do
    let bytes = prefix <> BS.replicate sourcePaddingBytes spaceByte
    pointer <- mallocBytes (BS.length bytes)
    foreignPointer <- Foreign.newForeignPtr pointer (free pointer >> putMVar released ())
    withForeignPtr foreignPointer $ \target ->
        BS.useAsCStringLen bytes $ \(source, size) -> copyBytes target (castPtr source) size
    pure (BSI.fromForeignPtr foreignPointer 0 (BS.length bytes))

-- | A reachable slice must prevent finalisation through a collection.
assertSourceHeld :: MVar () -> Expectation
assertSourceHeld released = do
    performMajorGC
    timeout heldProbeMicros (takeMVar released) `shouldReturn` Nothing

-- | A released source must permit its allocation's finaliser to run.
awaitSourceRelease :: MVar () -> Expectation
awaitSourceRelease released = do
    performMajorGC
    timeout releaseTimeoutMicros (takeMVar released) `shouldReturn` Just ()

-- | Cancel after a partial value, requiring the exception and the source cleanup to propagate.
assertCancelledRead :: (IO ByteString -> IO a) -> Expectation
assertCancelledRead run = do
    initial <- newIORef True
    entered <- newEmptyMVar
    blocked <- newEmptyMVar
    released <- newEmptyMVar
    let next =
            atomicModifyIORef' initial (False,) >>= \case
                True -> pure "["
                False -> (putMVar entered () >> takeMVar blocked) `finally` putMVar released ()
    withAsync (run next) $ \worker -> do
        timeout releaseTimeoutMicros (takeMVar entered) `shouldReturn` Just ()
        cancel worker
        either fromException (const Nothing) <$> waitCatch worker `shouldReturn` Just AsyncCancelled
        timeout releaseTimeoutMicros (takeMVar released) `shouldReturn` Just ()

sourcePaddingBytes, heldProbeMicros, releaseTimeoutMicros :: Int
sourcePaddingBytes = 4 * 1024 * 1024
heldProbeMicros = 100000
releaseTimeoutMicros = 5000000

spaceByte :: Word8
spaceByte = 32
