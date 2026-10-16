-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ForeignFunctionInterface #-}

{- | Owned zlib streams over the existing streaming-commons ABI. A caller opens a stream inside its
own bracket, advances it in bounded native calls, and releases it there, so the native state never
waits for a collector. Each call is an @unsafe@ foreign call, which keeps the bounded step short.
-}
module Ecluse.Core.Zlib (
    -- * Opening and releasing a stream
    ZlibFault (..),
    openStream,
    initialiseInflate,
    initialiseDeflate,
    releaseInflate,
    releaseDeflate,

    -- * One bounded native call
    advanceStream,
) where

import Data.ByteString.Unsafe qualified as BSU
import Data.Streaming.Zlib.Lowlevel qualified as Z
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import UnliftIO.Exception (mask_, onException, throwIO)

-- | A stream the native library could not allocate, or one that reported state no valid stream reaches.
data ZlibFault
    = ZlibAllocationFailed
    | ZlibInitialisationFailed CInt
    | InvalidZlibState CInt
    | InvalidZlibCounters
    deriving stock (Eq, Show)

instance Exception ZlibFault

-- | Allocate a stream and run its initialiser. A failed initialiser frees the allocation before the fault escapes.
openStream :: (Z.ZStream' -> IO CInt) -> IO Z.ZStream'
openStream initialise = mask_ $ do
    native <- Z.zstreamNew
    when (native == nullPtr) (throwIO ZlibAllocationFailed)
    status <- initialise native `onException` free native
    if status == 0
        then pure native
        else do
            free native
            throwIO (ZlibInitialisationFailed status)

-- | One call over at most @room@ output bytes: the status, the input bytes consumed, and the output bytes written.
advanceStream :: (Z.ZStream' -> IO CInt) -> Z.ZStream' -> ByteString -> Ptr Word8 -> Int -> IO (CInt, Int, Int)
advanceStream call native input destination room =
    BSU.unsafeUseAsCStringLen input $ \(source, inputLength) -> do
        Z.c_set_avail_in native source (fromIntegral inputLength)
        Z.c_set_avail_out native (castPtr destination) (fromIntegral room)
        status <- call native
        unread <- fromIntegral <$> Z.c_get_avail_in native
        unused <- fromIntegral <$> Z.c_get_avail_out native
        -- Reject impossible native counters before a caller builds a ByteString over the output.
        when (unread > inputLength || unused > room) (throwIO InvalidZlibCounters)
        pure (status, inputLength - unread, room - unused)

-- | Initialise an inflater with the given window bits.
foreign import ccall unsafe "streaming_commons_inflate_init2"
    initialiseInflate :: Z.ZStream' -> CInt -> IO CInt

-- | Initialise a deflater. The arguments after the stream are level, window bits, memory level, and strategy.
foreign import ccall unsafe "streaming_commons_deflate_init2"
    initialiseDeflate :: Z.ZStream' -> CInt -> CInt -> CInt -> CInt -> IO CInt

-- | End an inflater and free its stream.
foreign import ccall unsafe "streaming_commons_free_z_stream_inflate"
    releaseInflate :: Z.ZStream' -> IO ()

-- | End a deflater and free its stream.
foreign import ccall unsafe "streaming_commons_free_z_stream_deflate"
    releaseDeflate :: Z.ZStream' -> IO ()
