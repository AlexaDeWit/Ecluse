-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | A scoped inflater over the existing streaming-commons ABI, with deterministic release on every exit.
module Ecluse.Core.Registry.PyPI.Archive.Inflate (InflateFormat (..), withInflatedChunks) where

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Streaming.Zlib.Lowlevel qualified as Z
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (castPtr, nullPtr)
import UnliftIO.Exception (bracket, mask_, onException, throwIO)

import Ecluse.Core.Registry (ParseError (..))

-- | Raw ZIP members or a bounded sequence of gzip members.
data InflateFormat = RawDeflate | GzipMembers Int

data Inflater = Fresh !Int ByteString | Active !Int Z.ZStream' ByteString | Finished | Failed ParseError

data NativeInflateFault = InflaterAllocationFailed | InflaterInitialisationFailed CInt | InvalidInflaterState | InvalidInflaterCounters
    deriving stock (Show)

instance Exception NativeInflateFault

-- | Use the pull action sequentially inside its callback. Empty output denotes the verified stream end.
withInflatedChunks :: InflateFormat -> (Int -> IO ()) -> ByteString -> (IO (Either ParseError ByteString) -> IO a) -> IO a
withInflatedChunks format charge bytes consume = do
    -- zlib documents a 32 KiB window plus 7 KiB state. One 32 KiB output buffer is reused.
    charge (32768 + 7168 + chunkBytes)
    output <- mallocForeignPtrBytes chunkBytes
    bracket (newIORef (Fresh 0 bytes)) close $ \owner -> consume (nextChunk format owner output)

chunkBytes :: Int
chunkBytes = 32768

nextChunk :: InflateFormat -> IORef Inflater -> ForeignPtr Word8 -> IO (Either ParseError ByteString)
nextChunk format owner output =
    readIORef owner >>= \case
        Finished -> pure (Right BS.empty)
        Failed fault -> pure (Left fault)
        Fresh count input
            | count >= memberLimit format -> failStream owner "compressed member count exceeds its bound"
            | otherwise -> do
                mask_ $ acquire format >>= \native -> writeIORef owner (Active (count + 1) native input)
                nextChunk format owner output
        Active count native input -> do
            (status, consumed, bytes) <- step native input output
            let remaining = BS.drop consumed input
            case status of
                1 -> do
                    close owner
                    case format of
                        RawDeflate | not (BS.null remaining) -> failStream owner "deflate member has trailing compressed bytes"
                        GzipMembers _ | not (BS.null remaining) -> do
                            writeIORef owner (Fresh count remaining)
                            emitOrContinue bytes
                        _ -> pure (Right bytes)
                0
                    | consumed == 0 && BS.null bytes -> failStream owner "compressed stream made no progress"
                    | otherwise -> do
                        writeIORef owner (Active count native remaining)
                        emitOrContinue bytes
                -4 -> throwIO InflaterAllocationFailed
                -2 -> throwIO InvalidInflaterState
                _ -> failStream owner "malformed or truncated compressed stream"
  where
    emitOrContinue bytes = if BS.null bytes then nextChunk format owner output else pure (Right bytes)

memberLimit :: InflateFormat -> Int
memberLimit RawDeflate = 1
memberLimit (GzipMembers limit) = limit

acquire :: InflateFormat -> IO Z.ZStream'
acquire format = mask_ $ do
    native <- Z.zstreamNew
    -- Allocation failures belong to the worker fault boundary, not the archive-refusal result.
    when (native == nullPtr) (throwIO InflaterAllocationFailed)
    status <- initialise native (case format of RawDeflate -> -15; GzipMembers _ -> 31) `onException` free native
    if status == 0
        then pure native
        else do
            free native
            throwIO (InflaterInitialisationFailed status)

close :: IORef Inflater -> IO ()
close owner = mask_ $ do
    previous <- atomicModifyIORef' owner (Finished,)
    case previous of
        Active _ native _ -> release native
        Fresh{} -> pure ()
        Finished -> pure ()
        Failed{} -> pure ()

failStream :: IORef Inflater -> Text -> IO (Either ParseError ByteString)
failStream owner message = do
    close owner
    let fault = ParseError message
    writeIORef owner (Failed fault)
    pure (Left fault)

step :: Z.ZStream' -> ByteString -> ForeignPtr Word8 -> IO (CInt, Int, ByteString)
step native input output =
    BSU.unsafeUseAsCStringLen (BS.take chunkBytes input) $ \(source, inputLength) ->
        withForeignPtr output $ \destination -> do
            Z.c_set_avail_in native source (fromIntegral inputLength)
            Z.c_set_avail_out native (castPtr destination) (fromIntegral chunkBytes)
            status <- Z.c_call_inflate_noflush native
            unread <- fromIntegral <$> Z.c_get_avail_in native
            unused <- fromIntegral <$> Z.c_get_avail_out native
            -- Reject impossible native counters before constructing a ByteString from the output pointer.
            when (unread > inputLength || unused > chunkBytes) (throwIO InvalidInflaterCounters)
            bytes <- BS.packCStringLen (castPtr destination, chunkBytes - unused)
            pure (status, inputLength - unread, bytes)

foreign import ccall unsafe "streaming_commons_inflate_init2"
    initialise :: Z.ZStream' -> CInt -> IO CInt

foreign import ccall unsafe "streaming_commons_free_z_stream_inflate"
    release :: Z.ZStream' -> IO ()
