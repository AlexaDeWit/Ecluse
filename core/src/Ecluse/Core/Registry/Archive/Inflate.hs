-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | A scoped inflater over the existing streaming-commons ABI, with deterministic release on every exit.
module Ecluse.Core.Registry.Archive.Inflate (InflateFormat (..), withInflatedChunks, splitArchiveCharge) where

import Control.Concurrent.MVar qualified as MVar
import Data.ByteString qualified as BS
import Data.Streaming.Zlib.Lowlevel qualified as Z
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (castPtr)
import UnliftIO.Exception (bracket, mask_, throwIO)

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Zlib (ZlibFault (InvalidZlibState, ZlibAllocationFailed), advanceStream, initialiseInflate, openStream, releaseInflate)

-- | Raw deflate, one zlib stream, or a bounded sequence of gzip members.
data InflateFormat = RawDeflate | ZlibStream | GzipMembers Int

data Inflater = Fresh !Int ByteString | Active !Int Z.ZStream' ByteString | Finished | Failed ParseError

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
                        ZlibStream | not (BS.null remaining) -> failStream owner "zlib stream has trailing compressed bytes"
                        GzipMembers _ | not (BS.null remaining) -> do
                            writeIORef owner (Fresh count remaining)
                            emitOrContinue bytes
                        _ -> pure (Right bytes)
                0
                    | consumed == 0 && BS.null bytes -> failStream owner "compressed stream made no progress"
                    | otherwise -> do
                        writeIORef owner (Active count native remaining)
                        emitOrContinue bytes
                -4 -> throwIO ZlibAllocationFailed
                -2 -> throwIO (InvalidZlibState status)
                _ -> failStream owner "malformed or truncated compressed stream"
  where
    emitOrContinue bytes = if BS.null bytes then nextChunk format owner output else pure (Right bytes)

memberLimit :: InflateFormat -> Int
memberLimit RawDeflate = 1
memberLimit ZlibStream = 1
memberLimit (GzipMembers limit) = limit

-- Allocation failures belong to the worker fault boundary, not the archive-refusal result.
acquire :: InflateFormat -> IO Z.ZStream'
acquire format = openStream (`initialiseInflate` (case format of RawDeflate -> -15; ZlibStream -> 15; GzipMembers _ -> 31))

close :: IORef Inflater -> IO ()
close owner = mask_ $ do
    previous <- atomicModifyIORef' owner (Finished,)
    case previous of
        Active _ native _ -> releaseInflate native
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
    withForeignPtr output $ \destination -> do
        (status, consumed, written) <- advanceStream Z.c_call_inflate_noflush native (BS.take chunkBytes input) destination chunkBytes
        bytes <- BS.packCStringLen (castPtr destination, written)
        pure (status, consumed, bytes)

-- | Combine decoder and retained-content reservations into one absolute workspace charge.
splitArchiveCharge :: (Int -> IO ()) -> IO (Int -> IO (), Int -> IO ())
splitArchiveCharge pay = do
    portions <- MVar.newMVar (0, 0)
    let reserve change = MVar.modifyMVar_ portions $ \previous -> do
            let next@(decoder, retained) = change previous
            pay (decoder + retained)
            pure next
    pure (\decoder -> reserve (\(_, retained) -> (decoder, retained)), \retained -> reserve (\(decoder, _) -> (decoder, retained)))
