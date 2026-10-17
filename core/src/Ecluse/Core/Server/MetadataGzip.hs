-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Gzip one served metadata body at level 1, in native steps of at most 32 KiB. The caller's
charge pays for the native workspace before the deflater exists and for each output buffer
before it is allocated. The deflater is released on every exit, cancellation included, so a
response is sent with no native state held. Each request owns its deflater: no compression
history crosses requests.
-}
module Ecluse.Core.Server.MetadataGzip (
    GzipBody (..),
    gzipMetadata,
    gzipLazyBody,

    -- * The charges (exported for the residency probe)
    gzipWorkspaceBytes,
    gzipBufferChargeBytes,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as LBS
import Data.Streaming.Zlib.Lowlevel qualified as Z
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (plusPtr)
import UnliftIO.Exception (bracket, throwIO)

import Ecluse.Core.Zlib (ZlibFault (InvalidZlibState), advanceStream, initialiseDeflate, openStream, releaseDeflate)

-- | A complete gzip member, held as the buffers zlib wrote it into, and its exact length in bytes.
data GzipBody = GzipBody
    { gzipChunks :: [ByteString]
    , gzipLength :: Int
    }

-- | The body as one lazy value over its buffers, with no copy.
gzipLazyBody :: GzipBody -> LByteString
gzipLazyBody = LBS.fromChunks . gzipChunks

-- | The native allowance: zlib documents 256 KiB plus a few KiB of deflate state at this window and memory level.
gzipWorkspaceBytes :: Int
gzipWorkspaceBytes = 512 * 1024

-- | One output buffer's charge: a pinned 32 KiB buffer and its heap header occupy nine 4 KiB blocks.
gzipBufferChargeBytes :: Int
gzipBufferChargeBytes = stepBytes + 4096

stepBytes :: Int
stepBytes = 32768

-- | Compress a body. A native fault is a thrown 'ZlibFault': no request can act on it, so the perimeter answers 500.
gzipMetadata :: (Int -> IO ()) -> ByteString -> IO GzipBody
gzipMetadata charge body = do
    charge gzipWorkspaceBytes
    -- Level 1, a gzip wrapper over a 32 KiB window, memory level 8, and the default strategy.
    bracket (openStream (\native -> initialiseDeflate native 1 31 8 0)) releaseDeflate (deflateBody charge body)

-- The buffers already full, newest first, then the buffer zlib writes into and its used bytes.
data Output = Output [ByteString] (ForeignPtr Word8) Int

deflateBody :: (Int -> IO ()) -> ByteString -> Z.ZStream' -> IO GzipBody
deflateBody charge body native = newBuffer >>= \buffer -> advance 0 (Output [] buffer 0)
  where
    newBuffer = charge gzipBufferChargeBytes >> BSI.mallocByteString stepBytes
    advance offset (Output full buffer used) = do
        let slice = BS.take stepBytes (BS.drop offset body)
            call = if offset + BS.length slice == BS.length body then Z.c_call_deflate_finish else Z.c_call_deflate_noflush
        (status, consumed, written) <- withForeignPtr buffer (\start -> advanceStream call native slice (start `plusPtr` used) (stepBytes - used))
        let output = Output full buffer (used + written)
        case status of
            1 -> pure (finished output)
            0
                | consumed == 0 && written == 0 -> throwIO (InvalidZlibState status)
                | used + written == stepBytes -> do
                    fresh <- newBuffer
                    advance (offset + consumed) (Output (current output : full) fresh 0)
                | otherwise -> advance (offset + consumed) output
            _ -> throwIO (InvalidZlibState status)

finished :: Output -> GzipBody
finished output@(Output full _ used) =
    GzipBody (reverse (if used > 0 then current output : full else full)) (sum (map BS.length full) + used)

current :: Output -> ByteString
current (Output _ buffer used) = BSI.fromForeignPtr0 buffer used
