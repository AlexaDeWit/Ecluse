-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The streaming bench's pumps, its synthetic body, and the sink the pumps write to.
The sink drives streaming-commons' builder receiver as Warp's stream response does, without the
socket, the response header, or chunked framing. Every row sends the whole body, whatever its flush policy.
-}
module Ecluse.Core.StreamBench.Sink (
    Pump,
    strategies,
    chunkSizes,
    streamBody,
    bodyChunks,
    sentBytes,
    sentBody,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString)
import Data.ByteString.Builder.Extra (flush)
import Data.Streaming.ByteString.Builder (allocBuffer, newByteStringBuilderRecv, reuseBufferStrategy)
import UnliftIO.Exception (evaluate)

import Ecluse.Core.Server.Stream (pumpBody)
import Ecluse.Test.Registry.JsonStream (heldChunks, sliceChunks)

-- | A body pump: it reads chunks until an empty one, writes each to the sink, and flushes by its own policy.
type Pump = IO ByteString -> (Builder -> IO ()) -> IO () -> IO ()

-- | The measured pumps by row name: the shipped pump, a flush every 64 KiB, and one flush at the end.
strategies :: [(Text, Pump)]
strategies =
    [ ("opening-flush (shipped)", pumpBody)
    , ("flush-threshold-64k", pumpThreshold (64 * 1024))
    , ("flush-end-only", pumpEndOnly)
    ]

-- | The upstream read sizes the rows measure.
chunkSizes :: [Int]
chunkSizes = [4096, 16384, 65536]

-- | An 8 MiB body whose bytes repeat every 251, so a lost, repeated, or reordered chunk changes it.
streamBody :: ByteString
streamBody = fst (BS.unfoldrN (8 * 1024 * 1024) (\i -> Just (fromIntegral (i `mod` 251), i + 1)) (0 :: Int))

-- | 'streamBody' cut into chunks of the given size.
bodyChunks :: Int -> [ByteString]
bodyChunks size = sliceChunks size streamBody

-- | Pump the chunks into the sink and count the bytes it sends: the measured operation.
sentBytes :: Pump -> [ByteString] -> IO Int
sentBytes pump chunks = do
    total <- newIORef 0
    runSink (\piece -> modifyIORef' total (+ BS.length piece)) pump chunks
    readIORef total

-- | Pump the chunks into the sink and copy out every byte it sends, in order.
sentBody :: Pump -> [ByteString] -> IO ByteString
sentBody pump chunks = do
    pieces <- newIORef []
    -- The copy must run before send returns, while the piece still holds its bytes.
    let keep piece = evaluate (BS.copy piece) >>= \copied -> modifyIORef' pieces (copied :)
    runSink keep pump chunks
    BS.concat . reverse <$> readIORef pieces

-- Warp 3.4.9's sendRsp for RspStream: writes fill a reused 16 KiB buffer, each full or flushed buffer
-- goes to send, and the end of the stream sends the tail. A piece aliases the buffer until send returns.
runSink :: (ByteString -> IO ()) -> Pump -> [ByteString] -> IO ()
runSink send pump chunks = do
    readChunk <- heldChunks chunks
    (recv, finish) <- newByteStringBuilderRecv (reuseBufferStrategy (allocBuffer warpWriteBufferBytes))
    let write builder = recv builder >>= sendPieces
        sendPieces popper = do
            piece <- popper
            unless (BS.null piece) (send piece >> sendPieces popper)
    pump readChunk write (write flush)
    finish >>= traverse_ send
  where
    warpWriteBufferBytes = 16384

pumpThreshold :: Int -> Pump
pumpThreshold threshold readChunk write flushSink = go 0
  where
    go pending = do
        chunk <- readChunk
        if BS.null chunk
            then when (pending > 0) flushSink
            else do
                write (byteString chunk)
                let pending' = pending + BS.length chunk
                if pending' >= threshold
                    then flushSink >> go 0
                    else go pending'

pumpEndOnly :: Pump
pumpEndOnly readChunk write flushSink = go
  where
    go = do
        chunk <- readChunk
        if BS.null chunk
            then flushSink
            else write (byteString chunk) >> go
