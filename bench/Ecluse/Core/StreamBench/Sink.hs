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
    SinkBuffer,
    newSinkBuffer,
    sentBytes,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString)
import Data.ByteString.Builder.Extra (flush)
import Data.Streaming.ByteString.Builder (Buffer, allocBuffer, newByteStringBuilderRecv, reuseBufferStrategy)

import Ecluse.Core.Deadline (newDeadlines)
import Ecluse.Core.Registry.Progress (meteredReader, watchedRaising)
import Ecluse.Core.Security (defaultLimits, progressFloor)
import Ecluse.Core.Server.Stream (pumpBody)
import Ecluse.Test.Registry.JsonStream (heldChunks, sliceChunks)

-- | A body pump: it reads chunks until an empty one, writes each to the sink, and flushes by its own policy.
type Pump = IO ByteString -> (Builder -> IO ()) -> IO () -> IO ()

-- | The measured pumps by row name: the shipped pump bare and metered as the relay runs it, a flush every 64 KiB, and one flush at the end.
strategies :: [(Text, Pump)]
strategies =
    [ ("opening-flush (shipped)", pumpBody)
    , ("opening-flush metered (shipped relay)", metered pumpBody)
    , ("flush-threshold-64k", pumpThreshold (64 * 1024))
    , ("flush-end-only", pumpEndOnly)
    ]

-- | The upstream read sizes the rows measure.
chunkSizes :: [Int]
chunkSizes = [4096, 16384, 65536]

-- | An 8 MiB body whose bytes repeat every 2053, a prime, so no two of its 4096-byte-aligned blocks are equal.
streamBody :: ByteString
streamBody = fst (BS.unfoldrN (8 * 1024 * 1024) (\i -> Just (fromIntegral (i `mod` 2053), i + 1)) (0 :: Int))

-- | 'streamBody' cut into chunks of the given size.
bodyChunks :: Int -> [ByteString]
bodyChunks size = sliceChunks size streamBody

-- | The sink's 16 KiB buffer, allocated once and reused by every run, as Warp reuses a connection's write buffer.
newtype SinkBuffer = SinkBuffer Buffer

instance NFData SinkBuffer where
    rnf (SinkBuffer buffer) = buffer `seq` ()

-- | Allocate a 'SinkBuffer'. Runs that share one must not overlap.
newSinkBuffer :: IO SinkBuffer
newSinkBuffer = SinkBuffer <$> allocBuffer 16384

-- | Pump the chunks into the sink and count the bytes it sends: the measured operation.
sentBytes :: SinkBuffer -> Pump -> [ByteString] -> IO Int
sentBytes buffer pump chunks = do
    total <- newIORef 0
    runSink buffer (\piece -> modifyIORef' total (+ BS.length piece)) pump chunks
    readIORef total

-- Warp's sendRsp for RspStream: writes fill the reused buffer, each full or flushed buffer goes to send,
-- and the end of the stream sends the tail. A piece aliases the buffer until send returns.
runSink :: SinkBuffer -> (ByteString -> IO ()) -> Pump -> [ByteString] -> IO ()
runSink (SinkBuffer buffer) send pump chunks = do
    readChunk <- heldChunks chunks
    (recv, finish) <- newByteStringBuilderRecv (reuseBufferStrategy (pure buffer))
    let write builder = recv builder >>= sendPieces
        sendPieces popper = do
            piece <- popper
            unless (BS.null piece) (send piece >> sendPieces popper)
    pump readChunk write (write flush)
    finish >>= traverse_ send

{- The progress floor and per-chunk meter "Ecluse.Core.Server.Stream" puts around a relay's pump, under
the default floor. Nothing sweeps these deadlines: a relay adds only the arming and disarming. -}
metered :: Pump -> Pump
metered pump readChunk write flushSink = do
    deadlines <- newDeadlines
    watchedRaising deadlines (progressFloor defaultLimits) (\watch -> pump (meteredReader watch readChunk) write flushSink)

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
