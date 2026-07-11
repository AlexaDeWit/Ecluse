-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Flush-strategy benches for the artifact streaming pump
("Ecluse.Core.Server.Stream").

The shipped 'pumpBody' writes and __flushes after every upstream chunk__. On the real
serve path each flush is a socket send, so the flush cadence is a syscall-count knob:
batching several chunks per flush trades a little first-byte latency for fewer, larger
sends. That end-to-end (syscall) effect only shows over a real socket and is the load
harness's job (@bench-load@); this micro-bench isolates the __CPU side__ so the two are
not conflated.

To make the flush cadence drive measurable work __without__ a socket, the sink models
Warp's output buffer: each @write@ appends the chunk's 'Builder' to an accumulator, and
each @flush@ __commits__ that accumulator -- runs it to strict bytes, the copy Warp makes
when it sends -- and resets it. Fewer flushes therefore mean fewer, larger commits, so the
three strategies (flush-per-chunk, flush-on-threshold, flush-once-at-end) genuinely differ
in the bench, exactly as they differ in commit granularity on the wire.

The pump takes its reader and sink as plain actions, so the shipped 'pumpBody' and the two
bench-local variants ('pumpThreshold', 'pumpEndOnly') run over the identical synthetic body
with no upstream and no proxy -- the comparison is the flush policy alone. This measures
whether changing the shipped cadence has any CPU cost to weigh against the load harness's
syscall figures; it does not, on its own, justify a change.
-}
module Ecluse.Core.StreamBench (
    benchmarks,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.ByteString.Lazy qualified as BSL

import Ecluse.Core.Server.Stream (pumpBody)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfAppIO)

{- | The flush-strategy benches: the shipped flush-per-chunk pump against a
threshold-batched pump and a flush-once-at-end pump, over a synthetic multi-megabyte body
at three chunk sizes (so the interaction of chunk size and flush cadence is visible).
-}
benchmarks :: Benchmark
benchmarks =
    bgroup
        "stream (pump flush strategy)"
        [ bgroup
            (show chunkSize <> "B chunks")
            [ bench "flush-each (shipped)" (whnfAppIO (runPump pumpBody) chunks)
            , bench "flush-threshold-64k" (whnfAppIO (runPump (pumpThreshold flushThresholdBytes)) chunks)
            , bench "flush-end-only" (whnfAppIO (runPump pumpEndOnly) chunks)
            ]
        | chunkSize <- chunkSizes
        , let chunks = bodyChunks bodyBytes chunkSize
        ]
  where
    -- A mid-size tarball, streamed at a few realistic upstream read granularities.
    bodyBytes :: Int
    bodyBytes = 8 * 1024 * 1024

    chunkSizes :: [Int]
    chunkSizes = [4096, 16384, 65536]

    -- The threshold-batched pump commits roughly every 64 KiB.
    flushThresholdBytes :: Int
    flushThresholdBytes = 64 * 1024

{- | Run a pump over a fixed chunk list and a buffer-modelling sink, returning the total
bytes committed (forced by 'whnfAppIO'). Fresh reader and sink per run, so a run neither
sees a drained reader nor shares a buffer with the next.
-}
runPump :: (IO ByteString -> (Builder -> IO ()) -> IO () -> IO ()) -> [ByteString] -> IO Int
runPump pump chunks = do
    remaining <- newIORef chunks
    buffer <- newIORef mempty
    committed <- newIORef (0 :: Int)
    let readChunk =
            atomicModifyIORef' remaining $ \case
                [] -> ([], BS.empty)
                (c : cs) -> (cs, c)
        -- Warp's buffer: writes accumulate, a flush commits (serialises) and resets.
        write b = modifyIORef' buffer (<> b)
        flush = do
            acc <- readIORef buffer
            let n = BS.length (BSL.toStrict (toLazyByteString acc))
            writeIORef buffer mempty
            modifyIORef' committed (+ n)
    pump readChunk write flush
    readIORef committed

{- | A threshold-batched pump: write every chunk, but flush only once at least @threshold@
bytes have accumulated since the last flush (and once more for the tail). The same
constant-memory shape as 'pumpBody', with a coarser commit cadence.
-}
pumpThreshold :: Int -> IO ByteString -> (Builder -> IO ()) -> IO () -> IO ()
pumpThreshold threshold readChunk write flush = go 0
  where
    go pending = do
        chunk <- readChunk
        if BS.null chunk
            then when (pending > 0) flush
            else do
                write (byteString chunk)
                let pending' = pending + BS.length chunk
                if pending' >= threshold
                    then flush >> go 0
                    else go pending'

{- | A flush-once pump: write every chunk, flush a single time at end of body. The coarsest
cadence -- one commit for the whole stream.
-}
pumpEndOnly :: IO ByteString -> (Builder -> IO ()) -> IO () -> IO ()
pumpEndOnly readChunk write flush = go
  where
    go = do
        chunk <- readChunk
        if BS.null chunk
            then flush
            else write (byteString chunk) >> go

{- | Split a synthetic body of @total@ bytes into equal @chunkSize@ chunks (a final short
chunk carries any remainder), the shape an upstream 'BodyReader' yields. Built once, before
the measured window.
-}
bodyChunks :: Int -> Int -> [ByteString]
bodyChunks total chunkSize =
    replicate fullCount fullChunk <> tailChunk
  where
    fullChunk = BS.replicate chunkSize 0x61
    (fullCount, remainder) = total `divMod` chunkSize
    tailChunk = [BS.replicate remainder 0x61 | remainder > 0]
