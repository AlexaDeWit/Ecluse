-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Benches for the artifact streaming pump ("Ecluse.Core.Server.Stream"): each flush strategy, and
the shipped pump under the relay's progress meter. Each row pumps an 8 MiB body into the Warp sink
model of "Ecluse.Core.StreamBench.Sink". The rows price the CPU side of a flush cadence, and the
load harness (@bench-load@) prices the socket sends. A CPU difference here does not justify a
cadence change on its own.
-}
module Ecluse.Core.StreamBench (
    benchmarks,
) where

import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnfAppIO)

import Ecluse.Core.StreamBench.Sink (bodyChunks, chunkSizes, newSinkBuffer, sentBytes, strategies)

-- | One row for each pump strategy at each upstream chunk size. The chunks and the sink buffer are made before timing.
benchmarks :: Benchmark
benchmarks =
    bgroup
        "stream (pump into Warp sink)"
        [ env (pure (bodyChunks chunkSize)) $ \chunks ->
            bgroup
                (show chunkSize <> "B chunks")
                [ env newSinkBuffer $ \buffer -> bench (toString name) (whnfAppIO (sentBytes buffer pump) chunks)
                | (name, pump) <- strategies
                ]
        | chunkSize <- chunkSizes
        ]
