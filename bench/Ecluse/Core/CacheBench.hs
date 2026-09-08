-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

{- | Cache maintenance measurements without registry transport or parsing.
Cold fills create a store per sample. Churn and hits use prefilled stores.
-}
module Ecluse.Core.CacheBench (benchmarks) where

import Ecluse.Core.Server.Cache.Store (SingleFlight, newSingleFlight, resolveSingleFlight)
import Test.Tasty (localOption, mkTimeout)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfAppIO, whnfIO)

-- | Compare whole-store fill and churn with 100,000 hot hits at three capacities.
benchmarks :: IO Benchmark
benchmarks = bgroup "cache maintenance" <$> traverse capacityBench [256, 1024, 4096]

capacityBench :: Int -> IO Benchmark
capacityBench capacity = do
    churnStore <- filledStore capacity
    hotStore <- filledStore capacity
    nextRange <- newIORef capacity
    pure $
        localOption (mkTimeout 5_000_000) $
            bgroup
                (show capacity)
                [ bench "cold fill" (whnfAppIO filledStore capacity)
                , bench "full-store churn" (whnfIO (churn churnStore nextRange capacity))
                , bench "100000 hot hits" (whnfIO (replicateM_ 100000 (resolveKey hotStore 0)))
                ]

filledStore :: Int -> IO (SingleFlight () Int Int)
filledStore capacity = do
    store <- newSingleFlight 86400 capacity capacity (const 1)
    traverse_ (resolveKey store) [0 .. capacity - 1]
    pure store

churn :: SingleFlight () Int Int -> IORef Int -> Int -> IO ()
churn store nextRange capacity = do
    firstKey <- atomicModifyIORef' nextRange (\start -> (capacity - start, start))
    traverse_ (resolveKey store) [firstKey .. firstKey + capacity - 1]

resolveKey :: SingleFlight () Int Int -> Int -> IO ()
resolveKey store key =
    void (resolveSingleFlight (pure ()) (const pass) (const pass) store key (pure (Right key)))
