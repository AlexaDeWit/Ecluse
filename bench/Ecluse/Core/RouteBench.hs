-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure registered request batches and adapter-specific filename scaling.
The benchmark consumes route examples without knowing an ecosystem's grammar.
-}
module Ecluse.Core.RouteBench (benchmarks) where

import Ecluse.Bench.Fit (notWorseThanLinear)
import Ecluse.Test.EcosystemBench (EcosystemBench (..), RouteCase (..), RouteScaling (..))
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

-- | Classify each request batch and check the supplied scaling families.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup "route.match" $
        [ bench (rcName example) (whnf classifyBatch (rcRequests example))
        | example <- ebRoutes ecosystem
        ]
            <> [ notWorseThanLinear (rsName family) (64, 8192) (rsRequest family) (ebClassify ecosystem)
               | family <- ebRouteScaling ecosystem
               ]
  where
    classifyBatch = foldl' (\total request -> total + ebClassify ecosystem request) 0
