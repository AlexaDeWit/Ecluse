-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure registered request batches, the actions they match, and adapter-specific filename scaling.
The benchmark consumes route examples without knowing an ecosystem's grammar.
-}
module Ecluse.Core.RouteBench (benchmarks) where

import Ecluse.Bench.Fit (notWorseThanLinear)
import Ecluse.Test.EcosystemBench (EcosystemBench (..), RouteCase (..), RouteScaling (..))
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

-- | Classify each request batch, build each action batch's actions, and check the supplied scaling families.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup "route.match" $
        [ bench (rcName example) (whnf classifyBatch (rcRequests example))
        | example <- ebRoutes ecosystem
        ]
            <> [ bench (rcName example) (whnf actionBatch (rcRequests example))
               | example <- ebActionRoutes ecosystem
               ]
            <> [ notWorseThanLinear (rsName family) (64, 8192) (rsRequest family) (ebClassify ecosystem)
               | family <- ebRouteScaling ecosystem
               ]
  where
    classifyBatch = foldl' (\total request -> total + ebClassify ecosystem request) 0
    actionBatch = foldl' (\total request -> maybe total (\action -> action `seq` total + 1) (ebAction ecosystem request)) (0 :: Int)
