-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure registered request batches, the actions they match, adapter-specific filename scaling,
and the upstream requests a serve builds. The benchmark knows no ecosystem's grammar.
-}
module Ecluse.Core.RouteBench (benchmarks) where

import Data.ByteString qualified as BS
import Network.HTTP.Client (Request (host, path, queryString, requestHeaders))

import Ecluse.Bench.Fit (notWorseThanLinear)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (UrlFormationError)
import Ecluse.Test.Corpus (cpPackage)
import Ecluse.Test.EcosystemBench (EcosystemBench (..), LoadedEntry (lePackage), RequestBuild (..), RouteCase (..), RouteScaling (..))
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

{- | Classify each request batch, build each action batch's actions, check the supplied scaling
families, and build each upstream request for a batch of corpus names.
-}
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
            <> [ bench ("request building " <> rbName build) (whnf (requestBatch (rbBuild build)) names)
               | build <- ebRequestBuilds ecosystem
               ]
  where
    classifyBatch = foldl' (\total request -> total + ebClassify ecosystem request) 0
    actionBatch = foldl' (\total request -> maybe total (\action -> action `seq` total + 1) (ebAction ecosystem request)) (0 :: Int)
    names = concat (replicate 100 (map (cpPackage . lePackage) (ebCorpus ecosystem)))

requestBatch :: (PackageName -> Either UrlFormationError Request) -> [PackageName] -> Int
requestBatch build = foldl' (\total name -> either (const total) ((total +) . renderedBytes) (build name)) 0

-- The bytes a send renders, so each build is forced as far as sending it forces it.
renderedBytes :: Request -> Int
renderedBytes request =
    BS.length (host request) + BS.length (path request) + BS.length (queryString request) + sum (map (BS.length . snd) (requestHeaders request))
