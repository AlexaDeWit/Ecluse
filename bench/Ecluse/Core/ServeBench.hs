-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure filtering, merging, assembly, and serialisation of prepared metadata.
Decoding and fetch-digest construction stay outside the measured operation.
-}
module Ecluse.Core.ServeBench (benchmarks) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Ecluse.Bench.Corpus (benchEvalContext, entryName, syntheticInput)
import Ecluse.Bench.Fit (notWorseThanLinearIO)
import Ecluse.Core.Package (infoVersions)
import Ecluse.Core.Package.Filter (restrictToSurvivors)
import Ecluse.Core.Package.Merge (Provenance (GatedSource), mergePackuments)
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.Npm.Filter (assembleMergedPackument)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Test.Corpus (syntheticProxyBase)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Server.Transform (serveDocumentSize)
import Ecluse.Test.Snapshot (digestOf)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf, whnfAppIO)

-- | Measure real captures and the growth across synthetic release counts.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup "serve (filter + merge-assemble)" $
        [ bench (entryName entry) (whnfAppIO serveDepth (Snapshot (digestOf bytes) document, info))
        | entry@(_, bytes, info, document) <- ebCorpus ecosystem
        ]
            <> [ notWorseThanLinearIO
                    "scales linearly in version count"
                    (32, 4096)
                    (syntheticInput ecosystem . fromIntegral)
                    (either (const (pure (-1))) serveDepth)
               ]
            <> [npmAssemblyBenchmarks ecosystem]
  where
    serveDepth = serveDocumentSize (ebMetadata ecosystem) benchEvalContext

npmAssemblyBenchmarks :: EcosystemBench -> Benchmark
npmAssemblyBenchmarks ecosystem =
    bgroup
        "prepared npm assembly"
        [ bgroup
            (entryName entry)
            [ bench label (nf (assembleMergedPackument syntheticProxyBase (Map.singleton 0 source) plan) raw)
            | (label, survivors) <- [("all", Map.keysSet (infoVersions info)), ("one", Set.fromList (take 1 (Map.keys (infoVersions info))))]
            , Just plan <- [mergePackuments [(GatedSource, restrictToSurvivors survivors info <$ source)]]
            ]
        | entry@(_, bytes, info, document) <- ebCorpus ecosystem
        , Just raw <- [snd npmCached document]
        , let source = Snapshot (digestOf bytes) raw
        ]
