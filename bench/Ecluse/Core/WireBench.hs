-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure each adapter's wire decoding and full metadata projection.
Both operations receive the original bytes on every iteration.
-}
module Ecluse.Core.WireBench (benchmarks) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (entryName)
import Ecluse.Core.Package (PackageInfo, artHashes, infoVersions, pkgArtifacts)
import Ecluse.Test.Corpus (cpPackage)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

-- | Decode and project each captured document through its registered adapter.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup
        "wire+project (per package)"
        [ bgroup
            (entryName entry)
            [ bench "decode" (whnf (either (const (-1)) length . ebDecode ecosystem (cpPackage package)) raw)
            , bench "decode+project" (whnf (either (const (-1)) (infoDepth . fst) . ebProject ecosystem (cpPackage package)) raw)
            ]
        | entry@(package, raw, _, _) <- ebCorpus ecosystem
        ]

infoDepth :: PackageInfo -> Int
infoDepth info = Map.foldr (\details total -> sum (map (length . artHashes) (NE.toList (pkgArtifacts details))) + total) 0 (infoVersions info)
