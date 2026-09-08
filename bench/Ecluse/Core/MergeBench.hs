-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure merging overlapping source snapshots from the corpus and synthetic fixtures.
Snapshot construction stays outside the measured merge operation.
-}
module Ecluse.Core.MergeBench (
    benchmarks,
) where

import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (
    LoadedEntry,
    entryInfo,
    entryName,
    syntheticPackageInfo,
 )
import Ecluse.Bench.Fit (notWorseThanLinear)
import Ecluse.Core.Package (PackageInfo)
import Ecluse.Core.Package.Merge (
    MergePlan (mpSurvivors),
    Provenance (GatedSource, TrustedSource),
    mergePackuments,
 )
import Ecluse.Core.Snapshot (Snapshot (Snapshot), digestOf)
import Ecluse.Test.Snapshot (syntheticSnapshot)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

-- | The merge benches: realistic over the corpus, scaled over synthetic versions.
benchmarks :: [LoadedEntry] -> Benchmark
benchmarks loaded =
    bgroup "package.mergePackuments" $
        [ bench (entryName le) (whnf mergeDepth (Snapshot (digestOf bytes) (entryInfo le)))
        | le@(_, bytes, _) <- loaded
        ]
            <> [ notWorseThanLinear
                    "scales linearly in version count"
                    (64, 8192)
                    (syntheticSnapshot . syntheticPackageInfo . fromIntegral)
                    mergeDepth
               ]

mergeDepth :: Snapshot PackageInfo -> Int
mergeDepth info =
    maybe 0 (Map.size . mpSurvivors) (mergePackuments [(TrustedSource, info), (GatedSource, info)])
