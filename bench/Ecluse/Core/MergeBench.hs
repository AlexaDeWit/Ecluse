-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Work-per-request benches for packument merging ("Ecluse.Core.Package.Merge"): the
union of a trusted and a gated upstream's versions into one document. The merge runs
the shared-algorithm integrity divergence check over that union.

The realistic benches merge two copies of each corpus package, the collision-heavy case
the divergence check works on. They report the merge cost across the real distribution of
version-set sizes. A synthetic bench scales the version count and asserts the merge stays
linear, the guard against an accidentally quadratic union. The synthetic generator serves
__only__ this complexity-scaling assertion.
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
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

-- | The merge benches: realistic over the corpus, scaled over synthetic versions.
benchmarks :: [LoadedEntry] -> Benchmark
benchmarks loaded =
    bgroup "package.mergePackuments" $
        [ bench (entryName le) (whnf mergeDepth (entryInfo le))
        | le <- loaded
        ]
            <> [ notWorseThanLinear
                    "scales linearly in version count"
                    (64, 8192)
                    (syntheticPackageInfo . fromIntegral)
                    mergeDepth
               ]

{- | Merge a packument with a second (gated) copy of itself and force the resolved plan
by counting its survivors. Two overlapping sources is the collision-heavy case the
divergence check works on.
-}
mergeDepth :: PackageInfo -> Int
mergeDepth info =
    maybe 0 (Map.size . mpSurvivors) (mergePackuments [(TrustedSource, info), (GatedSource, info)])
