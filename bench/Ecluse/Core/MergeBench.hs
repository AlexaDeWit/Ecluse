{- | Work-per-request benches for packument merging ("Ecluse.Core.Package.Merge"): the
union of a trusted and a gated upstream's versions into one document, with the
shared-algorithm integrity divergence check.

A realistic micro-bench merges two copies of the @express@ packument; a synthetic
bench scales the version count and asserts the merge stays linear, the guard against
an accidentally-quadratic union (issues #373\/#374\/#299).
-}
module Ecluse.Core.MergeBench (
    benchmarks,
) where

import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (
    expressPackageName,
    loadExpress,
    projectInfo,
    syntheticPackageInfo,
 )
import Ecluse.Bench.Fit (notWorseThanLinear)
import Ecluse.Core.Package (PackageInfo)
import Ecluse.Core.Package.Merge (
    MergePlan (mpSurvivors),
    Provenance (GatedSource, TrustedSource),
    mergePackuments,
 )
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnf)

-- | The merge benches: realistic over @express@, scaled over synthetic versions.
benchmarks :: Benchmark
benchmarks =
    env loadExpress $ \ ~(_, json) ->
        bgroup
            "package.mergePackuments"
            [ bench "express × 2 (realistic)" (whnf mergeDepth (projectInfo expressPackageName json))
            , bench "synthetic / 2000 versions" (whnf mergeDepth (syntheticPackageInfo 2000))
            , notWorseThanLinear
                "scales linearly in version count"
                (64, 8192)
                (syntheticPackageInfo . fromIntegral)
                mergeDepth
            ]

{- | Merge a packument with a second (gated) copy of itself and force the resolved
plan by counting its survivors. Two overlapping sources is the collision-heavy case
the divergence check actually works on.
-}
mergeDepth :: PackageInfo -> Int
mergeDepth info =
    maybe 0 (Map.size . mpSurvivors) (mergePackuments [(TrustedSource, info), (GatedSource, info)])
