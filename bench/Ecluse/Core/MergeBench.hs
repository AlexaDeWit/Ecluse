-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure merge operations over each ecosystem's projected corpus.
Synthetic inputs check the operation's growth with release count.
-}
module Ecluse.Core.MergeBench (
    benchmarks,
) where

import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (
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
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Snapshot (syntheticSnapshot)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

-- | The merge benches: realistic over the corpus, scaled over synthetic versions.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup "package.mergePackuments" $
        [ bench (entryName le) (whnf mergeDepth (Snapshot (digestOf bytes) (entryInfo le)))
        | le@(_, bytes, _, _) <- ebCorpus ecosystem
        ]
            <> [ notWorseThanLinear
                    "scales linearly in version count"
                    (64, 8192)
                    (fmap syntheticSnapshot . syntheticPackageInfo ecosystem . fromIntegral)
                    (either (const (-1)) mergeDepth)
               ]

mergeDepth :: Snapshot PackageInfo -> Int
mergeDepth info =
    maybe 0 (Map.size . mpSurvivors) (mergePackuments [(TrustedSource, info), (GatedSource, info)])
