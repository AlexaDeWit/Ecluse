-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Work-per-request benches for the npm serve path: the transform a metadata response
goes through before the proxy serves it ("Ecluse.Test.Server.Transform").

The served validator derives from the inputs, so no hash over the output body is measured.
The realistic benches run over each corpus package and report the cost across the real
distribution of sizes and shapes. A synthetic bench scales the version count and asserts
the transform stays linear, guarding the accidentally quadratic class.
-}
module Ecluse.Core.ServeBench (
    benchmarks,
) where

import Data.Aeson (Value)
import Ecluse.Bench.Corpus (
    LoadedEntry,
    benchEvalContext,
    benchPackageName,
    entryInfo,
    entryName,
    projectInfo,
    syntheticPackumentValue,
 )
import Ecluse.Bench.Fit (notWorseThanLinearIO)
import Ecluse.Core.Package (PackageInfo)
import Ecluse.Test.Server.Transform (serveTransformSize)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfAppIO)

-- | The serve-transform benches: realistic over the corpus, scaled over synthetic versions.
benchmarks :: [LoadedEntry] -> Benchmark
benchmarks loaded =
    bgroup "serve (filter + merge-assemble)" $
        [ bench (entryName le) (whnfAppIO serveDepth (value, entryInfo le))
        | le@(_, _, value) <- loaded
        ]
            <> [ -- A smaller upper bound than the other scaled benches: the serve op is the
                 -- heaviest of the scaled ops, so each measured size costs more. The 128x range
                 -- still fits the curve and flags a super-linear regression.
                 notWorseThanLinearIO
                    "scales linearly in version count"
                    (32, 4096)
                    syntheticServeInput
                    serveDepth
               ]

serveDepth :: (Value, PackageInfo) -> IO Int
serveDepth = serveTransformSize benchEvalContext

-- | A synthetic packument of the given version count, paired with its projection.
syntheticServeInput :: Word -> (Value, PackageInfo)
syntheticServeInput n =
    let value = syntheticPackumentValue (fromIntegral n)
     in (value, projectInfo benchPackageName value)
