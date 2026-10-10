-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure each adapter's wire decoding, and its full read of the same capture through the
production read driver. The read takes the capture as the chunks a body arrives in.
-}
module Ecluse.Core.WireBench (benchmarks) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (entryName)
import Ecluse.Core.Package (PackageInfo, artHashes, infoVersions, pkgArtifacts)
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo))
import Ecluse.Test.Corpus (cpPackage)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Registry.Metadata.Fetch (captureChunks)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf, whnfAppIO)

-- | Decode and project each captured document through its registered adapter.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup
        "wire+project (per package)"
        [ bgroup
            (entryName entry)
            [ bench "version identifiers" (whnf (either (const (-1)) length . ebDecode ecosystem (cpPackage package)) raw)
            , bench "full metadata projection" (whnfAppIO (fmap (either (const (-1)) (infoDepth . manifestInfo)) . ebRead ecosystem (cpPackage package)) (captureChunks raw))
            ]
        | entry@(package, raw, _, _) <- ebCorpus ecosystem
        ]

infoDepth :: PackageInfo -> Int
infoDepth info = Map.foldr (\details total -> sum (map (length . artHashes) (NE.toList (pkgArtifacts details))) + total) 0 (infoVersions info)
