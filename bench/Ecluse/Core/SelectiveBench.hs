-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Compare selection from the production full read with the production selected read, each over a
capture's chunks. Each corpus package targets its highest retained version key.
-}
module Ecluse.Core.SelectiveBench (benchmarks) where

import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (entryName)
import Ecluse.Core.Package (infoVersions)
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo))
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.Test.Corpus (cpPackage)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Registry.Metadata.Fetch (captureChunks)
import Ecluse.Test.Server.Transform (SelectedDepth (DecodeFailed), detailsDepth)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfAppIO)

-- | Compare both reads at one retained release of every corpus entry.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup
        "single-version metadata (per package)"
        [ bgroup
            (entryName entry)
            [ bench "full decode + select" (whnfAppIO fullSelect chunks)
            , bench "selective decode" (whnfAppIO (fmap (either (const DecodeFailed) detailsDepth) . ebSelective ecosystem name version) chunks)
            ]
        | entry@(package, raw, info, _) <- ebCorpus ecosystem
        , (key, _) <- maybeToList (Map.lookupMax (infoVersions info))
        , let name = cpPackage package
              version = mkVersion (ebEcosystem ecosystem) key
              chunks = captureChunks raw
              fullSelect held = either (const DecodeFailed) (detailsDepth . Map.lookup (renderVersion version) . infoVersions . manifestInfo) <$> ebRead ecosystem name held
        ]
