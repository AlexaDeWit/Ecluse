-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Compare full-document selection with the adapter's selective release decoder.
Each corpus package targets its highest retained version key.
-}
module Ecluse.Core.SelectiveBench (benchmarks) where

import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (entryName)
import Ecluse.Core.Package (infoVersions)
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.Test.Corpus (cpPackage)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Server.Transform (SelectedDepth (DecodeFailed), detailsDepth)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

-- | Compare both decoders at one retained release of every corpus entry.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup
        "single-version metadata (per package)"
        [ bgroup
            (entryName entry)
            [ bench "full decode + select" (whnf fullSelect raw)
            , bench "selective decode" (whnf (either (const DecodeFailed) detailsDepth . ebSelective ecosystem name version) raw)
            ]
        | entry@(package, raw, info, _) <- ebCorpus ecosystem
        , (key, _) <- maybeToList (Map.lookupMax (infoVersions info))
        , let name = cpPackage package
              version = mkVersion (ebEcosystem ecosystem) key
              fullSelect bytes = either (const DecodeFailed) (detailsDepth . Map.lookup (renderVersion version) . infoVersions . fst) (ebProject ecosystem name bytes)
        ]
