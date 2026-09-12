-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure parsing, ordering, and latest selection over projected release keys.
The ecosystem tag selects the production version grammar.
-}
module Ecluse.Core.VersionBench (benchmarks) where

import Data.List qualified as List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Bench.Corpus (entryName)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (infoVersions)
import Ecluse.Core.Version (Version, compareVersions, mkVersion, parseVersionKey, renderVersion, selectLatest)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

-- | Exercise the version pipeline for every projected corpus package.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup
        "version (parse + order + latest, per package)"
        [ bench (entryName entry) (whnf (versionPipelineDepth (ebEcosystem ecosystem)) (Map.keys (infoVersions info)))
        | entry@(_, _, info, _) <- ebCorpus ecosystem
        ]

versionPipelineDepth :: Ecosystem -> [Text] -> Int
versionPipelineDepth ecosystem raws =
    parsed + ordered + latest
  where
    versions = map (mkVersion ecosystem) raws
    parsed = length (filter isRight (map (parseVersionKey ecosystem) raws))
    ordered = length (List.sortBy semanticCompare versions)
    latest = maybe 0 (T.length . renderVersion) (selectLatest Nothing versions)

semanticCompare :: Version -> Version -> Ordering
semanticCompare a b = fromMaybe EQ (compareVersions a b)
