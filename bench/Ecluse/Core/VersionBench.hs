-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Work-per-request benches for the version grammar ("Ecluse.Core.Version"): parsing
a raw version string into its canonical ordering key, ordering versions by the
semantic comparator, and resolving @dist-tags.latest@ over a candidate set.

The inputs are the real version strings of each corpus package, so the parse, order,
and latest-selection cost is reported across the real distribution of version-set
sizes (a few versions for @is-odd@, thousands for @\@types\/node@) the proxy parses
and orders on a metadata request.
-}
module Ecluse.Core.VersionBench (
    benchmarks,
) where

import Data.List qualified as List (sortBy)
import Data.Text qualified as T
import Ecluse.Bench.Corpus (LoadedEntry, entryName, versionKeysOf)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Version (
    Version,
    compareVersions,
    mkVersion,
    parseVersionKey,
    selectLatest,
    unVersion,
 )
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

-- | The parse, order, and latest-selection benches over each corpus package's versions.
benchmarks :: [LoadedEntry] -> Benchmark
benchmarks loaded =
    bgroup
        "version (parse + order + latest, per package)"
        [ bench (entryName le) (whnf versionPipelineDepth (versionKeysOf value))
        | le@(_, _, value) <- loaded
        ]

{- | The version read pipeline over a packument's raw version keys: parse every key
into its ordering key, order the parsed versions by the semantic comparator, and
resolve @latest@ -- summed to a forced 'Int' so the whole pipeline is evaluated.
-}
versionPipelineDepth :: [Text] -> Int
versionPipelineDepth raws =
    parsed + ordered + latest
  where
    versions = map (mkVersion Npm) raws
    parsed = length (filter isRight (map (parseVersionKey Npm) raws))
    ordered = length (List.sortBy semanticCompare versions)
    latest = maybe 0 (T.length . unVersion) (selectLatest Nothing versions)

{- | The semantic comparator, treating an unorderable pair as equal (the comparator
is total over the parsed npm versions here; this only keeps 'List.sortBy' total).
-}
semanticCompare :: Version -> Version -> Ordering
semanticCompare a b = fromMaybe EQ (compareVersions a b)
