{- | Work-per-request benches for the version grammar ("Ecluse.Core.Version"): parsing
a raw version string into its canonical ordering key, ordering versions by the
semantic comparator, and resolving @dist-tags.latest@ over a candidate set.

The inputs are the real @express@ packument's version strings (hundreds of npm
semver values), the set the proxy parses and orders on a metadata request.
-}
module Ecluse.Core.VersionBench (
    benchmarks,
) where

import Data.List qualified as List (sortBy)
import Data.Text qualified as T
import Ecluse.Bench.Corpus (loadExpress, versionKeysOf)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Version (
    Version,
    compareVersions,
    mkVersion,
    parseVersionKey,
    selectLatest,
    unVersion,
 )
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnf)

-- | The parse, compare\/order, and latest-selection benches over the @express@ versions.
benchmarks :: Benchmark
benchmarks =
    env loadExpress $ \ ~(_, json) ->
        let rawVersions = versionKeysOf json
            versions = map (mkVersion Npm) rawVersions
         in bgroup
                "version (express versions)"
                [ bench "parseVersionKey" (whnf parseCount rawVersions)
                , bench "order by compareVersions" (whnf orderDepth versions)
                , bench "selectLatest" (whnf latestDepth versions)
                ]

-- | Parse every raw version string, counting the ones that yield a key.
parseCount :: [Text] -> Int
parseCount = length . filter isRight . map (parseVersionKey Npm)

-- | Order the versions by the semantic comparator, forcing the full sort.
orderDepth :: [Version] -> Int
orderDepth = length . List.sortBy semanticCompare

{- | The semantic comparator, treating an unorderable pair as equal (the comparator
is total over the parsed npm versions here; this only keeps 'List.sortBy' total).
-}
semanticCompare :: Version -> Version -> Ordering
semanticCompare a b = fromMaybe EQ (compareVersions a b)

-- | Resolve @latest@ over the candidate set, forcing the comparisons it makes.
latestDepth :: [Version] -> Int
latestDepth versions = maybe 0 (T.length . unVersion) (selectLatest Nothing versions)
