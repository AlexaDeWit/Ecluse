-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Work-per-request benches for the npm metadata read path: decoding a packument
through the live wire decoder ("Ecluse.Core.Registry.Npm.Project") and projecting it
into the agnostic 'PackageInfo' through the live serve projection
'Ecluse.Core.Registry.Npm.Metadata.projectNpmManifest' (decode, nesting-bound,
project-and-validate, version-count-bound). That is the sequence the serve path runs per
request.

These run over the curated real-world corpus, from small @is-odd@ to heavy
@\@types\/node@. They therefore report the decode and projection cost across the real
distribution of package sizes and shapes rather than one anchor. The heterogeneous
per-version manifests are where a decode regression on a heavy packument shows. Each
result summarises to a forced 'Int' spanning every version, so the bench evaluates the
whole decoded\/projected structure rather than its outermost constructor alone.
-}
module Ecluse.Core.WireBench (
    benchmarks,
) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (CorpusEntry (cePackage), LoadedEntry, entryName)
import Ecluse.Core.Package (PackageInfo, PackageName, artHashes, infoVersions, pkgArtifacts)
import Ecluse.Core.Registry (RegistryResponse (RegistryResponse))
import Ecluse.Core.Registry.Metadata (MetadataError)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Core.Registry.Npm.Project (parseVersionList)
import Ecluse.Core.Security (defaultLimits)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

-- | The decode and projection benches over each corpus entry.
benchmarks :: [LoadedEntry] -> Benchmark
benchmarks loaded =
    bgroup
        "wire+project (per package)"
        [ bgroup
            (entryName le)
            [ bench "decode" (whnf decodeDepth raw)
            , bench "decode+project" (whnf projectDepth (raw, cePackage ce))
            ]
        | le@(ce, raw, _) <- loaded
        ]

{- | Decode bytes through 'parseVersionList', forcing every version. That forces the
per-version manifest decode, the read path's GC-dominant cost.
-}
decodeDepth :: ByteString -> Int
decodeDepth raw = either (const (-1)) length (parseVersionList (RegistryResponse raw))

-- | Decode and project to 'PackageInfo' in one pass, forcing every version.
projectDepth :: (ByteString, PackageName) -> Int
projectDepth (raw, name) = infoDepthE (projectNpmManifest defaultLimits name raw)

infoDepthE :: Either MetadataError (PackageInfo, Value) -> Int
infoDepthE = either (const (-1)) (infoDepth . fst)

{- | Force every projected version by folding a deep field (the artifact
digests) across the version map.
-}
infoDepth :: PackageInfo -> Int
infoDepth info = Map.foldr (\pd acc -> length (artHashes (NE.head (pkgArtifacts pd))) + acc) 0 (infoVersions info)
