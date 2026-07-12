-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Work-per-request benches for the npm metadata read path: decoding a packument
off the wire ("Ecluse.Core.Registry.Npm.Wire") and projecting it into the agnostic
'PackageInfo' through the live serve projection
'Ecluse.Core.Registry.Npm.Metadata.projectNpmManifest' (decode, nesting-bound,
project-and-validate, version-count-bound) -- the sequence the serve path runs per
request.

These run over the curated real-world corpus (small @is-odd@ through heavy
@\@types\/node@), so the decode and projection cost is reported across the real
distribution of package sizes and shapes rather than one anchor -- the heterogeneous
per-version manifests are where a decode regression on a heavy packument shows up.
Each result is summarised to a forced 'Int' that touches a deep field of every
version, so the whole decoded\/projected structure is evaluated, not just its
outermost constructor.
-}
module Ecluse.Core.WireBench (
    benchmarks,
) where

import Data.Aeson (Value, eitherDecodeStrict)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Bench.Corpus (CorpusEntry (cePackage), LoadedEntry, entryName)
import Ecluse.Core.Package (PackageInfo, PackageName, artHashes, infoVersions, pkgArtifacts)
import Ecluse.Core.Registry.Metadata (MetadataError)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Core.Registry.Npm.Wire (
    Dist (distIntegrity),
    Packument (pkmtVersions),
    VersionManifest (vmDist, vmVersion),
 )
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

-- | Decode bytes to a 'Packument', forcing every version manifest.
decodeDepth :: ByteString -> Int
decodeDepth raw = case eitherDecodeStrict raw :: Either String Packument of
    Left _ -> -1
    Right packument -> manifestDepth (pkmtVersions packument)

-- | Decode and project to 'PackageInfo' in one pass, forcing every version.
projectDepth :: (ByteString, PackageName) -> Int
projectDepth (raw, name) = infoDepthE (projectNpmManifest defaultLimits name raw)

{- | Force every manifest by folding a deep field (the integrity digest --
dependencies are no longer modelled) across all versions.
-}
manifestDepth :: Map.Map Text VersionManifest -> Int
manifestDepth = Map.foldr (\m acc -> T.length (vmVersion m) + maybe 0 T.length (distIntegrity (vmDist m)) + acc) 0

infoDepthE :: Either MetadataError (PackageInfo, Value) -> Int
infoDepthE = either (const (-1)) (infoDepth . fst)

{- | Force every projected version by folding a deep field (the artifact
digests) across the version map.
-}
infoDepth :: PackageInfo -> Int
infoDepth info = Map.foldr (\pd acc -> length (artHashes (NE.head (pkgArtifacts pd))) + acc) 0 (infoVersions info)
