{- | Work-per-request benches for the npm metadata read path: decoding a packument
off the wire ("Ecluse.Core.Registry.Npm.Wire") and projecting it into the agnostic
'PackageInfo' ("Ecluse.Core.Registry.Npm.Project").

These run on the real, untrimmed @express@ packument (hundreds of versions), the
realistic large input the proxy must decode on a metadata request. Each result is
summarised to a forced 'Int' that touches a deep field of every version, so the whole
decoded\/projected structure is evaluated, not just its outermost constructor.
-}
module Ecluse.Core.WireBench (
    benchmarks,
) where

import Data.Aeson (Value, eitherDecodeStrict)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Bench.Corpus (expressPackageName, loadExpress)
import Ecluse.Core.Package (PackageInfo, infoVersions, pkgDependencies)
import Ecluse.Core.Registry (ParseError, RegistryResponse (RegistryResponse))
import Ecluse.Core.Registry.Npm.Project (
    Projection (Projected),
    parsePackageInfo,
    parsePackageInfoFromValue,
 )
import Ecluse.Core.Registry.Npm.Wire (
    Packument (pkmtVersions),
    VersionManifest (vmDependencies, vmVersion),
 )
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnf)

-- | The decode and projection benches over the @express@ corpus.
benchmarks :: Benchmark
benchmarks =
    env loadExpress $ \ ~(raw, json) ->
        bgroup
            "wire+project (express)"
            [ bench "decode" (whnf decodeDepth raw)
            , bench "decode+project" (whnf projectDepth raw)
            , bench "project-only (pre-decoded value)" (whnf projectValueDepth json)
            ]

-- | Decode bytes to a 'Packument', forcing every version manifest.
decodeDepth :: ByteString -> Int
decodeDepth raw = case eitherDecodeStrict raw :: Either String Packument of
    Left _ -> -1
    Right packument -> manifestDepth (pkmtVersions packument)

-- | Decode and project to 'PackageInfo' in one pass, forcing every version.
projectDepth :: ByteString -> Int
projectDepth raw = infoDepthE (parsePackageInfo expressPackageName (RegistryResponse raw))

-- | Project an already-decoded 'Value', isolating the projection from the decode.
projectValueDepth :: Value -> Int
projectValueDepth json = case parsePackageInfoFromValue expressPackageName json of
    Left _ -> -1
    Right (Projected info) -> infoDepth info
    Right _ -> -2

-- | Force every manifest by folding a deep field across all versions.
manifestDepth :: Map.Map Text VersionManifest -> Int
manifestDepth = Map.foldr (\m acc -> T.length (vmVersion m) + Map.size (vmDependencies m) + acc) 0

infoDepthE :: Either ParseError PackageInfo -> Int
infoDepthE = either (const (-1)) infoDepth

-- | Force every projected version by folding across the version map.
infoDepth :: PackageInfo -> Int
infoDepth info = Map.foldr (\pd acc -> length (pkgDependencies pd) + acc) 0 (infoVersions info)
