-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure response bounds and the artifact location check over each ecosystem's wire documents and
projected releases. Parsed documents enter the nesting guard without timing their decoding.
-}
module Ecluse.Core.SecurityBench (benchmarks) where

import Ecluse.Test.Security.Limits (checkVersionCount)

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Ecluse.Bench.Corpus (entryInfo, entryName, forcedTree, syntheticPackageInfo, unHeldTree)
import Ecluse.Core.Package (PackageInfo, artUrl, infoVersions, pkgArtifacts)
import Ecluse.Core.Package.Filter.Internal (ArtifactOrigin, artifactOrigin, locateArtifact)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError, boundedRead, defaultLimits, maxMetadataBytes)
import Ecluse.Test.Corpus (CaptureUpstream (..))
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Test.Tasty (withResource)
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnf, whnfAppIO, whnfIO)

-- | Exercise bounded reads, both structural guards and the location check on real and synthetic inputs.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup "security guards" $
        [env (pure bodyChunks) $ \chunks -> bench "boundedRead (8 MiB body, 64 KiB chunks)" (whnfIO (boundedReadDepth chunks))]
            <> [ bgroup
                    (entryName entry)
                    [ withResource (forcedTree ecosystem document) (const pass) $ \held ->
                        bench "checkNestingDepth" (whnfAppIO (fmap (ebNestingDepth ecosystem . unHeldTree)) held)
                    , bench "checkVersionCount" (whnf versionCountDepth (entryInfo entry))
                    , bench "artifact locations" (whnf (locatedArtifacts origin) (entryInfo entry))
                    ]
               | entry@(_, _, _, document) <- ebCorpus ecosystem
               ]
            <> [ bench
                    "checkNestingDepth (synthetic / 100000)"
                    (whnf (either (const (-1)) (ebNestingDepth ecosystem)) (ebReadDocument ecosystem (ebSynthetic ecosystem 100000)))
               , bench
                    "checkVersionCount (synthetic / 2000)"
                    (whnf (either (const (-1)) versionCountDepth) (syntheticPackageInfo ecosystem 2000))
               ]
  where
    origin = artifactOrigin (upstreamAuthorities (ebUpstream ecosystem)) (upstreamOrigin (ebUpstream ecosystem))

boundedReadDepth :: [ByteString] -> IO Int
boundedReadDepth chunks = do
    cursor <- newIORef chunks
    result <- boundedRead (MetadataBodyLimit (maxMetadataBytes defaultLimits)) (popChunk cursor)
    pure $! either limitErrorCode (BS.length . snd) result
  where
    popChunk cursor = atomicModifyIORef' cursor $ \case
        [] -> ([], BS.empty)
        (c : cs) -> (cs, c)

bodyChunks :: [ByteString]
bodyChunks = replicate 128 (BS.replicate 65536 0x61)

versionCountDepth :: PackageInfo -> Int
versionCountDepth info = either limitErrorCode (const 1) (checkVersionCount defaultLimits info)

limitErrorCode :: LimitError -> Int
limitErrorCode _ = -1

-- The projected artifacts whose URL passes the check against the capture's registry, each result forced.
locatedArtifacts :: ArtifactOrigin -> PackageInfo -> Int
locatedArtifacts origin info =
    length [() | details <- Map.elems (infoVersions info), art <- toList (pkgArtifacts details), Right !_ <- [locateArtifact origin (artUrl art)]]
