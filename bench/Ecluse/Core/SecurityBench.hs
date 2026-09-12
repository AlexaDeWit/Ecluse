-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Measure response bounds over each ecosystem's wire documents and projected releases.
Parsed documents enter the nesting guard without timing their decoding.
-}
module Ecluse.Core.SecurityBench (benchmarks) where

import Data.ByteString qualified as BS
import Ecluse.Bench.Corpus (entryInfo, entryName, syntheticPackageInfo)
import Ecluse.Core.Package (PackageInfo)
import Ecluse.Core.Security (LimitError, boundedRead, checkVersionCount, defaultLimits)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnf, whnfIO)

-- | Exercise bounded reads and both structural guards on real and synthetic inputs.
benchmarks :: EcosystemBench -> Benchmark
benchmarks ecosystem =
    bgroup "security guards" $
        [env (pure bodyChunks) $ \chunks -> bench "boundedRead (8 MiB body, 64 KiB chunks)" (whnfIO (boundedReadDepth chunks))]
            <> [ bgroup
                    (entryName entry)
                    [ bench "checkNestingDepth" (whnf (ebNestingDepth ecosystem) document)
                    , bench "checkVersionCount" (whnf versionCountDepth (entryInfo entry))
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

boundedReadDepth :: [ByteString] -> IO Int
boundedReadDepth chunks = do
    cursor <- newIORef chunks
    result <- boundedRead defaultLimits (popChunk cursor)
    pure $! either limitErrorCode BS.length result
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
