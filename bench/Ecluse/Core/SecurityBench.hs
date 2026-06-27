{- | Work-per-request benches for the response-bound guards ("Ecluse.Core.Security"):
the bounded body read that caps an upstream response, the JSON nesting-depth guard,
and the version-count guard — the cheap checks that protect the proxy from a hostile
or oversized upstream document.

The bounded read runs over a multi-megabyte body; the structural guards run over the
realistic @express@ document and over a synthetic packument scaled toward @100k@
versions, the size at which a guard that was accidentally super-linear would bite.
-}
module Ecluse.Core.SecurityBench (
    benchmarks,
) where

import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Ecluse.Bench.Corpus (
    expressPackageName,
    loadExpress,
    projectInfo,
    syntheticPackageInfo,
    syntheticPackumentValue,
 )
import Ecluse.Core.Package (PackageInfo)
import Ecluse.Core.Security (
    LimitError,
    boundedRead,
    checkNestingDepth,
    checkVersionCount,
    defaultLimits,
 )
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, whnf, whnfIO)

-- | The bounded-read and structural-guard benches.
benchmarks :: Benchmark
benchmarks =
    env loadExpress $ \ ~(_, json) ->
        bgroup
            "security guards"
            [ env (pure bodyChunks) $ \chunks ->
                bench "boundedRead (8 MiB body, 64 KiB chunks)" (whnfIO (boundedReadDepth chunks))
            , bench "checkNestingDepth (express)" (whnf nestingDepth json)
            , bench "checkNestingDepth (synthetic / 100000)" (whnf nestingDepth (syntheticPackumentValue 100000))
            , bench "checkVersionCount (express)" (whnf versionCountDepth (projectInfo expressPackageName json))
            , bench "checkVersionCount (synthetic / 2000)" (whnf versionCountDepth (syntheticPackageInfo 2000))
            ]

{- | Drain a chunked body through 'boundedRead', forcing the assembled length (or an
error code). A fresh cursor is built per run so each measured iteration reads the
whole body from the start.
-}
boundedReadDepth :: [ByteString] -> IO Int
boundedReadDepth chunks = do
    cursor <- newIORef chunks
    result <- boundedRead defaultLimits (popChunk cursor)
    pure $! either limitErrorCode BS.length result
  where
    popChunk cursor = atomicModifyIORef' cursor $ \case
        [] -> ([], BS.empty)
        (c : cs) -> (cs, c)

{- | An 8 MiB body presented as 64 KiB chunks. The chunk bytes are shared, so the
input is compact while 'boundedRead' still accumulates the full eight megabytes.
-}
bodyChunks :: [ByteString]
bodyChunks = replicate 128 (BS.replicate 65536 0x61)

-- | Run the nesting-depth guard, forcing its decision (which traverses the value).
nestingDepth :: Value -> Int
nestingDepth value = either limitErrorCode (const 1) (checkNestingDepth defaultLimits value)

-- | Run the version-count guard, forcing its decision.
versionCountDepth :: PackageInfo -> Int
versionCountDepth info = either limitErrorCode (const 1) (checkVersionCount defaultLimits info)

-- | A sentinel for the (not-expected) limit-exceeded branch, so the result is a forced 'Int'.
limitErrorCode :: LimitError -> Int
limitErrorCode _ = -1
