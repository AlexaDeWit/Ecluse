-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The source digest of a production full read, alone. Each capture passes through
'digestingRead' in pieces of 32 KiB, the size an inflated registry response arrives in.
-}
module Ecluse.Core.DigestBench (benchmarks) where

import Data.ByteString qualified as BS
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfAppIO)
import Test.Tasty.HUnit ((@?=))

import Ecluse.Bench.Corpus (LoadedEntry, entryName)
import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Registry.Exchange (digestingRead)
import Ecluse.Core.Snapshot (ContentDigest)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Registry.JsonStream (heldChunks)
import Ecluse.Test.Registry.Metadata.Fetch (captureChunks)
import Ecluse.Test.Snapshot (digestOf)

-- | One group per ecosystem, one row per capture.
benchmarks :: [EcosystemBench] -> IO [Benchmark]
benchmarks = traverse ecosystemGroup

ecosystemGroup :: EcosystemBench -> IO Benchmark
ecosystemGroup ecosystem = do
    rows <- traverse captureRow (ebCorpus ecosystem)
    pure (bgroup ("ecosystem: " <> toString (ecosystemName (ebEcosystem ecosystem))) [bgroup "source digest (32 KiB chunks)" rows])

captureRow :: LoadedEntry -> IO Benchmark
captureRow entry@(_, raw, _, _) = do
    digest <- digested chunks
    digest @?= digestOf raw
    pure (bench (entryName entry) (whnfAppIO digested chunks))
  where
    chunks = captureChunks raw

digested :: [ByteString] -> IO ContentDigest
digested chunks = do
    next <- heldChunks chunks
    digestingRead drain next >>= either absurd (pure . snd)

drain :: IO ByteString -> IO (Either Void ())
drain next = next >>= \chunk -> if BS.null chunk then pure (Right ()) else drain next
