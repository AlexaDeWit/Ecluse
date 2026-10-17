-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The native metadata gzip path, enabled, in rows apart from the identity rows. Each capture's
production served body is gzipped at level 1 through 'gzipMetadata', as a held body (an assembled
hit) and after its render (a miss). A sizes row reports both lengths, the ratio, and the charge.
-}
module Ecluse.Core.MetadataGzipBench (benchmarks) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Numeric (showFFloat)
import Test.Tasty (TestTree)
import Test.Tasty.Bench (bench, bgroup, whnfAppIO)
import Test.Tasty.HUnit (assertFailure, testCaseInfo)

import Ecluse.Bench.Corpus (LoadedEntry (..), benchEvalContext, entryName)
import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Server.MetadataGzip (GzipBody (..), gzipLazyBody, gzipMetadata)
import Ecluse.Report.BenchLabels (ecosystemPrefix)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Server.Transform (serveDocumentBytes)

-- | One group per ecosystem, labelled with the enabled posture, one subgroup per capture.
benchmarks :: [EcosystemBench] -> IO [TestTree]
benchmarks = traverse ecosystemGroup

ecosystemGroup :: EcosystemBench -> IO TestTree
ecosystemGroup ecosystem = do
    rows <- traverse (captureRows ecosystem) (ebCorpus ecosystem)
    pure (bgroup (toString (ecosystemPrefix <> ecosystemName (ebEcosystem ecosystem))) [bgroup "metadata response (gzip enabled, level 1)" rows])

-- Setup inflates the output and refuses a capture whose bytes do not come back.
captureRows :: EcosystemBench -> LoadedEntry -> IO TestTree
captureRows ecosystem entry@LoadedEntry{leInfo = info, leSnapshot = source} = do
    body <- render (source, info)
    charged <- newIORef 0
    compressed <- gzipMetadata (\cost -> modifyIORef' charged (+ cost)) body
    unless (LBS.toStrict (GZip.decompress (gzipLazyBody compressed)) == body) (assertFailure (entryName entry <> ": gzip output does not inflate to the served body"))
    paid <- readIORef charged
    pure $
        bgroup
            (entryName entry)
            [ testCaseInfo "sizes" (pure (sizes (BS.length body) (gzipLength compressed) paid))
            , bench "held body, gzip" (whnfAppIO gzipped body)
            , bench "render, then gzip" (whnfAppIO (render >=> gzipped) (source, info))
            ]
  where
    render = serveDocumentBytes (ebMetadata ecosystem) benchEvalContext
    gzipped = fmap gzipLength . gzipMetadata (const pass)

sizes :: Int -> Int -> Int -> String
sizes plain gzip charged =
    "identity " <> show plain <> " B, gzip " <> show gzip <> " B, ratio " <> showFFloat (Just 2) ratio "" <> ", memory charge " <> show charged <> " B"
  where
    ratio = fromIntegral plain / fromIntegral (max 1 gzip) :: Double
