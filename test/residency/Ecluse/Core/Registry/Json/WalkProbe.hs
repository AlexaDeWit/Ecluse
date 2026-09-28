-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Live bytes a token walk holds while it reads a body, sampled between the body's chunks.
module Ecluse.Core.Registry.Json.WalkProbe (heldDuring, allowance) where

import Control.Concurrent (yield)
import Data.ByteString qualified as BS
import Data.JsonStream.TokenParser (TokenResult)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (gc), getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC)
import UnliftIO.Exception (evaluate)

import Ecluse.Core.Registry.Json.Walk (Step, readJsonWalk)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit))

{- | The most live bytes at any 32 KiB chunk boundary while the walk reads the body, above the live
bytes before it started, which include the body itself.
-}
heldDuring :: (TokenResult -> Step s) -> ByteString -> IO Integer
heldDuring walk body = do
    enabled <- getRTSStatsEnabled
    unless enabled (fail "walk residency requires RTS -T")
    size <- evaluate (BS.length body)
    before <- liveBytes
    remaining <- newIORef (pieces body)
    peak <- newIORef before
    let next = do
            live <- liveBytes
            modifyIORef' peak (max live)
            atomicModifyIORef' remaining $ \case
                [] -> ([], BS.empty)
                piece : rest -> (rest, piece)
    _ <- readJsonWalk (MetadataBodyLimit size) walk next >>= evaluate
    highest <- readIORef peak
    pure (toInteger highest - toInteger before)

-- | The most a walk's held bytes may rise when its input repeats eight times more of what it drops.
allowance :: Integer
allowance = 256 * 1024

pieces :: ByteString -> [ByteString]
pieces body
    | BS.null body = []
    | otherwise = BS.take 32768 body : pieces (BS.drop 32768 body)

liveBytes :: IO Word64
liveBytes = do
    performMajorGC
    yield
    performMajorGC
    gcdetails_live_bytes . gc <$> getRTSStats
