-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Live bytes a token walk holds while it reads a body, sampled between the body's chunks.
module Ecluse.Core.Registry.Json.WalkProbe (heldDuring, heldWritingDuring, allowance) where

import Control.Monad.ST (RealWorld, ST, stToIO)
import Data.ByteString qualified as BS
import Data.JsonStream.Lexer (Cursor)
import GHC.Stats (getRTSStatsEnabled)
import UnliftIO.Exception (evaluate)

import Ecluse.Core.Registry.Json.Walk (Step, Steps, readJsonWalk, readJsonWalkST)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit))
import Ecluse.Core.Server.MemoryModel.HeapSample qualified as Heap

-- | Peak live growth at 32 KiB chunk boundaries above the baseline, which includes the input body.
heldDuring :: (Cursor -> Step s) -> ByteString -> IO Integer
heldDuring walk = heldReading (`readJsonWalk` walk)

-- | 'heldDuring' for a walk that writes as it reads, from a setup that makes its writer.
heldWritingDuring :: ST RealWorld (Cursor -> ST RealWorld (Steps (ST RealWorld) s)) -> ByteString -> IO Integer
heldWritingDuring setup = heldReading $ \bound next -> stToIO setup >>= \walk -> readJsonWalkST stToIO bound walk next

heldReading :: (BodyLimit -> IO ByteString -> IO result) -> ByteString -> IO Integer
heldReading readBody body = do
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
    _ <- readBody (MetadataBodyLimit size) next >>= evaluate
    highest <- readIORef peak
    pure (toInteger highest - toInteger before)

-- | The most a walk's held bytes may rise when its input repeats eight times more of what it drops.
allowance :: Integer
allowance = 256 * 1024

pieces :: ByteString -> [ByteString]
pieces body
    | BS.null body = []
    | otherwise = BS.take fixtureChunkBytes body : pieces (BS.drop fixtureChunkBytes body)

liveBytes :: IO Word64
liveBytes = Heap.live <$> Heap.sample

fixtureChunkBytes :: Int
fixtureChunkBytes = 32768
