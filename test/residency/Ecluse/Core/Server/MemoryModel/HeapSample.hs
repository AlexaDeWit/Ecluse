-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Scalar heap counters after finalisers receive one scheduling opportunity between major collections.
module Ecluse.Core.Server.MemoryModel.HeapSample (HeapSample (..), sample) where

import Control.Concurrent (yield)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (allocated_bytes, gc, max_live_bytes), getRTSStats)
import System.Mem (performMajorGC)
import UnliftIO.Exception (evaluate)

-- | Scalar counters avoid retaining the RTS statistics graph across collections.
data HeapSample = HeapSample
    { live :: !Word64
    , sampleAllocated :: !Word64
    , samplePeakLive :: !Word64
    }

-- | Drop full RTSStats before another collection. Yield so closed handles can finish finalisation.
{-# NOINLINE sample #-}
sample :: IO HeapSample
sample = do
    performMajorGC
    yield
    performMajorGC
    stats <- getRTSStats
    evaluate (HeapSample (gcdetails_live_bytes (gc stats)) (allocated_bytes stats) (max_live_bytes stats))
