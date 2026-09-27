-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Read this process's RTS counters with the runtime posture its boot applied.
module Ecluse.BenchLoad.RtsProbe (readRtsSnapshot) where

import GHC.Stats (GCDetails (gcdetails_live_bytes, gcdetails_mem_in_use_bytes), RTSStats (..), getRTSStats)

import Ecluse.BenchLoad.RtsWindow (RtsSnapshot (..))
import Ecluse.Rts (RtsPosture (rpAllocAreaBytes, rpCapabilities, rpMaxHeapBytes), currentRtsPosture)

-- | One snapshot. The process must run with @-T@, which the load tool bakes in.
readRtsSnapshot :: IO RtsSnapshot
readRtsSnapshot = do
    stats <- getRTSStats
    posture <- currentRtsPosture
    pure
        RtsSnapshot
            { rsAllocatedBytes = allocated_bytes stats
            , rsGcs = gcs stats
            , rsMajorGcs = major_gcs stats
            , rsGcCpuNs = gc_cpu_ns stats
            , rsCpuNs = cpu_ns stats
            , rsGcElapsedNs = gc_elapsed_ns stats
            , rsMaxLiveBytes = max_live_bytes stats
            , rsMaxMemInUseBytes = max_mem_in_use_bytes stats
            , rsLiveBytes = gcdetails_live_bytes (gc stats)
            , rsMemInUseBytes = gcdetails_mem_in_use_bytes (gc stats)
            , rsCapabilities = rpCapabilities posture
            , rsMaxHeapBytes = rpMaxHeapBytes posture
            , rsAllocAreaBytes = rpAllocAreaBytes posture
            }
