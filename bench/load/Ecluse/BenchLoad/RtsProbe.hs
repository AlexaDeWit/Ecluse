-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Read this process's RTS counters with the runtime posture its boot applied.
module Ecluse.BenchLoad.RtsProbe (snapshotAfter) where

import GHC.RTS.Flags (GCFlags (compact, compactThreshold), getGCFlags)
import GHC.Stats (GCDetails (gcdetails_live_bytes, gcdetails_mem_in_use_bytes), RTSStats (..), getRTSStats)
import System.Mem (performMajorGC, performMinorGC)

import Ecluse.BenchLoad.RtsWindow (Collection (MajorCollection, MinorCollection), RtsSnapshot (..))
import Ecluse.Rts (RtsPosture (rpAllocAreaBytes, rpCapabilities, rpMaxHeapBytes), currentRtsPosture)

-- | Collect, then read one snapshot. The process must run with @-T@, which the load tool bakes in.
snapshotAfter :: Collection -> IO RtsSnapshot
snapshotAfter collection = do
    case collection of
        MajorCollection -> performMajorGC
        MinorCollection -> performMinorGC
    stats <- getRTSStats
    posture <- currentRtsPosture
    flags <- getGCFlags
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
            , rsMaxLargeObjectsBytes = max_large_objects_bytes stats
            , rsCumulativeLiveBytes = cumulative_live_bytes stats
            , rsLiveBytes = gcdetails_live_bytes (gc stats)
            , rsMemInUseBytes = gcdetails_mem_in_use_bytes (gc stats)
            , rsCapabilities = rpCapabilities posture
            , rsMaxHeapBytes = rpMaxHeapBytes posture
            , rsAllocAreaBytes = rpAllocAreaBytes posture
            , rsCompactAlways = compact flags
            , rsCompactThresholdPercent = compactThreshold flags
            }
