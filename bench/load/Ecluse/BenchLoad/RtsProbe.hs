-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Read this process's RTS counters with the runtime posture its boot applied. A scenario child
and a proxy read them only when @-T@ in @GHCRTS@ turned the statistics on.
-}
module Ecluse.BenchLoad.RtsProbe (
    rtsStatsFlag,
    rtsStatsRefusal,
    requireRtsStats,
    snapshotAfter,
) where

import GHC.RTS.Flags (GCFlags (compact, compactThreshold), getGCFlags)
import GHC.Stats (GCDetails (gcdetails_live_bytes, gcdetails_mem_in_use_bytes), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC, performMinorGC)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.RtsWindow (Collection (MajorCollection, MinorCollection), RtsSnapshot (..))
import Ecluse.Rts (RtsPosture (rpAllocAreaBytes, rpCapabilities, rpMaxHeapBytes), currentRtsPosture)

{- | The flag that turns on RTS statistics. Each scenario child and each proxy gets it through
@GHCRTS@ at launch, because the shipped RTS options they link with leave the statistics off.
-}
rtsStatsFlag :: String
rtsStatsFlag = "-T"

-- | The refusal for a process running without the statistics, naming it and 'rtsStatsFlag'.
rtsStatsRefusal :: Text -> Bool -> Maybe Text
rtsStatsRefusal process enabled =
    ("bench-load needs the RTS statistics. Run " <> process <> " with GHCRTS=" <> toText rtsStatsFlag <> ".") <$ guard (not enabled)

-- | Fail the harness with 'rtsStatsRefusal' when this process runs without the statistics.
requireRtsStats :: Text -> IO ()
requireRtsStats process = getRTSStatsEnabled >>= traverse_ benchFail . rtsStatsRefusal process

-- | Collect, then read one snapshot. The process must run with 'rtsStatsFlag' in @GHCRTS@.
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
