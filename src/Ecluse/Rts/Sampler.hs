-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The memory sampler: a background loop that reads the collector's statistics and the cgroup's
memory use, and steers the meter's budget through "Ecluse.Core.Server.Admission.Brake".

It runs off the served path. Without @-T@ it still reads the cgroup, and without a cgroup limit it
still reads the collector, so it never has a reason to stop.
-}
module Ecluse.Rts.Sampler (
    SamplerSettings (..),
    samplerPeriodMicros,
    shareWindowPeriods,
    readCollector,
    runMemorySampler,
) where

import Control.Concurrent (threadDelay)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (cpu_ns, gc, gc_cpu_ns, major_gcs), getRTSStats, getRTSStatsEnabled)

import Ecluse.Core.Server.Admission.Brake (
    BrakeBounds,
    BrakeMarks,
    BrakeState (bsBudget, bsLevel),
    CollectorReading (..),
    brakeStep,
    initialBrakeState,
    newSampleWindow,
    windowSample,
 )
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, MeterSnapshot (snChargedBytes), meterSnapshot, steerMeter)

-- | What one sampler loop reads and steers.
data SamplerSettings = SamplerSettings
    { ssMeter :: MemoryMeter
    , ssMarks :: BrakeMarks
    , ssBounds :: BrakeBounds
    , ssCollector :: IO (Maybe CollectorReading)
    -- ^ The collector's counters, absent without @-T@.
    , ssKernelPermille :: IO (Maybe Int)
    -- ^ The cgroup's non-reclaimable memory use against its limit, when a limit binds.
    , ssPeriodMicros :: Int
    , ssWindowPeriods :: Int
    -- ^ How many periods the collector's CPU share averages over.
    }

-- | The sampling period: ten readings a second.
samplerPeriodMicros :: Int
samplerPeriodMicros = 100_000

-- | The collector's CPU share averages over one second at the default period.
shareWindowPeriods :: Int
shareWindowPeriods = 10

-- | A reader for the collector's counters, or one that always reads nothing when @-T@ is off.
readCollector :: IO (IO (Maybe CollectorReading))
readCollector = do
    statsOn <- getRTSStatsEnabled
    pure (if statsOn then Just . collectorReading <$> getRTSStats else pure Nothing)
  where
    collectorReading stats =
        CollectorReading
            { crCpuNs = cpu_ns stats
            , crGcCpuNs = gc_cpu_ns stats
            , crMajorCollections = major_gcs stats
            , crLiveBytes = fromIntegral (gcdetails_live_bytes (gc stats))
            }

-- | Sample and steer forever. The caller runs it under supervision and cancels it at shutdown.
runMemorySampler :: SamplerSettings -> IO ()
runMemorySampler settings =
    loop (initialBrakeState (ssBounds settings)) (newSampleWindow (ssWindowPeriods settings))
  where
    loop current window = do
        threadDelay (ssPeriodMicros settings)
        reading <- ssCollector settings
        charged <- snChargedBytes <$> meterSnapshot (ssMeter settings)
        kernel <- ssKernelPermille settings
        let (sample, window') = windowSample window reading charged kernel
            next = brakeStep (ssMarks settings) (ssBounds settings) current sample
        when (bsBudget next /= bsBudget current || bsLevel next /= bsLevel current) $
            steerMeter (ssMeter settings) (bsBudget next) (bsLevel next)
        loop next window'
