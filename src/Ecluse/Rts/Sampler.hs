-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The memory sampler: a background loop that reads the collector's statistics and the cgroup's
memory use, and moves the meter's budget through "Ecluse.Core.Server.Admission.Brake".

It runs off the served path. Without @-T@ it still reads the cgroup, and without a cgroup limit it
still reads the collector, so it never has a reason to stop.
-}
module Ecluse.Rts.Sampler (
    SamplerSettings (..),
    samplerPeriodMicros,
    runMemorySampler,
) where

import Control.Concurrent (threadDelay)
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (cpu_ns, gc, gc_cpu_ns, major_gcs), getRTSStats, getRTSStatsEnabled)

import Ecluse.Core.Server.Admission.Brake (
    BrakeBounds,
    BrakeMarks,
    BrakeState (bsBudget, bsLevel),
    CpuReading (CpuReading),
    GcSample (..),
    brakeLevelCode,
    brakeStep,
    gcSharePermille,
    initialBrakeState,
 )
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, MeterSnapshot (snChargedBytes), meterSnapshot, setMeterBrakeLevel, setMeterBudget)

-- | What one sampler loop needs.
data SamplerSettings = SamplerSettings
    { ssMeter :: MemoryMeter
    , ssMarks :: BrakeMarks
    , ssBounds :: BrakeBounds
    , ssKernelPermille :: IO (Maybe Int)
    -- ^ The cgroup's non-reclaimable memory use against its limit, when a limit binds.
    , ssPeriodMicros :: Int
    }

-- | The sampling period: ten readings a second.
samplerPeriodMicros :: Int
samplerPeriodMicros = 100_000

-- The GC share averages over this many periods, one second at the default period.
windowSamples :: Int
windowSamples = 10

-- The collector's counters at one sample.
data Reading = Reading
    { rCpu :: CpuReading
    , rMajors :: Word32
    , rLive :: Int
    }

-- | Sample and steer forever. The caller runs it under supervision and cancels it at shutdown.
runMemorySampler :: SamplerSettings -> IO ()
runMemorySampler settings = do
    statsOn <- getRTSStatsEnabled
    let readStats = if statsOn then Just <$> readCollector else pure Nothing
    opening <- readStats
    loop readStats (initialBrakeState (ssBounds settings)) (maybeToList opening) (rMajors <$> opening)
  where
    loop readStats current window lastMajors = do
        threadDelay (ssPeriodMicros settings)
        reading <- readStats
        snapshot <- meterSnapshot (ssMeter settings)
        kernel <- ssKernelPermille settings
        let window' = take (windowSamples + 1) (maybeToList reading <> window)
            share = case (reading, listToMaybe (reverse window')) of
                (Just newest, Just oldest) -> gcSharePermille (rCpu oldest) (rCpu newest)
                _ -> Nothing
            majors = rMajors <$> reading
            live = do
                latest <- reading
                guard (Just (rMajors latest) /= lastMajors)
                pure (rLive latest)
            sample =
                GcSample
                    { gsGcSharePermille = share
                    , gsLiveAfterMajor = live
                    , gsChargedBytes = snChargedBytes snapshot
                    , gsKernelPermille = kernel
                    }
            next = brakeStep (ssMarks settings) (ssBounds settings) current sample
        when (bsBudget next /= bsBudget current) (setMeterBudget (ssMeter settings) (bsBudget next))
        when (bsLevel next /= bsLevel current) (setMeterBrakeLevel (ssMeter settings) (brakeLevelCode (bsLevel next)))
        loop readStats next window' majors

readCollector :: IO Reading
readCollector = do
    stats <- getRTSStats
    pure
        Reading
            { rCpu = CpuReading (cpu_ns stats) (gc_cpu_ns stats)
            , rMajors = major_gcs stats
            , rLive = fromIntegral (gcdetails_live_bytes (gc stats))
            }
