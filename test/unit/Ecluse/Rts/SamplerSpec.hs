-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Rts.SamplerSpec (spec) where

import Test.Hspec
import UnliftIO (race_)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Server.Admission.Brake (
    BrakeBounds (..),
    BrakeLevel (Braking, Calm),
    CollectorReading (..),
    defaultBrakeMarks,
 )
import Ecluse.Core.Server.Admission.Meter (MeterSettings (..), MeterSnapshot (..), meterSnapshot, newMemoryMeter)
import Ecluse.Rts.Sampler (SamplerSettings (..), runMemorySampler)

spec :: Spec
spec = describe "runMemorySampler" $ do
    it "halves the meter's budget to its floor while the collector takes most of the CPU" $ do
        -- Each reading adds 1,000 ns of CPU, 700 of it in the collector: a 70% share.
        ticks <- newIORef (0 :: Int64)
        let thrashing = do
                n <- atomicModifyIORef' ticks (\t -> (t + 1, t + 1))
                pure (Just CollectorReading{crCpuNs = 1_000 * n, crGcCpuNs = 700 * n, crMajorCollections = 0, crLiveBytes = 0})
        snapshot <- sampledFor thrashing
        snapshot `shouldBe` MeterSnapshot{snBudgetBytes = bbFloorBytes bounds, snChargedBytes = 0, snBrakeLevel = Braking}

    it "grows the meter's budget to its ceiling while nothing presses on it" $ do
        -- Without statistics nothing measures the remainder, so the boot estimate sets the ceiling.
        snapshot <- sampledFor (pure Nothing)
        snapshot `shouldBe` MeterSnapshot{snBudgetBytes = 150, snChargedBytes = 0, snBrakeLevel = Calm}
  where
    bounds =
        BrakeBounds
            { bbBootBytes = 100
            , bbFloorBytes = 16
            , bbLiveCeilingBytes = Just 200
            , bbFixedLiveBytes = 10
            , bbExplainedBytes = 50
            , bbOverflowLiveBytes = Nothing
            , bbGrowFloorBytes = 1
            }
    -- Run the sampler at a 1 ms period for long enough to settle, then read the meter.
    sampledFor collector = do
        meter <- newMemoryMeter MeterSettings{msBudgetBytes = bbBootBytes bounds, msStepBytes = 1, msEntryRoom = 1, msEntryWaitMicros = 0}
        let settings =
                SamplerSettings
                    { ssMeter = meter
                    , ssMarks = defaultBrakeMarks
                    , ssBounds = bounds
                    , ssCollector = collector
                    , ssKernelPermille = pure Nothing
                    , ssPeriodMicros = 1_000
                    , ssWindowPeriods = 2
                    }
        race_ (runMemorySampler settings) (threadDelay 400_000)
        meterSnapshot meter
