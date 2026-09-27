-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Rts.SamplerSpec (spec) where

import Test.Hspec
import UnliftIO (withAsync)

import Ecluse.Core.Server.Admission.Brake (CollectorReading (..), defaultBrakeMarks)
import Ecluse.Core.Server.Admission.Meter (MeterSettings (..), meterSnapshot, newMemoryMeter)
import Ecluse.Core.Server.Admission.Types (BrakeBounds (..), BrakeLevel (Braking, Calm), MeterSnapshot (..))
import Ecluse.Rts.Sampler (SamplerSettings (..), runMemorySampler)

spec :: Spec
spec = describe "runMemorySampler" $ do
    it "halves the meter's budget to its floor while the collector takes most of the CPU" $ do
        -- Each reading adds 1,000 ns of CPU, 700 of it in the collector: a 70% share.
        snapshot <- sampledFor $ \n -> Just CollectorReading{crCpuNs = 1_000 * n, crGcCpuNs = 700 * n, crMajorCollections = 0, crLiveBytes = 0}
        snapshot `shouldBe` MeterSnapshot{snBudgetBytes = bbFloorBytes bounds, snChargedBytes = 0, snWaiting = 0, snPaused = 0, snBrakeLevel = Braking}

    it "grows the meter's budget to its ceiling while nothing presses on it" $ do
        -- Without statistics nothing measures the remainder, so the boot estimate sets the ceiling.
        snapshot <- sampledFor (const Nothing)
        snapshot `shouldBe` MeterSnapshot{snBudgetBytes = 150, snChargedBytes = 0, snWaiting = 0, snPaused = 0, snBrakeLevel = Calm}
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
    -- Feed a fixed number of readings with no pause between samples, then read the meter. Asking
    -- for the reading after the last counted one means every counted sample has steered the meter.
    sampledFor reading = do
        meter <- newMemoryMeter MeterSettings{msBudgetBytes = bbBootBytes bounds, msStepBytes = 1, msEntryRoom = 1, msEntryWaitMicros = 0}
        ticks <- newIORef (0 :: Int64)
        settled <- newEmptyMVar
        let collector = do
                n <- atomicModifyIORef' ticks (\t -> (t + 1, t + 1))
                when (n > sampleCount) (void (tryPutMVar settled ()))
                pure (reading n)
            settings =
                SamplerSettings
                    { ssMeter = meter
                    , ssMarks = defaultBrakeMarks
                    , ssBounds = bounds
                    , ssCollector = collector
                    , ssKernelPermille = pure Nothing
                    , ssPeriodMicros = 0
                    , ssWindowPeriods = 2
                    }
        withAsync (runMemorySampler settings) (const (takeMVar settled))
        meterSnapshot meter
    sampleCount = 400
