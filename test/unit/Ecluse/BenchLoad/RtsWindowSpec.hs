-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the window arithmetic behind allocation per success and the GC share of CPU.
module Ecluse.BenchLoad.RtsWindowSpec (spec) where

import Test.Hspec

import Ecluse.BenchLoad.RtsWindow (RtsSnapshot (..), RtsWindow (..), compactionThresholdCrossed, gcCpuShare, meanLiveAtMajors, perSuccess, rtsWindow)
import Ecluse.BenchLoad.Support (rtsSnapshot)

spec :: Spec
spec = do
    describe "rtsWindow" $ do
        it "subtracts the opening snapshot from the closing one" $ do
            let w = rtsWindow rtsSnapshot rtsSnapshot{rsAllocatedBytes = 5_000, rsGcs = 14, rsMajorGcs = 3, rsGcCpuNs = 400, rsCpuNs = 2_000}
            (rwAllocatedBytes w, rwGcs w, rwMajorGcs w) `shouldBe` (4_000, 4, 1)
            gcCpuShare w `shouldBe` Just 0.3
        it "reads a counter that went backwards as zero" $
            rwAllocatedBytes (rtsWindow rtsSnapshot rtsSnapshot{rsAllocatedBytes = 10}) `shouldBe` 0
        it "has no GC share without CPU time" $
            gcCpuShare (rtsWindow rtsSnapshot rtsSnapshot) `shouldBe` Nothing
    describe "meanLiveAtMajors" $ do
        it "divides the live data the window's majors left by their count" $
            meanLiveAtMajors (rtsWindow rtsSnapshot rtsSnapshot{rsMajorGcs = 6, rsCumulativeLiveBytes = 400}) `shouldBe` Just 100
        it "is unknown when no major collection ran" $
            meanLiveAtMajors (rtsWindow rtsSnapshot rtsSnapshot) `shouldBe` Nothing
    describe "compactionThresholdCrossed" $ do
        let bounded = rtsSnapshot{rsMaxHeapBytes = Just 1_000}
        it "compares small-object live data with the threshold share of the heap ceiling" $ do
            compactionThresholdCrossed bounded{rsMaxLiveBytes = 400, rsMaxLargeObjectsBytes = 50} `shouldBe` Just True
            compactionThresholdCrossed bounded{rsMaxLiveBytes = 400, rsMaxLargeObjectsBytes = 150} `shouldBe` Just False
        it "is unknown without a heap ceiling, unless compaction is forced" $ do
            compactionThresholdCrossed rtsSnapshot{rsMaxLiveBytes = 400} `shouldBe` Nothing
            compactionThresholdCrossed rtsSnapshot{rsCompactAlways = True} `shouldBe` Just True
    describe "perSuccess" $ do
        it "divides by the successful requests" $
            perSuccess 4_000 8 `shouldBe` Just 500
        it "reports nothing when nothing succeeded" $
            perSuccess 4_000 0 `shouldBe` Nothing
