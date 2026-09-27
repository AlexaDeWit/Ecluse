-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the window arithmetic behind allocation per success and the GC share of CPU.
module Ecluse.BenchLoad.RtsWindowSpec (spec) where

import Test.Hspec

import Ecluse.BenchLoad.RtsWindow (RtsSnapshot (..), RtsWindow (..), compactionThresholdCrossed, gcCpuShare, meanLiveAtMajors, perSuccess, rtsWindow)

snapshot :: RtsSnapshot
snapshot =
    RtsSnapshot
        { rsAllocatedBytes = 1_000
        , rsGcs = 10
        , rsMajorGcs = 2
        , rsGcCpuNs = 100
        , rsCpuNs = 1_000
        , rsGcElapsedNs = 50
        , rsMaxLiveBytes = 0
        , rsMaxMemInUseBytes = 0
        , rsMaxLargeObjectsBytes = 0
        , rsCumulativeLiveBytes = 0
        , rsLiveBytes = 0
        , rsMemInUseBytes = 0
        , rsCapabilities = 2
        , rsMaxHeapBytes = Nothing
        , rsAllocAreaBytes = 0
        , rsCompactAlways = False
        , rsCompactThresholdPercent = 30
        }

spec :: Spec
spec = do
    describe "rtsWindow" $ do
        it "subtracts the opening snapshot from the closing one" $ do
            let w = rtsWindow snapshot snapshot{rsAllocatedBytes = 5_000, rsGcs = 14, rsMajorGcs = 3, rsGcCpuNs = 400, rsCpuNs = 2_000}
            (rwAllocatedBytes w, rwGcs w, rwMajorGcs w) `shouldBe` (4_000, 4, 1)
            gcCpuShare w `shouldBe` Just 0.3
        it "reads a counter that went backwards as zero" $
            rwAllocatedBytes (rtsWindow snapshot snapshot{rsAllocatedBytes = 10}) `shouldBe` 0
        it "has no GC share without CPU time" $
            gcCpuShare (rtsWindow snapshot snapshot) `shouldBe` Nothing
    describe "meanLiveAtMajors" $ do
        it "divides the live data the window's majors left by their count" $
            meanLiveAtMajors (rtsWindow snapshot snapshot{rsMajorGcs = 6, rsCumulativeLiveBytes = 400}) `shouldBe` Just 100
        it "is unknown when no major collection ran" $
            meanLiveAtMajors (rtsWindow snapshot snapshot) `shouldBe` Nothing
    describe "compactionThresholdCrossed" $ do
        let bounded = snapshot{rsMaxHeapBytes = Just 1_000}
        it "compares small-object live data with the threshold share of the heap ceiling" $ do
            compactionThresholdCrossed bounded{rsMaxLiveBytes = 400, rsMaxLargeObjectsBytes = 50} `shouldBe` Just True
            compactionThresholdCrossed bounded{rsMaxLiveBytes = 400, rsMaxLargeObjectsBytes = 150} `shouldBe` Just False
        it "is unknown without a heap ceiling, unless compaction is forced" $ do
            compactionThresholdCrossed snapshot{rsMaxLiveBytes = 400} `shouldBe` Nothing
            compactionThresholdCrossed snapshot{rsCompactAlways = True} `shouldBe` Just True
    describe "perSuccess" $ do
        it "divides by the successful requests" $
            perSuccess 4_000 8 `shouldBe` Just 500
        it "reports nothing when nothing succeeded" $
            perSuccess 4_000 0 `shouldBe` Nothing
