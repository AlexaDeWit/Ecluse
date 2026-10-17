-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the CPU arithmetic of a measured window: the proxy's share of its CPUs, the generator's headroom, and foreign time.
module Ecluse.BenchLoad.CpuUseSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.BenchLoad.CpuUse (
    CpuSample (..),
    CpuTicks (CpuTicks),
    CpuWindow (..),
    cpuBusyShare,
    cpuWindow,
    foreignCpuSeconds,
    generatorSaturated,
    harnessCpuShare,
    parseProcStat,
    proxyCpuRate,
    proxyCpuShare,
 )
import Ecluse.BenchLoad.Pod (CpuList, parseCpuList)

spec :: Spec
spec = do
    describe "parseProcStat" $ do
        it "reads each logical CPU's busy and total ticks, and skips the aggregate line" $
            parseProcStat "cpu  40 0 20 300 10 0 0 0 0 0\ncpu0 10 1 5 80 4 2 3 1 7 0\ncpu3 0 0 0 50 0 0 0 0\nintr 12 3\n"
                `shouldBe` Map.fromList [(0, CpuTicks 22 106), (3, CpuTicks 0 50)]
        it "skips a CPU line too short to read" $
            parseProcStat "cpu1 10 0 5\n" `shouldBe` mempty

    describe "a window" $ do
        let window = cpuWindow 100 (cpus "2-3") (cpus "0-1") (sample 10 1_000_000 500_000 [(0, 0, 0), (2, 0, 0), (3, 0, 0)]) (sample 30 31_000_000 30_500_000 [(0, 1_800, 2_000), (2, 1_700, 2_000), (3, 1_600, 2_000)])
        it "gives the proxy's CPU-seconds per second and its share of its CPUs" $ do
            proxyCpuRate window `shouldBe` Just 1.5
            proxyCpuShare window `shouldBe` Just 0.75
        it "reads CPU 0's busy share from its ticks alone" $
            cpuBusyShare 0 window `shouldBe` Just 0.9
        it "counts busy time on the proxy's CPUs that the proxy did not spend" $
            foreignCpuSeconds window `shouldBe` Just 3
        it "marks the generator saturated when its cgroup or oha's CPU reaches 85% busy" $ do
            harnessCpuShare window `shouldBe` Just 0.75
            generatorSaturated window `shouldBe` Just True
            generatorSaturated window{cwTicks = Map.insert 0 (CpuTicks 1_000 2_000) (cwTicks window)} `shouldBe` Just False
            generatorSaturated window{cwHarnessSeconds = Just 36} `shouldBe` Just True
        it "has no rate for an empty window, and no share or foreign time without a placement" $ do
            proxyCpuRate window{cwSeconds = 0} `shouldBe` Nothing
            proxyCpuShare window{cwProxyCpus = Nothing} `shouldBe` Nothing
            foreignCpuSeconds window{cwProxyCpus = Nothing} `shouldBe` Nothing
            generatorSaturated window{cwHarnessCpus = Nothing, cwTicks = mempty} `shouldBe` Nothing
        it "reads a counter that went backwards as no time" $
            cwProxySeconds (cpuWindow 100 Nothing Nothing (sample 0 5 0 []) (sample 1 4 0 [])) `shouldBe` Just 0

cpus :: Text -> Maybe CpuList
cpus = rightToMaybe . parseCpuList

-- A sample at a clock reading, with the proxy's and the harness's usage, and each CPU's busy and total ticks.
sample :: Double -> Int -> Int -> [(Int, Int, Int)] -> CpuSample
sample seconds proxyUsec harnessUsec ticks =
    CpuSample
        { csSeconds = seconds
        , csProxyUsec = Just proxyUsec
        , csHarnessUsec = Just harnessUsec
        , csTicks = Map.fromList [(cpu, CpuTicks busy total) | (cpu, busy, total) <- ticks]
        }
