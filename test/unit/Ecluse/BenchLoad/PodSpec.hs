-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the pod-shape grammar, the scheduled shapes, the CPU placement, and the cgroup files the load run writes and reads.
module Ecluse.BenchLoad.PodSpec (spec) where

import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec

import Ecluse.BenchLoad.Pod (
    CpuList,
    PodShape (Limited, MemoryOnly, QuotaOnly, Unlimited),
    ProxyTuning (..),
    counter,
    cpuListMembers,
    cpuMaxValue,
    cpusLeft,
    defaultProxyTuning,
    keyedCounters,
    memoryBackstopBytes,
    needsCgroup,
    noTuning,
    parseCpuList,
    parsePodShape,
    proxyCgroupFiles,
    renderCpuList,
    renderPodShape,
    scheduledPodShapes,
 )

spec :: Spec
spec = do
    describe "parsePodShape" $ do
        it "reads the unlimited control and both memory units" $ do
            parsePodShape "unlimited" `shouldBe` Right Unlimited
            parsePodShape "2cpu-512mib" `shouldBe` Right (Limited 2 (512 * 1024 * 1024))
            parsePodShape " 3CPU-2GiB " `shouldBe` Right (Limited 3 (2 * 1024 * 1024 * 1024))
        it "reads a CPU-time quota or a memory limit on its own" $ do
            parsePodShape "2cpu" `shouldBe` Right (QuotaOnly 2)
            parsePodShape "1gib" `shouldBe` Right (MemoryOnly gib)
            parsePodShape "768mib" `shouldBe` Right (MemoryOnly (768 * 1024 * 1024))
        it "refuses a shape it cannot bound exactly" $
            for_ ["", "cpu", "gib", "0cpu", "0gib", "0cpu-512mib", "2cpu-0mib", "2cpu-512mb", "2cpu-512mib-extra", "-2cpu-512mib", "2.5cpu-1gib", "1gib-2cpu", "2cpu-2cpu"] $ \raw ->
                parsePodShape raw `shouldSatisfy` isLeft
    describe "renderPodShape" $ do
        it "round-trips every shape a scheduled run measures" $
            for_ scheduledPodShapes $ \shape ->
                parsePodShape (renderPodShape shape) `shouldBe` Right shape
        it "names the four scheduled shapes as the floors and the baseline artifacts key them" $
            map renderPodShape scheduledPodShapes `shouldBe` ["unlimited", "2cpu-1gib", "3cpu-1gib", "3cpu-2gib"]
        it "round-trips a shape with one part, and never renders one as a scheduled name" $
            for_ [QuotaOnly 2, MemoryOnly gib, MemoryOnly (2 * gib), MemoryOnly (512 * 1024 * 1024)] $ \shape -> do
                parsePodShape (renderPodShape shape) `shouldBe` Right shape
                map renderPodShape scheduledPodShapes `shouldNotContain` [renderPodShape shape]
    describe "cpuMaxValue" $
        it "sets a CPU-time quota in whole CPU equivalents over the default period" $
            cpuMaxValue 2 `shouldBe` "200000 100000"
    describe "parseCpuList" $ do
        it "reads single CPUs and inclusive ranges, and renders each run of CPUs as one range" $ do
            members "2-3" `shouldBe` Just [2, 3]
            members " 0, 2-3 " `shouldBe` Just [0, 2, 3]
            renderCpuList <$> rightToMaybe (parseCpuList "3,0,1,2,5") `shouldBe` Just "0-3,5"
            renderCpuList <$> rightToMaybe (parseCpuList "3") `shouldBe` Just "3"
        it "refuses an empty list, a reversed range, and anything that is not a CPU number" $
            for_ ["", " ", "3-2", "a", "1-", "-1", "1,,2", "1-2-3", "2.5"] $ \raw ->
                parseCpuList raw `shouldSatisfy` isLeft
    describe "cpusLeft" $
        it "leaves the CPUs the proxy does not take, and none when it takes them all" $ do
            renderCpuList <$> left "0-3" "2-3" `shouldBe` Just "0-1"
            renderCpuList <$> left "0-3" "3" `shouldBe` Just "0-2"
            left "0-3" "0-3" `shouldBe` Nothing
    describe "defaultProxyTuning" $ do
        it "places both scheduled 3cpu proxies on CPUs 1-3 with a three-CPU quota" $
            for_ [Limited 3 gib, Limited 3 (2 * gib)] $ \shape -> do
                let tuning = defaultProxyTuning shape
                ptCpus tuning `shouldBe` cpus "1-3"
                lookup "cpu.max" (proxyCgroupFiles shape tuning) `shouldBe` Just "300000 100000"
                lookup "cpuset.cpus" (proxyCgroupFiles shape tuning) `shouldBe` Just "1-3"
                renderCpuList <$> join (cpusLeft <$> cpus "0-3" <*> ptCpus tuning) `shouldBe` Just "0"
        it "preserves the controls and leaves custom shapes unplaced" $
            for_ [Unlimited, Limited 2 gib, Limited 3 (512 * 1024 * 1024), QuotaOnly 3] $ \shape ->
                defaultProxyTuning shape `shouldBe` noTuning
    describe "proxyCgroupFiles" $ do
        it "writes the scheduled shapes' limits as before, and nothing for the untuned unlimited control" $ do
            proxyCgroupFiles Unlimited noTuning `shouldBe` []
            proxyCgroupFiles (Limited 2 gib) noTuning `shouldBe` [("cpu.max", "200000 100000"), ("memory.max", show gib)]
        it "writes one part of a shape alone, with the memory backstop where no memory limit binds" $ do
            proxyCgroupFiles (MemoryOnly gib) noTuning `shouldBe` [("memory.max", show gib)]
            proxyCgroupFiles (QuotaOnly 2) noTuning `shouldBe` [("cpu.max", "200000 100000"), ("memory.high", show memoryBackstopBytes)]
        it "places a tuned proxy on its CPUs, and backs an unlimited one with memory.high" $ do
            let placed = noTuning{ptCpus = cpus "2-3"}
            proxyCgroupFiles (MemoryOnly gib) placed `shouldBe` [("memory.max", show gib), ("cpuset.cpus", "2-3")]
            proxyCgroupFiles Unlimited placed `shouldBe` [("memory.high", show memoryBackstopBytes), ("cpuset.cpus", "2-3")]
            lookup "memory.high" (proxyCgroupFiles Unlimited noTuning{ptCores = Just 8}) `shouldBe` Just (show memoryBackstopBytes)
        it "needs a cgroup for a limit or a placement, and none for the unlimited control or runtime settings alone" $ do
            needsCgroup Unlimited noTuning `shouldBe` False
            needsCgroup Unlimited noTuning{ptCores = Just 2} `shouldBe` False
            needsCgroup Unlimited noTuning{ptCpus = cpus "3"} `shouldBe` True
            needsCgroup (MemoryOnly gib) noTuning `shouldBe` True
    describe "keyedCounters" $ do
        it "reads memory.events and skips a line it cannot read" $ do
            let events = keyedCounters "low 0\nhigh 3\nmax 12\noom 1\noom_kill 1\nnot a counter line\n"
            Map.lookup "oom_kill" events `shouldBe` Just 1
            Map.lookup "max" events `shouldBe` Just 12
            Map.size events `shouldBe` 5
        it "reads an absent counter as zero" $
            counter "oom_group_kill" (keyedCounters "oom_kill 0\n") `shouldBe` 0

gib :: Int
gib = 1024 * 1024 * 1024

cpus :: Text -> Maybe CpuList
cpus = rightToMaybe . parseCpuList

members :: Text -> Maybe [Int]
members = fmap (Set.toList . cpuListMembers) . cpus

left :: Text -> Text -> Maybe CpuList
left every taken = join (cpusLeft <$> cpus every <*> cpus taken)
