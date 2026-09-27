-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the pod-shape grammar and the cgroup file reads the load run depends on.
module Ecluse.BenchLoad.PodSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.BenchLoad.Pod (PodShape (Limited, Unlimited), counter, cpuMaxValue, keyedCounters, parsePodShape, renderPodShape)

spec :: Spec
spec = do
    describe "parsePodShape" $ do
        it "reads the unlimited control and both memory units" $ do
            parsePodShape "unlimited" `shouldBe` Right Unlimited
            parsePodShape "2cpu-512mib" `shouldBe` Right (Limited 2 (512 * 1024 * 1024))
            parsePodShape " 4CPU-2GiB " `shouldBe` Right (Limited 4 (2 * 1024 * 1024 * 1024))
        it "refuses a shape it cannot bound exactly" $
            for_ ["", "2cpu", "0cpu-512mib", "2cpu-0mib", "2cpu-512mb", "2cpu-512mib-extra", "-2cpu-512mib", "2.5cpu-1gib"] $ \raw ->
                parsePodShape raw `shouldSatisfy` isLeft
    describe "renderPodShape" $
        it "round-trips every shape the workflow schedules" $
            for_ ["unlimited", "2cpu-512mib", "4cpu-1gib", "4cpu-2gib"] $ \raw ->
                renderPodShape <$> parsePodShape raw `shouldBe` Right raw
    describe "cpuMaxValue" $
        it "grants whole cores over the default period" $
            cpuMaxValue 2 `shouldBe` "200000 100000"
    describe "keyedCounters" $ do
        it "reads memory.events and skips a line it cannot read" $ do
            let events = keyedCounters "low 0\nhigh 3\nmax 12\noom 1\noom_kill 1\nnot a counter line\n"
            Map.lookup "oom_kill" events `shouldBe` Just 1
            Map.lookup "max" events `shouldBe` Just 12
            Map.size events `shouldBe` 5
        it "reads an absent counter as zero" $
            counter "oom_group_kill" (keyedCounters "oom_kill 0\n") `shouldBe` 0
