-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the endings the load run observes and the invariants that fail it.
module Ecluse.BenchLoad.VerdictSpec (spec) where

import Test.Hspec

import Ecluse.BenchLoad.Verdict (ProxyEnding (..), RunEvidence (..), classifyEnding, runViolations)

spec :: Spec
spec = do
    describe "classifyEnding" $ do
        it "reads a graceful drain as a clean shutdown" $
            classifyEnding 0 "" 0 False `shouldBe` CleanShutdown
        it "recognises a heap overflow from the process's report or the RTS status" $ do
            classifyEnding 1 "ecluse: service exited: heap overflow\n" 0 False `shouldBe` HeapOverflow
            classifyEnding 251 "bench-load: Heap exhausted;" 0 False `shouldBe` HeapOverflow
        it "attributes a SIGKILL to the kernel only when the cgroup counted an OOM kill" $ do
            classifyEnding (-9) "" 1 False `shouldBe` KernelOomKill
            classifyEnding (-9) "" 0 False `shouldBe` KilledBySignal 9
            classifyEnding (-9) "" 0 True `shouldBe` StoppedByHarness
        it "keeps any other status" $ do
            classifyEnding 2 "boot refused" 0 False `shouldBe` ExitedWith 2
            classifyEnding (-15) "" 0 False `shouldBe` KilledBySignal 15
    describe "runViolations" $ do
        let healthy = RunEvidence "npm/merge-cold" [("", 130)] 0 (Just CleanShutdown) False
        it "holds for a run with successes and a clean ending" $
            runViolations healthy `shouldBe` []
        it "fails a scenario that answered only refusals" $
            runViolations healthy{reSuccesses = [("", 0)]} `shouldBe` ["npm/merge-cold: no successful responses"]
        it "names the load that got nothing when a scenario drives two" $
            runViolations healthy{reSuccesses = [("measured", 12), ("concurrent load", 0)]}
                `shouldBe` ["npm/merge-cold: no successful responses (concurrent load)"]
        it "fails on a kernel OOM kill and on a heap overflow" $ do
            runViolations healthy{reOomKills = 1, reEnding = Just KernelOomKill}
                `shouldBe` ["npm/merge-cold: the kernel OOM-killed the proxy (1 oom_kill events)"]
            runViolations healthy{reEnding = Just HeapOverflow} `shouldBe` ["npm/merge-cold: the proxy exited on heap overflow"]
        it "fails on any other ending than a clean shutdown, and on an early exit" $ do
            runViolations healthy{reEnding = Just StoppedByHarness}
                `shouldBe` ["npm/merge-cold: the proxy did not shut down cleanly (killed after the drain grace)"]
            runViolations healthy{reEnding = Just (ExitedWith 2), reExitedEarly = True}
                `shouldBe` ["npm/merge-cold: the proxy did not shut down cleanly (exited 2)", "npm/merge-cold: the proxy exited before the harness stopped it"]
        it "passes a scenario that runs in the harness process on its successes alone" $
            runViolations healthy{reEnding = Nothing} `shouldBe` []
