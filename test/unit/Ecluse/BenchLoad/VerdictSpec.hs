-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the endings the load run observes and the invariants that fail it.
module Ecluse.BenchLoad.VerdictSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec

import Ecluse.BenchLoad.Floors (
    Enforcement (Enforced, NotHeld),
    FloorCheck (AtLeast, NoFloor, Unchecked),
    Pass (ConcurrencyOne, Loaded),
    Trigger (OnDemand, Scheduled),
    Unheld (SettingsDiffer),
 )
import Ecluse.BenchLoad.Support (floorsAtTwoCores, slowNetwork, twoCores)
import Ecluse.BenchLoad.Verdict (ProxyEnding (..), RunEvidence (..), classifyEnding, runVerdict, runViolations)

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
        it "holds for a run with successes and a clean ending" $
            runViolations Unchecked healthy `shouldBe` []
        it "fails a scenario that answered only refusals" $
            runViolations Unchecked healthy{reSuccesses = [("", 0)]} `shouldBe` ["npm/merge-cold (2cpu-1gib, 100 connections): no successful responses"]
        it "names the load that got nothing when a scenario drives two" $
            runViolations Unchecked healthy{reSuccesses = [("measured", 12), ("concurrent load", 0)]}
                `shouldBe` ["npm/merge-cold (2cpu-1gib, 100 connections): no successful responses (concurrent load)"]
        it "fails on a kernel OOM kill and on a heap overflow" $ do
            runViolations Unchecked healthy{reOomKills = 1, reEnding = Just KernelOomKill}
                `shouldBe` ["npm/merge-cold (2cpu-1gib, 100 connections): the kernel OOM-killed the proxy (1 oom_kill events)"]
            runViolations Unchecked healthy{reEnding = Just HeapOverflow} `shouldBe` ["npm/merge-cold (2cpu-1gib, 100 connections): the proxy exited on heap overflow"]
        it "fails on any other ending than a clean shutdown, and on an early exit" $ do
            runViolations Unchecked healthy{reEnding = Just StoppedByHarness}
                `shouldBe` ["npm/merge-cold (2cpu-1gib, 100 connections): the proxy did not shut down cleanly (killed after the drain grace)"]
            runViolations Unchecked healthy{reEnding = Just (ExitedWith 2), reExitedEarly = True}
                `shouldBe` [ "npm/merge-cold (2cpu-1gib, 100 connections): the proxy did not shut down cleanly (exited 2)"
                           , "npm/merge-cold (2cpu-1gib, 100 connections): the proxy exited before the harness stopped it"
                           ]
        it "passes a scenario that runs in the harness process on its successes alone" $
            runViolations Unchecked healthy{reEnding = Nothing} `shouldBe` []
        it "fails a count below its floor, naming the scenario, the pod shape, the count, and the floor" $ do
            runViolations (AtLeast 100) healthy{reSuccesses = [("", 99)]}
                `shouldBe` ["npm/merge-cold (2cpu-1gib, 100 connections): 99 successful responses, below the floor of 100"]
            runViolations (AtLeast 964) pypi{reSuccesses = [("", 963)]}
                `shouldBe` ["pypi/index-cold (2cpu-1gib, 100 connections): 963 successful responses, below the floor of 964"]
        it "passes a count at its floor" $ do
            runViolations (AtLeast 100) healthy{reSuccesses = [("", 100)]} `shouldBe` []
            runViolations (AtLeast 964) pypi `shouldBe` []
        it "keeps the zero-success line for a count of zero under a floor" $
            runViolations (AtLeast 964) pypi{reSuccesses = [("", 0)]}
                `shouldBe` ["pypi/index-cold (2cpu-1gib, 100 connections): no successful responses"]
        it "holds every load of a scenario to the one floor, and names the load that fell below it" $
            runViolations (AtLeast 100) healthy{reSuccesses = [("10 connections", 100), ("25 connections", 40), ("50 connections", 0)]}
                `shouldBe` [ "npm/merge-cold (2cpu-1gib, 100 connections): 40 successful responses, below the floor of 100 (25 connections)"
                           , "npm/merge-cold (2cpu-1gib, 100 connections): no successful responses (50 connections)"
                           ]
        it "fails closed, once, on a scenario the floors do not cover, and names the pass" $ do
            runViolations (NoFloor Loaded) healthy{reSuccesses = [("measured", 5), ("concurrent load", 5)]}
                `shouldBe` ["npm/merge-cold (2cpu-1gib, 100 connections): no loaded floor for this pod shape in bench/load/floors.json"]
            runViolations (NoFloor ConcurrencyOne) pypi
                `shouldBe` ["pypi/index-cold (2cpu-1gib, 100 connections): no concurrencyOne floor for this pod shape in bench/load/floors.json"]
        it "holds a run that is not held to no floor" $
            runViolations Unchecked pypi{reSuccesses = [("", 1)]} `shouldBe` []

    describe "runVerdict" $ do
        it "reads each pass of each scenario against its own floor" $
            runVerdict OnDemand (Enforced twoCores floorsAtTwoCores) (Map.keysSet floorsAtTwoCores) [(("npm/merge-cold", Loaded), Right healthy), (("npm/merge-cold", ConcurrencyOne), Right healthy), (("pypi/index-cold", Loaded), Right pypi), (("pypi/index-cold", ConcurrencyOne), Right pypi{reSuccesses = [("", 133)]})]
                `shouldBe` [ "npm/merge-cold (2cpu-1gib, 100 connections): 130 successful responses, below the floor of 338"
                           , "pypi/index-cold (2cpu-1gib, 100 connections): 133 successful responses, below the floor of 134"
                           ]
        it "reports a pass whose scenario process failed, under its scenario's name" $
            runVerdict OnDemand (Enforced twoCores floorsAtTwoCores) (Map.keysSet floorsAtTwoCores) [(("pypi/index-cold", Loaded), Left "the scenario process exited 1.")]
                `shouldBe` ["pypi/index-cold: the scenario process exited 1."]
        it "appends a held run's floors for counts it does not check" $
            runVerdict OnDemand (Enforced twoCores floorsAtTwoCores) (Set.delete ("pypi/index-cold", ConcurrencyOne) (Map.keysSet floorsAtTwoCores)) [(("pypi/index-cold", Loaded), Right pypi{reSuccesses = [("", 1)]})]
                `shouldBe` [ "pypi/index-cold (2cpu-1gib, 100 connections): 1 successful responses, below the floor of 964"
                           , "bench/load/floors.json: the concurrencyOne floor for pypi/index-cold under 2cpu-1gib names no count this run checks"
                           ]
        it "holds a run that is not held to no floor, and keeps its other invariants" $
            runVerdict OnDemand slowNetwork Set.empty [(("npm/merge-cold", Loaded), Right healthy{reSuccesses = [("", 1)]}), (("pypi/index-cold", Loaded), Right pypi{reSuccesses = [("", 0)]})]
                `shouldBe` ["pypi/index-cold (2cpu-1gib, 100 connections): no successful responses"]
        it "leads with the one line of a scheduled run its configuration keeps off the floors" $
            runVerdict Scheduled (NotHeld (SettingsDiffer ("durationSeconds" :| []) :| [])) Set.empty [(("pypi/index-cold", Loaded), Right pypi{reSuccesses = [("", 0)]})]
                `shouldBe` [ "a scheduled run must be held to the success floors, and this one is not: it differs from the calibrated operating point in durationSeconds"
                           , "pypi/index-cold (2cpu-1gib, 100 connections): no successful responses"
                           ]
        it "passes a scheduled run that only a slow network keeps off the floors" $
            runVerdict Scheduled slowNetwork Set.empty [(("npm/merge-cold", Loaded), Right healthy{reSuccesses = [("", 1)]})] `shouldBe` []

healthy, pypi :: RunEvidence
healthy = RunEvidence "npm/merge-cold (2cpu-1gib, 100 connections)" [("", 130)] 0 (Just CleanShutdown) False
pypi = RunEvidence "pypi/index-cold (2cpu-1gib, 100 connections)" [("", 964)] 0 (Just CleanShutdown) False
