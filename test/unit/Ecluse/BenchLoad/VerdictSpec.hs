-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the endings the load run observes, the invariants that fail it, and how a floor breach is confirmed.
module Ecluse.BenchLoad.VerdictSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec

import Ecluse.BenchLoad.Floors (
    Enforcement (Enforced, NotHeld),
    FloorCheck (AtLeast, NoFloor, ReportOnly, Unchecked),
    FloorKey,
    Pass (ConcurrencyOne, Loaded),
    Trigger (OnDemand, Scheduled),
    Unheld (SettingsDiffer),
 )
import Ecluse.BenchLoad.Support (floorsAtTwoCores, otherLatency, twoCores)
import Ecluse.BenchLoad.Verdict (Attempts, Outcome, ProxyEnding (..), RunEvidence (..), attemptClean, classifyEnding, confirmBreaches, rerunsDue, runVerdict, runViolations)

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
            runViolations (AtLeast 100 167) healthy{reSuccesses = [("", 99)]}
                `shouldBe` ["npm/merge-cold (2cpu-1gib, 100 connections): 99 successful responses, below the floor of 100"]
            runViolations (AtLeast 964 1607) pypi{reSuccesses = [("", 963)]}
                `shouldBe` ["pypi/index-cold (2cpu-1gib, 100 connections): 963 successful responses, below the floor of 964"]
        it "passes a count at its floor" $ do
            runViolations (AtLeast 100 167) healthy{reSuccesses = [("", 100)]} `shouldBe` []
            runViolations (AtLeast 964 1607) pypi `shouldBe` []
        it "keeps the zero-success line for a count of zero under a floor" $
            runViolations (AtLeast 964 1607) pypi{reSuccesses = [("", 0)]}
                `shouldBe` ["pypi/index-cold (2cpu-1gib, 100 connections): no successful responses"]
        it "holds every load of a scenario to the one floor, and names the load that fell below it" $
            runViolations (AtLeast 100 167) healthy{reSuccesses = [("10 connections", 100), ("25 connections", 40), ("50 connections", 0)]}
                `shouldBe` [ "npm/merge-cold (2cpu-1gib, 100 connections): 40 successful responses, below the floor of 100 (25 connections)"
                           , "npm/merge-cold (2cpu-1gib, 100 connections): no successful responses (50 connections)"
                           ]
        it "fails closed, once, on a scenario the floors do not cover, and names the pass" $ do
            runViolations (NoFloor Loaded) healthy{reSuccesses = [("measured", 5), ("concurrent load", 5)]}
                `shouldBe` ["npm/merge-cold (2cpu-1gib, 100 connections): no loaded cell for this pod shape in bench/load/floors.json"]
            runViolations (NoFloor ConcurrencyOne) pypi
                `shouldBe` ["pypi/index-cold (2cpu-1gib, 100 connections): no concurrencyOne cell for this pod shape in bench/load/floors.json"]
        it "holds a run that is not held to no floor" $
            runViolations Unchecked pypi{reSuccesses = [("", 1)]} `shouldBe` []
        it "holds an uncalibrated or report-only cell to no floor, and keeps its zero-success and process checks" $
            for_ [ReportOnly Nothing, ReportOnly (Just "its counts vary too widely between runs for a floor")] $ \check -> do
                runViolations check pypi{reSuccesses = [("", 1)]} `shouldBe` []
                runViolations check pypi{reSuccesses = [("", 0)]} `shouldBe` ["pypi/index-cold (2cpu-1gib, 100 connections): no successful responses"]
                runViolations check pypi{reEnding = Just HeapOverflow} `shouldBe` ["pypi/index-cold (2cpu-1gib, 100 connections): the proxy exited on heap overflow"]

    describe "runVerdict" $ do
        it "reads each pass of each scenario against its own floor" $
            runVerdict OnDemand held (Map.keysSet floorsAtTwoCores) (once [(("npm/merge-cold", Loaded), Right healthy), (("npm/merge-cold", ConcurrencyOne), Right healthy), (("pypi/index-cold", Loaded), Right pypi), (("pypi/index-cold", ConcurrencyOne), Right pypi{reSuccesses = [("", 133)]})])
                `shouldBe` [ "npm/merge-cold (2cpu-1gib, 100 connections): 130 successful responses, below the floor of 338"
                           , "pypi/index-cold (2cpu-1gib, 100 connections): 133 successful responses, below the floor of 134"
                           ]
        it "reports a pass whose scenario process failed, under its scenario's name" $
            runVerdict OnDemand held (Map.keysSet floorsAtTwoCores) (once [(("pypi/index-cold", Loaded), Left "the scenario process exited 1.")])
                `shouldBe` ["pypi/index-cold: the scenario process exited 1."]
        it "appends a held run's floors for counts it does not check" $
            runVerdict OnDemand held (Set.delete ("pypi/index-cold", ConcurrencyOne) (Map.keysSet floorsAtTwoCores)) (once [(("pypi/index-cold", Loaded), Right pypi{reSuccesses = [("", 1)]})])
                `shouldBe` [ "pypi/index-cold (2cpu-1gib, 100 connections): 1 successful responses, below the floor of 964"
                           , "bench/load/floors.json: the concurrencyOne cell for pypi/index-cold under 2cpu-1gib names no count this run checks"
                           ]
        it "holds a run that is not held to no floor, and keeps its other invariants" $
            runVerdict OnDemand otherLatency Set.empty (once [(("npm/merge-cold", Loaded), Right healthy{reSuccesses = [("", 1)]}), (("pypi/index-cold", Loaded), Right pypi{reSuccesses = [("", 0)]})])
                `shouldBe` ["pypi/index-cold (2cpu-1gib, 100 connections): no successful responses"]
        it "leads with the one line of a scheduled run its configuration keeps off the floors" $
            runVerdict Scheduled (NotHeld (SettingsDiffer ("durationSeconds" :| []) :| [])) Set.empty (once [(("pypi/index-cold", Loaded), Right pypi{reSuccesses = [("", 0)]})])
                `shouldBe` [ "a scheduled run must be held to the success floors, and this one is not: it differs from the calibrated operating point in durationSeconds"
                           , "pypi/index-cold (2cpu-1gib, 100 connections): no successful responses"
                           ]
        it "fails a pass whose re-run is below its floor too, with both counts" $
            runVerdict OnDemand held checked [(mergeCold, Right (counted 330) :| [Right (counted 320)])]
                `shouldBe` ["npm/merge-cold (2cpu-1gib, 100 connections): 330 successful responses, then 320 on the re-run, both below the floor of 338"]
        it "passes a pass whose re-run reaches its floor" $
            runVerdict OnDemand held checked [(mergeCold, Right (counted 330) :| [Right (counted 338)])] `shouldBe` []
        it "fails a pass whose re-run failed, whatever the first attempt counted" $ do
            runVerdict OnDemand held checked [(mergeCold, Right (counted 330) :| [Left "the scenario process exited 1."])]
                `shouldBe` ["npm/merge-cold: the scenario process exited 1."]
            runVerdict OnDemand held checked [(mergeCold, Right (counted 330) :| [Right (counted 400){reOomKills = 1, reEnding = Just KernelOomKill}])]
                `shouldBe` ["npm/merge-cold (2cpu-1gib, 100 connections): the kernel OOM-killed the proxy (1 oom_kill events)"]

    describe "rerunsDue" $ do
        it "re-runs a pass below its floor whose attempts ran clean, while the calibrated re-runs remain" $ do
            rerunsDue held [(mergeCold, Right (counted 330) :| [])] `shouldBe` [mergeCold]
            rerunsDue held [(mergeCold, Right (counted 330) :| [Right (counted 320)])] `shouldBe` []
            rerunsDue (Enforced twoCores 2 floorsAtTwoCores) [(mergeCold, Right (counted 330) :| [Right (counted 320)])] `shouldBe` [mergeCold]
            rerunsDue (Enforced twoCores 0 floorsAtTwoCores) [(mergeCold, Right (counted 330) :| [])] `shouldBe` []
        it "does not re-run a pass with no successes, a process failure, or no report" $
            for_ [Right (counted 0), Right healthy{reSuccesses = [("measured", 330), ("concurrent load", 0)]}, Right (counted 330){reEnding = Just HeapOverflow}, Right (counted 330){reExitedEarly = True}, Left "the scenario process exited 1."] $ \attempt ->
                rerunsDue held [(mergeCold, attempt :| [])] `shouldBe` []
        it "does not re-run a pass at its floor, a pass without a floor, or any pass of a run that is not held" $ do
            rerunsDue held [(mergeCold, Right (counted 338) :| []), (("npm/herd", Loaded), Right (counted 1) :| []), (("pypi/slow-readers", Loaded), Right (counted 1) :| [])] `shouldBe` []
            rerunsDue otherLatency [(mergeCold, Right (counted 1) :| [])] `shouldBe` []
        it "reads an attempt as clean on its loads and its ending alone, whatever its floor" $ do
            map attemptClean [Right (counted 1), Right (counted 0), Right (counted 9){reOomKills = 1}, Right (counted 9){reEnding = Nothing}, Left "exited 1"] `shouldBe` [True, False, False, True, False]

    describe "confirmBreaches" $ do
        it "re-runs each breached pass once, in a round after every first attempt, and stops at a clear re-run" $ do
            let passes = [(mergeCold, Right (counted 330) :| []), (("pypi/index-cold", Loaded), Right pypi{reSuccesses = [("", 900)]} :| []), (("pypi/index-cold", ConcurrencyOne), Right pypi :| [])]
                (confirmed, calls) = scripted held [400, 970] passes
            calls `shouldBe` [mergeCold, ("pypi/index-cold", Loaded)]
            map (length . snd) confirmed `shouldBe` [2, 2, 1]
            runVerdict OnDemand held checked confirmed `shouldBe` []
        it "takes as many re-runs as the floors give, and fails a pass that breached on every one" $ do
            let (confirmed, calls) = scripted (Enforced twoCores 2 floorsAtTwoCores) [320, 310] [(mergeCold, Right (counted 330) :| [])]
            calls `shouldBe` [mergeCold, mergeCold]
            runVerdict OnDemand held checked confirmed
                `shouldBe` ["npm/merge-cold (2cpu-1gib, 100 connections): 330 successful responses, then 320 and 310 on the re-runs, all below the floor of 338"]
        it "re-runs nothing for a run that is not held" $
            snd (scripted otherLatency [1] [(mergeCold, Right (counted 1) :| [])]) `shouldBe` []
  where
    held = Enforced twoCores 1 floorsAtTwoCores
    checked = Map.keysSet floorsAtTwoCores
    mergeCold = ("npm/merge-cold", Loaded)
    counted n = healthy{reSuccesses = [("", n)]}
    once = map (second (:| []))

-- Confirm breaches with re-runs that count the scripted successes in turn, and the keys re-run in order.
scripted :: Enforcement -> [Int] -> [(FloorKey, Attempts)] -> ([(FloorKey, Attempts)], [FloorKey])
scripted enforcement counts passes = second (reverse . snd) (runState (confirmBreaches enforcement rerun passes) (counts, []))
  where
    rerun :: FloorKey -> State ([Int], [FloorKey]) Outcome
    rerun key = state $ \case
        (count : more, calls) -> (Right healthy{reSuccesses = [("", count)]}, (more, key : calls))
        ([], calls) -> (Left "no scripted re-run left", ([], key : calls))

healthy, pypi :: RunEvidence
healthy = RunEvidence "npm/merge-cold (2cpu-1gib, 100 connections)" [("", 130)] 0 (Just CleanShutdown) False
pypi = RunEvidence "pypi/index-cold (2cpu-1gib, 100 connections)" [("", 964)] 0 (Just CleanShutdown) False
