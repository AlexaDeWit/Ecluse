-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Server.AdmissionSpec (spec) where

import Control.Exception (ErrorCall (ErrorCall))
import Test.Hspec
import UnliftIO (async, mapConcurrently, throwIO, timeout, tryAny, wait)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Server.Admission (
    ServeAdmission,
    newServeAdmissionTuned,
    withServeAdmission,
 )
import Ecluse.Core.Telemetry.Record (MetricsPort (mpServeAdmissionInFlight, mpServeAdmissionQueued))
import Ecluse.Test.Port (noopMetricsPort)

{- | Wait budgets for the tuned handles: generous where a test must observe a wait
complete on a loaded runner, small where a test must observe a wait expire without
slowing the suite.
-}
generousWaitMicros :: Int
generousWaitMicros = 5_000_000

shortWaitMicros :: Int
shortWaitMicros = 50_000

-- Run an admission attempt through the noop metrics port.
admit :: ServeAdmission -> IO a -> IO (Maybe a)
admit = withServeAdmission noopMetricsPort

{- | Occupy the handle's one slot from another thread: returns once the slot is
held, together with the gate that releases it and the holder's handle.
-}
holdOneSlot :: ServeAdmission -> IO (MVar (), IO ())
holdOneSlot admission = do
    holderGate <- newEmptyMVar
    holderIn <- newEmptyMVar
    holder <- async . admit admission $ do
        putMVar holderIn ()
        takeMVar holderGate
    takeMVar holderIn
    pure (holderGate, void (wait holder))

spec :: Spec
spec = describe "withServeAdmission -- brief-wait admission" $ do
    it "admits immediately below capacity and returns the action's result" $ do
        admission <- newServeAdmissionTuned 2 2 shortWaitMicros
        result <- admit admission (pure (42 :: Int))
        result `shouldBe` Just 42

    it "refuses instantly at capacity when the waiting room is zero (pure shed)" $ do
        admission <- newServeAdmissionTuned 1 0 generousWaitMicros
        (holderGate, awaitHolder) <- holdOneSlot admission
        refused <- admit admission (pure ())
        refused `shouldBe` Nothing
        putMVar holderGate ()
        awaitHolder

    it "queues at capacity and proceeds when a slot frees within the budget" $ do
        admission <- newServeAdmissionTuned 1 1 generousWaitMicros
        (holderGate, awaitHolder) <- holdOneSlot admission
        waiter <- async (admit admission (pure (7 :: Int)))
        -- Give the waiter time to have shed if it were going to (a refusal is
        -- instant), then free the slot and expect admission.
        threadDelay 30_000
        putMVar holderGate ()
        wait waiter `shouldReturn` Just 7
        awaitHolder

    it "refuses instantly when the waiting room is full" $ do
        admission <- newServeAdmissionTuned 1 1 generousWaitMicros
        (holderGate, awaitHolder) <- holdOneSlot admission
        waiter <- async (admit admission (pure ()))
        threadDelay 30_000 -- let the waiter take the one room place
        -- The room is full, so a third arrival is refused without waiting out
        -- any budget: it must come back well inside the generous budget.
        refusal <- timeout 1_000_000 (admit admission (pure ()))
        refusal `shouldBe` Just Nothing
        putMVar holderGate ()
        wait waiter `shouldReturn` Just ()
        awaitHolder

    it "sheds after the wait budget when no slot frees, and restores the room" $ do
        admission <- newServeAdmissionTuned 1 1 shortWaitMicros
        (holderGate, awaitHolder) <- holdOneSlot admission
        firstTry <- admit admission (pure ())
        firstTry `shouldBe` Nothing
        -- The expired wait surrendered its room place: a later arrival waits
        -- again (rather than being refused at a leaked-full room) and sheds the
        -- same way.
        secondTry <- admit admission (pure ())
        secondTry `shouldBe` Nothing
        putMVar holderGate ()
        awaitHolder

    it "releases on an exception, admitting a queued waiter" $ do
        admission <- newServeAdmissionTuned 1 1 generousWaitMicros
        holderIn <- newEmptyMVar
        holderGate <- newEmptyMVar
        holder <- async . tryAny . admit admission $ do
            putMVar holderIn ()
            takeMVar holderGate
            throwIO (ErrorCall "holder failed") :: IO ()
        takeMVar holderIn
        waiter <- async (admit admission (pure (9 :: Int)))
        threadDelay 30_000
        putMVar holderGate ()
        wait waiter `shouldReturn` Just 9
        outcome <- wait holder
        outcome `shouldSatisfy` isLeft

    it "never exceeds capacity under a storm, and the room absorbs it" $ do
        admission <- newServeAdmissionTuned 4 4 generousWaitMicros
        inFlight <- newTVarIO (0 :: Int)
        highWater <- newTVarIO (0 :: Int)
        let job = admit admission $ do
                atomically $ do
                    n <- (+ 1) <$> readTVar inFlight
                    writeTVar inFlight n
                    modifyTVar' highWater (max n)
                threadDelay 10_000
                atomically (modifyTVar' inFlight (subtract 1))
        results <- mapConcurrently (const job) [1 :: Int .. 32]
        peak <- readTVarIO highWater
        peak `shouldSatisfy` (<= 4)
        -- With a generous budget and fast jobs, the room keeps refusals to the
        -- deep-overflow band: at least slots + room requests must succeed.
        length (filter isJust results) `shouldSatisfy` (>= 8)

    it "releases the slot and re-raises when the queued observer throws" $ do
        -- The queued record runs inside the release-protected region and after the
        -- in-flight increment, so a throw from it must propagate to the caller,
        -- still return the held slot, and leave the gauge balanced (increments
        -- matched by decrements). Regression for the pre-fix leak (#855).
        gauge <- newTVarIO (0 :: Int)
        let throwingQueued =
                noopMetricsPort
                    { mpServeAdmissionInFlight = \delta -> atomically (modifyTVar' gauge (+ delta))
                    , mpServeAdmissionQueued = throwIO (ErrorCall "queued observer failed")
                    }
        admission <- newServeAdmissionTuned 1 1 generousWaitMicros
        (holderGate, awaitHolder) <- holdOneSlot admission
        waiter <- async (tryAny (withServeAdmission throwingQueued admission (pure ())))
        threadDelay 30_000 -- let the waiter queue and block on the held slot
        putMVar holderGate () -- free the slot: the waiter acquires, then the record throws
        outcome <- wait waiter
        outcome `shouldSatisfy` isLeft -- the throw propagates, not a swallowed Nothing
        awaitHolder
        -- The slot came back despite the throw: a fresh acquire admits at once.
        admit admission (pure ()) `shouldReturn` Just ()
        readTVarIO gauge `shouldReturn` (0 :: Int)

    it "never runs the queued observer on the immediate-admit door path" $ do
        -- Below capacity with no one waiting, admission takes the door path, which
        -- records no queue: a throwing queued hook must not fire there.
        let throwingQueued = noopMetricsPort{mpServeAdmissionQueued = throwIO (ErrorCall "must not run")}
        admission <- newServeAdmissionTuned 1 1 shortWaitMicros
        withServeAdmission throwingQueued admission (pure (5 :: Int)) `shouldReturn` Just 5
