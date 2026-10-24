-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Admission state, cancellation, and metric contracts.
module Ecluse.Core.Server.Admission.InternalSpec (spec) where

import Control.Concurrent.STM (check)
import Control.Exception (ErrorCall (ErrorCall))
import Test.Hspec
import UnliftIO (cancel, mapConcurrently, throwIO, timeout, tryAny, wait, withAsync)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Deadline (newDeadlines)
import Ecluse.Core.Server.Admission.Internal (
    ServeAdmission (..),
    newServeAdmission,
    newServeAdmissionTuned,
    withServeAdmission,
 )
import Ecluse.Core.Server.Admission.Types (Shed (..))
import Ecluse.Core.Server.Admission.Weighted (weightedAdmissionWaiting)
import Ecluse.Core.Telemetry.Record (MetricsPort (mpServeAdmissionInFlight, mpServeAdmissionQueued))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Port (noopMetricsPort)

{- | Wait budgets for the tuned handles. The generous budget lets a wait complete on a loaded
runner, and the short one lets a wait expire without slowing the suite.
-}
generousWaitMicros :: Int
generousWaitMicros = 5_000_000

shortWaitMicros :: Int
shortWaitMicros = 50_000

-- Run an admission attempt through the noop metrics port.
admit :: ServeAdmission -> IO a -> IO (Either Shed a)
admit = withServeAdmission noopMetricsPort

withOneSlot :: ServeAdmission -> (MVar () -> IO ()) -> IO ()
withOneSlot admission body = do
    holderGate <- newEmptyMVar
    holderIn <- newEmptyMVar
    withAsync (admit admission (putMVar holderIn () >> takeMVar holderGate)) $ \holder -> do
        takeMVar holderIn
        body holderGate
        wait holder `shouldReturn` Right ()

awaitWaiters :: ServeAdmission -> Int -> IO ()
awaitWaiters admission count = atomically (weightedAdmissionWaiting (saSlots admission) >>= check . (== count))

spec :: Spec
spec = around_ (timeout 10_000_000 >=> (`shouldBe` Just ())) $ around withSweeper $ describe "withServeAdmission -- brief-wait admission" $ do
    it "rejects non-positive capacity with the existing error" $ \deadlines ->
        newServeAdmission deadlines 0 `shouldThrow` (\(ErrorCall message) -> message == "ServeAdmission capacity must be positive")

    it "does not report a waiter before registration" $ \deadlines -> do
        admission <- newServeAdmissionTuned deadlines 1 1 generousWaitMicros
        timeout 10_000 (awaitWaiters admission 1) `shouldReturn` Nothing

    it "removes a cancelled waiter and admits a replacement" $ \deadlines -> do
        admission <- newServeAdmissionTuned deadlines 1 1 generousWaitMicros
        withOneSlot admission $ \holderGate ->
            withAsync (admit admission (pure ())) $ \waiter -> do
                awaitWaiters admission 1
                cancel waiter
                awaitWaiters admission 0
                withAsync (admit admission (pure ())) $ \replacement -> do
                    awaitWaiters admission 1
                    putMVar holderGate ()
                    wait replacement `shouldReturn` Right ()

    it "admits immediately below capacity and returns the action's result" $ \deadlines -> do
        admission <- newServeAdmission deadlines 2
        result <- admit admission (pure (42 :: Int))
        result `shouldBe` Right 42

    it "refuses instantly at capacity when the waiting room is zero (pure shed)" $ \deadlines -> do
        admission <- newServeAdmissionTuned deadlines 1 0 generousWaitMicros
        withOneSlot admission $ \holderGate -> do
            refused <- admit admission (pure ())
            refused `shouldBe` Left RoomFull
            putMVar holderGate ()

    it "queues at capacity and proceeds when a slot frees within the budget" $ \deadlines -> do
        admission <- newServeAdmissionTuned deadlines 1 1 generousWaitMicros
        withOneSlot admission $ \holderGate -> do
            withAsync (admit admission (pure (7 :: Int))) $ \waiter -> do
                awaitWaiters admission 1
                putMVar holderGate ()
                wait waiter `shouldReturn` Right 7

    it "refuses instantly when the waiting room is full" $ \deadlines -> do
        admission <- newServeAdmissionTuned deadlines 1 1 generousWaitMicros
        withOneSlot admission $ \holderGate -> do
            withAsync (admit admission (pure ())) $ \waiter -> do
                awaitWaiters admission 1
                -- The room is full, so admission refuses a third arrival without waiting out
                -- any budget. It must come back well inside the generous budget.
                refusal <- timeout 1_000_000 (admit admission (pure ()))
                refusal `shouldBe` Just (Left RoomFull)
                putMVar holderGate ()
                wait waiter `shouldReturn` Right ()

    it "sheds after the wait budget when no slot frees, and restores the room" $ \deadlines -> do
        admission <- newServeAdmissionTuned deadlines 1 1 shortWaitMicros
        withOneSlot admission $ \holderGate -> do
            firstTry <- admit admission (pure ())
            firstTry `shouldBe` Left WaitExpired
            -- The expired wait surrendered its room place, so a later arrival waits again
            -- rather than meeting a leaked-full room, and sheds the same way.
            secondTry <- admit admission (pure ())
            secondTry `shouldBe` Left WaitExpired
            putMVar holderGate ()

    it "releases on an exception, admitting a queued waiter" $ \deadlines -> do
        admission <- newServeAdmissionTuned deadlines 1 1 generousWaitMicros
        holderIn <- newEmptyMVar
        holderGate <- newEmptyMVar
        withAsync
            ( tryAny
                ( admit admission $ do
                    putMVar holderIn ()
                    takeMVar holderGate
                    throwIO (ErrorCall "holder failed") :: IO ()
                )
            )
            $ \holder -> do
                takeMVar holderIn
                withAsync (admit admission (pure (9 :: Int))) $ \waiter -> do
                    awaitWaiters admission 1
                    putMVar holderGate ()
                    wait waiter `shouldReturn` Right 9
                    outcome <- wait holder
                    outcome `shouldSatisfy` isLeft

    it "never exceeds capacity under a storm, and the room absorbs it" $ \deadlines -> do
        admission <- newServeAdmissionTuned deadlines 4 4 generousWaitMicros
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
        length (filter isRight results) `shouldSatisfy` (>= 8)

    it "releases the slot and re-raises when the queued observer throws" $ \deadlines -> do
        -- The queued record runs inside the release-protected region, so a throw must propagate to
        -- the caller, still return the slot, and leave the gauge balanced. Cover for a slot leak.
        gauge <- newTVarIO (0 :: Int)
        let throwingQueued =
                noopMetricsPort
                    { mpServeAdmissionInFlight = \delta -> atomically (modifyTVar' gauge (+ delta))
                    , mpServeAdmissionQueued = throwIO (ErrorCall "queued observer failed")
                    }
        admission <- newServeAdmissionTuned deadlines 1 1 generousWaitMicros
        withOneSlot admission $ \holderGate -> do
            withAsync (tryAny (withServeAdmission throwingQueued admission (pure ()))) $ \waiter -> do
                awaitWaiters admission 1
                putMVar holderGate () -- free the slot: the waiter acquires, then the record throws
                outcome <- wait waiter
                outcome `shouldSatisfy` isLeft -- the throw propagates, not a swallowed Nothing
                -- The slot came back despite the throw: a fresh acquire admits at once.
                admit admission (pure ()) `shouldReturn` Right ()
                readTVarIO gauge `shouldReturn` (0 :: Int)

    it "never runs the queued observer on the immediate-admit door path" $ \deadlines -> do
        -- Below capacity with no one waiting, admission takes the door path, which
        -- records no queue: a throwing queued hook must not fire there.
        let throwingQueued = noopMetricsPort{mpServeAdmissionQueued = throwIO (ErrorCall "must not run")}
        admission <- newServeAdmissionTuned deadlines 1 1 shortWaitMicros
        withServeAdmission throwingQueued admission (pure (5 :: Int)) `shouldReturn` Right 5

    it "ends a room wait only on a sweep of its deadlines" $ \swept -> do
        unswept <- newDeadlines
        stalled <- newServeAdmissionTuned unswept 1 1 shortWaitMicros
        withOneSlot stalled $ \holderGate -> do
            -- Nothing sweeps these deadlines, so the 50 ms wait is still open long after it passed.
            timeout 400_000 (admit stalled (pure ())) `shouldReturn` Nothing
            putMVar holderGate ()
        sweptRoom <- newServeAdmissionTuned swept 1 1 shortWaitMicros
        withOneSlot sweptRoom $ \holderGate -> do
            timeout 1_000_000 (admit sweptRoom (pure ())) `shouldReturn` Just (Left WaitExpired)
            putMVar holderGate ()
