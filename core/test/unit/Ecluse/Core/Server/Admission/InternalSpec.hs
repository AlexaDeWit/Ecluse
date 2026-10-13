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

import Ecluse.Core.Server.Admission.Internal (
    ServeAdmission,
    newServeAdmission,
    newServeAdmissionTuned,
    serveAdmissionWaiting,
    withServeAdmission,
 )
import Ecluse.Core.Telemetry.Record (MetricsPort (mpServeAdmissionInFlight, mpServeAdmissionQueued))
import Ecluse.Test.Port (noopMetricsPort)

{- | Wait budgets for the tuned handles. The generous budget lets a wait complete on a loaded
runner, and the short one lets a wait expire without slowing the suite.
-}
generousWaitMicros :: Int
generousWaitMicros = 5_000_000

shortWaitMicros :: Int
shortWaitMicros = 50_000

-- Run an admission attempt through the noop metrics port.
admit :: ServeAdmission -> IO a -> IO (Maybe a)
admit = withServeAdmission noopMetricsPort

withOneSlot :: ServeAdmission -> (MVar () -> IO ()) -> IO ()
withOneSlot admission body = do
    holderGate <- newEmptyMVar
    holderIn <- newEmptyMVar
    withAsync (admit admission (putMVar holderIn () >> takeMVar holderGate)) $ \holder -> do
        takeMVar holderIn
        body holderGate
        wait holder `shouldReturn` Just ()

awaitWaiters :: ServeAdmission -> Int -> IO ()
awaitWaiters admission count = atomically (serveAdmissionWaiting admission >>= check . (== count))

spec :: Spec
spec = around_ (timeout 10_000_000 >=> (`shouldBe` Just ())) $ describe "withServeAdmission -- brief-wait admission" $ do
    it "rejects non-positive capacity with the existing error" $
        newServeAdmission 0 `shouldThrow` (\(ErrorCall message) -> message == "ServeAdmission capacity must be positive")

    it "does not report a waiter before registration" $ do
        admission <- newServeAdmissionTuned 1 1 generousWaitMicros
        timeout 10_000 (awaitWaiters admission 1) `shouldReturn` Nothing

    it "removes a cancelled waiter and admits a replacement" $ do
        admission <- newServeAdmissionTuned 1 1 generousWaitMicros
        withOneSlot admission $ \holderGate ->
            withAsync (admit admission (pure ())) $ \waiter -> do
                awaitWaiters admission 1
                cancel waiter
                awaitWaiters admission 0
                withAsync (admit admission (pure ())) $ \replacement -> do
                    awaitWaiters admission 1
                    putMVar holderGate ()
                    wait replacement `shouldReturn` Just ()

    it "admits immediately below capacity and returns the action's result" $ do
        admission <- newServeAdmission 2
        result <- admit admission (pure (42 :: Int))
        result `shouldBe` Just 42

    it "refuses instantly at capacity when the waiting room is zero (pure shed)" $ do
        admission <- newServeAdmissionTuned 1 0 generousWaitMicros
        withOneSlot admission $ \holderGate -> do
            refused <- admit admission (pure ())
            refused `shouldBe` Nothing
            putMVar holderGate ()

    it "queues at capacity and proceeds when a slot frees within the budget" $ do
        admission <- newServeAdmissionTuned 1 1 generousWaitMicros
        withOneSlot admission $ \holderGate -> do
            withAsync (admit admission (pure (7 :: Int))) $ \waiter -> do
                awaitWaiters admission 1
                putMVar holderGate ()
                wait waiter `shouldReturn` Just 7

    it "refuses instantly when the waiting room is full" $ do
        admission <- newServeAdmissionTuned 1 1 generousWaitMicros
        withOneSlot admission $ \holderGate -> do
            withAsync (admit admission (pure ())) $ \waiter -> do
                awaitWaiters admission 1
                -- The room is full, so admission refuses a third arrival without waiting out
                -- any budget. It must come back well inside the generous budget.
                refusal <- timeout 1_000_000 (admit admission (pure ()))
                refusal `shouldBe` Just Nothing
                putMVar holderGate ()
                wait waiter `shouldReturn` Just ()

    it "sheds after the wait budget when no slot frees, and restores the room" $ do
        admission <- newServeAdmissionTuned 1 1 shortWaitMicros
        withOneSlot admission $ \holderGate -> do
            firstTry <- admit admission (pure ())
            firstTry `shouldBe` Nothing
            -- The expired wait surrendered its room place, so a later arrival waits again
            -- rather than meeting a leaked-full room, and sheds the same way.
            secondTry <- admit admission (pure ())
            secondTry `shouldBe` Nothing
            putMVar holderGate ()

    it "releases on an exception, admitting a queued waiter" $ do
        admission <- newServeAdmissionTuned 1 1 generousWaitMicros
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
        -- The queued record runs inside the release-protected region, so a throw must propagate to
        -- the caller, still return the slot, and leave the gauge balanced. Cover for a slot leak.
        gauge <- newTVarIO (0 :: Int)
        let throwingQueued =
                noopMetricsPort
                    { mpServeAdmissionInFlight = \delta -> atomically (modifyTVar' gauge (+ delta))
                    , mpServeAdmissionQueued = throwIO (ErrorCall "queued observer failed")
                    }
        admission <- newServeAdmissionTuned 1 1 generousWaitMicros
        withOneSlot admission $ \holderGate -> do
            withAsync (tryAny (withServeAdmission throwingQueued admission (pure ()))) $ \waiter -> do
                awaitWaiters admission 1
                putMVar holderGate () -- free the slot: the waiter acquires, then the record throws
                outcome <- wait waiter
                outcome `shouldSatisfy` isLeft -- the throw propagates, not a swallowed Nothing
                -- The slot came back despite the throw: a fresh acquire admits at once.
                admit admission (pure ()) `shouldReturn` Just ()
                readTVarIO gauge `shouldReturn` (0 :: Int)

    it "never runs the queued observer on the immediate-admit door path" $ do
        -- Below capacity with no one waiting, admission takes the door path, which
        -- records no queue: a throwing queued hook must not fire there.
        let throwingQueued = noopMetricsPort{mpServeAdmissionQueued = throwIO (ErrorCall "must not run")}
        admission <- newServeAdmissionTuned 1 1 shortWaitMicros
        withServeAdmission throwingQueued admission (pure (5 :: Int)) `shouldReturn` Just 5
