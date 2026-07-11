{-# OPTIONS_GHC -Wno-orphans #-}

module Ecluse.Worker.LoopSpec (spec) where

import Test.Hspec
import UnliftIO (timeout)

import Ecluse.Core.Worker (workerLoop, wrHeartbeat)
import Ecluse.Core.Worker.Liveness (lastPoll)
import Ecluse.Worker.Support

spec :: Spec
spec = do
    describe "workerLoop -- supervision (one bad iteration must not kill the loop)" $ do
        it "survives a faulting receive: logs the typed fault, backs off, and polls again" $ do
            -- A persistently-failing backend: every poll reports the handle's typed
            -- 'QueueFault'. The loop's value branch must log it and retry after a
            -- backoff -- never escape and tear the worker thread down. The witness is
            -- the receive count: more than one call across the window proves the loop
            -- polled AGAIN after the first fault (it recovered), rather than dying.
            calls <- newIORef (0 :: Int)
            queue <- faultingReceiveQueue calls
            withQueueRuntime queue $ \runtime -> do
                -- The backoff after a failed iteration is ~1s, so a ~2.5s window admits a
                -- couple of attempts; assert at least a second poll occurred.
                _ <- timeout 2_500_000 (runWM runtime workerLoop)
                attempts <- readIORef calls
                attempts `shouldSatisfy` (>= 2)

        it "survives residue: a receive that throws past its typed contract is caught, backed off, and retried" $ do
            -- The handle contract reports every backend failure as a value, so a
            -- throwing receive is an invariant break. The loop's residual tryAny must
            -- still contain it -- caught, logged, backed off, polled again -- so one
            -- broken invariant cannot kill the worker thread.
            calls <- newIORef (0 :: Int)
            queue <- throwingReceiveQueue calls
            withQueueRuntime queue $ \runtime -> do
                _ <- timeout 2_500_000 (runWM runtime workerLoop)
                attempts <- readIORef calls
                attempts `shouldSatisfy` (>= 2)

    describe "workerLoop -- liveness (a fully-dead worker must fail the heartbeat)" $
        it "a persistently faulting receive never advances the heartbeat" $ do
            -- recordPoll runs only after a successful receive, so a worker that cannot
            -- poll at all keeps retrying (proven above) yet never advances the heartbeat:
            -- lastPoll stays Nothing across the window. The single-process /livez folds
            -- this heartbeat in, so a fully-dead worker eventually reads unhealthy and the
            -- orchestrator restarts the pod.
            calls <- newIORef (0 :: Int)
            queue <- faultingReceiveQueue calls
            withQueueRuntime queue $ \runtime -> do
                _ <- timeout 2_500_000 (runWM runtime workerLoop)
                lastPoll (wrHeartbeat runtime) `shouldReturn` Nothing
