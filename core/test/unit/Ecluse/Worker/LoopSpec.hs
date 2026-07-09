{-# OPTIONS_GHC -Wno-orphans #-}

module Ecluse.Worker.LoopSpec (spec) where

import Test.Hspec
import UnliftIO (timeout)

import Ecluse.Core.Worker (workerLoop, wrHeartbeat)
import Ecluse.Core.Worker.Liveness (lastPoll)
import Ecluse.Worker.Support

spec :: Spec
spec = do
    describe "workerLoop -- supervision (one bad iteration must not kill the loop)" $
        it "survives a throwing receive: catches, backs off, and polls again" $ do
            -- A persistently-failing queue: every poll throws. The loop is wrapped in
            -- tryAny, so a throwing iteration must be caught, logged, and retried after a
            -- backoff -- never escape and tear the worker thread down. The witness is the
            -- receive count: more than one call across the window proves the loop polled
            -- AGAIN after the first throw (it recovered), rather than dying on it.
            calls <- newIORef (0 :: Int)
            queue <- throwingReceiveQueue calls
            withQueueRuntime queue $ \runtime -> do
                -- The backoff after a failed iteration is ~1s, so a ~2.5s window admits a
                -- couple of attempts; assert at least a second poll occurred.
                _ <- timeout 2_500_000 (runWM runtime workerLoop)
                attempts <- readIORef calls
                attempts `shouldSatisfy` (>= 2)

    describe "workerLoop -- liveness (a fully-dead worker must fail the heartbeat)" $
        it "a persistently throwing receive never advances the heartbeat" $ do
            -- recordPoll runs only after a successful receive, so a worker that cannot
            -- poll at all keeps retrying (proven above) yet never advances the heartbeat:
            -- lastPoll stays Nothing across the window. The single-process /livez folds
            -- this heartbeat in, so a fully-dead worker eventually reads unhealthy and the
            -- orchestrator restarts the pod.
            calls <- newIORef (0 :: Int)
            queue <- throwingReceiveQueue calls
            withQueueRuntime queue $ \runtime -> do
                _ <- timeout 2_500_000 (runWM runtime workerLoop)
                lastPoll (wrHeartbeat runtime) `shouldReturn` Nothing
