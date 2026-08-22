-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
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
            -- Every poll reports the handle's typed 'QueueFault'. The loop must log it and retry
            -- after a backoff, never escape and tear the worker thread down. More than one receive
            -- proves it retried.
            calls <- newIORef (0 :: Int)
            queue <- faultingReceiveQueue calls
            withQueueRuntime queue $ \runtime -> do
                -- The backoff after a failed iteration is ~1s, so a ~2.5s window admits a
                -- couple of attempts. Assert that at least a second poll occurred.
                _ <- timeout 2_500_000 (runWM runtime (workerLoop testSupervision))
                attempts <- readIORef calls
                attempts `shouldSatisfy` (>= 2)

        it "survives residue: a receive that throws past its typed contract is caught, backed off, and retried" $ do
            -- The handle contract reports every backend failure as a value, so a throwing receive
            -- is an invariant break. The residual tryAny must contain it: one broken invariant
            -- cannot kill the worker.
            calls <- newIORef (0 :: Int)
            queue <- throwingReceiveQueue calls
            withQueueRuntime queue $ \runtime -> do
                _ <- timeout 2_500_000 (runWM runtime (workerLoop testSupervision))
                attempts <- readIORef calls
                attempts `shouldSatisfy` (>= 2)

    describe "workerLoop -- liveness (a fully-dead worker must fail the heartbeat)" $
        it "a persistently faulting receive never advances the heartbeat" $ do
            -- The heartbeat advances only on progress, so a worker that cannot poll at all leaves
            -- lastPoll at 'Nothing'. /livez folds it in, so a fully-dead worker reads unhealthy and
            -- the pod restarts.
            calls <- newIORef (0 :: Int)
            queue <- faultingReceiveQueue calls
            withQueueRuntime queue $ \runtime -> do
                _ <- timeout 2_500_000 (runWM runtime (workerLoop testSupervision))
                lastPoll (wrHeartbeat runtime) `shouldReturn` Nothing
