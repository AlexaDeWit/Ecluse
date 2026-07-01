{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-orphans #-}

module Ecluse.Worker.LoopSpec (spec) where

import Test.Hspec
import UnliftIO (timeout)

import Ecluse.Core.Worker (workerLoop)
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
