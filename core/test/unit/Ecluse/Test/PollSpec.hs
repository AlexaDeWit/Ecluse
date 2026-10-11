-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Supervised fixture waits fail when progress never arrives.
module Ecluse.Test.PollSpec (spec) where

import Test.Hspec
import UnliftIO.Exception (finally, throwIO)

import Ecluse.Test.Poll (runUntil)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

-- | Regressions for timeout, worker failure, and cancellation of the losing branch.
spec :: Spec
spec = describe "runUntil" $ do
    it "fails rather than accepting an exhausted progress wait" $ do
        blocked <- newEmptyMVar
        runUntil 10_000 1_000 (takeMVar blocked) (pure False)
            `shouldThrow` (== TestContractEscape "worker progress timed out")
    it "fails when the worker returns before progress" $
        runUntil 1_000_000 1_000 pass (pure False)
            `shouldThrow` (== TestContractEscape "worker stopped before progress")
    it "propagates a worker failure" $
        runUntil 1_000_000 1_000 (throwIO (TestContractEscape "worker")) (pure False)
            `shouldThrow` (== TestContractEscape "worker")
    it "cancels the worker and runs its cleanup after progress" $ do
        started <- newEmptyMVar
        blocked <- newEmptyMVar
        cleaned <- newIORef False
        let worker = (putMVar started () >> takeMVar blocked) `finally` writeIORef cleaned True
        runUntil 1_000_000 1_000 worker (isJust <$> tryReadMVar started)
        readIORef cleaned `shouldReturn` True
