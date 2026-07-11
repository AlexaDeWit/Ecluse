-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.SupervisionSpec (spec) where

import Katip (Environment (Environment), KatipContextT, Namespace (Namespace), SimpleLogPayload, initLogEnv, runKatipContextT)
import Test.Hspec
import UnliftIO (timeout)
import UnliftIO.Async (asyncWithUnmask, cancel, waitCatch)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Supervision (
    BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros),
    FaultDisposition (Permanent, Transient),
    SupervisionPolicy (SupervisionPolicy, spBackoff, spClassify, spLabel),
    backoffMicros,
    superviseLoop,
 )

-- | A typed fault for the loop under test to throw; never stringly.
newtype StepFault = StepFault Text
    deriving stock (Eq, Show)

instance Exception StepFault

-- | Run a Katip-constrained action against a scribe-less environment.
runQuiet :: KatipContextT IO a -> IO a
runQuiet action = do
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty action

-- | A policy over the given classifier with a tiny backoff, so a test never sleeps long.
fastPolicy :: (SomeException -> FaultDisposition) -> SupervisionPolicy
fastPolicy classify =
    SupervisionPolicy
        { spLabel = "test-loop"
        , spClassify = classify
        , spBackoff = BackoffSchedule{bsBaseMicros = 1_000, bsCapMicros = 8_000}
        }

spec :: Spec
spec = do
    describe "backoffMicros" $ do
        it "doubles from the base towards the cap, then saturates" $ do
            let schedule = BackoffSchedule{bsBaseMicros = 100, bsCapMicros = 1_000}
            map (backoffMicros schedule) [0 .. 5] `shouldBe` [100, 200, 400, 800, 1_000, 1_000]

        it "a base equal to the cap is a fixed-interval retry" $ do
            let schedule = BackoffSchedule{bsBaseMicros = 500, bsCapMicros = 500}
            map (backoffMicros schedule) [0, 1, 7] `shouldBe` [500, 500, 500]

        it "the exponent clamp keeps a huge failure count finite (constant past the clamp, never negative)" $ do
            let schedule = BackoffSchedule{bsBaseMicros = 100, bsCapMicros = 30_000_000}
            backoffMicros schedule 10_000 `shouldBe` backoffMicros schedule 12
            backoffMicros schedule 10_000 `shouldSatisfy` (> 0)

    describe "superviseLoop" $ do
        it "reruns the step after a transient fault (log, back off, continue)" $ do
            calls <- newIORef (0 :: Int)
            let step = do
                    atomicModifyIORef' calls (\n -> (n + 1, ()))
                    throwIO (StepFault "still down")
            _ <- timeout 200_000 (runQuiet (superviseLoop (fastPolicy (const Transient)) step))
            attempts <- readIORef calls
            attempts `shouldSatisfy` (>= 3)

        it "a completed step resets the backoff (alternating fault and success stays at the base delay)" $ do
            -- Alternate fault and success. Were the backoff not reset by the
            -- successful steps, ~15 faults would pace at 1ms, 2ms, 4ms ... 8ms
            -- (the cap) and the window could not fit them; reset, every retry
            -- waits only the 1ms base, so the window fits comfortably.
            calls <- newIORef (0 :: Int)
            let step = do
                    n <- atomicModifyIORef' calls (\k -> (k + 1, k + 1))
                    when (odd n) (throwIO (StepFault "odd blip"))
            _ <- timeout 300_000 (runQuiet (superviseLoop (fastPolicy (const Transient)) step))
            attempts <- readIORef calls
            attempts `shouldSatisfy` (>= 30)

        it "rethrows a permanent fault after one attempt (fail up, no retry)" $ do
            calls <- newIORef (0 :: Int)
            let step = do
                    atomicModifyIORef' calls (\n -> (n + 1, ()))
                    throwIO (StepFault "wiring fault")
            outcome <- try (runQuiet (superviseLoop (fastPolicy (const Permanent)) step))
            case outcome of
                Left fault -> fromException fault `shouldBe` Just (StepFault "wiring fault")
                Right v -> absurd v
            readIORef calls `shouldReturn` 1

        it "classification decides per fault: transient faults are absorbed until the permanent one" $ do
            calls <- newIORef (0 :: Int)
            let step = do
                    n <- atomicModifyIORef' calls (\k -> (k + 1, k + 1))
                    if n < 3
                        then throwIO (StepFault "transient blip")
                        else throwIO (StepFault "wiring fault")
                classify fault = case fromException fault of
                    Just (StepFault "wiring fault") -> Permanent
                    _ -> Transient
            outcome <- try (runQuiet (superviseLoop (fastPolicy classify) step))
            case outcome of
                Left fault -> fromException fault `shouldBe` Just (StepFault "wiring fault")
                Right v -> absurd v
            readIORef calls `shouldReturn` 3

        it "never absorbs cancellation: a cancelled loop dies like any other thread" $ do
            entered <- newEmptyMVar
            loop <- asyncWithUnmask $ \unmask ->
                unmask . runQuiet . superviseLoop (fastPolicy (const Transient)) $ do
                    putMVar entered ()
                    threadDelay 10_000_000
            takeMVar entered
            cancel loop
            outcome <- waitCatch loop
            case outcome of
                Left _cancelled -> pass
                Right v -> absurd v
