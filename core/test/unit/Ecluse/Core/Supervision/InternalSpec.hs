-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Supervision delay, logging, and fault propagation contracts.
module Ecluse.Core.Supervision.InternalSpec (spec) where

import Control.Retry (simulatePolicy)
import Data.Text qualified as T
import Network.HTTP.Types.Status (status503)
import Test.Hspec
import UnliftIO (timeout)
import UnliftIO.Async (asyncWithUnmask, cancel, waitCatch)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Fault (TransportCause (TransportUnreachable), renderTransportCause)
import Ecluse.Core.Supervision.Internal (
    BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros),
    FaultDisposition (Permanent, Transient),
    SupervisionPolicy (spBackoff, spClassify, spLabel),
    backoffMicros,
    delayListPolicy,
    superviseLoop,
    superviseLoopWithDelay,
    transientPolicy,
 )
import Ecluse.Test.Log (runJsonLog, runQuietKatip)
import Ecluse.Test.Stub (redirectSecrets, redirectedStatusException, refusedFetch, requestSecrets, signedRequest)
import Ecluse.Test.Supervision (fastPolicy, superviseBriefly)

-- | A typed fault for the loop under test to throw, never stringly.
newtype StepFault = StepFault Text
    deriving stock (Eq, Show)

instance Exception StepFault

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

    describe "delayListPolicy, a delay list as a retry policy" $ do
        -- 'simulatePolicy' walks the policy without sleeping. The list's length is the
        -- retry budget, and the policy yields 'Nothing' once it runs out.
        it "waits the nth delay at the nth retry, then stops" $ do
            delays <- simulatePolicy 2 (delayListPolicy [100_000, 250_000])
            map snd delays `shouldBe` [Just 100_000, Just 250_000, Nothing]

        it "an empty list admits no retry (the single initial attempt only)" $ do
            delays <- simulatePolicy 0 (delayListPolicy [])
            map snd delays `shouldBe` [Nothing]

    describe "transientPolicy" $
        it "classifies every synchronous fault as transient, at the given pace" $ do
            let schedule = BackoffSchedule{bsBaseMicros = 100, bsCapMicros = 1_000}
                policy = transientPolicy "shell-loop" schedule
            spLabel policy `shouldBe` "shell-loop"
            spBackoff policy `shouldBe` schedule
            spClassify policy (toException (StepFault "residue")) `shouldBe` Transient

    describe "superviseLoop" $ do
        it "reruns the step after a transient fault (log, back off, continue)" $ do
            calls <- newIORef (0 :: Int)
            let step = do
                    atomicModifyIORef' calls (\n -> (n + 1, ()))
                    throwIO (StepFault "still down")
            _ <- timeout 200_000 (runQuietKatip (superviseLoop (fastPolicy (const Transient)) step))
            attempts <- readIORef calls
            attempts `shouldSatisfy` (>= 3)

        it "requests the base delay again after a successful step" $
            requestedDelays [False, True, False] `shouldReturn` [1_000, 1_000]

        it "doubles consecutive fault delays up to the cap" $
            requestedDelays (replicate 6 False) `shouldReturn` [1_000, 2_000, 4_000, 8_000, 8_000, 8_000]

        it "rethrows a permanent fault after one attempt (fail up, no retry)" $ do
            calls <- newIORef (0 :: Int)
            let step = do
                    atomicModifyIORef' calls (\n -> (n + 1, ()))
                    throwIO (StepFault "wiring fault")
            outcome <- try (runQuietKatip (superviseLoop (fastPolicy (const Permanent)) step))
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
            outcome <- try (runQuietKatip (superviseLoop (fastPolicy classify) step))
            case outcome of
                Left fault -> fromException fault `shouldBe` Just (StepFault "wiring fault")
                Right v -> absurd v
            readIORef calls `shouldReturn` 3

        -- The pair below pins both arms, so neither negative assertion is vacuous: the
        -- scribe demonstrably renders "sev":"Error" when a line earns it.
        it "warns on a transient fault, so a self-healing retry never pages an operator" $ do
            let step = throwIO (StepFault "still down")
            logged <-
                runJsonLog . void . timeout 200_000 $
                    superviseLoop (fastPolicy (const Transient)) step
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
            logged `shouldSatisfy` T.isInfixOf "iteration faulted"
            logged `shouldSatisfy` (not . T.isInfixOf "\"sev\":\"Error\"")

        it "errors on a permanent fault, the one that takes the process down" $ do
            let step = throwIO (StepFault "wiring fault")
            logged <- runJsonLog $ do
                outcome <- try (superviseLoop (fastPolicy (const Permanent)) step)
                case outcome of
                    Left fault -> liftIO (fromException fault `shouldBe` Just (StepFault "wiring fault"))
                    Right v -> absurd v
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Error\""
            logged `shouldSatisfy` T.isInfixOf "permanent fault, failing up"

        it "never absorbs cancellation: a cancelled loop dies like any other thread" $ do
            entered <- newEmptyMVar
            loop <- asyncWithUnmask $ \unmask ->
                unmask . runQuietKatip . superviseLoop (fastPolicy (const Transient)) $ do
                    putMVar entered ()
                    threadDelay 10_000_000
            takeMVar entered
            cancel loop
            outcome <- waitCatch loop
            case outcome of
                Left _cancelled -> pass
                Right v -> absurd v

    describe "superviseLoop, rendering an HTTP fault" $ do
        for_ [Transient, Permanent] $ \disposition ->
            it ("renders a redirected status by its code at the dialled host:port, never the request or the response, " <> show disposition) $ do
                (authority, err) <- redirectedStatusException status503
                logged <- runJsonLog (superviseBriefly disposition (throwIO err))
                logged `shouldSatisfy` T.isInfixOf (authority <> ": HTTP 503")
                for_ redirectSecrets $ \secret -> logged `shouldSatisfy` (not . T.isInfixOf secret)

        it "renders a refused connection by the client's reason at the dialled host:port, then its cause, never the request" $ do
            refused <- refusedFetch =<< signedRequest
            logged <- runJsonLog (superviseBriefly Transient (throwIO refused))
            logged `shouldSatisfy` T.isInfixOf "127.0.0.1:1: ConnectionFailure: "
            logged `shouldSatisfy` T.isInfixOf ("(" <> renderTransportCause TransportUnreachable <> ")")
            for_ requestSecrets $ \secret -> logged `shouldSatisfy` (not . T.isInfixOf secret)

requestedDelays :: [Bool] -> IO [Int]
requestedDelays outcomes = do
    remaining <- newIORef outcomes
    delays <- newIORef []
    let step = do
            next <- atomicModifyIORef' remaining $ \case
                [] -> ([], Nothing)
                value : rest -> (rest, Just value)
            case next of
                Nothing -> throwIO (StepFault "finished")
                Just False -> throwIO (StepFault "transient")
                Just True -> pure ()
        classify fault = case fromException fault of
            Just (StepFault "finished") -> Permanent
            _ -> Transient
        recordDelay micros = atomicModifyIORef' delays (\seen -> (micros : seen, ()))
    outcome <- try (runQuietKatip (superviseLoopWithDelay recordDelay (fastPolicy classify) step))
    case outcome of
        Left fault -> fault `shouldBe` StepFault "finished"
        Right value -> absurd value
    reverse <$> readIORef delays
