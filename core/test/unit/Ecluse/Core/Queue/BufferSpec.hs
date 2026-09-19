-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Queue.BufferSpec (spec) where

import Test.Hspec
import UnliftIO (withAsync)

import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent),
    MirrorJob,
    MirrorQueue (..),
    defaultDeliveryBudget,
 )
import Ecluse.Core.Queue.Buffer (
    newEnqueueBuffer,
 )
import Ecluse.Test.Poll (awaitUntil)
import Ecluse.Test.Queue (otherJob, sampleJob, thirdJob)
import Ecluse.Test.Support (expectRightIO)

-- | Tests the contract module's buffered producer hand-off.
spec :: Spec
spec = do
    describe "newEnqueueBuffer" $ do
        it "delivers handed-off jobs to the backend in order" $ do
            delivered <- newIORef []
            (q, drainLoop) <- newEnqueueBuffer 8 (const pass) (\_ _ -> pass) (recordingBackend delivered)
            withAsync drainLoop $ \_ -> do
                traverse_ (expectRightIO . enqueue q) [sampleJob, otherJob, thirdJob]
                drained ((== (3 :: Int)) . length <$> readIORef delivered)
            readIORef delivered `shouldReturn` [sampleJob, otherJob, thirdJob]

        it "drops the newest hand-off at the cap, reporting every drop's running total" $ do
            -- The drain loop deliberately never runs, so the buffer stays full at its depth and
            -- every further hand-off is a drop. The callback fires on every drop. Rate-limiting is
            -- the caller's job.
            delivered <- newIORef []
            drops <- newIORef []
            (q, _drainLoop) <- newEnqueueBuffer 2 (\n -> modifyIORef' drops (<> [n])) (\_ _ -> pass) (recordingBackend delivered)
            traverse_ (expectRightIO . enqueue q) [sampleJob, otherJob, thirdJob, thirdJob]
            readIORef drops `shouldReturn` [1, 2]
            readIORef delivered `shouldReturn` [] -- nothing drained, nothing delivered
        it "keeps draining past a backend delivery fault, reporting its total and detail" $ do
            delivered <- newIORef []
            failures <- newIORef []
            failFirst <- newIORef True
            let flaky job = do
                    failNow <- atomicModifyIORef' failFirst (False,)
                    if failNow
                        then pure (Left (transportFault TransportUnreachable "backend unavailable"))
                        else Right () <$ modifyIORef' delivered (<> [job])
            (q, drainLoop) <-
                newEnqueueBuffer
                    8
                    (const pass)
                    (\n detail -> modifyIORef' failures (<> [(n, detail)]))
                    (recordingBackend delivered){enqueue = flaky}
            withAsync drainLoop $ \_ -> do
                traverse_ (expectRightIO . enqueue q) [sampleJob, otherJob]
                drained ((== (1 :: Int)) . length <$> readIORef delivered)
            -- The typed fault's detail arrives verbatim on the failure callback.
            readIORef failures `shouldReturn` [(1, "backend unavailable")]
            readIORef delivered `shouldReturn` [otherJob] -- the loop survived the failure
  where
    -- A backend stub recording what the buffer's drain loop delivered, and in what order. Its
    -- consumer fields are inert.
    recordingBackend :: IORef [MirrorJob] -> MirrorQueue
    recordingBackend delivered =
        MirrorQueue
            { enqueue = \job -> Right () <$ modifyIORef' delivered (<> [job])
            , receive = pure (Right [])
            , ack = const (pure (Right ()))
            , extendVisibility = \_ _ -> pure (Right ())
            , deadLetter = const (pure (Right ()))
            , deliveryBudget = defaultDeliveryBudget
            , deadLetterTerminus = Right TerminusAbsent
            }

    -- A broken drain loop fails the test loudly rather than hanging the suite.
    drained :: IO Bool -> IO ()
    drained cond = awaitUntil 2_000_000 1_000 cond `shouldReturn` True
