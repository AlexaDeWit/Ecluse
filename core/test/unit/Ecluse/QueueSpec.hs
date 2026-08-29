-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.QueueSpec (spec) where

import System.Timeout (timeout)
import Test.Hspec
import UnliftIO (withAsync)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent, TerminusAttached),
    DeliveryBudget (DeliveryBudget),
    MirrorJob,
    MirrorQueue (..),
    QueueMessage (QueueMessage, msgJob, msgReceipt, msgReceiveCount),
    defaultDeliveryBudget,
    deliveryBudgetSpent,
    effectiveDeliveryBudget,
    mkReceiptHandle,
    newEnqueueBuffer,
 )
import Ecluse.Queue.Support (otherJob, thirdJob, unwrap)
import Ecluse.Test.Queue (sampleJob)

{- | Tests for the contract module's buffered producer hand-off. The in-memory
backend's coverage lives beside it in "Ecluse.Queue.MemorySpec".
-}
spec :: Spec
spec = do
    describe "newEnqueueBuffer" $ do
        it "delivers handed-off jobs to the backend in order" $ do
            delivered <- newIORef []
            (q, drainLoop) <- newEnqueueBuffer 8 (const pass) (\_ _ -> pass) (recordingBackend delivered)
            withAsync drainLoop $ \_ -> do
                traverse_ (unwrap . enqueue q) [sampleJob, otherJob, thirdJob]
                awaitUntil ((== (3 :: Int)) . length <$> readIORef delivered)
            readIORef delivered `shouldReturn` [sampleJob, otherJob, thirdJob]

        it "drops the newest hand-off at the cap, reporting every drop's running total" $ do
            -- The drain loop deliberately never runs, so the buffer stays full at its depth and
            -- every further hand-off is a drop. The callback fires on every drop. Rate-limiting is
            -- the caller's job.
            delivered <- newIORef []
            drops <- newIORef []
            (q, _drainLoop) <- newEnqueueBuffer 2 (\n -> modifyIORef' drops (<> [n])) (\_ _ -> pass) (recordingBackend delivered)
            traverse_ (unwrap . enqueue q) [sampleJob, otherJob, thirdJob, thirdJob]
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
                traverse_ (unwrap . enqueue q) [sampleJob, otherJob]
                awaitUntil ((== (1 :: Int)) . length <$> readIORef delivered)
            -- The typed fault's detail arrives verbatim on the failure callback.
            readIORef failures `shouldReturn` [(1, "backend unavailable")]
            readIORef delivered `shouldReturn` [otherJob] -- the loop survived the failure
    describe "deliveryBudgetSpent -- the shared redelivery verdict" $ do
        it "grants every delivery below the budget" $
            map (deliveryBudgetSpent (DeliveryBudget 5) . deliveredTimes) [1, 2, 3, 4]
                `shouldBe` [False, False, False, False]

        it "is spent at the budget, and stays spent past it" $
            map (deliveryBudgetSpent (DeliveryBudget 5) . deliveredTimes) [5, 6, 50]
                `shouldBe` [True, True, True]

        it "still grants a first delivery under any budget, however small" $
            -- A budget of one (or zero, or below) would otherwise retire a job that never
            -- ran. The worker retires no message before it tries that message at least once.
            map (\budget -> deliveryBudgetSpent budget (deliveredTimes 1)) [DeliveryBudget 1, DeliveryBudget 0, DeliveryBudget (-3)]
                `shouldBe` [False, False, False]

        it "retires on the second delivery under a budget too small to reach" $
            map (\budget -> deliveryBudgetSpent budget (deliveredTimes 2)) [DeliveryBudget 1, DeliveryBudget 0]
                `shouldBe` [True, True]

    describe "effectiveDeliveryBudget -- the dead-letter queue captures first" $ do
        it "raises the configured floor one delivery past an attached terminus's capture count" $
            -- Écluse must not retire the message at the configured 5 and rob the
            -- dead-letter queue: the operator's redrive policy captures it at 10.
            effectiveDeliveryBudget (DeliveryBudget 5) (TerminusAttached (Just (DeliveryBudget 10)))
                `shouldBe` DeliveryBudget 11

        it "keeps the configured floor when it already sits above the capture count" $
            effectiveDeliveryBudget (DeliveryBudget 20) (TerminusAttached (Just (DeliveryBudget 3)))
                `shouldBe` DeliveryBudget 20

        it "keeps the configured floor when a terminus declares no capture count" $
            effectiveDeliveryBudget (DeliveryBudget 5) (TerminusAttached Nothing)
                `shouldBe` DeliveryBudget 5

        it "keeps the configured floor when nothing captures poison messages" $
            -- The no-terminus case the budget exists for: the budget is the only terminus.
            effectiveDeliveryBudget (DeliveryBudget 5) TerminusAbsent `shouldBe` DeliveryBudget 5
  where
    -- A delivery of the sample job on its n-th receive. These verdicts read only the
    -- count, so the rest of the message stays fixed.
    deliveredTimes :: Int -> QueueMessage
    deliveredTimes n =
        QueueMessage{msgJob = sampleJob, msgReceipt = mkReceiptHandle "receipt", msgReceiveCount = n}

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

    -- Poll (1ms cadence) until the condition holds, bounded at 2s so a broken
    -- drain loop fails the test loudly rather than hanging the suite.
    awaitUntil :: IO Bool -> IO ()
    awaitUntil cond = do
        outcome <- timeout 2_000_000 wait
        outcome `shouldBe` Just ()
      where
        wait = unlessM cond (threadDelay 1_000 *> wait)
