-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Queue.MemorySpec (spec) where

import System.Timeout (timeout)
import Test.Hspec

import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent),
    MirrorJob (..),
    MirrorQueue (..),
    QueueMessage (..),
 )
import Ecluse.Core.Queue.Memory (
    MemoryQueueConfig (..),
    memoryQueueDropReportInterval,
    newBoundedInMemoryQueue,
 )
import Ecluse.Queue.Support (otherJob, sampleJob, thirdJob, unwrap)

spec :: Spec
spec = do
    describe "newBoundedInMemoryQueue" $ do
        it "returns [] on an idle queue within the poll window (never blocks forever)" $ do
            -- The worker advances its heartbeat only when receive returns, so an idle receive
            -- must return [] within its 50ms window. The 2s timeout fails loudly if it blocks.
            (q, _drops) <- boundedQueue 4
            result <- timeout 2_000_000 (unwrap (receive q))
            result `shouldBe` Just []

        it "carries a job from enqueue through receive to ack (round-trip)" $ do
            -- A cap well above the one job, so nothing is dropped: the job arrives
            -- unchanged and ack (a no-op on this backend) completes without error.
            (q, _drops) <- boundedQueue 10
            unwrap (enqueue q sampleJob)
            [msg] <- unwrap (receive q)
            msgJob msg `shouldBe` sampleJob
            unwrap (ack q (msgReceipt msg))

        it "dead-letters a received job without redelivering it (the memory terminus is a drop; issue #846)" $ do
            -- This backend has no dead-letter queue, so a terminal fault is the drop a delivered
            -- job already is. It never redelivers.
            (q, _drops) <- boundedQueue 10
            unwrap (enqueue q sampleJob)
            [msg] <- unwrap (receive q)
            unwrap (deadLetter q (msgReceipt msg))
            afterDeadLetter <- unwrap (receive q)
            afterDeadLetter `shouldBe` []

        it "reports every delivery as a first delivery, so the redelivery budget never bites" $ do
            -- This backend removes a job at delivery, so every delivery is a first delivery.
            -- The truthful count of 1 keeps the worker's redelivery budget inert here.
            (q, _drops) <- boundedQueue 10
            unwrap (enqueue q sampleJob)
            unwrap (enqueue q otherJob)
            delivered <- unwrap (receive q)
            map msgReceiveCount delivered `shouldBe` [1, 1]

        it "reports that it has no dead-letter terminus" $ do
            -- The answer is honest and load-bearing. The composition root reads it to
            -- decide its boot warning, and the memory backend captures nothing.
            (q, _drops) <- boundedQueue 10
            deadLetterTerminus q `shouldBe` Right TerminusAbsent

        it "carries every job field through unchanged from enqueue to receive" $ do
            -- Assert field by field rather than on the whole record, so a regression names the
            -- single field the queue mangled.
            (q, _drops) <- boundedQueue 10
            unwrap (enqueue q sampleJob)
            [msg] <- unwrap (receive q)
            let job = msgJob msg
            jobPackage job `shouldBe` jobPackage sampleJob
            jobVersion job `shouldBe` jobVersion sampleJob
            jobArtifactUrl job `shouldBe` jobArtifactUrl sampleJob
            jobArtifactFilename job `shouldBe` jobArtifactFilename sampleJob

        it "delivers jobs in FIFO order" $ do
            (q, _drops) <- boundedQueue 10
            unwrap (enqueue q sampleJob)
            unwrap (enqueue q otherJob)
            received <- drain q
            received `shouldBe` [sampleJob, otherJob]

        it "drops the newest enqueue at the cap and keeps the earlier jobs" $ do
            (q, drops) <- boundedQueue 2
            traverse_ (unwrap . enqueue q) [sampleJob, otherJob, thirdJob]
            received <- map msgJob <$> unwrap (receive q)
            received `shouldBe` [sampleJob, otherJob]
            -- The queue always reports the first overflow.
            readIORef drops `shouldReturn` [1]

        it "honours the cap under a flood far larger than it" $ do
            -- Many enqueues into a tiny cap retain at most 'cap' jobs, so memory stays
            -- hard bounded. The queue drops the rest and reports at least the first drop.
            (q, drops) <- boundedQueue 2
            traverse_ (unwrap . enqueue q) (replicate 5 sampleJob)
            received <- unwrap (receive q)
            length received `shouldBe` 2
            readIORef drops `shouldReturn` [1]

        it "reports the first drop then every interval-th, rate-limiting a flood" $ do
            -- A sustained flood must not spam the log, so the queue reports the first drop and
            -- every 'memoryQueueDropReportInterval'-th drop after it, with the running total.
            (q, drops) <- boundedQueue 1
            unwrap (enqueue q sampleJob) -- fills the single slot: nothing receives it
            traverse_ (unwrap . enqueue q) (replicate memoryQueueDropReportInterval sampleJob)
            readIORef drops `shouldReturn` [1, memoryQueueDropReportInterval]
  where
    -- A bounded queue at the given cap, plus an 'IORef' of the running drop totals its
    -- callback saw. Its poll window is 50ms, against a production default of about 20s.
    boundedQueue :: Int -> IO (MirrorQueue, IORef [Int])
    boundedQueue cap = do
        drops <- newIORef []
        let cfg = MemoryQueueConfig{memQueueMaxDepth = cap, memQueuePollWaitMicros = 50_000}
        q <- newBoundedInMemoryQueue cfg (\n -> modifyIORef' drops (<> [n]))
        pure (q, drops)

    -- Receive repeatedly, acking everything, until the queue is empty. Returns the
    -- jobs in delivery order. Total: it stops as soon as a receive yields nothing.
    drain :: MirrorQueue -> IO [MirrorJob]
    drain q = go []
      where
        go acc = do
            msgs <- unwrap (receive q)
            case msgs of
                [] -> pure (reverse acc)
                _ -> do
                    traverse_ (unwrap . ack q . msgReceipt) msgs
                    go (reverse (map msgJob msgs) <> acc)
