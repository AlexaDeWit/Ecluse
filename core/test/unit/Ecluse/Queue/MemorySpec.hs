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
            -- The load-bearing liveness property: the worker advances its heartbeat
            -- only when receive returns. An idle receive must return [] (a healthy
            -- empty poll) within its bounded window rather than block indefinitely.
            -- The helper uses a 50ms window. The 2s timeout fails loudly if receive
            -- ever reverts to blocking forever.
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
            -- The in-memory backend has no dead-letter queue, so a terminal fault is
            -- the drop a delivered job already is. The deadLetter call succeeds and the
            -- message does not reappear: this backend never redelivers.
            (q, _drops) <- boundedQueue 10
            unwrap (enqueue q sampleJob)
            [msg] <- unwrap (receive q)
            unwrap (deadLetter q (msgReceipt msg))
            afterDeadLetter <- unwrap (receive q)
            afterDeadLetter `shouldBe` []

        it "reports every delivery as a first delivery, so the redelivery budget never bites" $ do
            -- This backend removes a job at delivery and never redelivers it, so every
            -- delivery it makes is a first delivery. A truthful count of one keeps the
            -- worker's budget inert here.
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
            -- The queue is a transparent carrier: each field the producer set must
            -- arrive on the consumer side byte-for-byte. Assert field by field through
            -- the 'MirrorJob' selectors, not on the whole record, so a regression names
            -- the single field it mangled.
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
            -- The load-bearing bound: at the cap the queue rejects a fresh enqueue
            -- (drop-newest). It then holds exactly the first 'cap' jobs, and the
            -- overflowing newest one never arrives.
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
            -- A sustained flood must not spam the log. The queue reports the first drop
            -- and every 'memoryQueueDropReportInterval'-th drop after it, each carrying
            -- the running total, so log volume stays bounded under load.
            (q, drops) <- boundedQueue 1
            unwrap (enqueue q sampleJob) -- fills the single slot: nothing receives it
            traverse_ (unwrap . enqueue q) (replicate memoryQueueDropReportInterval sampleJob)
            readIORef drops `shouldReturn` [1, memoryQueueDropReportInterval]
  where
    -- A bounded in-memory queue at the given cap, paired with an 'IORef' recording the
    -- running drop totals its drop callback saw, in order. A test can then assert both
    -- the cap behaviour and the rate-limited drop reporting. The idle poll window is
    -- 50ms here, against a production default of about 20s. An idle-receive test then
    -- returns promptly rather than waits out a real long-poll.
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
