module Ecluse.QueueSpec (spec) where

import Test.Hspec

import Ecluse.Ecosystem (Ecosystem (..))
import Ecluse.Package (mkPackageName)
import Ecluse.Queue
import Ecluse.Version (mkVersion)

{- | A sample mirror job. The in-memory queue under test does not inspect a
job's contents — it only carries it from 'enqueue' to 'receive' — so one fixed
job suffices for the FIFO / ack / redelivery assertions.
-}
sampleJob :: MirrorJob
sampleJob =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "thing"
        , jobVersion = mkVersion Npm "1.0.0"
        , jobArtifactUrl = "https://public.test/thing/-/thing-1.0.0.tgz"
        , jobMirrorTarget = "https://mirror.test/thing/-/thing-1.0.0.tgz"
        }

{- | A second, distinct job, used to assert FIFO ordering across two enqueues.
It differs from 'sampleJob' only in its version, which is enough to tell the two
apart on receive.
-}
otherJob :: MirrorJob
otherJob = sampleJob{jobVersion = mkVersion Npm "2.0.0"}

spec :: Spec
spec = do
    describe "newInMemoryQueue" $ do
        it "receives [] from an empty queue" $ do
            q <- newInMemoryQueue
            msgs <- receive q
            map msgJob msgs `shouldBe` []

        it "delivers an enqueued job on the next receive" $ do
            q <- newInMemoryQueue
            enqueue q sampleJob
            msgs <- receive q
            map msgJob msgs `shouldBe` [sampleJob]

        it "carries every job field through unchanged from enqueue to receive" $ do
            -- The queue is a transparent carrier: each field the producer set must
            -- arrive on the consumer side byte-for-byte. Assert field-by-field
            -- (via the 'MirrorJob' selectors) rather than on the whole record, so a
            -- regression that mangled a single field is pinpointed.
            q <- newInMemoryQueue
            enqueue q sampleJob
            [msg] <- receive q
            let job = msgJob msg
            jobPackage job `shouldBe` jobPackage sampleJob
            jobVersion job `shouldBe` jobVersion sampleJob
            jobArtifactUrl job `shouldBe` jobArtifactUrl sampleJob
            jobMirrorTarget job `shouldBe` jobMirrorTarget sampleJob

        it "delivers jobs in FIFO order" $ do
            q <- newInMemoryQueue
            enqueue q sampleJob
            enqueue q otherJob
            received <- drain q
            received `shouldBe` [sampleJob, otherJob]

        it "does not redeliver a job that was acked" $ do
            q <- newInMemoryQueue
            enqueue q sampleJob
            [msg] <- receive q
            ack q (msgReceipt msg)
            -- After the ack, the job is gone: a later receive is empty.
            afterAck <- receive q
            map msgJob afterAck `shouldBe` []

        it "redelivers a job that was received but never acked" $ do
            q <- newInMemoryQueue
            enqueue q sampleJob
            -- Receive (taking the job out of sight) but deliberately do not ack:
            -- retry-is-don't-ack, so the job must become visible again.
            _ <- receive q
            redelivered <- receive q
            map msgJob redelivered `shouldBe` [sampleJob]

        it "stops redelivering once a redelivered job is acked" $ do
            q <- newInMemoryQueue
            enqueue q sampleJob
            _ <- receive q
            [msg] <- receive q
            ack q (msgReceipt msg)
            afterAck <- receive q
            map msgJob afterAck `shouldBe` []

        it "gives each delivery of the same job a distinct message (fresh receipt)" $ do
            -- A job that is received but not acked is redelivered, and each delivery
            -- must be a *distinct* 'QueueMessage' — same job, but a fresh receipt —
            -- so acking one delivery cannot be confused with another. This pins the
            -- receipt-per-delivery invariant via 'QueueMessage' equality.
            q <- newInMemoryQueue
            enqueue q sampleJob
            [firstDelivery] <- receive q
            [secondDelivery] <- receive q
            msgJob firstDelivery `shouldBe` msgJob secondDelivery
            firstDelivery `shouldNotBe` secondDelivery
            msgReceipt firstDelivery `shouldNotBe` msgReceipt secondDelivery

        it "extendVisibility keeps an in-flight job from redelivering immediately" $ do
            -- extendVisibility is an optimisation, not correctness-critical; for
            -- the in-memory double it simply leaves the in-flight job in flight,
            -- so the very next receive does not redeliver it.
            q <- newInMemoryQueue
            enqueue q sampleJob
            [msg] <- receive q
            let hold = Seconds 30
            -- The window is a typed duration, not a bare Int: the held value is the
            -- one we pass through, and Seconds are ordered (a longer hold is larger).
            hold `shouldBe` Seconds 30
            hold `shouldSatisfy` (> Seconds 0)
            extendVisibility q (msgReceipt msg) hold
            afterHold <- receive q
            map msgJob afterHold `shouldBe` []
  where
    -- Receive repeatedly, acking everything, until the queue is empty; returns
    -- the jobs in the order they were delivered. Total: it stops as soon as a
    -- receive yields nothing.
    drain :: MirrorQueue -> IO [MirrorJob]
    drain q = go []
      where
        go acc = do
            msgs <- receive q
            case msgs of
                [] -> pure (reverse acc)
                _ -> do
                    traverse_ (ack q . msgReceipt) msgs
                    go (reverse (map msgJob msgs) <> acc)
