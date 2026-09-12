-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Cover for the worker's visibility-lease controller. The in-memory queue never expires a
delivery, so these cases model expiry themselves: an injected clock that moves only when the
controller waits, a deadline per receipt, and a second consumer that takes whatever lapsed.
-}
module Ecluse.Core.Worker.LeaseSpec (spec) where

import Data.Map.Strict qualified as Map
import Katip (KatipContextT, SimpleLogPayload, runKatipContextT)
import Test.Hspec
import UnliftIO (timeout)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Fault (TransportCause (TransportTls, TransportUnreachable), TransportFault, transportFault)
import Ecluse.Core.Queue (QueueMessage (..), mkReceiptHandle, unReceiptHandle)
import Ecluse.Core.Queue.Lease (
    MonoTime (MonoTime),
    ReceiptLease (rlExpiresAt),
    Seconds (Seconds),
    receiptLease,
 )
import Ecluse.Core.Worker.Lease (
    LeaseOps (LeaseOps, loNow, loRenew, loRetryDelays, loWaitUntil),
    LeasedReceipt,
    disposing,
    leaseRenewAt,
    leaseRequest,
    leaseRetryUntil,
    whileLeased,
    withLeasedBatch,
 )
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Queue (sampleJob)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

spec :: Spec
spec = do
    describe "renewal arithmetic" $ do
        it "renews a third of the way into what is left of the window" $
            -- A third, so two renewals can fail before the window lapses.
            leaseRenewAt (MonoTime 10) (MonoTime 40) `shouldBe` MonoTime 20

        it "renews at once on a window that has already lapsed" $
            leaseRenewAt (MonoTime 50) (MonoTime 40) `shouldBe` MonoTime 50

        it "stops sending renewals a tenth of the window before the deadline" $
            -- The transport margin: a request sent later could land after the lease lapsed.
            leaseRetryUntil (Seconds 30) (MonoTime 100) `shouldBe` MonoTime 97

        it "asks for the granted window while the ceiling is far off" $
            leaseRequest (receiptLease (MonoTime 0) (Seconds 30) twelveHours) (MonoTime 100)
                `shouldBe` Just (Seconds 30)

        it "clips the request to what is left before the ceiling from receipt" $
            -- SQS refuses a renewal past its twelve-hour maximum, so the request never asks.
            leaseRequest (receiptLease (MonoTime 0) (Seconds 30) (Seconds 100)) (MonoTime 80)
                `shouldBe` Just (Seconds 20)

        it "asks for nothing once under a second of the ceiling is left" $
            leaseRequest (receiptLease (MonoTime 0) (Seconds 30) (Seconds 100)) (MonoTime 99.5)
                `shouldBe` Nothing

    describe "withLeasedBatch -- holding every received receipt" $ do
        it "keeps a blocked job's own receipt hidden well past its original window" $ do
            -- The bug this fixes: a job that outran the visibility window was handed to a second
            -- consumer while the first worker was still mirroring it.
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            runLeases world keepsEveryLease batch $ \leased -> do
                held <- leaseAt 0 leased
                void . whileLeased held . liftIO $ do
                    awaitRenewals world 5
                    readIORef (lwNow world) >>= (`shouldSatisfy` (> 30))
                    redeliverable world `shouldReturn` []

        it "keeps a sibling receipt hidden while it waits its turn behind the first job" $ do
            -- A batch runs sequentially, so the last receipt waits out several windows before
            -- its own job starts. It is renewed from receipt, not from the moment it runs.
            let batch = [delivery "a" window30 twelveHours, delivery "b" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            runLeases world keepsEveryLease batch $ \leased -> do
                held <- leaseAt 0 leased
                void . whileLeased held . liftIO $ do
                    awaitRenewals world 8
                    readIORef (lwNow world) >>= (`shouldSatisfy` (> 30))
                    redeliverable world `shouldReturn` []

        it "renews on what is left of a window a slow poll has already spent" $ do
            -- The lease is stamped before the request, so a receive that answered late hands
            -- over a part-spent window. Renewal reads the remainder, never a fresh window.
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 25 batch
            runLeases world keepsEveryLease batch $ \leased -> do
                held <- leaseAt 0 leased
                void . whileLeased held . liftIO $ do
                    awaitRenewals world 3
                    redeliverable world `shouldReturn` []

        it "starts no renewal task for a delivery whose backend never expires it" $ do
            -- The in-memory backend grants no lease, so the controller runs no task for it.
            let batch = [unleased "m"]
            world <- newLeaseWorld 0 batch
            runLeases world keepsEveryLease batch $ \leased -> do
                held <- leaseAt 0 leased
                void (whileLeased held (liftIO (threadDelay 20_000)))
            readIORef (lwRenewals world) `shouldReturn` []

        it "runs one renewal task per receipt across a full batch of ten, and none besides" $ do
            -- The task bound: ten small renewal tasks beside the single artifact task.
            let batch = map (\n -> delivery (show n) window30 twelveHours) [1 :: Int .. 10]
            world <- newLeaseWorld 0 batch
            runLeases world keepsEveryLease batch $ \leased -> do
                held <- leaseAt 0 leased
                void . whileLeased held . liftIO $ do
                    awaitRenewals world 30
                    redeliverable world `shouldReturn` []
            renewed <- readIORef (lwRenewals world)
            sortNub renewed `shouldBe` sort (map show [1 :: Int .. 10])

    describe "withLeasedBatch -- a renewal that cannot be kept drops only its own receipt" $ do
        it "cancels the running job of the receipt it dropped, leaving it undisposed" $ do
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            runLeases world (faultingFor ["a"] unreachable) batch $ \leased -> do
                held <- leaseAt 0 leased
                -- The job would never end on its own: only the dropped lease stops it.
                outcome <- whileLeased held (liftIO neverEnds)
                liftIO (outcome `shouldBe` Nothing)

        it "lets a sibling whose own renewals hold finish its job, still leased" $ do
            -- Per-receipt continuation: one transport failure must not abandon healthy siblings.
            let batch = [delivery "a" window30 twelveHours, delivery "b" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            outcomes <- runLeases world (faultingFor ["a"] unreachable) batch $ \leased -> do
                first' <- leaseAt 0 leased
                second' <- leaseAt 1 leased
                -- The faulting receipt's job never ends on its own, so only the drop stops it.
                abandoned <- whileLeased first' (liftIO neverEnds)
                -- Hold the sibling past its own original window, then read who a second consumer
                -- could take: only the dropped receipt, never the one still being renewed.
                finished <- whileLeased second' . liftIO $ do
                    awaitClock world 31
                    redeliverable world `shouldReturn` ["a"]
                pure (abandoned, finished)
            outcomes `shouldBe` (Nothing, Just ())

        it "skips a waiting receipt whose lease was dropped before its job could start" $ do
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            runLeases world (faultingFor ["a"] unreachable) batch $ \leased -> do
                held <- leaseAt 0 leased
                -- Wait for the drop, then offer the job: it must never run.
                liftIO (awaitRenewals world 4 >> threadDelay 20_000)
                started <- newIORef False
                outcome <- whileLeased held (writeIORef started True)
                liftIO (outcome `shouldBe` Nothing)
                liftIO (readIORef started `shouldReturn` False)

        it "retries a transient renewal fault inside the receipt's own margin before dropping" $ do
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            runLeases world (faultingFor ["a"] unreachable) batch $ \leased -> do
                held <- leaseAt 0 leased
                void (whileLeased held (liftIO neverEnds))
            -- The first attempt plus the shipped retry budget, all inside the margin.
            renewed <- readIORef (lwRenewals world)
            length renewed `shouldBe` 4

        it "drops without a retry when the renewal spends the margin on its first attempt" $ do
            -- A renewal that answers only once the margin is gone: retrying it would ask for a
            -- window the backend may already have given away.
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            runLeases world (slowFaultFor "a") batch $ \leased -> do
                held <- leaseAt 0 leased
                outcome <- whileLeased held (liftIO neverEnds)
                liftIO (outcome `shouldBe` Nothing)
            readIORef (lwRenewals world) `shouldReturn` ["a"]

        it "drops at once on a fault no retry can clear" $ do
            -- The shared typed transience decides: a TLS refusal needs an operator, not a retry.
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            runLeases world (faultingFor ["a"] refused) batch $ \leased -> do
                held <- leaseAt 0 leased
                outcome <- whileLeased held (liftIO neverEnds)
                liftIO (outcome `shouldBe` Nothing)
            readIORef (lwRenewals world) `shouldReturn` ["a"]

        it "drops the receipt when the renewal task dies outside its typed contract" $ do
            -- The queue handle reports faults as values, so a throw anywhere in the task is an
            -- invariant break. Its exit must still drop the receipt, or the job runs unleased.
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            let residue = (worldOps world keepsEveryLease){loWaitUntil = \_ -> throwIO (TestContractEscape "simulated renewal residue")}
            runLeasesWith residue batch $ \leased -> do
                held <- leaseAt 0 leased
                outcome <- whileLeased held (liftIO neverEnds)
                liftIO (outcome `shouldBe` Nothing)

        it "drops the receipt once the backend's maximum time in flight is spent" $ do
            -- SQS holds one receipt for twelve hours whatever the renewals, so the controller
            -- stops there rather than letting the lease lapse unnoticed.
            let batch = [delivery "a" window30 (Seconds 31)]
            world <- newLeaseWorld 0 batch
            runLeases world keepsEveryLease batch $ \leased -> do
                held <- leaseAt 0 leased
                outcome <- whileLeased held (liftIO neverEnds)
                liftIO (outcome `shouldBe` Nothing)

    describe "disposing -- no renewal follows a completed disposition" $ do
        it "stops renewal for good, whichever disposition the job reached" $ do
            -- An ack, a release for retry, and a terminal backoff are all dispositions. A
            -- renewal landing after any of them would re-hide a message the worker let go.
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            runLeases world keepsEveryLease batch $ \leased -> do
                held <- leaseAt 0 leased
                liftIO (awaitRenewals world 2)
                disposing held pass
                settled <- readIORef (lwRenewals world)
                liftIO (threadDelay 20_000)
                liftIO (readIORef (lwRenewals world) `shouldReturn` settled)

        it "stops renewal even when the disposition's own queue call faulted" $ do
            -- A failed ack is absorbed, but it still ends the lease: the message is going to
            -- redeliver, and holding it hidden would only delay that.
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            runLeases world keepsEveryLease batch $ \leased -> do
                held <- leaseAt 0 leased
                liftIO (awaitRenewals world 2)
                void (disposing held (pure (Left unreachable :: Either TransportFault ())))
                settled <- readIORef (lwRenewals world)
                liftIO (threadDelay 20_000)
                liftIO (readIORef (lwRenewals world) `shouldReturn` settled)

    describe "withLeasedBatch -- cancellation" $
        it "leaves an unfinished receipt unacknowledged and stops every renewal" $ do
            -- Shutdown cancels the loop thread. An un-acked message simply redelivers, which is
            -- safe because publishing is idempotent, so nothing may be disposed on the way out.
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            disposed <- newIORef (0 :: Int)
            _ <- timeout 30_000 . runLeases world keepsEveryLease batch $ \leased -> do
                held <- leaseAt 0 leased
                void (whileLeased held (liftIO neverEnds))
                disposing held (modifyIORef' disposed (+ 1))
            readIORef disposed `shouldReturn` 0
            settled <- readIORef (lwRenewals world)
            threadDelay 20_000
            readIORef (lwRenewals world) `shouldReturn` settled

{- | A job that never ends on its own, so only a dropped lease stops it. Its result type is
fixed, which is what lets a case assert on the 'Maybe' that 'whileLeased' hands back.
-}
neverEnds :: IO ()
neverEnds = forever (threadDelay 1_000)

-- | SQS's own ceiling on one receipt, the value the production backend stamps.
twelveHours :: Seconds
twelveHours = Seconds 43_200

-- | The visibility window these cases lease a delivery for.
window30 :: Seconds
window30 = Seconds 30

{- | A queue world that models receipt expiry, which the in-memory backend cannot. Its clock
moves only when the controller waits, so every case is deterministic.
-}
data LeaseWorld = LeaseWorld
    { lwNow :: IORef Double
    , -- Each receipt's current deadline: when a second consumer could take the delivery.
      lwVisible :: IORef (Map Text Double)
    , -- Every renewal the controller asked for, oldest first.
      lwRenewals :: IORef [Text]
    }

-- What a renewal of one receipt answers. The world extends that receipt's deadline on a Right.
type RenewalAnswer = LeaseWorld -> Text -> IO (Either TransportFault ())

-- | A world holding the batch's leases, with its clock at the given instant.
newLeaseWorld :: Double -> [QueueMessage] -> IO LeaseWorld
newLeaseWorld startedAt batch = do
    now <- newIORef startedAt
    visible <- newIORef (Map.fromList (mapMaybe deadlineOf batch))
    renewals <- newIORef []
    pure LeaseWorld{lwNow = now, lwVisible = visible, lwRenewals = renewals}
  where
    deadlineOf message = do
        lease <- msgLease message
        let MonoTime expiresAt = rlExpiresAt lease
        pure (unReceiptHandle (msgReceipt message), expiresAt)

-- | The controller's transport, clock, and waiting over one world. Retries are unpaced here.
worldOps :: LeaseWorld -> RenewalAnswer -> LeaseOps
worldOps world answer =
    LeaseOps
        { loRenew = renewInWorld world answer . unReceiptHandle
        , loNow = MonoTime <$> readIORef (lwNow world)
        , loWaitUntil = waitUntilInWorld world
        , loRetryDelays = [0, 0, 0]
        }

renewInWorld :: LeaseWorld -> RenewalAnswer -> Text -> Seconds -> IO (Either TransportFault ())
renewInWorld world answer receipt (Seconds window) = do
    modifyIORef' (lwRenewals world) (<> [receipt])
    answer world receipt >>= \case
        Left fault -> pure (Left fault)
        Right () -> do
            now <- readIORef (lwNow world)
            Right () <$ modifyIORef' (lwVisible world) (Map.insert receipt (now + fromIntegral window))

{- Move the clock to the waiting task's target, never past it, so ten tasks waiting at once
advance it once rather than ten times. The real pause lets the others run.
-}
waitUntilInWorld :: LeaseWorld -> MonoTime -> IO ()
waitUntilInWorld world (MonoTime target) = do
    atomicModifyIORef' (lwNow world) (\now -> (max now target, ()))
    threadDelay 200

-- | A renewal the backend always grants.
keepsEveryLease :: RenewalAnswer
keepsEveryLease _ _ = pure (Right ())

-- | A renewal the backend refuses for the named receipts alone, with the given fault.
faultingFor :: [Text] -> TransportFault -> RenewalAnswer
faultingFor receipts fault _ receipt
    | receipt `elem` receipts = pure (Left fault)
    | otherwise = pure (Right ())

-- | A renewal for one receipt that answers only once its transport margin is spent.
slowFaultFor :: Text -> RenewalAnswer
slowFaultFor named world receipt
    | receipt == named = do
        atomicModifyIORef' (lwNow world) (\now -> (now + 30, ()))
        pure (Left unreachable)
    | otherwise = pure (Right ())

unreachable :: TransportFault
unreachable = transportFault TransportUnreachable "simulated renewal outage"

refused :: TransportFault
refused = transportFault TransportTls "simulated certificate refusal"

-- | What a second consumer would receive: every receipt whose window has lapsed unrenewed.
redeliverable :: LeaseWorld -> IO [Text]
redeliverable world = do
    now <- readIORef (lwNow world)
    Map.keys . Map.filter (<= now) <$> readIORef (lwVisible world)

-- Wait, bounded, until the controller has asked for at least this many renewals.
awaitRenewals :: LeaseWorld -> Int -> IO ()
awaitRenewals world wanted =
    void (pollUntil 2_000 1_000 (>= wanted) (length <$> readIORef (lwRenewals world)))

-- Wait, bounded, until the renewals have carried the world's clock past this instant.
awaitClock :: LeaseWorld -> Double -> IO ()
awaitClock world wanted =
    void (pollUntil 2_000 1_000 (>= wanted) (readIORef (lwNow world)))

-- | One delivery of the sample job, leased for a window under a ceiling from the same instant.
delivery :: Text -> Seconds -> Seconds -> QueueMessage
delivery receipt window maxHold =
    (unleased receipt){msgLease = Just (receiptLease (MonoTime 0) window maxHold)}

-- | One delivery from a backend that never expires it, so it carries no lease.
unleased :: Text -> QueueMessage
unleased receipt =
    QueueMessage
        { msgJob = sampleJob
        , msgReceipt = mkReceiptHandle receipt
        , msgReceiveCount = 1
        , msgLease = Nothing
        }

-- | Run a leased batch against one world, discarding the log lines the controller writes.
runLeases :: LeaseWorld -> RenewalAnswer -> [QueueMessage] -> ([LeasedReceipt] -> KatipContextT IO a) -> IO a
runLeases world answer = runLeasesWith (worldOps world answer)

-- | 'runLeases' over caller-built ops, for a case that perturbs the clock or the waiting itself.
runLeasesWith :: LeaseOps -> [QueueMessage] -> ([LeasedReceipt] -> KatipContextT IO a) -> IO a
runLeasesWith ops batch body = do
    logEnv <- newTestLogEnv
    runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty (withLeasedBatch ops batch body)

-- A lease the batch does not hold is a broken premise, so it fails loudly.
leaseAt :: (MonadIO m) => Int -> [LeasedReceipt] -> m LeasedReceipt
leaseAt index leased = maybe (liftIO (fail ("the batch holds no lease at index " <> show index))) pure (leased !!? index)
