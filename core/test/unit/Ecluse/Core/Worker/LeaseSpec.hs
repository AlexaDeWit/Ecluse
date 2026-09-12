-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Cover for the worker's visibility-lease controller. The in-memory queue never expires a
delivery, so these cases model expiry themselves: an injected clock, a deadline per receipt,
and a second consumer that takes whatever lapsed. The clock is stepped rather than timed (see
'withWorldClock'), so no case turns on how promptly a thread is scheduled.
-}
module Ecluse.Core.Worker.LeaseSpec (spec) where

import Control.Concurrent.STM (check, retry)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Conc (ThreadStatus (ThreadDied, ThreadFinished), threadStatus)
import Katip (KatipContextT, SimpleLogPayload, runKatipContextT)
import Test.Hspec
import UnliftIO (timeout)
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (ThreadId, myThreadId, threadDelay)
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
                    readTVarIO (lwNow world) >>= (`shouldSatisfy` (> 30))
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
                    readTVarIO (lwNow world) >>= (`shouldSatisfy` (> 30))
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
                void (whileLeased held (liftIO (threadDelay settleMicros)))
            renewalsSoFar world `shouldReturn` []

        it "runs one renewal task per receipt across a full batch of ten, and none besides" $ do
            -- The task bound: ten small renewal tasks beside the single artifact task.
            let batch = map (\n -> delivery (show n) window30 twelveHours) [1 :: Int .. 10]
            world <- newLeaseWorld 0 batch
            runLeases world keepsEveryLease batch $ \leased -> do
                held <- leaseAt 0 leased
                void . whileLeased held . liftIO $ do
                    awaitRenewals world 30
                    redeliverable world `shouldReturn` []
            renewed <- renewalsSoFar world
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
                liftIO (awaitRenewals world 4 >> awaitEndedTasks world 1)
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
            renewed <- renewalsSoFar world
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
            renewalsSoFar world `shouldReturn` ["a"]

        it "drops at once on a fault no retry can clear" $ do
            -- The shared typed transience decides: a TLS refusal needs an operator, not a retry.
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            runLeases world (faultingFor ["a"] refused) batch $ \leased -> do
                held <- leaseAt 0 leased
                outcome <- whileLeased held (liftIO neverEnds)
                liftIO (outcome `shouldBe` Nothing)
            renewalsSoFar world `shouldReturn` ["a"]

        it "drops the receipt when the renewal task dies outside its typed contract" $ do
            -- The queue handle reports faults as values, so a throw anywhere in the task is an
            -- invariant break. Its exit must still drop the receipt, or the job runs unleased.
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            let residue = (worldOps world keepsEveryLease){loWaitUntil = \_ -> throwIO (TestContractEscape "simulated renewal residue")}
            runLeasesWith world residue batch $ \leased -> do
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
                -- Force virtual time on, which wakes a renewal still parked on this receipt: it
                -- must find the lease gone and end rather than ask again. Waiting for the reap
                -- is what proves it ended, and the log must not have grown while it did.
                liftIO (advanceWorld world 120)
                settled <- renewalsSoFar world
                liftIO (awaitEndedTasks world 1)
                liftIO (renewalsSoFar world `shouldReturn` settled)

        it "stops renewal even when the disposition's own queue call faulted" $ do
            -- A failed ack is absorbed, but it still ends the lease: the message is going to
            -- redeliver, and holding it hidden would only delay that.
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            runLeases world keepsEveryLease batch $ \leased -> do
                held <- leaseAt 0 leased
                liftIO (awaitRenewals world 2)
                void (disposing held (pure (Left unreachable :: Either TransportFault ())))
                liftIO (advanceWorld world 120)
                settled <- renewalsSoFar world
                liftIO (awaitEndedTasks world 1)
                liftIO (renewalsSoFar world `shouldReturn` settled)

    describe "withLeasedBatch -- cancellation" $
        it "leaves an unfinished receipt unacknowledged and stops every renewal" $ do
            -- Shutdown cancels the loop thread. An un-acked message simply redelivers, which is
            -- safe because publishing is idempotent, so nothing may be disposed on the way out.
            -- Leaving the batch's scope cancels every task and waits for it, so the renewal log
            -- is already settled by the time the timeout returns.
            let batch = [delivery "a" window30 twelveHours]
            world <- newLeaseWorld 0 batch
            disposed <- newIORef (0 :: Int)
            _ <- timeout settleMicros . runLeases world keepsEveryLease batch $ \leased -> do
                held <- leaseAt 0 leased
                void (whileLeased held (liftIO neverEnds))
                disposing held (modifyIORef' disposed (+ 1))
            readIORef disposed `shouldReturn` 0
            settled <- renewalsSoFar world
            advanceWorld world 120
            renewalsSoFar world `shouldReturn` settled

{- | The one real wait left: how long a case lets the controller run before cancelling it, and
how long a job that models a backend with no lease to renew takes.
-}
settleMicros :: Int
settleMicros = 20_000

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

{- | A queue world that models receipt expiry, which the in-memory backend cannot. Its clock is
__stepped, never timed__: it moves to the earliest instant any waiter is parked for, and only
while every live renewal task is parked. A task that is slow to be scheduled therefore holds
virtual time still rather than losing its lease to it, so a loaded runner cannot fail a case.
-}
data LeaseWorld = LeaseWorld
    { lwNow :: TVar Double
    , -- Each parked waiter and the instant it is waiting for.
      lwParked :: TVar (Map ThreadId Double)
    , -- Every waiter that has parked at least once and whose thread has not since ended.
      lwSeen :: TVar (Set ThreadId)
    , -- How many of the renewal tasks have ended, so the clock stops waiting on them.
      lwEnded :: TVar Int
    , -- One renewal task runs per leased receipt, which is how many waiters to expect.
      lwRenewalTasks :: Int
    , -- Each receipt's current deadline: when a second consumer could take the delivery.
      lwVisible :: IORef (Map Text Double)
    , -- Every renewal the controller asked for, newest first.
      lwRenewals :: IORef [Text]
    }

-- What a renewal of one receipt answers. The world extends that receipt's deadline on a Right.
type RenewalAnswer = LeaseWorld -> Text -> IO (Either TransportFault ())

-- | A world holding the batch's leases, with its clock at the given instant.
newLeaseWorld :: Double -> [QueueMessage] -> IO LeaseWorld
newLeaseWorld startedAt batch = do
    now <- newTVarIO startedAt
    parked <- newTVarIO mempty
    seen <- newTVarIO mempty
    ended <- newTVarIO 0
    visible <- newIORef (Map.fromList (mapMaybe deadlineOf batch))
    renewals <- newIORef []
    pure
        LeaseWorld
            { lwNow = now
            , lwParked = parked
            , lwSeen = seen
            , lwEnded = ended
            , lwRenewalTasks = length (mapMaybe msgLease batch)
            , lwVisible = visible
            , lwRenewals = renewals
            }
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
        , loNow = MonoTime <$> readTVarIO (lwNow world)
        , loWaitUntil = waitUntilInWorld world
        , loRetryDelays = [0, 0, 0]
        }

renewInWorld :: LeaseWorld -> RenewalAnswer -> Text -> Seconds -> IO (Either TransportFault ())
renewInWorld world answer receipt (Seconds window) = do
    modifyIORef' (lwRenewals world) (receipt :)
    answer world receipt >>= \case
        Left fault -> pure (Left fault)
        Right () -> do
            now <- readTVarIO (lwNow world)
            Right () <$ modifyIORef' (lwVisible world) (Map.insert receipt (now + fromIntegral window))

{- Park the caller at its target and block there. Registering before blocking is what lets the
stepper tell a waiter that is waiting from one that is between waits and must not be stepped
over. -}
waitUntilInWorld :: LeaseWorld -> MonoTime -> IO ()
waitUntilInWorld world (MonoTime target) = do
    waiter <- myThreadId
    atomically $ do
        modifyTVar' (lwSeen world) (Set.insert waiter)
        modifyTVar' (lwParked world) (Map.insert waiter target)
    atomically $ do
        readTVar (lwNow world) >>= check . (>= target)
        modifyTVar' (lwParked world) (Map.delete waiter)

{- Step the world's clock for the body. It settles on its own the moment every renewal task is
parked, so no real-time pacing decides anything. The one timed part is reaping a task that
ended instead of parking again, and reaping late only ever pauses the clock. -}
withWorldClock :: LeaseWorld -> IO a -> IO a
withWorldClock world body = withAsync stepping (const body)
  where
    stepping :: IO ()
    stepping = forever $ do
        stepped <- timeout reapMicros (atomically (awaitStep world))
        whenNothing_ stepped (reapEndedWaiters world)

-- Move virtual time to the earliest instant a waiter is parked for, once they all are.
awaitStep :: LeaseWorld -> STM ()
awaitStep world = do
    parked <- readTVar (lwParked world)
    ended <- readTVar (lwEnded world)
    now <- readTVar (lwNow world)
    case Map.elems parked of
        target : rest
            | Map.size parked == lwRenewalTasks world - ended
            , earliest <- foldr min target rest
            , earliest > now
            , earliest <= worldHorizon ->
                writeTVar (lwNow world) earliest
        _ -> retry

{- How far virtual time may run. The stepper is otherwise free to race, so a case whose work
thread is blocked in real time would reach a virtual instant that depends on how fast the
runner is: far enough, on a fast one, to spend a receipt's twelve-hour ceiling. Every case
works well inside this, and nothing may rely on passing it. -}
worldHorizon :: Double
worldHorizon = 1_000

{- Stop waiting on the tasks whose threads have ended, so a dropped or disposed receipt cannot
stall the clock for its siblings. A thread that has finished never resumes, so this can only
ever release the clock late, never early. -}
reapEndedWaiters :: LeaseWorld -> IO ()
reapEndedWaiters world = do
    seen <- readTVarIO (lwSeen world)
    finished <- filterM threadEnded (toList seen)
    unless (null finished) . atomically $ do
        modifyTVar' (lwSeen world) (`Set.difference` Set.fromList finished)
        modifyTVar' (lwEnded world) (+ length finished)

threadEnded :: ThreadId -> IO Bool
threadEnded waiter = hasEnded <$> threadStatus waiter
  where
    hasEnded = \case
        ThreadFinished -> True
        ThreadDied -> True
        _ -> False

{- How long the stepper waits for the world to settle before it looks for an ended task. It
paces nothing else: a step lands the instant the last renewal task parks. -}
reapMicros :: Int
reapMicros = 2_000

-- | Force virtual time on, for a case that must give a stopped renewal a chance to misbehave.
advanceWorld :: (MonadIO m) => LeaseWorld -> Double -> m ()
advanceWorld world by = atomically (modifyTVar' (lwNow world) (+ by))

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
        advanceWorld world 30
        pure (Left unreachable)
    | otherwise = pure (Right ())

unreachable :: TransportFault
unreachable = transportFault TransportUnreachable "simulated renewal outage"

refused :: TransportFault
refused = transportFault TransportTls "simulated certificate refusal"

-- | Every renewal the controller has asked for, oldest first.
renewalsSoFar :: (MonadIO m) => LeaseWorld -> m [Text]
renewalsSoFar = fmap reverse . readIORef . lwRenewals

-- | What a second consumer would receive: every receipt whose window has lapsed unrenewed.
redeliverable :: (MonadIO m) => LeaseWorld -> m [Text]
redeliverable world = do
    now <- readTVarIO (lwNow world)
    Map.keys . Map.filter (<= now) <$> readIORef (lwVisible world)

-- Wait, bounded, until the controller has asked for at least this many renewals.
awaitRenewals :: (MonadIO m) => LeaseWorld -> Int -> m ()
awaitRenewals world wanted =
    void (pollUntil 2_000 1_000 (>= wanted) (length <$> readIORef (lwRenewals world)))

{- Wait, bounded, until the stepper has reaped this many ended renewal tasks. A reap counts only
a thread that has already finished, so the receipt's lease was marked dropped before it. -}
awaitEndedTasks :: (MonadIO m) => LeaseWorld -> Int -> m ()
awaitEndedTasks world wanted =
    void (pollUntil 2_000 1_000 (>= wanted) (readTVarIO (lwEnded world)))

-- Wait, bounded, until the renewals have carried the world's clock past this instant.
awaitClock :: (MonadIO m) => LeaseWorld -> Double -> m ()
awaitClock world wanted =
    void (pollUntil 2_000 1_000 (>= wanted) (readTVarIO (lwNow world)))

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
runLeases world answer = runLeasesWith world (worldOps world answer)

-- | 'runLeases' over caller-built ops, for a case that perturbs the clock or the waiting itself.
runLeasesWith :: LeaseWorld -> LeaseOps -> [QueueMessage] -> ([LeasedReceipt] -> KatipContextT IO a) -> IO a
runLeasesWith world ops batch body = do
    logEnv <- newTestLogEnv
    withWorldClock world (runKatipContextT logEnv (mempty :: SimpleLogPayload) mempty (withLeasedBatch ops batch body))

-- A lease the batch does not hold is a broken premise, so it fails loudly.
leaseAt :: (MonadIO m) => Int -> [LeasedReceipt] -> m LeasedReceipt
leaseAt index leased = maybe (liftIO (fail ("the batch holds no lease at index " <> show index))) pure (leased !!? index)
