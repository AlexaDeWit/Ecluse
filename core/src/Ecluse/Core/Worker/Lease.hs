-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Holding every received receipt for as long as the worker needs it.

A backend hides a delivery for one window ("Ecluse.Core.Queue.Lease"). A batch runs
sequentially, so a receipt waiting its turn and a receipt whose job runs long both outlive
that window and would be redelivered to a second consumer. The controller renews each one
continually, from receipt until its disposition, and a renewal and a disposition never
overlap on the same receipt. A receipt whose lease cannot be kept is dropped alone: its job
is cancelled while running and skipped while waiting, and it is left unacknowledged.
-}
module Ecluse.Core.Worker.Lease (
    -- * The controller
    LeasedReceipt,
    leasedMessage,
    withLeasedBatch,
    whileLeased,
    disposing,

    -- * The transport, clock, and pacing it runs on
    LeaseOps (..),
    queueLeaseOps,

    -- * Renewal arithmetic
    leaseRenewAt,
    leaseRetryUntil,
    leaseRequest,
) where

import Control.Concurrent.STM (check, orElse)
import Control.Retry (retrying)
import Katip (KatipContext, Severity (WarningS), logFM, ls)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (waitSTM, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (tryAny)
import UnliftIO.MVar qualified as MVar

import Ecluse.Core.Fault (TransportCause (TransportProtocol), TransportFault, tfCause, tfDetail, transportFault, transportRetryable)
import Ecluse.Core.Queue (MirrorQueue (extendVisibility), QueueMessage (msgLease, msgReceipt), ReceiptHandle)
import Ecluse.Core.Queue.Lease (
    MonoTime,
    ReceiptLease (rlCeilingAt, rlExpiresAt, rlWindow),
    Seconds (Seconds),
    monoAfter,
    monoSecondsBetween,
    monotonicNow,
 )
import Ecluse.Core.Supervision (delayListPolicy)
import Ecluse.Core.Text (displayExceptionT)

{- | The transport, clock, and pacing a lease runs on, injected so a test drives renewal on a
clock it controls rather than on real time.
-}
data LeaseOps = LeaseOps
    { loRenew :: ReceiptHandle -> Seconds -> IO (Either TransportFault ())
    -- ^ Reset one receipt's visibility window to the given duration.
    , loNow :: IO MonoTime
    -- ^ The monotonic clock every lease deadline is measured on.
    , loWaitUntil :: MonoTime -> IO ()
    {- ^ Wait until the given instant. It takes the instant, not a duration, so several waits
    running at once cannot each push a shared test clock on by their own full pause.
    -}
    , loRetryDelays :: [Int]
    {- ^ The pacing between renewal attempts, in microseconds. Its length is the retry budget,
    and a retry stops early once the receipt's own margin runs out.
    -}
    }

-- | The lease transport over a live queue handle, at the shipped clock and pacing.
queueLeaseOps :: MirrorQueue -> LeaseOps
queueLeaseOps queue =
    LeaseOps
        { loRenew = extendVisibility queue
        , loNow = monotonicNow
        , loWaitUntil = waitUntilMonotonic
        , loRetryDelays = leaseRetryDelays
        }

-- Re-read the clock rather than trust the caller's, so a wait can only ever be short.
waitUntilMonotonic :: MonoTime -> IO ()
waitUntilMonotonic target = do
    now <- monotonicNow
    let pause = monoSecondsBetween now target
    when (pause > 0) (threadDelay (round (pause * 1_000_000)))

{- | The renewal retry pacing: three further attempts inside about two seconds, which fits the
transport margin of even the shortest window an operator configures.
-}
leaseRetryDelays :: [Int]
leaseRetryDelays = [200_000, 500_000, 1_000_000]

{- | One received receipt and the lease held over it. A renewal task runs per leased receipt,
so a batch runs at most ten of them beside its single artifact task.
-}
data LeasedReceipt = LeasedReceipt
    { lrMessage :: QueueMessage
    , -- When the current window lapses, or Nothing once the receipt is disposed or dropped.
      -- Taking it is what stops a renewal and a disposition from overlapping.
      lrHeld :: MVar (Maybe MonoTime)
    , -- Set once renewal has given up, so the receipt's job is cancelled or skipped.
      lrDropped :: TVar Bool
    }

-- | The message this receipt delivered.
leasedMessage :: LeasedReceipt -> QueueMessage
leasedMessage = lrMessage

{- | Lease every receipt in a batch for the body's whole run, renewing each continually.
Leaving the body cancels every renewal, so an unfinished receipt stays unacknowledged.
-}
withLeasedBatch :: (MonadUnliftIO m, KatipContext m) => LeaseOps -> [QueueMessage] -> ([LeasedReceipt] -> m a) -> m a
withLeasedBatch ops messages body = do
    leased <- traverse newLeasedReceipt messages
    withRenewals ops leased (body leased)

newLeasedReceipt :: (MonadIO m) => QueueMessage -> m LeasedReceipt
newLeasedReceipt message = do
    held <- MVar.newMVar (rlExpiresAt <$> msgLease message)
    dropped <- newTVarIO False
    pure LeasedReceipt{lrMessage = message, lrHeld = held, lrDropped = dropped}

{- Nest one renewal task per receipt that carries a lease, so leaving the scope cancels every
one of them. A backend that never expires a delivery grants no lease and gets no task. -}
withRenewals :: (MonadUnliftIO m, KatipContext m) => LeaseOps -> [LeasedReceipt] -> m a -> m a
withRenewals ops leased inner = foldr renewing inner leased
  where
    renewing one rest = case msgLease (lrMessage one) of
        Nothing -> rest
        Just lease -> withAsync (renewalLoop ops lease one) (const rest)

{- | Run this receipt's work while its lease holds, cancelling the work the moment the lease is
dropped. 'Nothing' says the receipt was dropped, so nothing was decided for it.
-}
whileLeased :: (MonadUnliftIO m) => LeasedReceipt -> m a -> m (Maybe a)
whileLeased leased work = do
    dropped <- readTVarIO (lrDropped leased)
    if dropped
        then pure Nothing
        else withAsync work $ \running ->
            atomically ((Just <$> waitSTM running) `orElse` (Nothing <$ awaitDropped leased))

-- Block until this receipt's renewal has given up on it.
awaitDropped :: LeasedReceipt -> STM ()
awaitDropped leased = readTVar (lrDropped leased) >>= check

{- | Realise a receipt's disposition with its renewal stopped first, so none can follow an
acknowledgement, a release for retry, or a terminal backoff.
-}
disposing :: (MonadUnliftIO m) => LeasedReceipt -> m a -> m a
disposing leased act = MVar.modifyMVar (lrHeld leased) (const ((Nothing,) <$> act))

-- What one renewal attempt settled. A disposed receipt ends the loop, a transient fault may
-- retry inside the margin, and a refusal is the ceiling or a fault no retry could clear.
data RenewalStep
    = RenewalKept
    | RenewalStopped
    | RenewalFailed Text
    | RenewalRefused Text

{- Renew one receipt until it is disposed, or until a renewal it cannot keep drops it. The loop
is total: the queue handle reports faults as values, and residue is contained below. -}
renewalLoop :: (MonadUnliftIO m, KatipContext m) => LeaseOps -> ReceiptLease -> LeasedReceipt -> m ()
renewalLoop ops lease leased = go
  where
    go =
        MVar.readMVar (lrHeld leased) >>= \case
            Nothing -> pass
            Just deadline -> do
                waitForRenewal ops deadline
                attemptRenewal ops lease leased deadline >>= \case
                    RenewalKept -> go
                    RenewalStopped -> pass
                    RenewalFailed detail -> dropReceipt leased detail
                    RenewalRefused detail -> dropReceipt leased detail

-- Wait until the renewal falls due, which a disposition in the meantime simply outlasts.
waitForRenewal :: (MonadIO m) => LeaseOps -> MonoTime -> m ()
waitForRenewal ops deadline = do
    now <- liftIO (loNow ops)
    liftIO (loWaitUntil ops (leaseRenewAt now deadline))

{- Ask for another window, retrying a transient fault while this receipt's own margin lasts.
The shared delay-list policy paces the attempts, and the margin check ends them early. -}
attemptRenewal :: (MonadUnliftIO m) => LeaseOps -> ReceiptLease -> LeasedReceipt -> MonoTime -> m RenewalStep
attemptRenewal ops lease leased deadline =
    retrying (delayListPolicy (loRetryDelays ops)) withinMargin (const (renewOnce ops lease leased))
  where
    withinMargin _ = \case
        RenewalFailed _ -> do
            now <- liftIO (loNow ops)
            pure (now < leaseRetryUntil (rlWindow lease) deadline)
        _ -> pure False

-- One attempt under the receipt's own lock, so it can never overlap that receipt's disposition.
renewOnce :: (MonadUnliftIO m) => LeaseOps -> ReceiptLease -> LeasedReceipt -> m RenewalStep
renewOnce ops lease leased =
    MVar.modifyMVar (lrHeld leased) $ \case
        Nothing -> pure (Nothing, RenewalStopped)
        Just deadline -> do
            -- Read before the request, so the extended deadline never claims more than it won.
            now <- liftIO (loNow ops)
            case leaseRequest lease now of
                Nothing -> pure (Just deadline, RenewalRefused ceilingReason)
                Just window -> settle now deadline window <$> liftIO (renewOrResidue ops (msgReceipt (lrMessage leased)) window)

-- A kept window moves the deadline on, a retryable fault leaves it for another attempt, and
-- anything else ends the lease: the typed cause alone splits retry from refusal.
settle :: MonoTime -> MonoTime -> Seconds -> Either TransportFault () -> (Maybe MonoTime, RenewalStep)
settle now deadline (Seconds window) = \case
    Right () -> (Just (monoAfter now (fromIntegral window)), RenewalKept)
    Left fault
        | transportRetryable (tfCause fault) -> (Just deadline, RenewalFailed (tfDetail fault))
        | otherwise -> (Just deadline, RenewalRefused (tfDetail fault))

{- The handle reports every backend failure as a value, so an exception is an invariant break.
Contain it here: a renewal task that died would let its lease lapse unnoticed. -}
renewOrResidue :: LeaseOps -> ReceiptHandle -> Seconds -> IO (Either TransportFault ())
renewOrResidue ops receipt window =
    either (Left . residue) id <$> tryAny (loRenew ops receipt window)
  where
    residue e = transportFault TransportProtocol ("visibility renewal escaped its typed contract: " <> displayExceptionT e)

{- Give up on one receipt: stop renewing it, cancel or skip its job, and leave it
unacknowledged, so the backend redelivers it once the window lapses. -}
dropReceipt :: (MonadUnliftIO m, KatipContext m) => LeasedReceipt -> Text -> m ()
dropReceipt leased detail = do
    logFM WarningS (ls ("dropping a mirror receipt whose visibility could not be renewed: " <> detail))
    MVar.modifyMVar_ (lrHeld leased) (const (pure Nothing))
    atomically (writeTVar (lrDropped leased) True)

ceilingReason :: Text
ceilingReason = "the queue's own maximum time in flight from receipt is spent"

{- | When a held lease is renewed: a third of the way into what is left of it, so two renewals
can fail before the window lapses.
-}
leaseRenewAt :: MonoTime -> MonoTime -> MonoTime
leaseRenewAt now deadline = monoAfter now (max 0 (monoSecondsBetween now deadline) / 3)

{- | The last instant a renewal may be sent: a tenth of the window before the deadline, the
transport margin a request needs to land while the lease still holds.
-}
leaseRetryUntil :: Seconds -> MonoTime -> MonoTime
leaseRetryUntil (Seconds window) deadline = monoAfter deadline (negate (fromIntegral window / 10))

{- | The window a renewal asks for: the one the backend granted, clipped to what is left before
its ceiling from receipt. 'Nothing' once under a second is left, which drops the receipt.
-}
leaseRequest :: ReceiptLease -> MonoTime -> Maybe Seconds
leaseRequest lease now
    | remaining < 1 = Nothing
    | otherwise = Just (Seconds (min window (floor remaining)))
  where
    Seconds window = rlWindow lease
    remaining = monoSecondsBetween now (rlCeilingAt lease)
