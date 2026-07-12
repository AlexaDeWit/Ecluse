-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Brief-wait admission control for metadata-bearing serve work.

The handle caps concurrent operations and retains a __bounded room of waiters__: an
operation acquires a slot immediately, waits briefly for one, or is refused. This
bounds aggregate metadata residency by construction while absorbing a burst that
merely brushes the cap, so near-capacity load degrades into short queueing delay
rather than a refusal the client immediately retries. Refusal is reserved for genuine
overload: a waiting room already at its bound (the deep-overflow band, refused
instantly and cheaply) or a wait that outlives its budget.

Instant shedding is self-amplifying under a hammering client: each refusal is
answered in microseconds, so the client comes straight back, and the refusal work
itself competes for the cores the admitted work needs. Waiting in-process is a
blocked green thread -- nearly free -- and every slot release goes to work that has
already arrived. The wait budget equals the shed path's @Retry-After: 1@ hint, so a
request is never refused faster than the client would have been told to come back.

Two fairness properties, one deliberate limit:

* __A newcomer never jumps a non-empty waiting room__: a freed slot is only taken
  directly when no one is waiting, so arrival order is respected between the room
  and the door.
* __Within the room, wake-up order is not FIFO__ (STM retry semantics: all waiters
  race, first commit wins). With the room bounded at the capacity and slot turnover
  far faster than the budget, starvation is not a practical concern, and strict
  ticketing is complexity this surface has not earned.

Acquired slots are released across normal completion, failure, and asynchronous
cancellation. The waits run masked: a blocked STM retry remains interruptible (a
cancellation lands and aborts the transaction, taking nothing), while a committed
acquire returns with exceptions still masked, so a slot can never be lost between
acquisition and the protected run.
-}
module Ecluse.Core.Server.Admission (
    ServeAdmission,
    newServeAdmission,
    withServeAdmission,
    serveAdmissionWaitMicros,

    -- * Internals exported for testing
    newServeAdmissionTuned,
) where

import Control.Concurrent.STM (retry)
import GHC.Conc (registerDelay)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception qualified as UE

import Ecluse.Core.Telemetry.Record (MetricsPort (..))

{- | A process-wide serve admission handle. The constructor is hidden so only the
checked acquire\/wait\/release operations can mutate its capacity and waiting room.
-}
newtype ServeAdmission = BoundedServeAdmission BoundedAdmission

-- The bounded handle's state: the free slots, the current waiter count, and the
-- tuning (room bound and wait budget) fixed at construction.
data BoundedAdmission = BoundedAdmission
    { baSlots :: TVar Int
    , baWaiting :: TVar Int
    , baWaitingRoom :: Int
    , baWaitMicros :: Int
    }

{- | How long an operation finding the cap busy waits for a slot before it is
refused: deliberately equal to the shed path's @Retry-After: 1@ hint, so a refusal
only ever reaches a client that has already waited one full retry interval
in-process, where the wait is a blocked green thread instead of a wire round trip.
-}
serveAdmissionWaitMicros :: Int
serveAdmissionWaitMicros = 1_000_000

{- | Allocate a bounded handle with the given positive capacity, a waiting room of
the same size, and the 'serveAdmissionWaitMicros' budget.

The room equals the capacity so a burst of twice the cap is absorbed as brief
queueing while anything deeper still gets the instant, cheap refusal -- bounding
both waiting memory and worst-case latency. Configuration parsing enforces the
positive-capacity precondition; the unchecked integer stays at this internal
composition boundary so every request pays only an STM transaction, not another
validation step.
-}

-- The configuration parser guarantees capacity > 0; this is a defense-in-depth bounds check.
{- HLINT ignore newServeAdmission "Avoid restricted function" -}
newServeAdmission :: Int -> IO ServeAdmission
newServeAdmission capacity
    | capacity <= 0 = error "ServeAdmission capacity must be positive"
    | otherwise = newServeAdmissionTuned capacity capacity serveAdmissionWaitMicros

{- | Allocate a bounded handle with an explicit waiting-room bound and wait budget
(microseconds), so a test can exercise the queueing behaviour without real-second
sleeps. Production goes through 'newServeAdmission', which fixes both from the
capacity; a room of zero reproduces pure acquire-or-refuse admission.
-}
newServeAdmissionTuned :: Int -> Int -> Int -> IO ServeAdmission
newServeAdmissionTuned capacity room waitMicros = do
    slots <- newTVarIO capacity
    waiting <- newTVarIO 0
    pure $
        BoundedServeAdmission
            BoundedAdmission
                { baSlots = slots
                , baWaiting = waiting
                , baWaitingRoom = max 0 room
                , baWaitMicros = max 0 waitMicros
                }

-- The outcome of the door transaction: a slot taken directly, a place taken in the
-- waiting room, or a refusal (the room was full).
data Gate = Admitted | Queued | Refused

-- The door transaction: decide a 'Gate' in one STM step.
doorDecision :: BoundedAdmission -> STM Gate
doorDecision ba = do
    available <- readTVar (baSlots ba)
    waiting <- readTVar (baWaiting ba)
    -- A slot is taken directly only when no one is waiting: a newcomer
    -- never jumps a non-empty waiting room.
    if available > 0 && waiting == 0
        then writeTVar (baSlots ba) (available - 1) $> Admitted
        else
            if waiting >= baWaitingRoom ba
                then pure Refused
                else writeTVar (baWaiting ba) (waiting + 1) $> Queued

-- Take a slot the moment one is free, or report expiry -- one transaction, so
-- a timeout can never race a committed acquire into a leaked slot.
acquireOrExpire :: BoundedAdmission -> TVar Bool -> STM Bool
acquireOrExpire ba deadline = do
    available <- readTVar (baSlots ba)
    if available > 0
        then writeTVar (baSlots ba) (available - 1) $> True
        else do
            expired <- readTVar deadline
            if expired then pure False else retry

{- | Run an action within the admission bound. 'Nothing' means the request was
refused -- the waiting room was full, or no slot freed within the wait budget -- and
the caller should shed it.

A request that had to wait records @ecluse.serve.admission.queued@ on admission, so
the queue's work is visible beside the in-flight gauge and the shed decisions.
-}
withServeAdmission :: (MonadUnliftIO m) => MetricsPort -> ServeAdmission -> m a -> m (Maybe a)
withServeAdmission metrics (BoundedServeAdmission ba) action =
    UE.mask $ \restore -> do
        gate <- atomically (doorDecision ba)
        case gate of
            Refused -> pure Nothing
            Admitted -> admitted restore
            Queued -> do
                deadline <- liftIO (registerDelay (baWaitMicros ba))
                -- The wait runs masked, not restored: a blocked retry is still
                -- interruptible (a cancellation aborts the transaction, taking
                -- nothing), while a committed acquire returns with exceptions
                -- masked, so the slot reaches the protected run below. The room
                -- place is surrendered on every path.
                acquired <-
                    atomically (acquireOrExpire ba deadline)
                        `UE.finally` atomically (modifyTVar' (baWaiting ba) (subtract 1))
                if acquired
                    then liftIO (mpServeAdmissionQueued metrics) >> admitted restore
                    else pure Nothing
  where
    -- The in-flight gauge is incremented under the enclosing mask, before
    -- 'restore', so it is paired with the 'release' decrement on every path. Were
    -- the increment inside 'restore' (interruptible), a cancellation delivered
    -- after unmasking but before the increment ran would still run 'release' via
    -- 'finally', decrementing a gauge that was never incremented and drifting it
    -- negative. 'restore' therefore wraps only the interruptible run.
    admitted restore =
        Just <$> ((liftIO (mpServeAdmissionInFlight metrics 1) >> restore action) `UE.finally` release)

    release = atomically (modifyTVar' (baSlots ba) (+ 1)) >> liftIO (mpServeAdmissionInFlight metrics (-1))
