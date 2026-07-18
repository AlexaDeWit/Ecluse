-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The shared brief-wait admission core: a weighted door\/wait\/shed machine both
serve admission ("Ecluse.Core.Server.Admission") and byte-weighted publish admission
("Ecluse.Core.Server.Admission.Bytes") are built from. The unit-slot version is this
core at weight one with the room equal to the capacity.

A handle caps the aggregate __weight__ concurrently held and retains a __bounded room
of waiters__: an acquisition takes its weight immediately, waits briefly for room, or
is refused. This bounds aggregate residency by construction while absorbing a burst
that merely brushes the capacity, so near-capacity load degrades into short queueing
delay rather than a refusal the client immediately retries. Refusal is reserved for
genuine overload: a waiting room already at its bound (the deep-overflow band, refused
instantly and cheaply) or a wait that outlives its budget.

Instant shedding is self-amplifying under a hammering client: each refusal is answered
in microseconds, so the client comes straight back, and the refusal work itself
competes for the cores the admitted work needs. Waiting in-process is a blocked green
thread -- nearly free -- and every release goes to work that has already arrived. The
wait budget ('admissionWaitMicros') equals the shed path's @Retry-After: 1@ hint, so a
request is never refused faster than the client would have been told to come back.

Two fairness properties, one deliberate limit:

* __A newcomer never jumps a non-empty waiting room__: capacity is taken directly only
  when no one is waiting, so arrival order is respected between the room and the door.
* __Within the room, wake-up order is not FIFO__ (STM retry semantics: all waiters
  race, first commit wins). With the room bounded and turnover far faster than the
  budget, starvation is not a practical concern, and strict ticketing is complexity
  this surface has not earned.

Held weight is released across normal completion, failure, and asynchronous
cancellation. The waits run masked: a blocked STM retry remains interruptible (a
cancellation lands and aborts the transaction, taking nothing), while a committed
acquire returns with exceptions still masked, so weight can never be lost between
acquisition and the protected run. Release publishes the in-flight gauge decrement
before returning capacity to the door, so a newly admitted request cannot make the
observable gauge transiently exceed the configured bound; capacity is still returned
if that observer throws.

The two instances differ only in their construction policy (the serve handle errors on
a non-positive capacity, the byte handle clamps to one byte and clamps each call's
weight to the capacity) and in the observer callbacks they supply; the door discipline
lives here so a fix to the slot-leak-prone reasoning is made once for both.
-}
module Ecluse.Core.Server.Admission.Weighted (
    WeightedAdmission,
    newWeightedAdmission,
    withWeightedAdmission,
    AdmissionObservers (..),
    admissionWaitMicros,
) where

import Control.Concurrent.STM (retry)
import GHC.Conc (registerDelay)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception qualified as UE

{- | The bounded handle's mutable state and its tuning. The available weight and the
waiter count are the two 'TVar's the door transaction races over; the room bound and
wait budget are fixed at construction. The constructor is hidden so only the checked
acquire\/wait\/release operations can mutate it.
-}
data WeightedAdmission = WeightedAdmission
    { waAvailable :: TVar Int
    , waWaiting :: TVar Int
    , waWaitingRoom :: Int
    , waWaitMicros :: Int
    }

{- | The metric hooks the door\/wait\/release bracket calls, so the shared machine owns
no telemetry vocabulary of its own and each instance records under its own signals.
-}
data AdmissionObservers = AdmissionObservers
    { onQueued :: IO ()
    {- ^ A request that had to wait cleared the wait and is now admitted. Serve
    admission records its queued metric here; byte admission does nothing.
    -}
    , onShed :: IO ()
    {- ^ The request was shed: refused at a full door, or its wait outlived the
    budget. Byte admission records its shed metric here; serve admission is silent.
    -}
    , onInFlightDelta :: Int -> IO ()
    {- ^ Move the in-flight gauge by the signed weight: @+weight@ on admission,
    @-weight@ on release. Both calls run under the acquire mask, so the gauge is
    paired on every path.
    -}
    }

{- | The wait budget (microseconds) an acquisition finding the capacity busy waits
before it is shed: deliberately equal to the shed path's @Retry-After: 1@ hint, so a
refusal only ever reaches a client that has already waited one full retry interval
in-process, where the wait is a blocked green thread instead of a wire round trip.
-}
admissionWaitMicros :: Int
admissionWaitMicros = 1_000_000

{- | Allocate a bounded handle over the given capacity, a waiter-room bound, and a wait
budget (microseconds). The capacity is taken verbatim; the constructor's caller (the
serve or byte wrapper) owns the positive-capacity policy. The room and budget are
floored at zero, so a room of zero reproduces pure acquire-or-refuse admission.
-}
newWeightedAdmission :: Int -> Int -> Int -> IO WeightedAdmission
newWeightedAdmission capacity room waitMicros = do
    available <- newTVarIO capacity
    waiting <- newTVarIO 0
    pure
        WeightedAdmission
            { waAvailable = available
            , waWaiting = waiting
            , waWaitingRoom = max 0 room
            , waWaitMicros = max 0 waitMicros
            }

-- The outcome of the door transaction: the weight taken directly, a place taken in the
-- waiting room, or a refusal (the room was full).
data Gate = Admitted | Queued | Refused

-- The door transaction: decide a 'Gate' in one STM step. The weight is taken directly
-- only when no one is waiting, so a newcomer never jumps a non-empty waiting room.
doorDecision :: WeightedAdmission -> Int -> STM Gate
doorDecision wa weight = do
    available <- readTVar (waAvailable wa)
    waiting <- readTVar (waWaiting wa)
    if available >= weight && waiting == 0
        then writeTVar (waAvailable wa) (available - weight) $> Admitted
        else
            if waiting >= waWaitingRoom wa
                then pure Refused
                else writeTVar (waWaiting wa) (waiting + 1) $> Queued

-- Take the weight the moment it fits, or report expiry -- one transaction, so a
-- timeout can never race a committed acquire into leaked weight.
acquireOrExpire :: WeightedAdmission -> Int -> TVar Bool -> STM Bool
acquireOrExpire wa weight deadline = do
    available <- readTVar (waAvailable wa)
    if available >= weight
        then writeTVar (waAvailable wa) (available - weight) $> True
        else do
            expired <- readTVar deadline
            if expired then pure False else retry

{- | Run an action holding the given weight against the aggregate. 'Nothing' means the
request was shed -- the room was full, or the weight did not fit within the wait budget
-- and the caller should refuse it. The weight is used as given; a per-instance clamp is
the wrapper's responsibility. Held weight is released on every exit path: normal
completion, a synchronous throw, and asynchronous cancellation.

Marked @INLINE@ so each wrapper's literal 'AdmissionObservers' is eliminated at its call
site (case-of-known-constructor), leaving the same code the two hand-written twins
compiled to: the extraction is allocation-neutral on the admitted hot path.
-}
{-# INLINE withWeightedAdmission #-}
withWeightedAdmission ::
    (MonadUnliftIO m) =>
    AdmissionObservers ->
    WeightedAdmission ->
    Int ->
    m a ->
    m (Maybe a)
withWeightedAdmission obs wa weight action =
    UE.mask $ \restore -> do
        gate <- atomically (doorDecision wa weight)
        case gate of
            Refused -> shed
            Admitted -> admitted restore (pure ())
            Queued -> do
                deadline <- liftIO (registerDelay (waWaitMicros wa))
                -- The wait runs masked, not restored: a blocked retry is still
                -- interruptible (a cancellation aborts the transaction, taking
                -- nothing), while a committed acquire returns with exceptions masked,
                -- so the weight reaches the protected run below. The room place is
                -- surrendered on every path.
                acquired <-
                    atomically (acquireOrExpire wa weight deadline)
                        `UE.finally` atomically (modifyTVar' (waWaiting wa) (subtract 1))
                -- The queued record runs through 'admitted', after the in-flight
                -- increment and under the release 'finally', so a throwing observer
                -- releases the held weight instead of leaking it (see 'admitted').
                if acquired then admitted restore (onQueued obs) else shed
  where
    shed = liftIO (onShed obs) $> Nothing

    -- The in-flight gauge is moved under the enclosing mask, before 'restore', so it is
    -- paired with the 'release' decrement on every path. Were the increment inside
    -- 'restore' (interruptible), a cancellation delivered after unmasking but before it
    -- ran would still run 'release' via 'finally', decrementing a gauge that was never
    -- incremented and drifting it negative. 'restore' therefore wraps only the
    -- interruptible run.
    --
    -- 'afterArm' runs in that same masked, release-protected step, after the increment:
    -- the queued path passes 'onQueued' here (the door path passes nothing) so a
    -- throwing queued observer releases the held weight rather than leaking it, and,
    -- running after the increment, can never decrement a gauge that was not raised.
    admitted restore afterArm =
        Just <$> ((liftIO (onInFlightDelta obs weight >> afterArm) >> restore action) `UE.finally` release)

    -- Publish the gauge decrement before waking a waiter. Returning capacity first
    -- would let that waiter publish its increment while the departing holder was
    -- still observable, transiently putting the gauge above the configured bound.
    -- The STM release is the finalizer so a throwing observer cannot leak capacity.
    release =
        liftIO (onInFlightDelta obs (negate weight))
            `UE.finally` atomically (modifyTVar' (waAvailable wa) (+ weight))
