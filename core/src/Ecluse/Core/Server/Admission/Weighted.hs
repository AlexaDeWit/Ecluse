-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The shared brief-wait admission core: a weighted door\/wait\/shed machine both
serve admission ("Ecluse.Core.Server.Admission") and byte-weighted publish admission
("Ecluse.Core.Server.Admission.Bytes") are built from. The unit-slot version is this
core at weight one with the room equal to the capacity.

A handle caps the aggregate __weight__ concurrently held and keeps a __bounded room
of waiters__. An acquisition takes its weight at once, waits briefly for room, or is
refused. That bounds aggregate residency by construction and still absorbs a burst
that merely brushes the capacity. Near-capacity load degrades into short queueing delay
rather than a refusal the client retries at once. Refusal is reserved for genuine
overload: a waiting room already at its bound, or a wait that outlives its budget. A
room at its bound is the deep-overflow band, refused instantly and cheaply.

Instant shedding is self-amplifying under a hammering client. Each refusal is answered
in microseconds, so the client comes straight back, and the refusal work competes for
the cores the admitted work needs. Waiting in-process is a blocked green thread, which
is nearly free, and every release goes to work that has already arrived. The wait budget
('admissionWaitMicros') equals the shed path's @Retry-After: 1@ hint. A request is
therefore never refused faster than the interval the client was told to wait.

Two fairness properties, one deliberate limit:

* __A newcomer never jumps a non-empty waiting room__. Capacity is taken directly only
  when no one is waiting, so the door respects arrival order.
* Within the room, __wake-up order is not FIFO__ (STM retry semantics: all waiters
  race, first commit wins). With the room bounded and turnover far faster than the
  budget, starvation is not a practical concern. Strict ticketing is complexity this
  surface has not earned.

Held weight is released across normal completion, failure, and asynchronous
cancellation. The waits run masked. A blocked STM retry stays interruptible: a
cancellation lands and aborts the transaction, taking nothing. A committed acquire
returns with exceptions still masked. Weight can therefore never be lost between
acquisition and the protected run. Release publishes the in-flight gauge decrement
before it returns capacity to the door. A newly admitted request therefore cannot push
the observable gauge past the configured bound. Capacity is still returned if that
observer throws.

The two instances differ only in their construction policy and in the observer callbacks
they supply. The serve handle errors on a non-positive capacity. The byte handle clamps
to one byte, and clamps each call's weight to the capacity. The door discipline lives
here, so a fix to the slot-leak-prone reasoning is made once for both.
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

{- | The bounded handle's mutable state and its tuning. The constructor stays hidden so only
the checked acquire, wait, and release operations can mutate it.
-}
data WeightedAdmission = WeightedAdmission
    { waAvailable :: TVar Int
    , waWaiting :: TVar Int
    , waWaitingRoom :: Int
    , waWaitMicros :: Int
    }

{- | The metric hooks the door\/wait\/release bracket calls. The shared machine owns no
telemetry vocabulary, so each instance records under its own signals.
-}
data AdmissionObservers = AdmissionObservers
    { onQueued :: IO ()
    {- ^ A request that had to wait cleared the wait and is now admitted. Serve
    admission records its queued metric here. Byte admission does nothing.
    -}
    , onShed :: IO ()
    {- ^ The request was shed: refused at a full door, or its wait outlived the
    budget. Byte admission records its shed metric here. Serve admission is silent.
    -}
    , onInFlightDelta :: Int -> IO ()
    {- ^ Move the in-flight gauge by the signed weight: @+weight@ on admission,
    @-weight@ on release. Both calls run under the acquire mask, so the gauge is
    paired on every path.
    -}
    }

{- | The wait budget (microseconds) an acquisition finding the capacity busy waits
before it is shed: deliberately equal to the shed path's @Retry-After: 1@ hint. A
refusal therefore only reaches a client that has already waited one full retry interval
in-process. That wait is a blocked green thread, not a wire round trip.
-}
admissionWaitMicros :: Int
admissionWaitMicros = 1_000_000

{- | Allocate a bounded handle over the given capacity, a waiter-room bound, and a wait
budget (microseconds). The capacity is taken verbatim: the caller (the serve or byte
wrapper) owns the positive-capacity policy. The room and budget are floored at zero, so
a room of zero reproduces pure acquire-or-refuse admission.
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

-- Take the weight the moment it fits, or report expiry. One transaction, so a
-- timeout can never race a committed acquire into leaked weight.
acquireOrExpire :: WeightedAdmission -> Int -> TVar Bool -> STM Bool
acquireOrExpire wa weight deadline = do
    available <- readTVar (waAvailable wa)
    if available >= weight
        then writeTVar (waAvailable wa) (available - weight) $> True
        else do
            expired <- readTVar deadline
            if expired then pure False else retry

{- | Run an action holding the given weight against the aggregate. 'Nothing' means the request
was shed: the room was full, or the weight did not fit within the wait budget. The weight is
used as given, so a per-instance clamp is the wrapper's responsibility. Held weight is released
on every exit path, including a synchronous throw and asynchronous cancellation.

Marked @INLINE@, with its arm helpers, so each wrapper's literal 'AdmissionObservers' is
eliminated at the call site and the admitted hot path stays allocation-neutral.
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
            Refused -> shedRecording obs
            Admitted -> admittedRun obs wa weight (pure ()) restore action
            Queued -> queuedWait obs wa weight restore action

-- Record the shed and refuse. A room place taken on the queued path is already
-- surrendered before this runs.
{-# INLINE shedRecording #-}
shedRecording :: (MonadIO m) => AdmissionObservers -> m (Maybe a)
shedRecording obs = liftIO (onShed obs) $> Nothing

-- The wait runs masked. A blocked STM retry stays interruptible, so a cancellation aborts the
-- transaction taking nothing, and a committed acquire returns masked with the weight held. The
-- room place is surrendered on every path. 'onQueued' is passed to 'admittedRun' so a throwing
-- observer releases the held weight instead of leaking it.
{-# INLINE queuedWait #-}
queuedWait ::
    (MonadUnliftIO m) =>
    AdmissionObservers ->
    WeightedAdmission ->
    Int ->
    (m a -> m a) ->
    m a ->
    m (Maybe a)
queuedWait obs wa weight restore action = do
    deadline <- liftIO (registerDelay (waWaitMicros wa))
    acquired <-
        atomically (acquireOrExpire wa weight deadline)
            `UE.finally` atomically (modifyTVar' (waWaiting wa) (subtract 1))
    if acquired
        then admittedRun obs wa weight (onQueued obs) restore action
        else shedRecording obs

-- The in-flight increment runs under the enclosing mask, before 'restore', so it pairs with
-- the 'releaseWeight' decrement on every path. Inside 'restore' it would be interruptible: a
-- cancellation delivered after unmasking but before it ran would still trigger the 'finally',
-- decrementing a gauge that was never incremented and drifting it negative. 'afterArm' runs in
-- that same masked step after the increment, so a throwing observer cannot leak the weight.
{-# INLINE admittedRun #-}
admittedRun ::
    (MonadUnliftIO m) =>
    AdmissionObservers ->
    WeightedAdmission ->
    Int ->
    IO () ->
    (m a -> m a) ->
    m a ->
    m (Maybe a)
admittedRun obs wa weight afterArm restore action =
    Just
        <$> ( (liftIO (onInFlightDelta obs weight >> afterArm) >> restore action)
                `UE.finally` releaseWeight obs wa weight
            )

-- Publish the gauge decrement before waking a waiter. Returning capacity first would let
-- that waiter publish its increment while the departing holder was still observable,
-- transiently putting the gauge above the configured bound. The STM release is the
-- finaliser, so a throwing observer cannot leak capacity.
{-# INLINE releaseWeight #-}
releaseWeight :: (MonadUnliftIO m) => AdmissionObservers -> WeightedAdmission -> Int -> m ()
releaseWeight obs wa weight =
    liftIO (onInFlightDelta obs (negate weight))
        `UE.finally` atomically (modifyTVar' (waAvailable wa) (+ weight))
