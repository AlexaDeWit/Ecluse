-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The weighted door\/wait\/shed admission core behind serve admission
("Ecluse.Core.Server.Admission") and byte-weighted publish admission
("Ecluse.Core.Server.Admission.Bytes").

A handle caps the aggregate weight held at once and keeps a bounded room of waiters.
Capacity is taken directly only when the room is empty, so a newcomer never jumps a
non-empty room, though wake order within the room is not FIFO (an STM retry races every
waiter). The wait budget equals the shed path's @Retry-After: 1@ hint, so nothing is
refused faster than the interval the client was told to wait.
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
    {- ^ Move the in-flight gauge by the signed weight. Both calls run under the
    acquire mask, so the gauge is paired on every path.
    -}
    }

{- | The wait budget (microseconds) before a busy acquisition is shed, deliberately equal to
the shed path's @Retry-After: 1@ hint, so nothing is refused faster than the client was told.
-}
admissionWaitMicros :: Int
admissionWaitMicros = 1_000_000

{- | Allocate a handle over a capacity, a waiter-room bound, and a wait budget (microseconds).
The capacity is verbatim, the wrapper owning that policy, and the other two floor at zero.
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

{- | Run an action holding the given weight. 'Nothing' is a shed, at a full room or an expired
wait. The weight is used as given, and released on every exit path, cancellation included.
-}

-- Inlined with its arm helpers so each wrapper's literal observers vanish at the call site.
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

-- A blocked STM retry stays interruptible under the mask, so a cancellation aborts it taking
-- nothing while a committed acquire returns with the weight held and exceptions still masked.
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

-- The gauge increment runs under the enclosing mask, before 'restore'. Inside 'restore' a
-- cancellation could fire the finaliser's decrement without it, drifting the gauge negative.
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

-- Publish the gauge decrement before returning capacity. The other order would let a woken
-- waiter increment while the departing holder is still observable, breaching the bound.
{-# INLINE releaseWeight #-}
releaseWeight :: (MonadUnliftIO m) => AdmissionObservers -> WeightedAdmission -> Int -> m ()
releaseWeight obs wa weight =
    liftIO (onInFlightDelta obs (negate weight))
        `UE.finally` atomically (modifyTVar' (waAvailable wa) (+ weight))
