-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared weighted admission for serve slots and publish bytes.
A bounded waiting room prevents newcomers from bypassing queued requests. Wake order is not FIFO.
-}
module Ecluse.Core.Server.Admission.Weighted (
    WeightedAdmission,
    weightedAdmissionWaiting,
    newWeightedAdmission,
    withWeightedAdmission,
    AdmissionObservers (..),
    admissionWaitMicros,
) where

import GHC.Conc (registerDelay)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception qualified as UE

import Ecluse.Core.Server.Admission.Probe (Site (..), invStrict, probeRetry, probed)

-- | Capacity and waiting-room state. Only the controller can mutate these fields.
data WeightedAdmission = WeightedAdmission
    { waAvailable :: TVar Int
    , waWaiting :: TVar Int
    , waWaitingRoom :: Int
    , waWaitMicros :: Int
    }

-- | Admission observers, independent of the telemetry vocabulary.
data AdmissionObservers = AdmissionObservers
    { onQueued :: IO ()
    -- ^ Runs after a queued request acquires capacity, not when it registers.
    , onShed :: IO ()
    -- ^ Records a refusal at the door or an expired wait.
    , onInFlightDelta :: Int -> IO ()
    -- ^ Records signed weight under the acquisition mask.
    }

-- | Wait one second, matching the shed response Retry-After hint.
admissionWaitMicros :: Int
admissionWaitMicros = 1_000_000

-- | Keep capacity verbatim and clamp the room and microsecond wait budget at zero.
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
    waiting <- weightedAdmissionWaiting wa
    if available >= weight && waiting == 0
        then writeCell (waAvailable wa) (available - weight) $> Admitted
        else
            if waiting >= waWaitingRoom wa
                then pure Refused
                else writeCell (waWaiting wa) (waiting + 1) $> Queued

-- Take the weight the moment it fits, or report expiry. One transaction, so a
-- timeout can never race a committed acquire into leaked weight.
acquireOrExpire :: WeightedAdmission -> Int -> TVar Bool -> STM Bool
acquireOrExpire wa weight deadline = do
    available <- readTVar (waAvailable wa)
    if available >= weight
        then writeCell (waAvailable wa) (available - weight) $> True
        else do
            expired <- readTVar deadline
            if expired then pure False else probeRetry GateWait

-- Spike only: the strict model writes evaluated counts.
writeCell :: TVar Int -> Int -> STM ()
writeCell cell value = if invStrict then writeTVar cell $! value else writeTVar cell value

-- | Hold the supplied weight through the action. Refusal or expiry returns Nothing.

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
        gate <- liftIO (probed GateDoor (doorDecision wa weight))
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
        liftIO (probed GateWait (acquireOrExpire wa weight deadline))
            `UE.finally` liftIO (probed GateLeave (modifyTVar' (waWaiting wa) (subtract 1)))
    if acquired
        then admittedRun obs wa weight (onQueued obs) restore action
        else shedRecording obs

-- The gauge increment runs under the enclosing mask, before 'restore', and 'afterArm' with it, so a
-- cancellation or a throwing observer cannot fire the finaliser's decrement without the increment.
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

-- Publish the gauge decrement before returning capacity, or a woken waiter increments while the departing
-- holder is still observable. The STM release is the finaliser, so a throwing observer cannot leak capacity.
{-# INLINE releaseWeight #-}
releaseWeight :: (MonadUnliftIO m) => AdmissionObservers -> WeightedAdmission -> Int -> m ()
releaseWeight obs wa weight =
    liftIO (onInFlightDelta obs (negate weight))
        `UE.finally` liftIO (probed GateRelease (modifyTVar' (waAvailable wa) (+ weight)))

-- | Observe committed waiting-room occupancy without changing admission state.
weightedAdmissionWaiting :: WeightedAdmission -> STM Int
weightedAdmissionWaiting = readTVar . waWaiting
