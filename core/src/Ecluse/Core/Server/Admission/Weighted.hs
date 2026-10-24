-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared weighted admission for serve slots and publish bytes.
A bounded waiting room prevents newcomers from bypassing queued requests. Wake order is not FIFO.
The room's wait runs on the deadline sweeper, and each freed place wakes the gate's overflow line.
-}
module Ecluse.Core.Server.Admission.Weighted (
    WeightedAdmission,
    weightedAdmissionWaiting,
    weightedAdmissionLine,
    newWeightedAdmission,
    withWeightedAdmission,
    AdmissionObservers (..),
    admissionWaitMicros,
) where

import Control.Concurrent.STM (retry)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception qualified as UE

import Ecluse.Core.Deadline (Deadlines, withDeadlineFlag)
import Ecluse.Core.Server.Admission.Overflow (Line, newLine, signalLine)
import Ecluse.Core.Server.Admission.Types (Shed (..))

-- | Capacity and waiting-room state. Only the controller can mutate these fields.
data WeightedAdmission = WeightedAdmission
    { waAvailable :: TVar Int
    , waWaiting :: TVar Int
    , waWaitingRoom :: Int
    , waWaitMicros :: Int
    , waDeadlines :: Deadlines
    , waLine :: Line
    -- ^ Requests this gate turned away at a full room, woken as places free.
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

{- | Keep capacity verbatim and clamp the room and microsecond wait budget at zero. A positive wait
ends on a sweep of the deadlines, so a fixture that never waits one out may leave them unswept.
-}
newWeightedAdmission :: Deadlines -> Int -> Int -> Int -> IO WeightedAdmission
newWeightedAdmission deadlines capacity room waitMicros = do
    available <- newTVarIO capacity
    waiting <- newTVarIO 0
    line <- newLine
    pure
        WeightedAdmission
            { waAvailable = available
            , waWaiting = waiting
            , waWaitingRoom = max 0 room
            , waWaitMicros = max 0 waitMicros
            , waDeadlines = deadlines
            , waLine = line
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

-- | Hold the supplied weight through the action, or say why the gate turned the request away.

-- Inlined with its arm helpers so each wrapper's literal observers vanish at the call site.
{-# INLINE withWeightedAdmission #-}
withWeightedAdmission ::
    (MonadUnliftIO m) =>
    AdmissionObservers ->
    WeightedAdmission ->
    Int ->
    m a ->
    m (Either Shed a)
withWeightedAdmission obs wa weight action =
    UE.mask $ \restore -> do
        gate <- atomically (doorDecision wa weight)
        case gate of
            Refused -> shedRecording obs RoomFull
            Admitted -> admittedRun obs wa weight (pure ()) restore action
            Queued -> queuedWait obs wa weight restore action

-- Record the shed and refuse. A room place taken on the queued path is already
-- surrendered before this runs.
{-# INLINE shedRecording #-}
shedRecording :: (MonadIO m) => AdmissionObservers -> Shed -> m (Either Shed a)
shedRecording obs why = liftIO (onShed obs) $> Left why

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
    m (Either Shed a)
queuedWait obs wa weight restore action = do
    acquired <-
        liftIO (withDeadlineFlag (waDeadlines wa) (waWaitMicros wa) (atomically . acquireOrExpire wa weight))
            `UE.finally` atomically (leaveRoom wa)
    if acquired
        then admittedRun obs wa weight (onQueued obs) restore action
        else shedRecording obs WaitExpired

-- A room place freed, so the oldest request this gate turned away may take it.
leaveRoom :: WeightedAdmission -> STM ()
leaveRoom wa = modifyTVar' (waWaiting wa) (subtract 1) >> signalLine (waLine wa)

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
    m (Either Shed a)
admittedRun obs wa weight afterArm restore action =
    Right
        <$> ( (liftIO (onInFlightDelta obs weight >> afterArm) >> restore action)
                `UE.finally` releaseWeight obs wa weight
            )

-- Publish the gauge decrement before returning capacity, or a woken waiter increments while the departing
-- holder is still observable. The STM release is the finaliser, so a throwing observer cannot leak capacity.
{-# INLINE releaseWeight #-}
releaseWeight :: (MonadUnliftIO m) => AdmissionObservers -> WeightedAdmission -> Int -> m ()
releaseWeight obs wa weight =
    liftIO (onInFlightDelta obs (negate weight))
        `UE.finally` atomically (returnWeight wa weight)

-- With a request in the room the freed weight is its, and its departure wakes the line. Otherwise the weight is the place.
returnWeight :: WeightedAdmission -> Int -> STM ()
returnWeight wa weight = do
    modifyTVar' (waAvailable wa) (+ weight)
    waiting <- readTVar (waWaiting wa)
    when (waiting == 0) (signalLine (waLine wa))

-- | The line of requests this gate turned away at a full room.
weightedAdmissionLine :: WeightedAdmission -> Line
weightedAdmissionLine = waLine

-- | Observe committed waiting-room occupancy without changing admission state.
weightedAdmissionWaiting :: WeightedAdmission -> STM Int
weightedAdmissionWaiting = readTVar . waWaiting
