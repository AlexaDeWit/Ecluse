-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared weighted admission for serve slots and publish bytes.
A bounded waiting room prevents newcomers from bypassing queued requests. Wake order is not FIFO.
-}
module Ecluse.Core.Server.Admission.Weighted (
    WeightedAdmission,
    weightedAdmissionWaiting,
    weightedAdmissionLevels,
    newWeightedAdmission,
    newWeightedAdmissionSwept,
    withWeightedAdmission,
    withWeightedAdmissionE,
    withWeightedAdmissionP,
    spikePlacesFreed,
    Shed (..),
    spikeRoomSwept,
    AdmissionObservers (..),
    admissionWaitMicros,
) where

import Control.Concurrent.STM (retry)
import GHC.Conc (registerDelay)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception qualified as UE

import Ecluse.Core.Deadline (Deadlines, withSweptFlag)

-- | Capacity and waiting-room state. Only the controller can mutate these fields.
data WeightedAdmission = WeightedAdmission
    { waAvailable :: TVar Int
    , waWaiting :: TVar Int
    , waWaitingRoom :: Int
    , waWaitMicros :: Int
    , waDeadlines :: Maybe Deadlines
    -- ^ Spike 1866: time the room wait on the sweeper instead of 'registerDelay'.
    }

-- | Spike 1866: why a request was shed.
data Shed = DoorFull | WaitExpired
    deriving stock (Eq, Show)

-- | Spike 1866: bumped whenever a room place or capacity frees, so held requests wake to try again.
spikePlacesFreed :: TVar Int
spikePlacesFreed = unsafePerformIO (newTVarIO 0)
{-# NOINLINE spikePlacesFreed #-}

-- | Spike 1866: SPIKE1866_ROOM=ghc keeps 'registerDelay' for the room wait.
spikeRoomSwept :: Bool
spikeRoomSwept = unsafePerformIO (lookupEnv "SPIKE1866_ROOM") /= Just "ghc"
{-# NOINLINE spikeRoomSwept #-}

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
newWeightedAdmission = newWeightedAdmissionSwept Nothing

-- | Spike 1866: 'newWeightedAdmission' whose room wait runs on the sweeper.
newWeightedAdmissionSwept :: Maybe Deadlines -> Int -> Int -> Int -> IO WeightedAdmission
newWeightedAdmissionSwept deadlines capacity room waitMicros = do
    available <- newTVarIO capacity
    waiting <- newTVarIO 0
    pure
        WeightedAdmission
            { waAvailable = available
            , waWaiting = waiting
            , waWaitingRoom = max 0 room
            , waWaitMicros = max 0 waitMicros
            , waDeadlines = if spikeRoomSwept then deadlines else Nothing
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
withWeightedAdmission obs wa weight action = either (const Nothing) Just <$> withWeightedAdmissionE obs wa weight action

-- | Spike 1866: 'withWeightedAdmission' that says why it shed.
{-# INLINE withWeightedAdmissionE #-}
withWeightedAdmissionE ::
    (MonadUnliftIO m) =>
    AdmissionObservers ->
    WeightedAdmission ->
    Int ->
    m a ->
    m (Either Shed a)
withWeightedAdmissionE obs wa weight = withWeightedAdmissionP obs wa weight pass

-- | Spike 1866: 'withWeightedAdmissionE' that runs @placed@ once the request holds a place or a slot.
{-# INLINE withWeightedAdmissionP #-}
withWeightedAdmissionP ::
    (MonadUnliftIO m) =>
    AdmissionObservers ->
    WeightedAdmission ->
    Int ->
    IO () ->
    m a ->
    m (Either Shed a)
withWeightedAdmissionP obs wa weight placed action =
    UE.mask $ \restore -> do
        gate <- atomically (doorDecision wa weight)
        case gate of
            Refused -> shedRecording obs DoorFull
            Admitted -> liftIO placed >> admittedRun obs wa weight (pure ()) restore action
            Queued -> liftIO placed >> queuedWait obs wa weight restore action

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
    let waitFor deadline = atomically (acquireOrExpire wa weight deadline)
    acquired <-
        liftIO (maybe (registerDelay (waWaitMicros wa) >>= waitFor) (\d -> withSweptFlag d (waWaitMicros wa) waitFor) (waDeadlines wa))
            `UE.finally` atomically (modifyTVar' (waWaiting wa) (subtract 1) >> modifyTVar' spikePlacesFreed (+ 1))
    if acquired
        then admittedRun obs wa weight (onQueued obs) restore action
        else shedRecording obs WaitExpired

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
        `UE.finally` atomically (modifyTVar' (waAvailable wa) (+ weight) >> modifyTVar' spikePlacesFreed (+ 1))

-- | Spike 1866: free capacity and room occupancy, read without a transaction.
weightedAdmissionLevels :: WeightedAdmission -> IO (Int, Int)
weightedAdmissionLevels wa = (,) <$> readTVarIO (waAvailable wa) <*> readTVarIO (waWaiting wa)

-- | Observe committed waiting-room occupancy without changing admission state.
weightedAdmissionWaiting :: WeightedAdmission -> STM Int
weightedAdmissionWaiting = readTVar . waWaiting
