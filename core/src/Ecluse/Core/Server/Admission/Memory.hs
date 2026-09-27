-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Feedback memory admission: memory-heavy metadata work starts only while measured memory,
plus a reservation for admitted work not yet measured, stays under a share of the process's
ceiling, and only while the collector brake is released. Cheap work never touches it.

A held request waits up to 'admissionWaitMicros', then sheds with the serve path's retry hint.
Callers take this gate before the CPU slot ("Ecluse.Core.Server.Admission"), so a waiting request
never holds CPU capacity. The decisions are pure ("Ecluse.Core.Server.Admission.Memory.Gate"),
and a sampler outside the core publishes each reading with 'publishReading'.
-}
module Ecluse.Core.Server.Admission.Memory (
    -- * The handle
    MemoryAdmission,
    newMemoryAdmission,
    newMemoryAdmissionTuned,
    waitingRoomPerCpuSlot,

    -- * Admitting work
    MemoryWork (..),
    selectedMemoryWork,
    withMemoryAdmission,

    -- * The sampler's side
    publishReading,
    GateStats (..),
    readGateStats,
    renderGateStats,
) where

import Control.Concurrent.STM (retry)
import GHC.Conc (registerDelay)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception qualified as UE

import Ecluse.Core.Server.Admission.Memory.Brake (BrakeState (BrakeEngaged))
import Ecluse.Core.Server.Admission.Memory.Gate (
    Decision (Admit, Shed, Wait),
    GateCore,
    GateState (GateClosed),
    GateThresholds,
    Hold (HoldBrake, HoldMemory),
    Reading (rdBrake),
    ReservationKey,
    corePressure,
    coreState,
    decide,
    defaultGateThresholds,
    listingReservationBytes,
    newGateCore,
    observe,
    release,
    selectedReservationBytes,
 )
import Ecluse.Core.Server.Admission.Weighted (admissionWaitMicros)
import Ecluse.Core.Server.Cache.Store (MaterialReuse (KnownLocalReuse, NeedsMaterialisation))
import Ecluse.Core.Telemetry.Record (MetricsPort (..))

-- | The process-wide gate. Its constructor stays hidden so only the checked operations change it.
data MemoryAdmission = MemoryAdmission
    { maCore :: TVar GateCore
    , maWaiting :: TVar Int
    , maStats :: IORef GateStats
    , maThresholds :: GateThresholds
    , maRoom :: Int
    , maWaitMicros :: Int
    }

-- | Counters over the gate's life, for a load test's report. Metrics carry the same signals.
data GateStats = GateStats
    { gsAdmitted :: Int
    , gsWaited :: Int
    -- ^ Requests that had to wait, admitted or not.
    , gsShedMemory :: Int
    , gsShedBrake :: Int
    , gsSamples :: Int
    , gsClosedSamples :: Int
    , gsBrakeSamples :: Int
    , gsPeakPressurePermille :: Int
    }
    deriving stock (Eq, Show)

{- | A gate with the shipped thresholds and the shared wait budget, its waiting room sized from
the CPU admission capacity.
-}
newMemoryAdmission :: Int -> IO MemoryAdmission
newMemoryAdmission cpuCapacity =
    newMemoryAdmissionTuned defaultGateThresholds (waitingRoomPerCpuSlot * max 1 cpuCapacity) admissionWaitMicros

{- | Waiting places per CPU slot. A full room sheds at once, and an instant shed invites an
instant retry, so the room is wider than the CPU gate's.
-}
waitingRoomPerCpuSlot :: Int
waitingRoomPerCpuSlot = 4

-- | A gate with explicit thresholds, room, and wait budget (microseconds), for tests.
newMemoryAdmissionTuned :: GateThresholds -> Int -> Int -> IO MemoryAdmission
newMemoryAdmissionTuned thresholds room waitMicros = do
    core <- newTVarIO newGateCore
    waiting <- newTVarIO 0
    stats <- newIORef (GateStats 0 0 0 0 0 0 0 0)
    pure (MemoryAdmission core waiting stats thresholds (max 0 room) (max 0 waitMicros))

-- | How much memory a request's work may materialise.
data MemoryWork
    = -- | Served from retained values, or small: never held.
      CheapWork
    | -- | A listing, which reads and decodes its origins on every request.
      ColdListing
    | -- | A selected-version read that misses local retention.
      ColdSelectedRead
    deriving stock (Eq, Show)

-- | A selected read is heavy only when it cannot reuse a locally retained value.
selectedMemoryWork :: MaterialReuse -> MemoryWork
selectedMemoryWork = \case
    KnownLocalReuse -> CheapWork
    NeedsMaterialisation -> ColdSelectedRead

reservationFor :: MemoryWork -> Maybe Int
reservationFor = \case
    CheapWork -> Nothing
    ColdListing -> Just listingReservationBytes
    ColdSelectedRead -> Just selectedReservationBytes

{- | Run heavy work once the gate admits it, releasing its reservation on every exit path.
'Nothing' is a shed: no room to wait, or the gate stayed shut past the wait budget.
-}
withMemoryAdmission :: (MonadUnliftIO m) => MetricsPort -> MemoryAdmission -> MemoryWork -> m a -> m (Maybe a)
withMemoryAdmission metrics gate work action = case reservationFor work of
    Nothing -> Just <$> action
    Just bytes -> UE.mask $ \restore -> do
        atDoor <- atomically (door gate bytes)
        case atDoor of
            Admit key -> admittedRun gate key restore action
            Shed hold -> shedRecording metrics gate hold
            Wait _ -> do
                liftIO (mpMemoryAdmissionWait metrics >> bump gate (\s -> s{gsWaited = gsWaited s + 1}))
                waited <- awaitAdmission gate bytes
                case waited of
                    Right key -> admittedRun gate key restore action
                    Left hold -> shedRecording metrics gate hold

-- The first decision. A hold takes a place in the waiting room when one is free.
door :: MemoryAdmission -> Int -> STM Decision
door gate bytes = do
    waiting <- readTVar (maWaiting gate)
    decision <- step gate bytes (waiting < maRoom gate)
    case decision of
        Wait _ -> writeTVar (maWaiting gate) (waiting + 1)
        _ -> pass
    pure decision

step :: MemoryAdmission -> Int -> Bool -> STM Decision
step gate bytes mayWait = do
    core <- readTVar (maCore gate)
    let (decision, core') = decide (maThresholds gate) bytes mayWait core
    writeTVar (maCore gate) core'
    pure decision

-- Block until admitted or the budget expires. The STM retry stays interruptible under the mask,
-- and the room place is surrendered on every exit.
awaitAdmission :: (MonadUnliftIO m) => MemoryAdmission -> Int -> m (Either Hold ReservationKey)
awaitAdmission gate bytes = do
    deadline <- liftIO (registerDelay (maWaitMicros gate))
    atomically (waitStep deadline)
        `UE.finally` atomically (modifyTVar' (maWaiting gate) (subtract 1))
  where
    waitStep deadline = do
        expired <- readTVar deadline
        decision <- step gate bytes (not expired)
        case decision of
            Admit key -> pure (Right key)
            Shed hold -> pure (Left hold)
            Wait _ -> retry

admittedRun :: (MonadUnliftIO m) => MemoryAdmission -> ReservationKey -> (m a -> m a) -> m a -> m (Maybe a)
admittedRun gate key restore action = do
    liftIO (bump gate (\s -> s{gsAdmitted = gsAdmitted s + 1}))
    Just <$> (restore action `UE.finally` atomically (modifyTVar' (maCore gate) (release key)))

shedRecording :: (MonadIO m) => MetricsPort -> MemoryAdmission -> Hold -> m (Maybe a)
shedRecording metrics gate hold = liftIO $ do
    mpMemoryAdmissionShed metrics
    bump gate $ \s -> case hold of
        HoldMemory -> s{gsShedMemory = gsShedMemory s + 1}
        HoldBrake -> s{gsShedBrake = gsShedBrake s + 1}
    pure Nothing

bump :: MemoryAdmission -> (GateStats -> GateStats) -> IO ()
bump gate f = atomicModifyIORef' (maStats gate) (\s -> (f s, ()))

{- | Install the sampler's latest reading and return the settled core, so the sampler can
report the gate state, the reservation, and the pressure it produced.
-}
publishReading :: MemoryAdmission -> Reading -> IO GateCore
publishReading gate reading = do
    core <- atomically $ do
        core <- observe (maThresholds gate) reading <$> readTVar (maCore gate)
        writeTVar (maCore gate) core
        pure core
    bump gate $ \s ->
        s
            { gsSamples = gsSamples s + 1
            , gsClosedSamples = gsClosedSamples s + fromEnum (coreState core == GateClosed)
            , gsBrakeSamples = gsBrakeSamples s + fromEnum (rdBrake reading == BrakeEngaged)
            , gsPeakPressurePermille = max (gsPeakPressurePermille s) (maybe 0 permille (corePressure core))
            }
    pure core
  where
    permille p = round (p * 1000) :: Int

-- | The counters so far.
readGateStats :: MemoryAdmission -> IO GateStats
readGateStats = readIORef . maStats

-- | One log line of the counters.
renderGateStats :: GateStats -> Text
renderGateStats s =
    "memory admission: admitted "
        <> show (gsAdmitted s)
        <> ", waited "
        <> show (gsWaited s)
        <> ", shed on memory "
        <> show (gsShedMemory s)
        <> ", shed on brake "
        <> show (gsShedBrake s)
        <> ", samples "
        <> show (gsSamples s)
        <> " (closed "
        <> show (gsClosedSamples s)
        <> ", braking "
        <> show (gsBrakeSamples s)
        <> "), peak pressure "
        <> show (gsPeakPressurePermille s)
        <> " permille"
