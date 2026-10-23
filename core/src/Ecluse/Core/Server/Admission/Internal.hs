-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Unit-slot admission over the shared weighted controller.
module Ecluse.Core.Server.Admission.Internal (
    ServeAdmission (..),
    newServeAdmission,
    newServeAdmissionSwept,
    withServeAdmission,
    withServeAdmissionE,
    withServeAdmissionP,
    serveAdmissionLevels,

    -- * Internals exported for testing
    newServeAdmissionTuned,
) where

import Control.Exception (ErrorCall (ErrorCall))
import UnliftIO (MonadUnliftIO, throwIO)

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Server.Admission.Weighted (
    AdmissionObservers (..),
    Shed,
    WeightedAdmission,
    admissionWaitMicros,
    newWeightedAdmission,
    newWeightedAdmissionSwept,
    withWeightedAdmission,
    withWeightedAdmissionE,
    withWeightedAdmissionP,
    weightedAdmissionLevels,
 )
import Ecluse.Core.Telemetry.Record (MetricsPort (..))

-- | Process-wide serve capacity and bounded waiting-room state.
newtype ServeAdmission = ServeAdmission WeightedAdmission

-- | Allocate positive capacity with a matching waiting room and the shared wait budget.

-- The configuration parser guarantees capacity > 0. This bounds check is defence in depth.
newServeAdmission :: Int -> IO ServeAdmission
newServeAdmission capacity
    | capacity <= 0 = throwIO (ErrorCall "ServeAdmission capacity must be positive")
    | otherwise = newServeAdmissionTuned capacity capacity admissionWaitMicros

-- | Spike 1866: capacity with its room wait on the sweeper. SPIKE1866_ROOM_FACTOR scales the room.
newServeAdmissionSwept :: Deadlines -> Int -> Int -> IO ServeAdmission
newServeAdmissionSwept deadlines roomFactor capacity
    | capacity <= 0 = throwIO (ErrorCall "ServeAdmission capacity must be positive")
    | otherwise = ServeAdmission <$> newWeightedAdmissionSwept (Just deadlines) capacity (capacity * max 1 roomFactor) admissionWaitMicros

-- | Set the waiting-room bound and microsecond wait budget explicitly.
newServeAdmissionTuned :: Int -> Int -> Int -> IO ServeAdmission
newServeAdmissionTuned capacity room waitMicros =
    ServeAdmission <$> newWeightedAdmission capacity room waitMicros

-- | Hold one slot for the action. A full room or expired wait returns Nothing.
{-# INLINE withServeAdmission #-}
withServeAdmission :: (MonadUnliftIO m) => MetricsPort -> ServeAdmission -> m a -> m (Maybe a)
withServeAdmission metrics (ServeAdmission core) =
    withWeightedAdmission (serveObservers metrics) core 1

-- | Spike 1866: free slots and room occupancy.
serveAdmissionLevels :: ServeAdmission -> IO (Int, Int)
serveAdmissionLevels (ServeAdmission core) = weightedAdmissionLevels core

-- | Spike 1866: 'withServeAdmission' that says why it shed.
{-# INLINE withServeAdmissionE #-}
withServeAdmissionE :: (MonadUnliftIO m) => MetricsPort -> ServeAdmission -> m a -> m (Either Shed a)
withServeAdmissionE metrics (ServeAdmission core) =
    withWeightedAdmissionE (serveObservers metrics) core 1

-- | Spike 1866: 'withServeAdmissionE' that runs @placed@ once the request holds a place or a slot.
{-# INLINE withServeAdmissionP #-}
withServeAdmissionP :: (MonadUnliftIO m) => MetricsPort -> ServeAdmission -> IO () -> m a -> m (Either Shed a)
withServeAdmissionP metrics (ServeAdmission core) =
    withWeightedAdmissionP (serveObservers metrics) core 1

serveObservers :: MetricsPort -> AdmissionObservers
serveObservers metrics = observers
  where
    observers =
        AdmissionObservers
            { onQueued = mpServeAdmissionQueued metrics
            , onShed = pure ()
            , onInFlightDelta = mpServeAdmissionInFlight metrics
            }
