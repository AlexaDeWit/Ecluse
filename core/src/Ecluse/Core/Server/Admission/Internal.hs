-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Unit-slot admission over the shared weighted controller.
module Ecluse.Core.Server.Admission.Internal (
    ServeAdmission (..),
    newServeAdmission,
    withServeAdmission,

    -- * Internals exported for testing
    newServeAdmissionTuned,
) where

import Control.Exception (ErrorCall (ErrorCall))
import UnliftIO (MonadUnliftIO, throwIO)

import Ecluse.Core.Server.Admission.Weighted (
    AdmissionObservers (..),
    WeightedAdmission,
    admissionWaitMicros,
    newWeightedAdmission,
    withWeightedAdmission,
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

-- | Set the waiting-room bound and microsecond wait budget explicitly.
newServeAdmissionTuned :: Int -> Int -> Int -> IO ServeAdmission
newServeAdmissionTuned capacity room waitMicros =
    ServeAdmission <$> newWeightedAdmission capacity room waitMicros

-- | Hold one slot for the action. A full room or expired wait returns Nothing.
{-# INLINE withServeAdmission #-}
withServeAdmission :: (MonadUnliftIO m) => MetricsPort -> ServeAdmission -> m a -> m (Maybe a)
withServeAdmission metrics (ServeAdmission core) =
    withWeightedAdmission observers core 1
  where
    observers =
        AdmissionObservers
            { onQueued = mpServeAdmissionQueued metrics
            , onShed = pure ()
            , onInFlightDelta = mpServeAdmissionInFlight metrics
            }
