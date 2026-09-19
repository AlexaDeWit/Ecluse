-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Brief-wait admission for metadata-bearing serve work: the unit-slot instance of
"Ecluse.Core.Server.Admission.Weighted", at weight one with the room equal to the capacity.
It adds only the serve-path metric hooks. A refused request is silently 'Nothing' here,
because the serve path records its own unavailability.
-}
module Ecluse.Core.Server.Admission (
    ServeAdmission,
    newServeAdmission,
    withServeAdmission,

    -- * Internals exported for testing
    newServeAdmissionTuned,
) where

import UnliftIO (MonadUnliftIO)

import Ecluse.Core.Server.Admission.Weighted (
    AdmissionObservers (..),
    WeightedAdmission,
    admissionWaitMicros,
    newWeightedAdmission,
    withWeightedAdmission,
 )
import Ecluse.Core.Telemetry.Record (MetricsPort (..))

{- | A process-wide serve admission handle. The constructor is hidden so only the
checked acquire\/wait\/release operations can mutate its capacity and waiting room.
-}
newtype ServeAdmission = ServeAdmission WeightedAdmission

{- | Allocate a handle with the given positive capacity, a waiting room of the same size, and
the shared 'admissionWaitMicros' budget, so a burst of twice the cap queues briefly.
-}

-- The configuration parser guarantees capacity > 0. This bounds check is defence in depth.
{- HLINT ignore newServeAdmission "Avoid restricted function" -}
newServeAdmission :: Int -> IO ServeAdmission
newServeAdmission capacity
    | capacity <= 0 = error "ServeAdmission capacity must be positive"
    | otherwise = newServeAdmissionTuned capacity capacity admissionWaitMicros

{- | Allocate a handle with an explicit room bound and wait budget (microseconds), so a test
exercises the queueing without real-second sleeps. A room of zero is acquire-or-refuse.
-}
newServeAdmissionTuned :: Int -> Int -> Int -> IO ServeAdmission
newServeAdmissionTuned capacity room waitMicros =
    ServeAdmission <$> newWeightedAdmission capacity room waitMicros

{- | Run an action within the admission bound. 'Nothing' means the caller should shed it: the
room was full, or no slot freed within the wait budget.
-}
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
