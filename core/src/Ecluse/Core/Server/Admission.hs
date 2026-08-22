-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Brief-wait admission control for metadata-bearing serve work: the unit-slot
instance of the shared "Ecluse.Core.Server.Admission.Weighted" core (weight one, room
equal to the capacity).

The handle caps concurrent operations and keeps a bounded room of waiters, so it
bounds aggregate metadata residency by construction. A burst that merely brushes the cap
is absorbed: near-capacity load degrades into short queueing delay rather than a refusal
the client retries at once. The core owns the door discipline, the fairness properties,
and the mask reasoning that keeps a slot from leaking between acquisition and the
protected run. This module supplies only the unit weight and the serve-path metric hooks
(the in-flight gauge and the queued signal). A refused request is silently 'Nothing'
here: the serve path records its unavailability itself.
-}
module Ecluse.Core.Server.Admission (
    ServeAdmission,
    newServeAdmission,
    withServeAdmission,
    serveAdmissionWaitMicros,

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

{- | How long an operation waits for a slot when it finds the cap busy, before the
request is refused. The budget is the shared 'admissionWaitMicros' value, equal to the
shed path's @Retry-After: 1@ hint.
-}
serveAdmissionWaitMicros :: Int
serveAdmissionWaitMicros = admissionWaitMicros

{- | Allocate a bounded handle with the given positive capacity, a waiting room of the
same size, and the 'serveAdmissionWaitMicros' budget.

The room equals the capacity, so a burst of twice the cap is absorbed as brief queueing
while anything deeper gets the instant, cheap refusal. That bounds both the waiting
memory and the worst-case latency. Configuration parsing enforces the positive-capacity
precondition. The unchecked integer stays at this internal composition boundary, so
every request pays only an STM transaction, not another validation step.
-}

-- The configuration parser guarantees capacity > 0. This bounds check is defence in depth.
{- HLINT ignore newServeAdmission "Avoid restricted function" -}
newServeAdmission :: Int -> IO ServeAdmission
newServeAdmission capacity
    | capacity <= 0 = error "ServeAdmission capacity must be positive"
    | otherwise = newServeAdmissionTuned capacity capacity serveAdmissionWaitMicros

{- | Allocate a bounded handle with an explicit waiting-room bound and wait budget
(microseconds), so a test can exercise the queueing behaviour without real-second
sleeps. Production goes through 'newServeAdmission', which fixes both from the
capacity. A room of zero reproduces pure acquire-or-refuse admission.
-}
newServeAdmissionTuned :: Int -> Int -> Int -> IO ServeAdmission
newServeAdmissionTuned capacity room waitMicros =
    ServeAdmission <$> newWeightedAdmission capacity room waitMicros

{- | Run an action within the admission bound. 'Nothing' means the request was refused,
because the waiting room was full or no slot freed within the wait budget. The caller
should shed it.

A request that had to wait records @ecluse.serve.admission.queued@ on admission, so the
queue's work is visible beside the in-flight gauge and the shed decisions.

Inlined so the literal 'AdmissionObservers' folds into the shared core's saturated call
at each request site. The admitted hot path then allocates no per-request record.
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
