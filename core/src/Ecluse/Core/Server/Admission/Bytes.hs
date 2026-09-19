-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Brief-wait __byte-weighted__ admission: the Content-Length-weighted instance of
"Ecluse.Core.Server.Admission.Weighted", capping the aggregate bytes held rather than a count
of slots.

The publish path buffers whole request bodies, bounded only per request, so concurrent
publishes could otherwise hold many caps' worth of heap at once. The weight is the declared
Content-Length, or the per-request cap for a chunked body, and it is reserved before the body
is read, so the reservation is always at least the bytes buffered.
-}
module Ecluse.Core.Server.Admission.Bytes (
    ByteAdmission,
    newByteAdmission,
    withByteAdmission,

    -- * Internals exported for testing
    newByteAdmissionTuned,
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

{- | A process-wide byte-admission handle. The constructor is hidden so only the checked
acquire\/wait\/release operations can mutate it.
-}
data ByteAdmission = ByteAdmission
    { baCore :: WeightedAdmission
    , baCapacity :: Int
    }

{- | The bounded waiting room, a count of waiters. Publishes are rare and heavy, so a short
queue absorbs a brush with the capacity and anything deeper is refused at once.
-}
byteAdmissionWaiterRoom :: Int
byteAdmissionWaiterRoom = 8

{- | Allocate a handle over the given byte capacity (clamped to at least one byte), with the
shared 'admissionWaitMicros' budget.
-}
newByteAdmission :: Int -> IO ByteAdmission
newByteAdmission capacity = newByteAdmissionTuned capacity byteAdmissionWaiterRoom admissionWaitMicros

{- | Allocate a handle with an explicit waiter-room bound and wait budget (microseconds), so a
test exercises the queueing without real-second sleeps.
-}
newByteAdmissionTuned :: Int -> Int -> Int -> IO ByteAdmission
newByteAdmissionTuned capacity room waitMicros = do
    let cap = max 1 capacity
    core <- newWeightedAdmission cap room waitMicros
    pure ByteAdmission{baCore = core, baCapacity = cap}

{- | Run an action holding the given weight. 'Nothing' is a shed. The weight is clamped to the
capacity, because a bound must never deadlock on arithmetic it did not make.
-}
{-# INLINE withByteAdmission #-}
withByteAdmission :: (MonadUnliftIO m) => MetricsPort -> ByteAdmission -> Int -> m a -> m (Maybe a)
withByteAdmission metrics ba rawWeight =
    withWeightedAdmission observers (baCore ba) weight
  where
    weight = min (baCapacity ba) (max 0 rawWeight)

    observers =
        AdmissionObservers
            { onQueued = pure ()
            , onShed = mpPublishBodyShed metrics
            , onInFlightDelta = mpPublishBodyInFlightBytes metrics
            }
