-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Brief-wait __byte-weighted__ admission: the Content-Length-weighted instance of the
shared "Ecluse.Core.Server.Admission.Weighted" core, capping the aggregate bytes
concurrently held rather than a count of unit slots.

The publish path buffers whole request bodies: base64-inflated tarballs bounded only by
the per-request cap. A burst of concurrent publishes could therefore hold many caps'
worth of heap at once, with no aggregate bound. An acquisition here reserves the
request's weight against a fixed byte capacity before the body is read, and releases it
on every exit path. The weight is the declared Content-Length, or the per-request cap
when the body is chunked and declares nothing. Reservation precedes buffering, and the
publish route's bounded read refuses a body past the cap. The reserved weight is
therefore always at least the bytes buffered, so the aggregate holds by construction.

The door discipline is the core's: take the weight at once when the capacity holds it
and no one is waiting. Otherwise wait briefly in a bounded room, and shed past the room
or past the wait budget. This module supplies only the byte capacity for the
per-call weight clamp and the publish-path metric hooks (the in-flight byte gauge and
the shed signal). Unlike serve admission, a shed here records
@ecluse.publish.body.shed@ on both the door refusal and the expired wait.
-}
module Ecluse.Core.Server.Admission.Bytes (
    ByteAdmission,
    newByteAdmission,
    withByteAdmission,
    byteAdmissionWaitMicros,

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

{- | A process-wide byte-admission handle: the shared bounded core plus the byte
capacity the per-call weight is clamped to. The constructor is hidden so only the
checked acquire\/wait\/release operations can mutate it.
-}
data ByteAdmission = ByteAdmission
    { baCore :: WeightedAdmission
    , baCapacity :: Int
    }

{- | How long an acquisition finding the capacity busy waits before it is shed: the shared
'admissionWaitMicros' budget, which explains why a refusal never beats the retry hint.
-}
byteAdmissionWaitMicros :: Int
byteAdmissionWaitMicros = admissionWaitMicros

{- | The bounded waiting room, a count of waiters. Publishes are rare and heavy, so a short
queue absorbs a brush with the capacity and anything deeper is refused at once.
-}
byteAdmissionWaiterRoom :: Int
byteAdmissionWaiterRoom = 8

-- | Allocate a handle over the given byte capacity (clamped to at least one byte).
newByteAdmission :: Int -> IO ByteAdmission
newByteAdmission capacity = newByteAdmissionTuned capacity byteAdmissionWaiterRoom byteAdmissionWaitMicros

{- | Allocate a handle with an explicit waiter-room bound and wait budget (microseconds),
so a test can exercise the queueing behaviour without real-second sleeps. Production
goes through 'newByteAdmission'.
-}
newByteAdmissionTuned :: Int -> Int -> Int -> IO ByteAdmission
newByteAdmissionTuned capacity room waitMicros = do
    let cap = max 1 capacity
    core <- newWeightedAdmission cap room waitMicros
    pure ByteAdmission{baCore = core, baCapacity = cap}

{- | Run an action holding the given weight against the aggregate. 'Nothing' means the request
was shed: the room was full, or the weight did not fit within the wait budget.

The weight is clamped to the capacity defensively, since a bound must never deadlock on
arithmetic it did not make. The in-flight gauge @ecluse.publish.body.in_flight_bytes@ moves
with the reserved weight, and a shed records @ecluse.publish.body.shed@.
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
