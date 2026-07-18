-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Brief-wait __byte-weighted__ admission: the weighted sibling of
"Ecluse.Core.Server.Admission", capping the aggregate bytes concurrently held
rather than a count of unit slots.

The publish path buffers whole request bodies (base64-inflated tarballs bounded
only by the per-request cap), so a burst of concurrent publishes could hold many
caps' worth of heap at once with no aggregate bound. An acquisition here reserves
the request's weight -- its declared Content-Length, or the per-request cap when
the body is chunked and declares nothing -- against a fixed byte capacity before
the body is read, and releases it on every exit path. Reservation precedes
buffering and is always at least the bytes actually buffered (the size-limit
middleware refuses a body past the cap), so the aggregate holds by construction.

The door discipline is Admission's, unchanged: acquire immediately when the
capacity holds the weight and no one is waiting (a newcomer never jumps a
non-empty room), wait briefly in a bounded room otherwise, and shed past the room
or past the wait budget -- refusal is reserved for genuine overload, and the wait
budget equals the shed path's @Retry-After: 1@ hint. Waits run masked with the
blocked retry still interruptible, and a committed acquire returns with
exceptions masked, so weight can never leak between acquisition and the
protected run.
-}
module Ecluse.Core.Server.Admission.Bytes (
    ByteAdmission,
    newByteAdmission,
    withByteAdmission,
    byteAdmissionWaitMicros,

    -- * Internals exported for testing
    newByteAdmissionTuned,
) where

import Control.Concurrent.STM (retry)
import GHC.Conc (registerDelay)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception qualified as UE

import Ecluse.Core.Telemetry.Record (MetricsPort (..))

{- | A process-wide byte-admission handle. The constructor is hidden so only the
checked acquire\/wait\/release operations can mutate its capacity and room.
-}
data ByteAdmission = ByteAdmission
    { baAvailable :: TVar Int
    , baCapacity :: Int
    , baWaiting :: TVar Int
    , baWaitingRoom :: Int
    , baWaitMicros :: Int
    }

{- | How long an acquisition finding the capacity busy waits before it is shed:
the same one-retry-interval budget as the unit-slot admission, and for the same
reason -- a refusal must never be faster than the client was told to come back.
-}
byteAdmissionWaitMicros :: Int
byteAdmissionWaitMicros = 1_000_000

{- | The bounded waiting room, a count of waiters: publishes are rare and heavy,
so a short queue absorbs a brush with the capacity while anything deeper gets the
instant, cheap refusal.
-}
byteAdmissionWaiterRoom :: Int
byteAdmissionWaiterRoom = 8

-- | Allocate a handle over the given byte capacity (clamped to at least one byte).
newByteAdmission :: Int -> IO ByteAdmission
newByteAdmission capacity = newByteAdmissionTuned capacity byteAdmissionWaiterRoom byteAdmissionWaitMicros

{- | Allocate a handle with an explicit waiter-room bound and wait budget
(microseconds), so a test can exercise the queueing behaviour without real-second
sleeps. Production goes through 'newByteAdmission'.
-}
newByteAdmissionTuned :: Int -> Int -> Int -> IO ByteAdmission
newByteAdmissionTuned capacity room waitMicros = do
    let cap = max 1 capacity
    available <- newTVarIO cap
    waiting <- newTVarIO 0
    pure
        ByteAdmission
            { baAvailable = available
            , baCapacity = cap
            , baWaiting = waiting
            , baWaitingRoom = max 0 room
            , baWaitMicros = max 0 waitMicros
            }

-- The outcome of the door transaction, Admission's shape.
data Gate = Admitted | Queued | Refused

doorDecision :: ByteAdmission -> Int -> STM Gate
doorDecision ba weight = do
    available <- readTVar (baAvailable ba)
    waiting <- readTVar (baWaiting ba)
    if available >= weight && waiting == 0
        then writeTVar (baAvailable ba) (available - weight) $> Admitted
        else
            if waiting >= baWaitingRoom ba
                then pure Refused
                else writeTVar (baWaiting ba) (waiting + 1) $> Queued

-- Take the weight the moment it fits, or report expiry -- one transaction, so a
-- timeout can never race a committed acquire into leaked weight.
acquireOrExpire :: ByteAdmission -> Int -> TVar Bool -> STM Bool
acquireOrExpire ba weight deadline = do
    available <- readTVar (baAvailable ba)
    if available >= weight
        then writeTVar (baAvailable ba) (available - weight) $> True
        else do
            expired <- readTVar deadline
            if expired then pure False else retry

{- | Run an action holding the given weight against the aggregate. 'Nothing' means
the request was shed: the room was full, or the weight did not fit within the wait
budget. The weight is clamped to the capacity defensively (a request the size-limit
middleware admitted always fits the plan's floors, but a bound must never deadlock
on arithmetic it did not make), and released on every exit path -- normal
completion, a synchronous throw, and asynchronous cancellation.

The in-flight gauge (@ecluse.publish.body.in_flight_bytes@) moves with the
reserved weight, and a shed records @ecluse.publish.body.shed@.
-}
withByteAdmission :: (MonadUnliftIO m) => MetricsPort -> ByteAdmission -> Int -> m a -> m (Maybe a)
withByteAdmission metrics ba rawWeight action =
    UE.mask $ \restore -> do
        gate <- atomically (doorDecision ba weight)
        case gate of
            Refused -> shed
            Admitted -> admitted restore
            Queued -> do
                deadline <- liftIO (registerDelay (baWaitMicros ba))
                acquired <-
                    atomically (acquireOrExpire ba weight deadline)
                        `UE.finally` atomically (modifyTVar' (baWaiting ba) (subtract 1))
                if acquired then admitted restore else shed
  where
    weight = min (baCapacity ba) (max 0 rawWeight)

    shed = liftIO (mpPublishBodyShed metrics) $> Nothing

    admitted restore =
        Just <$> ((liftIO (mpPublishBodyInFlightBytes metrics weight) >> restore action) `UE.finally` release)

    release = atomically (modifyTVar' (baAvailable ba) (+ weight)) >> liftIO (mpPublishBodyInFlightBytes metrics (negate weight))
