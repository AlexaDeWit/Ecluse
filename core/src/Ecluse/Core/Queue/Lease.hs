-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The receipt-lease vocabulary a queue backend writes and the worker's renewal
controller reads ("Ecluse.Core.Worker.Lease").

A backend that hides a received message stamps each delivery with the window it granted,
when that window lapses, and the ceiling its own protocol puts on one receipt. The
instants are monotonic, so a wall-clock adjustment can never read as a longer lease. A
backend that never expires a delivery supplies no lease at all.
-}
module Ecluse.Core.Queue.Lease (
    -- * Durations
    Seconds (..),

    -- * The clock a lease is measured on
    MonoTime (..),
    monotonicNow,
    monoAfter,
    monoSecondsBetween,

    -- * One delivery's lease
    ReceiptLease (..),
    receiptLease,
) where

import GHC.Clock (getMonotonicTime)

{- | A duration in whole seconds, for 'Ecluse.Core.Queue.extendVisibility'. A 'newtype', so a
raw @Int@ of seconds cannot pass for some other count.
-}
newtype Seconds = Seconds Int
    deriving stock (Eq, Ord, Show)

{- | A reading of the monotonic clock, in seconds from an arbitrary origin. Lease deadlines
are measured on it, because a wall-clock step must never appear to extend one.
-}
newtype MonoTime = MonoTime Double
    deriving stock (Eq, Ord, Show)

-- | Read the monotonic clock, for a backend stamping a delivery it is about to hand over.
monotonicNow :: IO MonoTime
monotonicNow = MonoTime <$> getMonotonicTime

-- | The instant this many seconds after the given one. A negative offset reads backwards.
monoAfter :: MonoTime -> Double -> MonoTime
monoAfter (MonoTime at) offset = MonoTime (at + offset)

-- | The seconds from the first instant to the second, negative once the second has passed.
monoSecondsBetween :: MonoTime -> MonoTime -> Double
monoSecondsBetween (MonoTime from') (MonoTime to') = to' - from'

{- | What one delivery's lease grants: the window it stays hidden for, when that window
lapses, and the ceiling the backend holds the whole receipt under.
-}
data ReceiptLease = ReceiptLease
    { rlWindow :: Seconds
    -- ^ The window the backend granted, which each renewal asks for again.
    , rlExpiresAt :: MonoTime
    -- ^ When the current window lapses and another consumer may take the delivery.
    , rlCeilingAt :: MonoTime
    {- ^ The last instant the backend holds this receipt at all, however often it is renewed
    (SQS allows twelve hours from the first receipt).
    -}
    }
    deriving stock (Eq, Show)

{- | A lease stamped from the instant the delivery was asked for, so it never claims more
time than the backend granted.
-}
receiptLease :: MonoTime -> Seconds -> Seconds -> ReceiptLease
receiptLease receivedAt window maxHold =
    ReceiptLease
        { rlWindow = window
        , rlExpiresAt = monoAfter receivedAt (secondsOf window)
        , rlCeilingAt = monoAfter receivedAt (secondsOf maxHold)
        }

secondsOf :: Seconds -> Double
secondsOf (Seconds seconds) = fromIntegral seconds
