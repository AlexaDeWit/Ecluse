-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The receipt-lease vocabulary a queue backend writes and the worker's renewal controller
reads ("Ecluse.Core.Worker.Lease").

The monotonic instants come from "Ecluse.Core.Clock" and are re-exported here, because a lease
is where most callers meet them. A backend that never expires a delivery supplies no lease.
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

import Ecluse.Core.Clock (MonoTime (MonoTime), monoAfter, monoSecondsBetween, monotonicNow)

{- | A duration in whole seconds, for 'Ecluse.Core.Queue.extendVisibility'. A 'newtype', so a
raw @Int@ of seconds cannot pass for some other count.
-}
newtype Seconds = Seconds Int
    deriving stock (Eq, Ord, Show)

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
