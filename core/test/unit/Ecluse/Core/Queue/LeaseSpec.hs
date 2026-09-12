-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Cover for the receipt-lease vocabulary: the monotonic arithmetic every renewal decision
reads, and the lease a backend stamps a delivery with.
-}
module Ecluse.Core.Queue.LeaseSpec (spec) where

import Test.Hspec

import Ecluse.Core.Queue.Lease (
    MonoTime (MonoTime),
    ReceiptLease (rlCeilingAt, rlExpiresAt, rlWindow),
    Seconds (Seconds),
    monoAfter,
    monoSecondsBetween,
    monotonicNow,
    receiptLease,
 )

spec :: Spec
spec = do
    describe "the monotonic clock a lease is measured on" $ do
        it "reads forward, never backward, across two readings" $ do
            before' <- monotonicNow
            after' <- monotonicNow
            after' `shouldSatisfy` (>= before')

        it "counts the seconds from one instant to a later one" $
            monoSecondsBetween (MonoTime 10) (MonoTime 42) `shouldBe` 32

        it "counts negative seconds once the later instant has passed" $
            monoSecondsBetween (MonoTime 42) (MonoTime 10) `shouldBe` -32

        it "reads backwards on a negative offset, for the margin held back from a deadline" $
            monoAfter (MonoTime 100) (-3) `shouldBe` MonoTime 97

    describe "receiptLease -- what a backend stamps one delivery with" $ do
        it "keeps the granted window, so every renewal asks for the same one again" $
            rlWindow (receiptLease (MonoTime 1000) (Seconds 30) (Seconds 43_200))
                `shouldBe` Seconds 30

        it "expires one window after the instant the delivery was asked for" $
            -- Stamped from before the poll, so the lease never claims more than the backend gave.
            rlExpiresAt (receiptLease (MonoTime 1000) (Seconds 30) (Seconds 43_200))
                `shouldBe` MonoTime 1030

        it "puts the ceiling a whole receipt lives under at the backend's own maximum" $
            rlCeilingAt (receiptLease (MonoTime 1000) (Seconds 30) (Seconds 43_200))
                `shouldBe` MonoTime 44_200
