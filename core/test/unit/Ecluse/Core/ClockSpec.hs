-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.ClockSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Ecluse.Core.Clock (MonoTime (MonoTime), monoAfter, monoSecondsBetween, monotonicNow, waitSeconds, waitUntilMonotonic)

spec :: Spec
spec = describe "the monotonic clock" $ do
    it "reads an offset forwards and backwards from an instant" $ do
        monoAfter (MonoTime 10) 5 `shouldBe` MonoTime 15
        monoAfter (MonoTime 10) (-5) `shouldBe` MonoTime 5

    it "reads the seconds to an instant as negative once it has passed" $ do
        monoSecondsBetween (MonoTime 10) (MonoTime 12) `shouldBe` 2
        monoSecondsBetween (MonoTime 12) (MonoTime 10) `shouldBe` (-2)

    it "waits a sub-second duration rather than rounding it away" $ do
        startedAt <- monotonicNow
        waitSeconds 0.05
        served <- monoSecondsBetween startedAt <$> monotonicNow
        served `shouldSatisfy` (> 0.02)

    it "returns at once for a duration beneath a microsecond" $ do
        startedAt <- monotonicNow
        waitSeconds 0
        served <- monoSecondsBetween startedAt <$> monotonicNow
        served `shouldSatisfy` (< 1)

    it "advances over a wait and returns at once for an instant already passed" $ do
        startedAt <- monotonicNow
        waitUntilMonotonic (monoAfter startedAt (-60))
        finishedAt <- monotonicNow
        monoSecondsBetween startedAt finishedAt `shouldSatisfy` (< 30)
