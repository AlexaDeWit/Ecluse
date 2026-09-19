-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.BreakerSpec (spec) where

import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, fromGregorian)

import Hedgehog (forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Breaker
import Ecluse.Core.Telemetry.Metrics qualified as Metric

-- | A fixed instant so the cooldown arithmetic is deterministic.
now :: UTCTime
now = UTCTime (fromGregorian 2026 6 23) 0

-- | The trip threshold and cooldown for the example tests.
threshold :: Int
threshold = 3

cooldown :: NominalDiffTime
cooldown = 30

-- | 'recordFailure' specialised to the example threshold and cooldown at 'now'.
failAt :: UTCTime -> Breaker -> Breaker
failAt = recordFailure threshold cooldown

spec :: Spec
spec = do
    describe "initialBreaker" $
        it "starts healthy with no failures recorded" $
            initialBreaker `shouldBe` Closed 0

    describe "admit" $ do
        it "admits a half-open breaker unchanged (the probe is in flight)" $
            admit now HalfOpen `shouldBe` (True, HalfOpen)

        it "denies an open breaker while the cooldown has not elapsed, unchanged" $ do
            let until' = addUTCTime cooldown now
            admit now (Open until') `shouldBe` (False, Open until')

        it "half-opens and admits one probe at the cooldown instant and after it" $ do
            -- At exactly the cooldown instant the deadline is no longer in the future,
            -- so the probe passes and the state advances to half-open.
            let until' = addUTCTime cooldown now
            admit now (Open now) `shouldBe` (True, HalfOpen)
            admit (addUTCTime 1 until') (Open until') `shouldBe` (True, HalfOpen)

    describe "recordSuccess" $ do
        it "resets a failure-counting closed breaker to healthy" $
            recordSuccess (Closed 2) `shouldBe` Closed 0

        it "closes a half-open breaker (a successful recovery probe)" $
            recordSuccess HalfOpen `shouldBe` Closed 0

        it "closes an open breaker" $
            recordSuccess (Open (addUTCTime cooldown now)) `shouldBe` Closed 0

    describe "recordFailure" $ do
        it "trips open for the cooldown once the count reaches the threshold" $
            -- Two failures already counted (Closed 2). The third reaches threshold 3.
            failAt now (Closed (threshold - 1)) `shouldBe` Open (addUTCTime cooldown now)

        it "re-opens for a fresh cooldown when a half-open probe fails" $
            failAt now HalfOpen `shouldBe` Open (addUTCTime cooldown now)

        it "re-opens for a fresh cooldown from an already-open state" $ do
            -- A failure folded in while already open re-opens against the new instant.
            let later = addUTCTime 100 now
            failAt later (Open now) `shouldBe` Open (addUTCTime cooldown later)

        it "trips after exactly threshold consecutive failures from healthy" $ do
            -- initialBreaker, then `threshold` failures: the last one trips it open.
            let stepN k = foldl' (\br _ -> failAt now br) initialBreaker [1 .. k]
            stepN threshold `shouldBe` Open (addUTCTime cooldown now)
            stepN (threshold - 1) `shouldBe` Closed (threshold - 1)

    describe "breakerState" $
        it "projects the breaker onto the bounded gauge value" $ do
            breakerState (Closed 0) `shouldBe` Metric.Closed
            breakerState (Closed 7) `shouldBe` Metric.Closed -- the failure tally is not observable
            breakerState HalfOpen `shouldBe` Metric.HalfOpen
            breakerState (Open now) `shouldBe` Metric.Open

    describe "properties" $ do
        it "recordSuccess always lands on the healthy initial state" $
            hedgehog $ do
                br <- forAll genBreaker
                recordSuccess br === initialBreaker

        it "a closed breaker is always admitted unchanged" $
            hedgehog $ do
                n <- forAll (Gen.int (Range.linear 0 10))
                t <- forAll genInstant
                admit t (Closed n) === (True, Closed n)

        it "an open breaker is denied iff the clock is before its instant" $
            hedgehog $ do
                offset <- forAll (Gen.integral (Range.linearFrom 0 (-600) 600))
                let until' = addUTCTime cooldown now
                    clk = addUTCTime (fromInteger offset) until'
                    (admitted, _) = admit clk (Open until')
                admitted === (clk >= until')

        it "recordFailure below the threshold only ever increments the count" $
            hedgehog $ do
                -- A count strictly below `threshold - 1` cannot trip yet.
                n <- forAll (Gen.int (Range.linear 0 (threshold - 2)))
                t <- forAll genInstant
                recordFailure threshold cooldown t (Closed n) === Closed (n + 1)
  where
    genInstant :: H.Gen UTCTime
    genInstant = do
        offset <- Gen.integral (Range.linearFrom 0 (-100_000) 100_000)
        pure (addUTCTime (fromInteger offset) now)

    genBreaker :: H.Gen Breaker
    genBreaker =
        Gen.choice
            [ Closed <$> Gen.int (Range.linear 0 10)
            , Open <$> genInstant
            , pure HalfOpen
            ]
