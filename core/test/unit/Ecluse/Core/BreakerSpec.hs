-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Reachable breaker transitions and ownership laws.
module Ecluse.Core.BreakerSpec (spec) where

import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Breaker
import Ecluse.Core.Telemetry.Metrics qualified as Metric

now :: UTCTime
now = UTCTime (fromGregorian 2026 6 23) 0

failed :: Int -> Breaker -> Breaker
failed threshold breaker = case admit now breaker of
    (Just ticket, claimed) -> recordFailure threshold 30 now ticket claimed
    (Nothing, unchanged) -> unchanged

tripped :: Breaker
tripped = failed 1 initialBreaker

withAttempt :: UTCTime -> Breaker -> (Attempt -> Breaker -> Expectation) -> Expectation
withAttempt clock breaker use = case admit clock breaker of
    (Just ticket, claimed) -> use ticket claimed
    (Nothing, _) -> expectationFailure "expected an admitted attempt"

spec :: Spec
spec = do
    describe "owned recovery" $ do
        it "refuses during cooldown and admits exactly one at its boundary" $ do
            admit now tripped `shouldBe` (Nothing, tripped)
            withAttempt (addUTCTime 30 now) tripped $ \ticket claimed -> do
                breakerPhase claimed `shouldBe` HalfOpen
                admit (addUTCTime 100 now) claimed `shouldBe` (Nothing, claimed)
                breakerPhase (recordSuccess ticket claimed) `shouldBe` Closed 0

        it "starts a fresh cooldown from a genuine probe failure" $
            withAttempt (addUTCTime 30 now) tripped $ \ticket claimed ->
                breakerPhase (recordFailure 3 30 (addUTCTime 50 now) ticket claimed)
                    `shouldBe` Open (addUTCTime 80 now)

        it "releases cancellation for immediate retry without a failure or cooldown" $
            withAttempt (addUTCTime 30 now) tripped $ \ticket claimed -> do
                let ready = release ticket claimed
                breakerPhase ready `shouldBe` RecoveryReady
                breakerState ready `shouldBe` Metric.HalfOpen
                withAttempt (addUTCTime 30 now) ready $ \next owned -> do
                    next `shouldNotBe` ticket
                    admit (addUTCTime 30 now) owned `shouldBe` (Nothing, owned)

        it "fences old recovery settlement and repeated cleanup from a newer owner" $
            withAttempt (addUTCTime 30 now) tripped $ \old claimed ->
                withAttempt (addUTCTime 30 now) (release old claimed) $ \current owned -> do
                    recordSuccess old owned `shouldBe` owned
                    recordFailure 1 30 now old owned `shouldBe` owned
                    release old owned `shouldBe` owned
                    breakerPhase (recordSuccess current owned) `shouldBe` Closed 0

        it "fences a completed probe from another probe and from its healthy successors" $
            withAttempt (addUTCTime 30 now) tripped $ \old claimed -> do
                let healthy = recordSuccess old claimed
                recordFailure 1 30 now old healthy `shouldBe` healthy
                release old healthy `shouldBe` healthy
                withAttempt (addUTCTime 30 now) (failed 1 healthy) $ \_ owned -> do
                    recordSuccess old owned `shouldBe` owned
                    recordFailure 1 30 now old owned `shouldBe` owned
                    release old owned `shouldBe` owned

        it "fences late healthy outcomes after a recovery generation starts" $
            withAttempt now initialBreaker $ \old healthy ->
                withAttempt (addUTCTime 30 now) (recordFailure 1 30 now old healthy) $ \_ owned -> do
                    recordSuccess old owned `shouldBe` owned
                    recordFailure 1 30 now old owned `shouldBe` owned
                    release old owned `shouldBe` owned

    describe "ordinary outcomes" $ do
        it "keeps healthy admission concurrent and counts consecutive genuine failures" $ do
            let twice = failed 3 (failed 3 initialBreaker)
            breakerPhase twice `shouldBe` Closed 2
            withAttempt now twice $ \ticket claimed -> do
                snd (admit now claimed) `shouldBe` claimed
                breakerPhase (recordSuccess ticket claimed) `shouldBe` Closed 0
                breakerPhase (recordFailure 3 30 now ticket claimed) `shouldBe` Open (addUTCTime 30 now)

        it "retains ordinary outcome folds during cooldown until a recovery generation starts" $
            withAttempt now initialBreaker $ \ticket healthy -> do
                let opened = recordFailure 1 30 now ticket healthy
                breakerPhase (recordSuccess ticket opened) `shouldBe` Closed 0
                breakerPhase (recordFailure 1 30 (addUTCTime 5 now) ticket opened)
                    `shouldBe` Open (addUTCTime 35 now)

    describe "properties" $ do
        it "trips at exactly the configured threshold in reachable states" $
            hedgehog $ do
                threshold <- forAll (Gen.int (Range.linear 1 20))
                let step count = foldl' (\br _ -> failed threshold br) initialBreaker [1 .. count]
                breakerPhase (step (threshold - 1)) === Closed (threshold - 1)
                breakerPhase (step threshold) === Open (addUTCTime 30 now)

        it "admits recovery exactly at or after the cooldown" $
            hedgehog $ do
                offset <- forAll (Gen.int (Range.linear (-50) 50))
                let (ticket, claimed) = admit (addUTCTime (fromIntegral (30 + offset)) now) tripped
                isJust ticket === (offset >= 0)
                when (isJust ticket) $ fst (admit (addUTCTime 100 now) claimed) === Nothing

    describe "observations" $
        it "suppresses count, release and generation changes and retains three gauge values" $ do
            reports <- newIORef []
            let reporter = BreakerReporter (\br -> modifyIORef' reports (<> [breakerState br]))
            reportBreakerChange reporter initialBreaker (failed 3 initialBreaker)
            reportBreakerChange reporter initialBreaker tripped
            withAttempt (addUTCTime 30 now) tripped $ \ticket claimed -> do
                reportBreakerChange reporter tripped claimed
                let ready = release ticket claimed
                reportBreakerChange reporter claimed ready
                withAttempt (addUTCTime 30 now) ready $ \next owned -> do
                    reportBreakerChange reporter ready owned
                    reportBreakerChange reporter owned (recordSuccess next owned)
            readIORef reports `shouldReturn` [Metric.Open, Metric.HalfOpen, Metric.Closed]
