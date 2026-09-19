-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Telemetry.ExportFailureSpec (spec) where

import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian)
import Test.Hspec

import Ecluse.Runtime.Telemetry.ExportFailure (
    ThrottleEmit (..),
    ThrottleState (..),
    initialThrottle,
    throttleStep,
 )

-- | Tests the export-failure throttle: export errors coalesce into a first report and a heartbeat.
spec :: Spec
spec = throttleSpec

throttleSpec :: Spec
throttleSpec = describe "throttleStep" $ do
    let t0 = UTCTime (fromGregorian 2026 1 1) 0
        interval = 60

    it "surfaces the first error and records when it was logged" $ do
        let (state', emit) = throttleStep interval t0 initialThrottle
        emit `shouldBe` EmitFirst
        tsLastLogged state' `shouldBe` Just t0
        tsSuppressed state' `shouldBe` 0

    it "suppresses and counts errors within the window" $ do
        let (state', _) = throttleStep interval t0 initialThrottle
            (state'', emit) = throttleStep interval (addUTCTime 1 t0) state'
        emit `shouldBe` EmitSuppress
        tsSuppressed state'' `shouldBe` 1

    it "surfaces a heartbeat once the window elapses, carrying the suppressed count and resetting" $ do
        let (s1, _) = throttleStep interval t0 initialThrottle
            (s2, _) = throttleStep interval (addUTCTime 1 t0) s1
            (s3, emit) = throttleStep interval (addUTCTime 61 t0) s2
        emit `shouldBe` EmitHeartbeat 2
        tsSuppressed s3 `shouldBe` 0
        tsLastLogged s3 `shouldBe` Just (addUTCTime 61 t0)
