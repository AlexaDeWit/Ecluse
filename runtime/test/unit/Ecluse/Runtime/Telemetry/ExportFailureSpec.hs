-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Telemetry.ExportFailureSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian)
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (InternalException))
import Network.TLS qualified as TLS
import OpenTelemetry.Exporter.Span (ExportResult (Failure))
import Test.Hspec

import Ecluse.Core.Fault (TransportCause (TransportTls), renderTransportCause)
import Ecluse.Runtime.Telemetry.ExportFailure.Internal (
    ThrottleEmit (..),
    ThrottleState (..),
    initialThrottle,
    newExportFailureSink,
    observeExportResult,
    throttleStep,
 )
import Ecluse.Test.Stub (requestSecrets, signedRequest)

-- | Export errors coalesce into a first report and a heartbeat, and neither line carries the exporter's request.
spec :: Spec
spec = do
    throttleSpec
    observeSpec

throttleSpec :: Spec
throttleSpec = describe "throttleStep" $ do
    let interval = 60

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

observeSpec :: Spec
observeSpec = describe "observeExportResult" $
    it "names the signal and the transport cause on the first line and the heartbeat, never the request's headers, path or query" $ do
        request <- signedRequest
        clock <- newIORef t0
        surfaced <- newIORef []
        sink <- newExportFailureSink (readIORef clock) (\_ line -> modifyIORef' surfaced (line :))
        let refused = HttpExceptionRequest request (InternalException (toException (TLS.HandshakeFailed (TLS.Error_Misc "refused"))))
            export = observeExportResult sink "span" (Failure (Just (toException refused)))
        export
        writeIORef clock (addUTCTime 61 t0) >> export
        lines_ <- reverse <$> readIORef surfaced
        case lines_ of
            [firstLine, heartbeat] -> do
                firstLine `shouldSatisfy` T.isPrefixOf "telemetry export error"
                heartbeat `shouldSatisfy` T.isPrefixOf "telemetry export still failing"
                for_ lines_ $ \line -> do
                    line `shouldSatisfy` T.isInfixOf "span export failed: "
                    line `shouldSatisfy` T.isInfixOf (renderTransportCause TransportTls)
                    for_ requestSecrets $ \secret -> line `shouldNotSatisfy` T.isInfixOf secret
            other -> expectationFailure ("expected a first line and a heartbeat, got " <> show (length other))

-- The injected clock's start. The throttle keys on differences, so the instant is arbitrary.
t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) 0
