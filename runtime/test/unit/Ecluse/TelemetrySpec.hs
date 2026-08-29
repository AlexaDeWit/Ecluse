-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.TelemetrySpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian)
import Test.Hspec

import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.WireVocab (wireRoundTrips)
import Katip (Severity (WarningS))
import OpenTelemetry.Exporter.Metric (MetricExporter (..))
import OpenTelemetry.Exporter.Span (ExportResult (Failure), SpanExporter (..))
import OpenTelemetry.Log.Core (createLoggerProvider, emptyLoggerProviderOptions)
import OpenTelemetry.Metric (
    FlushResult (FlushSuccess),
    ShutdownResult (ShutdownSuccess),
    noopMeterProvider,
 )
import OpenTelemetry.SDK (OTelSignals (..))
import OpenTelemetry.Trace (
    createTracerProvider,
    emptyTracerProviderOptions,
 )

import Ecluse.Runtime.Telemetry (
    Telemetry (..),
    TelemetryProviders (..),
    TelemetrySwitch (..),
    observeMetricExporter,
    observeSpanExporter,
    parseTelemetrySwitch,
    telemetryDisabled,
    telemetryEnabled,
    telemetryMeterProvider,
    telemetryTracerProvider,
    withTelemetry,
 )
import Ecluse.Runtime.Telemetry.Resolve (newExportFailureSink)

{- | Tests the OpenTelemetry substrate: the @ECLUSE_OBSERVABILITY__TELEMETRY@ switch parses
strictly, the off handle initialises no SDK, and 'telemetryEnabled' wires the SDK providers
through. The live @on@ path opens a real exporter, so the integration tier covers it.
-}
spec :: Spec
spec = do
    switchSpec
    handleSpec
    enabledHandleSpec
    lifecycleSpec
    exportObservationSpec

switchSpec :: Spec
switchSpec = describe "TelemetrySwitch" $ do
    wireRoundTrips @TelemetrySwitch

    it "parses each accepted mode" $ do
        parseTelemetrySwitch "off" `shouldBe` Right TelemetryOff
        parseTelemetrySwitch "on" `shouldBe` Right TelemetryOn

    it "rejects an unknown value, naming the accepted set" $
        parseTelemetrySwitch "maybe"
            `shouldBe` Left "unknown telemetry switch \"maybe\" (expected one of: on, off)"

    it "shows each mode without erroring (derived Show)" $ do
        show TelemetryOff `shouldBe` ("TelemetryOff" :: String)
        show TelemetryOn `shouldBe` ("TelemetryOn" :: String)

handleSpec :: Spec
handleSpec = describe "telemetryDisabled" $ do
    -- A 'TracerProvider'/'MeterProvider' has no 'Show', so the provider absence is
    -- asserted through 'isNothing' rather than 'shouldSatisfy' (which would print).
    it "exposes no tracer provider (nothing to emit through)" $
        isNothing (telemetryTracerProvider telemetryDisabled) `shouldBe` True

    it "exposes no meter provider (nothing to emit through)" $
        isNothing (telemetryMeterProvider telemetryDisabled) `shouldBe` True

    it "is the TelemetryDisabled constructor" $ case telemetryDisabled of
        TelemetryDisabled -> pure ()
        TelemetryEnabled{} -> expectationFailure "expected the disabled no-op handle"

{- | An 'OTelSignals' of inert providers, so a test drives 'telemetryEnabled' without the real
SDK. It reads only the tracer and meter fields, the rest keep the value total.
-}
offlineSignals :: IO OTelSignals
offlineSignals = do
    tracerProvider <- createTracerProvider [] emptyTracerProviderOptions
    loggerProvider <- createLoggerProvider [] emptyLoggerProviderOptions
    pure
        OTelSignals
            { otelTracerProvider = tracerProvider
            , otelMeterProvider = noopMeterProvider
            , otelLoggerProvider = loggerProvider
            , otelPropagators = mempty
            , otelShutdown = pure ()
            }

enabledHandleSpec :: Spec
enabledHandleSpec = describe "telemetryEnabled" $ do
    -- A 'TracerProvider'/'MeterProvider' has no 'Eq' or 'Show', so the assertion is the
    -- constructor shape plus a forced projection rather than value equality.
    it "carries the SDK providers into the TelemetryEnabled handle" $ do
        signals <- offlineSignals
        case telemetryEnabled signals of
            TelemetryDisabled ->
                expectationFailure "expected the enabled handle from telemetryEnabled"
            TelemetryEnabled TelemetryProviders{} -> pure ()

    it "wires the signals' tracer provider through to the tracer accessor" $ do
        signals <- offlineSignals
        present <- forceProvider (telemetryTracerProvider (telemetryEnabled signals))
        present `shouldBe` True

    it "wires the signals' meter provider through to the meter accessor" $ do
        signals <- offlineSignals
        present <- forceProvider (telemetryMeterProvider (telemetryEnabled signals))
        present `shouldBe` True
  where
    -- Force the projected provider to WHNF and report whether it was present. A provider has no
    -- 'Eq'/'Show', so this proves the projection yielded a real value, not a dropped thunk.
    forceProvider :: Maybe a -> IO Bool
    forceProvider = \case
        Nothing -> pure False
        Just provider -> True <$ evaluateWHNF provider

lifecycleSpec :: Spec
lifecycleSpec = describe "withTelemetry" $ do
    it "runs the body against the disabled no-op when off, initialising no SDK" $ do
        -- The off path is a pure pass-through: it opens no exporter and reads no OTEL_* env. A
        -- provider has no 'Eq', so absence is checked through 'isNothing'.
        logEnv <- newTestLogEnv
        (noTracer, noMeter) <-
            withTelemetry TelemetryOff logEnv $ \telemetry ->
                pure
                    ( isNothing (telemetryTracerProvider telemetry)
                    , isNothing (telemetryMeterProvider telemetry)
                    )
        noTracer `shouldBe` True
        noMeter `shouldBe` True

    it "returns the body's result through the off bracket" $ do
        logEnv <- newTestLogEnv
        result <- withTelemetry TelemetryOff logEnv (const (pure (42 :: Int)))
        result `shouldBe` 42

{- The wrapper surfaces the first export failure, suppresses repeats inside the throttle
window, then heartbeats the suppressed count off an injected clock. -}
exportObservationSpec :: Spec
exportObservationSpec = describe "observeSpanExporter / observeMetricExporter" $ do
    it "surfaces the first span-export failure, throttles repeats, then heartbeats the count" $ do
        clock <- newIORef t0
        surfaced <- newIORef []
        sink <- newExportFailureSink (readIORef clock) (\sev msg -> modifyIORef' surfaced ((sev, msg) :))
        let flush = void (spanExporterExport (observeSpanExporter sink failingSpanExporter) mempty)
        flush -- the first failure, surfaced plainly
        writeIORef clock (addUTCTime 1 t0) >> flush -- within the window: suppressed, counted
        writeIORef clock (addUTCTime 61 t0) >> flush -- past the window: a heartbeat
        lines_ <- reverse <$> readIORef surfaced
        map fst lines_ `shouldBe` [WarningS, WarningS]
        case map snd lines_ of
            [firstLine, heartbeat] -> do
                firstLine `shouldSatisfy` T.isInfixOf "telemetry export error"
                firstLine `shouldSatisfy` T.isInfixOf "span export failed"
                heartbeat `shouldSatisfy` T.isInfixOf "telemetry export still failing"
                heartbeat `shouldSatisfy` T.isInfixOf "2 export errors"
            other -> expectationFailure ("expected a first line and a heartbeat, got " <> show (length other))

    it "surfaces a metric-export failure through the same sink" $ do
        surfaced <- newIORef []
        sink <- newExportFailureSink (pure t0) (\_sev msg -> modifyIORef' surfaced (msg :))
        void (metricExporterExport (observeMetricExporter sink failingMetricExporter) mempty)
        lines_ <- readIORef surfaced
        case lines_ of
            [only] -> do
                only `shouldSatisfy` T.isInfixOf "telemetry export error"
                only `shouldSatisfy` T.isInfixOf "metric export failed"
            other -> expectationFailure ("expected one surfaced line, got " <> show (length other))

-- A stub span exporter whose every export fails (no carried exception), so the wrapper's
-- failure path is the only thing exercised.
failingSpanExporter :: SpanExporter
failingSpanExporter =
    SpanExporter
        { spanExporterExport = \_ -> pure (Failure Nothing)
        , spanExporterShutdown = pure ShutdownSuccess
        , spanExporterForceFlush = pure FlushSuccess
        }

-- The metric-exporter dual of 'failingSpanExporter'.
failingMetricExporter :: MetricExporter
failingMetricExporter =
    MetricExporter
        { metricExporterExport = \_ -> pure (Failure Nothing)
        , metricExporterShutdown = pure ShutdownSuccess
        , metricExporterForceFlush = pure FlushSuccess
        }

-- An arbitrary fixed instant the injected clock starts at. The throttle keys on
-- differences, so the absolute value is immaterial.
t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) 0
