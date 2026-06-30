module Ecluse.TelemetrySpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian)
import Test.Hspec

import Katip (
    Environment (Environment),
    LogEnv,
    Namespace (Namespace),
    Severity (WarningS),
    initLogEnv,
 )
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

import Ecluse.Telemetry (
    Telemetry (..),
    TelemetryProviders (..),
    TelemetrySwitch (..),
    observeMetricExporter,
    observeSpanExporter,
    parseTelemetrySwitch,
    renderTelemetrySwitch,
    telemetryDisabled,
    telemetryEnabled,
    telemetryMeterProvider,
    telemetryTracerProvider,
    withTelemetry,
 )
import Ecluse.Telemetry.Resolve (newExportFailureSink)

{- | Tests for the OpenTelemetry substrate. They exercise the substrate's
promises: the @ECLUSE_TELEMETRY@ master switch parses strictly (on \/ off \/
malformed); the off-by-default handle is a genuine no-op (the SDK is never
initialised and no provider is exposed); and 'telemetryEnabled' wires the SDK's
tracer and meter providers through to the handle's accessors. The enabled handle
under test is built from /offline/ providers (an empty-processor tracer provider
and the no-op meter provider) — no exporter is opened and no @OTEL_*@ env is read,
so this stays pure-tier. Only the live 'withTelemetry' @on@ path (which reads
@OTEL_*@ and opens an OTLP exporter against a real collector) is reserved for the
integration tier (see @docs\/architecture\/observability.md@ → "Verifying it").

The 'observeSpanExporter' \/ 'observeMetricExporter' cases below stay pure: a stub
exporter that always fails drives the wrapper, asserting the export failure is
surfaced through an injected sink without a live SDK. Pure and offline.
-}
spec :: Spec
spec = do
    switchSpec
    handleSpec
    enabledHandleSpec
    lifecycleSpec
    exportObservationSpec

-- ── the master switch ────────────────────────────────────────────────────────

switchSpec :: Spec
switchSpec = describe "TelemetrySwitch" $ do
    it "round-trips each mode through parse/render" $ do
        parseTelemetrySwitch "off" `shouldBe` Right TelemetryOff
        parseTelemetrySwitch "on" `shouldBe` Right TelemetryOn
        renderTelemetrySwitch TelemetryOff `shouldBe` "off"
        renderTelemetrySwitch TelemetryOn `shouldBe` "on"

    it "rejects an unknown value, naming the accepted set" $
        parseTelemetrySwitch "maybe"
            `shouldBe` Left "unknown telemetry switch \"maybe\" (expected one of: on, off)"

    it "shows each mode without erroring (derived Show)" $ do
        show TelemetryOff `shouldBe` ("TelemetryOff" :: String)
        show TelemetryOn `shouldBe` ("TelemetryOn" :: String)

-- ── the telemetry handle ─────────────────────────────────────────────────────

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

-- ── the enabled handle ───────────────────────────────────────────────────────

{- | An 'OTelSignals' assembled from /offline/ providers, so 'telemetryEnabled'
can be exercised without standing up the real SDK. Every provider is inert: the
tracer and logger providers have no processors (they export nothing), the meter
provider is the SDK's no-op, and the propagator is the empty 'mempty'. No exporter
is opened and no @OTEL_*@ env is read — this is pure substrate wiring, not the live
@on@ path. ('telemetryEnabled' reads only the tracer and meter fields; the rest are
present so the value is total rather than relying on a bottom.)
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
    -- 'telemetryEnabled' is the only way to obtain an enabled handle: it must take
    -- the tracer and meter providers from the SDK signals and expose exactly those
    -- through the handle's accessors. A 'TracerProvider'/'MeterProvider' has no 'Eq'
    -- or 'Show', so the wiring is asserted by constructor shape and by /forcing/ the
    -- projected provider (it must be a real value, not a discarded thunk) rather
    -- than by value equality. 'forceProvider' below evaluates the projection to
    -- WHNF so the wiring genuinely runs end to end.
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
    -- Force the projected provider to WHNF and report whether it was present. A
    -- provider has no 'Eq'/'Show', so this asserts the projection actually ran and
    -- yielded a real value ('Just' forced), not a lazily-dropped thunk —
    -- exercising the enabled branches of the accessors and the field projections.
    forceProvider :: Maybe a -> IO Bool
    forceProvider = \case
        Nothing -> pure False
        Just provider -> True <$ evaluateWHNF provider

-- ── lifecycle ────────────────────────────────────────────────────────────────

lifecycleSpec :: Spec
lifecycleSpec = describe "withTelemetry" $ do
    it "runs the body against the disabled no-op when off, initialising no SDK" $ do
        -- The off path must be a pure pass-through: it opens no exporter and
        -- reads no OTEL_* env, so the body simply receives the inert handle. The
        -- assertion is that the handle exposes no providers (a 'TracerProvider'
        -- has no 'Eq', so each is checked through 'isNothing'). The 'LogEnv' is
        -- unused on the off path, so a scribe-less one suffices.
        logEnv <- quietLogEnv
        (noTracer, noMeter) <-
            withTelemetry TelemetryOff logEnv $ \telemetry ->
                pure
                    ( isNothing (telemetryTracerProvider telemetry)
                    , isNothing (telemetryMeterProvider telemetry)
                    )
        noTracer `shouldBe` True
        noMeter `shouldBe` True

    it "returns the body's result through the off bracket" $ do
        logEnv <- quietLogEnv
        result <- withTelemetry TelemetryOff logEnv (const (pure (42 :: Int)))
        result `shouldBe` 42

-- ── export-failure observation (the exporter wrappers) ───────────────────────

{- The exporter wrappers turn a dropped export into a visible warning. Driving a stub
exporter that always returns 'Failure' through the wrapper must surface the first failure
through the sink, suppress repeats inside the throttle window, then heartbeat the
suppressed count once the window elapses — the IO-boundary mirror of the pure
'throttleStep' tests, with the clock and surfacing action injected so the decision is
asserted without wall-clock timing or a live @katip@ scribe. -}
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

-- An arbitrary fixed instant the injected clock starts at; the throttle keys on
-- differences, so the absolute value is immaterial.
t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) 0

-- A scribe-less katip environment: log calls are accepted and dropped.
quietLogEnv :: IO LogEnv
quietLogEnv = initLogEnv (Namespace ["test"]) (Environment "test")
