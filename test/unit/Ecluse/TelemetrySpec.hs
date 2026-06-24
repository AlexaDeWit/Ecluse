module Ecluse.TelemetrySpec (spec) where

import Test.Hspec

import OpenTelemetry.Log.Core (createLoggerProvider, emptyLoggerProviderOptions)
import OpenTelemetry.Metric (noopMeterProvider)
import OpenTelemetry.SDK (OTelSignals (..))
import OpenTelemetry.Trace (
    createTracerProvider,
    emptyTracerProviderOptions,
 )

import Ecluse.Telemetry (
    Telemetry (..),
    TelemetryProviders (..),
    TelemetrySwitch (..),
    parseTelemetrySwitch,
    renderTelemetrySwitch,
    telemetryDisabled,
    telemetryEnabled,
    telemetryMeterProvider,
    telemetryTracerProvider,
    withTelemetry,
 )

{- | Tests for the OpenTelemetry substrate. They exercise the substrate's
promises: the @PROXY_TELEMETRY@ master switch parses strictly (on \/ off \/
malformed); the off-by-default handle is a genuine no-op (the SDK is never
initialised and no provider is exposed); and 'telemetryEnabled' wires the SDK's
tracer and meter providers through to the handle's accessors. The enabled handle
under test is built from /offline/ providers (an empty-processor tracer provider
and the no-op meter provider) — no exporter is opened and no @OTEL_*@ env is read,
so this stays pure-tier. Only the live 'withTelemetry' @on@ path
(@hs-opentelemetry-sdk@'s @withOpenTelemetry@, which reads @OTEL_*@ and opens an
OTLP exporter) is reserved for the integration tier
(see @docs\/architecture\/observability.md@ → "Verifying it"). Pure and offline.
-}
spec :: Spec
spec = do
    switchSpec
    handleSpec
    enabledHandleSpec
    lifecycleSpec

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
        -- has no 'Eq', so each is checked through 'isNothing').
        (noTracer, noMeter) <-
            withTelemetry TelemetryOff $ \telemetry ->
                pure
                    ( isNothing (telemetryTracerProvider telemetry)
                    , isNothing (telemetryMeterProvider telemetry)
                    )
        noTracer `shouldBe` True
        noMeter `shouldBe` True

    it "returns the body's result through the off bracket" $ do
        result <- withTelemetry TelemetryOff (const (pure (42 :: Int)))
        result `shouldBe` 42
