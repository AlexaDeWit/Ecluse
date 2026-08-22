-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The OpenTelemetry substrate. It holds the tracer and meter providers the rest of
the proxy hangs spans and metrics on, behind a master switch that defaults to __off__.

Écluse is a self-hosted proxy operators run inside their own infrastructure, so
observability is __opt-in and vendor-neutral__. The substrate is OpenTelemetry,
emitting OTLP that any compatible backend can receive. The maintainer's choice of
backend (Datadog) must never become every consumer's obligation, so __with
@ECLUSE_OBSERVABILITY__TELEMETRY@ unset nothing is wired and no telemetry is
emitted__. The SDK is not even initialised.

This module is purely the __substrate__: it stands up (or, by default, declines to
stand up) the providers and brackets their lifecycle. The spans on the request
lifecycle and the metric instruments layer on top of this substrate. Nothing here
instruments the hot path.

== The switch and the handle

'TelemetrySwitch' is the @ECLUSE_OBSERVABILITY__TELEMETRY@ master switch, parsed at the
configuration boundary (@Ecluse.Config@) in the same strict, fail-loud style as
the other enums. The 'Telemetry' handle it produces is one of two shapes:

* __'telemetryDisabled'__: the off-by-default no-op. It holds no providers, the
  SDK is never initialised, and nothing is exported. This is what an unset
  @ECLUSE_OBSERVABILITY__TELEMETRY@ yields.

* an __enabled__ handle carrying the SDK's tracer and meter providers. The SDK builds
  them from the standard @OTEL_*@ environment variables it reads directly:
  @OTEL_SERVICE_NAME@, @OTEL_RESOURCE_ATTRIBUTES@, @OTEL_EXPORTER_OTLP_ENDPOINT@,
  @OTEL_EXPORTER_OTLP_PROTOCOL@, and the sampler. The OTLP exporter defaults to
  HTTP\/protobuf. gRPC stays behind the exporter's cabal flag, off.

'withTelemetry' is the lifecycle bracket the composition root ("Ecluse.Runtime.Env")
runs the proxy within. When enabled it initialises the providers and tears them down
along every exit path, flushing buffered spans and metrics. When disabled it is a pure
pass-through that opens nothing to tear down.

When enabled it also makes export failures __visible__. The OTLP span and metric
exporters are wrapped, so a failed export is observed and routed through the shared
@katip@ throttle ("Ecluse.Runtime.Telemetry.Resolve"). Without those wrappers,
@hs-opentelemetry 1.0.0.0@ drops that failure silently. The throttle logs the first
failure plainly, then a periodic heartbeat. The wrappers only /observe/. Export
semantics are unchanged, so an unreachable collector still degrades off the request
path.

@docs\/architecture\/observability.md@ describes the configuration model and the
signal catalogue.
-}
module Ecluse.Runtime.Telemetry (
    -- * Master switch
    TelemetrySwitch (..),
    parseTelemetrySwitch,

    -- * The telemetry handle
    Telemetry (..),
    TelemetryProviders (..),
    telemetryDisabled,
    telemetryEnabled,
    telemetryTracerProvider,
    telemetryMeterProvider,

    -- * Lifecycle
    withTelemetry,

    -- * Export-failure observation (exporter wrappers)
    observeSpanExporter,
    observeMetricExporter,
) where

import Katip (LogEnv)
import OpenTelemetry.Environment (lookupBooleanEnv)
import OpenTelemetry.Exporter.Metric (MetricExporter (..))
import OpenTelemetry.Exporter.OTLP.Span (loadExporterEnvironmentVariables, otlpExporter)
import OpenTelemetry.Exporter.Span (SpanExporter (..))
import OpenTelemetry.Log (initializeGlobalLoggerProvider, shutdownLoggerProvider)
import OpenTelemetry.Metric (
    MeterProvider (..),
    PeriodicMetricReaderHandle (..),
    createMeterProvider,
    defaultSdkMeterProviderOptions,
    forkPeriodicMetricReader,
    noopMeterProvider,
    periodicMetricReaderOptionsFromEnv,
    resolveMetricExporter,
    setGlobalMeterProvider,
    shutdownMeterProvider,
 )
import OpenTelemetry.Registry (registerSpanExporterFactory)
import OpenTelemetry.Resource (materializeResources, mergeResources, mkResource)
import OpenTelemetry.Resource.Detect (detectBuiltInResources, detectResourceAttributes)
import OpenTelemetry.SDK (OTelSignals (..))
import OpenTelemetry.Trace (TracerProvider, initializeGlobalTracerProvider, shutdownTracerProvider)
import UnliftIO (bracket)
import UnliftIO.Exception (catchAny)

import Ecluse.Runtime.Telemetry.Resolve (
    ExportFailureSink,
    exportFailureSink,
    installExportErrorHandler,
    observeExportResult,
 )

import Ecluse.Core.Wire (WireVocab (..), parseWire)

{- | The @ECLUSE_OBSERVABILITY__TELEMETRY@ master switch. Telemetry is opt-in, so
'TelemetryOff' is the default.
-}
data TelemetrySwitch
    = -- | Telemetry is disabled (the default): nothing is wired and nothing is emitted.
      TelemetryOff
    | {- | Telemetry is enabled: the SDK providers are built from the standard
      @OTEL_*@ environment and the OTLP exporter is active.
      -}
      TelemetryOn
    deriving stock (Eq, Show)

-- Listed @on@ before @off@: that is the order the accepted-set message names them.
instance WireVocab TelemetrySwitch where
    wireKind = "telemetry switch"
    wireTable =
        (TelemetryOn, "on")
            :| [(TelemetryOff, "off")]

{- | Parse a 'TelemetrySwitch' from its wire name. An unrecognised value fails loudly with the
accepted set, never falling back to a mode.

>>> parseTelemetrySwitch "off"
Right TelemetryOff

>>> parseTelemetrySwitch "on"
Right TelemetryOn

>>> parseTelemetrySwitch "maybe"
Left "unknown telemetry switch \"maybe\" (expected one of: on, off)"
-}
parseTelemetrySwitch :: Text -> Either Text TelemetrySwitch
parseTelemetrySwitch = parseWire

{- | The telemetry handle held in the composition root: the off-by-default no-op or the enabled
providers. The disabled case carries no provider, so telemetry is inert rather than unsampled.
-}
data Telemetry
    = -- | The off-by-default no-op: no providers, nothing emitted.
      TelemetryDisabled
    | {- | The enabled handle carrying the SDK's providers, built from the standard
      @OTEL_*@ environment. The providers live in a 'TelemetryProviders' product so
      neither field is a partial record selector on this sum.
      -}
      TelemetryEnabled TelemetryProviders

{- | The SDK providers an enabled 'Telemetry' handle carries: a total product, so its
fields are not partial selectors over the 'Telemetry' sum.
-}
data TelemetryProviders = TelemetryProviders
    { tpTracerProvider :: TracerProvider
    -- ^ The SDK tracer provider the proxy hangs spans on.
    , tpMeterProvider :: MeterProvider
    -- ^ The SDK meter provider the proxy hangs metric instruments on.
    }

{- | The disabled telemetry handle: the off-by-default no-op that holds no providers
and emits nothing. This is what an unset @ECLUSE_OBSERVABILITY__TELEMETRY@ resolves to.
-}
telemetryDisabled :: Telemetry
telemetryDisabled = TelemetryDisabled

{- | Build an enabled telemetry handle from the SDK signals. This is the only way to obtain
one, so an enabled handle's providers always come from the bracketed SDK lifecycle.
-}
telemetryEnabled :: OTelSignals -> Telemetry
telemetryEnabled signals =
    TelemetryEnabled
        TelemetryProviders
            { tpTracerProvider = otelTracerProvider signals
            , tpMeterProvider = otelMeterProvider signals
            }

{- | The tracer provider a 'Telemetry' handle exposes, 'Nothing' when telemetry is disabled.
'Nothing' means emit nothing, never fabricate a no-op provider at the edge.
-}
telemetryTracerProvider :: Telemetry -> Maybe TracerProvider
telemetryTracerProvider = \case
    TelemetryDisabled -> Nothing
    TelemetryEnabled providers -> Just (tpTracerProvider providers)

{- | The meter provider a 'Telemetry' handle exposes, 'Nothing' when telemetry is
disabled (the dual of 'telemetryTracerProvider' for metric instruments).
-}
telemetryMeterProvider :: Telemetry -> Maybe MeterProvider
telemetryMeterProvider = \case
    TelemetryDisabled -> Nothing
    TelemetryEnabled providers -> Just (tpMeterProvider providers)

{- | Run an action with a 'Telemetry' handle bracketed by the 'TelemetrySwitch'. 'TelemetryOff'
never initialises the SDK, so no exporter opens. 'TelemetryOn' builds the providers from the
@OTEL_*@ environment and tears them down on every exit path, flushing spans and metrics.
-}
withTelemetry :: TelemetrySwitch -> LogEnv -> (Telemetry -> IO a) -> IO a
withTelemetry switch logEnv use = case switch of
    TelemetryOff -> use telemetryDisabled
    TelemetryOn -> do
        sink <- exportFailureSink logEnv
        installExportErrorHandler sink
        registerObservedSpanExporter sink
        bracket (initializeObservedOpenTelemetry sink) otelShutdown (use . telemetryEnabled)

{- Wrap the OTLP span exporter so a failed export is observed: @hs-opentelemetry@ 1.0.0.0
discards the 'ExportResult' in the batch processor, so the failure is otherwise invisible. -}
observeSpanExporter :: ExportFailureSink -> SpanExporter -> SpanExporter
observeSpanExporter sink inner =
    inner
        { spanExporterExport = \completedSpans -> do
            result <- spanExporterExport inner completedSpans
            observeExportResult sink "span" result
            pure result
        }

-- Dual of 'observeSpanExporter' for the periodic metric reader's exporter (which likewise
-- discards the 'ExportResult').
observeMetricExporter :: ExportFailureSink -> MetricExporter -> MetricExporter
observeMetricExporter sink inner =
    inner
        { metricExporterExport = \batches -> do
            result <- metricExporterExport inner batches
            observeExportResult sink "metric" result
            pure result
        }

{- Register the observed OTLP span exporter under the @otlp@ key before the SDK's env-driven
tracer init runs: the registry prefers a registered factory over the built-in default. -}
registerObservedSpanExporter :: ExportFailureSink -> IO ()
registerObservedSpanExporter sink =
    registerSpanExporterFactory
        "otlp"
        (observeSpanExporter sink <$> (otlpExporter =<< loadExporterEnvironmentVariables))

{- Mirror @hs-opentelemetry-sdk@ 1.0.0.0's @initializeOpenTelemetry@, wrapping the OTLP
exporters for failure observation. 'registerObservedSpanExporter' must run first, since the
tracer takes its exporter from the registry. Re-diff against the SDK on any version bump. -}
initializeObservedOpenTelemetry :: ExportFailureSink -> IO OTelSignals
initializeObservedOpenTelemetry sink = do
    tracerProvider <- initializeGlobalTracerProvider
    meterProvider <- initializeObservedMeterProvider sink
    loggerProvider <- initializeGlobalLoggerProvider
    let shutdown = do
            void (shutdownTracerProvider tracerProvider Nothing) `catchAny` const pass
            void (shutdownMeterProvider meterProvider Nothing) `catchAny` const pass
            void (shutdownLoggerProvider loggerProvider Nothing) `catchAny` const pass
    pure
        OTelSignals
            { otelTracerProvider = tracerProvider
            , otelMeterProvider = meterProvider
            , otelLoggerProvider = loggerProvider
            , otelPropagators = mempty
            , otelShutdown = shutdown
            }

{- Mirror @hs-opentelemetry-sdk@ 1.0.0.0's @initializeGlobalMeterProvider@, differing only in
wrapping the metric exporter for failure observation. The SDK's metric init takes the exporter
directly, with no registry hook like the span path. Re-diff against the SDK on any version bump. -}
initializeObservedMeterProvider :: ExportFailureSink -> IO MeterProvider
initializeObservedMeterProvider sink = do
    disabled <- lookupBooleanEnv "OTEL_SDK_DISABLED"
    if disabled
        then noopMeterProvider <$ setGlobalMeterProvider noopMeterProvider
        else do
            exporter <- observeMetricExporter sink <$> resolveMetricExporter
            readerOptions <- periodicMetricReaderOptionsFromEnv
            builtInResources <- detectBuiltInResources
            envResources <- mkResource . map Just <$> detectResourceAttributes
            let resources = materializeResources (mergeResources envResources builtInResources)
            (provider, env) <- createMeterProvider resources defaultSdkMeterProviderOptions
            readerHandle <- forkPeriodicMetricReader env exporter readerOptions
            let provider' = stopReaderOnShutdown readerHandle provider
            setGlobalMeterProvider provider'
            pure provider'

{- Mirrors the SDK's own shutdown ordering, and is part of the same version-pin re-diff
surface as 'initializeObservedMeterProvider'. -}
stopReaderOnShutdown :: PeriodicMetricReaderHandle -> MeterProvider -> MeterProvider
stopReaderOnShutdown readerHandle provider =
    provider
        { meterProviderShutdown = \timeout -> do
            stopPeriodicMetricReader readerHandle
            meterProviderShutdown provider timeout
        }
