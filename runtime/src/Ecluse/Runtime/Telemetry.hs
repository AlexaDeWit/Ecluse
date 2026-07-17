-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The OpenTelemetry substrate: the tracer and meter providers the rest of the
proxy will hang spans and metrics on, behind a master switch that defaults to
__off__.

Écluse is a self-hosted proxy operators run inside their own infrastructure, so
observability is __opt-in and vendor-neutral__: the substrate is OpenTelemetry,
emitting OTLP that any compatible backend can receive. The maintainer's choice of backend (Datadog)
must never become every consumer's obligation, so __with @ECLUSE_OBSERVABILITY__TELEMETRY@ unset
nothing is wired and no telemetry is emitted__ -- the SDK is not even initialised.

This module is purely the __substrate__: it stands up (or, by default, declines to
stand up) the providers and brackets their lifecycle. The spans on the request
lifecycle and the metric instruments layer on top of this substrate; nothing
here instruments the hot path.

== The switch and the handle

'TelemetrySwitch' is the @ECLUSE_OBSERVABILITY__TELEMETRY@ master switch, parsed at the
configuration boundary (@Ecluse.Config@) in the same strict, fail-loud style as
the other enums. The 'Telemetry' handle it produces is one of two shapes:

* __'telemetryDisabled'__ -- the off-by-default no-op. It holds no providers, the
  SDK is never initialised, and nothing is exported. This is what an unset
  @ECLUSE_OBSERVABILITY__TELEMETRY@ yields.
* an __enabled__ handle carrying the SDK's tracer and meter providers, built from
  the standard @OTEL_*@ environment variables the SDK reads directly
  (@OTEL_SERVICE_NAME@, @OTEL_RESOURCE_ATTRIBUTES@, @OTEL_EXPORTER_OTLP_ENDPOINT@,
  @OTEL_EXPORTER_OTLP_PROTOCOL@, the sampler). The OTLP exporter defaults to
  HTTP\/protobuf; gRPC stays behind the exporter's cabal flag, off.

'withTelemetry' is the lifecycle bracket the composition root ("Ecluse.Runtime.Env") runs
the proxy within: when enabled it initialises the providers and tears them down --
flushing buffered spans and metrics -- along every exit path; when disabled it is a
pure pass-through that opens nothing to tear down.

When enabled it also makes export failures __visible__: the OTLP span and metric
exporters are wrapped so a failed export -- which @hs-opentelemetry 1.0.0.0@ otherwise
drops silently -- is observed and routed through the shared @katip@ throttle
("Ecluse.Runtime.Telemetry.Resolve"), the first failure logged plainly then a periodic
heartbeat. The wrappers only /observe/; export semantics are unchanged, so an
unreachable collector still degrades off the request path.

The configuration model and the signal catalogue are described in
@docs\/architecture\/observability.md@.
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

{- | The @ECLUSE_OBSERVABILITY__TELEMETRY@ master switch: telemetry is opt-in, so 'TelemetryOff' is
the default and the FOSS posture. A sum type rather than a 'Bool' so each case
names its intent and a future mode (e.g. a metrics-only switch) is a new
constructor, not a second flag.
-}
data TelemetrySwitch
    = -- | Telemetry is disabled (the default): nothing is wired and nothing is emitted.
      TelemetryOff
    | {- | Telemetry is enabled: the SDK providers are built from the standard
      @OTEL_*@ environment and the OTLP exporter is active.
      -}
      TelemetryOn
    deriving stock (Eq, Show)

-- The wire vocabulary of a 'TelemetrySwitch': the single source both 'parseWire' and
-- 'renderWire' derive from for this type. Listed @on@ before @off@, the order the
-- accepted-set message has always named them.
instance WireVocab TelemetrySwitch where
    wireKind = "telemetry switch"
    wireTable =
        (TelemetryOn, "on")
            :| [(TelemetryOff, "off")]

{- | Parse a 'TelemetrySwitch' from its wire name, naming the accepted set on
failure. The same strict, fail-loud style as the other configuration enums
(@Ecluse.Config@): an unrecognised value is a loud failure, never a silent
fallback to one mode or the other.

>>> parseTelemetrySwitch "off"
Right TelemetryOff

>>> parseTelemetrySwitch "on"
Right TelemetryOn

>>> parseTelemetrySwitch "maybe"
Left "unknown telemetry switch \"maybe\" (expected one of: on, off)"
-}
parseTelemetrySwitch :: Text -> Either Text TelemetrySwitch
parseTelemetrySwitch = parseWire

{- | The telemetry handle held in the composition root: either the off-by-default
no-op or the enabled providers. Spans and metric instruments are derived from the
providers it carries; the disabled case carries none, so a layer that reaches for a
provider finds nothing to emit through -- telemetry is inert, not merely
unsampled.
-}
data Telemetry
    = -- | The off-by-default no-op: no providers, nothing emitted.
      TelemetryDisabled
    | {- | The enabled handle carrying the SDK's providers, built from the standard
      @OTEL_*@ environment. The providers live in a 'TelemetryProviders' product so
      neither field is a partial record selector on this sum.
      -}
      TelemetryEnabled TelemetryProviders

{- | The SDK providers an enabled 'Telemetry' handle carries -- a total product, so
its fields are not partial selectors over the 'Telemetry' sum.
-}
data TelemetryProviders = TelemetryProviders
    { tpTracerProvider :: TracerProvider
    -- ^ The SDK tracer provider spans are created through.
    , tpMeterProvider :: MeterProvider
    -- ^ The SDK meter provider metric instruments are created through.
    }

{- | The disabled telemetry handle: the off-by-default no-op that holds no
providers and emits nothing. This is what an unset @ECLUSE_OBSERVABILITY__TELEMETRY@ resolves to.
-}
telemetryDisabled :: Telemetry
telemetryDisabled = TelemetryDisabled

{- | Build an enabled telemetry handle from the SDK signals -- the tracer and meter
providers. The disabled case has no constructor argument, so this is the only way
to obtain an enabled handle, keeping its providers' origin (the bracketed SDK
lifecycle) explicit.
-}
telemetryEnabled :: OTelSignals -> Telemetry
telemetryEnabled signals =
    TelemetryEnabled
        TelemetryProviders
            { tpTracerProvider = otelTracerProvider signals
            , tpMeterProvider = otelMeterProvider signals
            }

{- | The tracer provider a 'Telemetry' handle exposes, 'Nothing' when telemetry is
disabled. A caller that wants to create a span resolves this first; 'Nothing' is
the signal to emit nothing rather than to fabricate a no-op provider at the edge.
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

{- | Run an action with a 'Telemetry' handle whose lifecycle is bracketed by the
'TelemetrySwitch', tearing the providers down -- flushing buffered spans and
metrics -- along every exit path.

* __'TelemetryOff'__ (the default) is a pure pass-through: the SDK is __never
  initialised__, the body runs against 'telemetryDisabled', the 'LogEnv' is unused,
  and there is nothing to tear down. An unset @ECLUSE_OBSERVABILITY__TELEMETRY@ therefore opens no
  exporter and emits nothing.
* __'TelemetryOn'__ initialises the SDK from the standard @OTEL_*@ environment with the
  OTLP exporters wrapped for failure observation (the shared throttle feeds the supplied
  'LogEnv'), runs the body against the enabled handle, and shuts the providers down on
  exit.

This is the scope the composition root ("Ecluse.Runtime.Env") runs the server and worker
within, so telemetry is established once and flushed on shutdown.
-}
withTelemetry :: TelemetrySwitch -> LogEnv -> (Telemetry -> IO a) -> IO a
withTelemetry switch logEnv use = case switch of
    TelemetryOff -> use telemetryDisabled
    TelemetryOn -> do
        sink <- exportFailureSink logEnv
        installExportErrorHandler sink
        registerObservedSpanExporter sink
        bracket (initializeObservedOpenTelemetry sink) otelShutdown (use . telemetryEnabled)

{- Wrap the OTLP span exporter so a failed export is observed -- routed through the shared
'ExportFailureSink' into @katip@ under a throttle -- and the inner result returned
unchanged. @hs-opentelemetry 1.0.0.0@ drops a failed export silently (the batch processor
discards the 'ExportResult'), so this wrapper is where Écluse learns the export failed
without changing export semantics: the failure stays off the request path. -}
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

{- Register the observed OTLP span exporter under the @otlp@ key before the SDK's
env-driven tracer init runs: 'initializeGlobalTracerProvider' resolves
@OTEL_TRACES_EXPORTER@ through the exporter 'OpenTelemetry.Registry', which prefers a
registered factory over the built-in default, so the wrapped exporter is the one the
batch processor drives. The metric path has no such registry hook, so it is wrapped
directly in 'initializeObservedMeterProvider'. -}
registerObservedSpanExporter :: ExportFailureSink -> IO ()
registerObservedSpanExporter sink =
    registerSpanExporterFactory
        "otlp"
        (observeSpanExporter sink <$> (otlpExporter =<< loadExporterEnvironmentVariables))

{- Stand up the three SDK signal providers from the @OTEL_*@ environment with the OTLP
exporters wrapped for failure observation, mirroring @hs-opentelemetry-sdk@'s own
@initializeOpenTelemetry@. The tracer picks up the observed span exporter through the
registry ('registerObservedSpanExporter', run before this); the meter is built here
because the SDK's metric init exposes no registry hook for its exporter. This and
'initializeObservedMeterProvider' are pinned to @hs-opentelemetry-sdk 1.0.0.0@; re-diff
both against the SDK on any version bump. -}
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

{- Build the global meter provider with the OTLP metric exporter wrapped for failure
observation. Mirrors @hs-opentelemetry-sdk@'s @initializeGlobalMeterProvider@ exactly,
differing only in wrapping the exporter the periodic reader drives -- the SDK's metric
init takes the exporter directly ('resolveMetricExporter') with no registry injection
point, unlike the span path. Pinned to @hs-opentelemetry-sdk 1.0.0.0@; re-verify against
the SDK's @initializeGlobalMeterProvider@ on any version bump. -}
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

{- Wrap a meter provider so its shutdown stops the periodic metric reader before the
provider's own shutdown, as the mirrored SDK init does; part of the same version-pin
re-diff surface as 'initializeObservedMeterProvider'. -}
stopReaderOnShutdown :: PeriodicMetricReaderHandle -> MeterProvider -> MeterProvider
stopReaderOnShutdown readerHandle provider =
    provider
        { meterProviderShutdown = \timeout -> do
            stopPeriodicMetricReader readerHandle
            meterProviderShutdown provider timeout
        }
