-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | In-process telemetry collection without exporters or background readers, a captured span's events, and the thread's active span.
module Ecluse.Runtime.Test.Telemetry (withTestTelemetry, withGlobalTestTelemetry, gaugePoints, sumPoints, histogramCounts, spanEvents, activeSpanContext) where

import OpenTelemetry.Attributes (Attributes)
import OpenTelemetry.Context (lookupSpan)
import OpenTelemetry.Context.ThreadLocal (getContext)
import OpenTelemetry.Exporter.Metric (
    GaugeDataPoint (gaugeDataPointAttributes, gaugeDataPointValue),
    HistogramDataPoint (histogramDataPointCount),
    MetricExport (MetricExportGauge, MetricExportHistogram, MetricExportSum, megGaugePoints, megName, mehName, mehPoints, mesName, mesSumPoints),
    NumberValue (IntNumber),
    ResourceMetricsExport (resourceMetricsScopes),
    ScopeMetricsExport (scopeMetricsExports),
    SumDataPoint (sumDataPointAttributes, sumDataPointValue),
 )
import OpenTelemetry.MeterProvider (SdkMeterEnv, collectResourceMetrics)
import OpenTelemetry.Metric (
    MeterProvider,
    createMeterProvider,
    defaultSdkMeterProviderOptions,
    getGlobalMeterProvider,
    setGlobalMeterProvider,
    shutdownMeterProvider,
 )
import OpenTelemetry.Resource (emptyMaterializedResources)
import OpenTelemetry.Trace (
    TracerProvider,
    createTracerProvider,
    emptyTracerProviderOptions,
    getGlobalTracerProvider,
    setGlobalTracerProvider,
    shutdownTracerProvider,
 )
import OpenTelemetry.Trace.Core (Event, SpanContext, SpanHot (hotEvents), getSpanContext)
import OpenTelemetry.Util (appendOnlyBoundedCollectionValues)
import UnliftIO (bracket)

import Ecluse.Runtime.Telemetry.Internal (Telemetry (TelemetryEnabled), TelemetryProviders (TelemetryProviders))

-- | Keep both SDK providers alive until all measurements and assertions finish.
withTestTelemetry :: (Telemetry -> SdkMeterEnv -> IO a) -> IO a
withTestTelemetry use = withTestProviders (\tracer meter -> use (TelemetryEnabled (TelemetryProviders tracer meter)))

-- | 'withTestTelemetry' with both providers installed as the process's global ones, as the boot installs them, until the action ends.
withGlobalTestTelemetry :: (Telemetry -> SdkMeterEnv -> IO a) -> IO a
withGlobalTestTelemetry use =
    withTestProviders $ \tracer meter meterEnv ->
        bracket (swapGlobals tracer meter) (uncurry swapGlobals) $ \_ ->
            use (TelemetryEnabled (TelemetryProviders tracer meter)) meterEnv

withTestProviders :: (TracerProvider -> MeterProvider -> SdkMeterEnv -> IO a) -> IO a
withTestProviders use =
    bracket (createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions) (\(meter, _) -> void (shutdownMeterProvider meter Nothing)) $ \(meter, meterEnv) ->
        bracket (createTracerProvider [] emptyTracerProviderOptions) (\tracer -> void (shutdownTracerProvider tracer Nothing)) $ \tracer ->
            use tracer meter meterEnv

-- Install the providers as the global ones and hand back the ones they replaced.
swapGlobals :: TracerProvider -> MeterProvider -> IO (TracerProvider, MeterProvider)
swapGlobals tracer meter = do
    replaced <- (,) <$> getGlobalTracerProvider <*> getGlobalMeterProvider
    setGlobalTracerProvider tracer
    setGlobalMeterProvider meter
    pure replaced

-- | Collect integer gauge observations under a metric name.
gaugePoints :: Text -> SdkMeterEnv -> IO [(Attributes, Int64)]
gaugePoints wanted = collectPoints $ \case
    MetricExportGauge{megName = name, megGaugePoints = points}
        | name == wanted ->
            [(gaugeDataPointAttributes point, gaugeDataPointValue point) | point <- toList points]
    _ -> []

-- | Collect integer counter observations under a metric name.
sumPoints :: Text -> SdkMeterEnv -> IO [(Attributes, Int64)]
sumPoints wanted = collectPoints $ \case
    MetricExportSum{mesName = name, mesSumPoints = points}
        | name == wanted ->
            [(sumDataPointAttributes point, sumDataPointValue point) | point <- toList points]
    _ -> []

-- | How many measurements each histogram point under a metric name holds.
histogramCounts :: Text -> SdkMeterEnv -> IO [Word64]
histogramCounts wanted meterEnv =
    collectResourceMetrics meterEnv <&> \batches ->
        [ histogramDataPointCount point
        | MetricExportHistogram{mehName = name, mehPoints = points} <- metricExports batches
        , name == wanted
        , point <- toList points
        ]

-- | The events a captured span holds, the library's own @exception@ event included.
spanEvents :: SpanHot -> [Event]
spanEvents = toList . appendOnlyBoundedCollectionValues . hotEvents

-- | The context of the span active on this thread, if any.
activeSpanContext :: IO (Maybe SpanContext)
activeSpanContext = getContext >>= traverse getSpanContext . lookupSpan

collectPoints :: (MetricExport -> [(Attributes, NumberValue)]) -> SdkMeterEnv -> IO [(Attributes, Int64)]
collectPoints select meterEnv =
    collectResourceMetrics meterEnv <&> \batches ->
        [(attrs, value) | metric <- metricExports batches, (attrs, IntNumber value) <- select metric]

-- Every metric in a collection, across its resources and scopes.
metricExports :: [ResourceMetricsExport] -> [MetricExport]
metricExports batches =
    [ metric
    | batch <- batches
    , scope <- toList (resourceMetricsScopes batch)
    , metric <- toList (scopeMetricsExports scope)
    ]
