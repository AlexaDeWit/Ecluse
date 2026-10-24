-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Production metric batching with in-process collection, captured span events, and the active span.
module Ecluse.Runtime.Test.Telemetry (withTestTelemetry, withGlobalTestTelemetry, gaugePoints, sumPoints, sumPointsIn, histogramCounts, metricExports, spanEvents, activeSpanContext) where

import Control.Exception qualified as Exception
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
import UnliftIO.Exception (throwIO)

import Ecluse.Runtime.Telemetry.Batched (batchMeterProvider)

import Ecluse.Runtime.Telemetry.Internal (Telemetry (TelemetryEnabled), TelemetryProviders (TelemetryProviders))

-- | Keep both SDK providers alive until all measurements and assertions finish.
withTestTelemetry :: (Telemetry -> SdkMeterEnv -> IO a) -> IO a
withTestTelemetry use = withTestProviders (\tracer meter -> use (TelemetryEnabled (TelemetryProviders tracer meter)))

-- | Install both test providers globally until the action ends.
withGlobalTestTelemetry :: (Telemetry -> SdkMeterEnv -> IO a) -> IO a
withGlobalTestTelemetry use =
    withTestProviders $ \tracer meter meterEnv ->
        bracket (swapGlobals tracer meter) (uncurry swapGlobals) $ \_ ->
            use (TelemetryEnabled (TelemetryProviders tracer meter)) meterEnv

withTestProviders :: (TracerProvider -> MeterProvider -> SdkMeterEnv -> IO a) -> IO a
withTestProviders use =
    bracket createBatchedProvider (\(meter, _) -> void (shutdownMeterProvider meter Nothing)) $ \(meter, meterEnv) ->
        bracket (createTracerProvider [] emptyTracerProviderOptions) (\tracer -> void (shutdownTracerProvider tracer Nothing)) $ \tracer ->
            use tracer meter meterEnv
  where
    createBatchedProvider =
        Exception.bracketOnError (createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions) (\(sdk, _) -> void (shutdownMeterProvider sdk Nothing)) $ \(sdk, meterEnv) -> do
            (meter, _) <- batchMeterProvider throwIO sdk
            pure (meter, meterEnv)

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
sumPoints wanted meterEnv = sumPointsIn wanted <$> collectResourceMetrics meterEnv

-- | Integer counter observations under a metric name in a collection already taken, such as an exporter's.
sumPointsIn :: Text -> [ResourceMetricsExport] -> [(Attributes, Int64)]
sumPointsIn wanted = selectPoints $ \case
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

-- | Every metric in a collection, across its resources and scopes.
metricExports :: [ResourceMetricsExport] -> [MetricExport]
metricExports batches =
    [ metric
    | batch <- batches
    , scope <- toList (resourceMetricsScopes batch)
    , metric <- toList (scopeMetricsExports scope)
    ]

-- | The events a captured span holds, the library's own @exception@ event included.
spanEvents :: SpanHot -> [Event]
spanEvents = toList . appendOnlyBoundedCollectionValues . hotEvents

-- | The context of the span active on this thread, if any.
activeSpanContext :: IO (Maybe SpanContext)
activeSpanContext = getContext >>= traverse getSpanContext . lookupSpan

collectPoints :: (MetricExport -> [(Attributes, NumberValue)]) -> SdkMeterEnv -> IO [(Attributes, Int64)]
collectPoints select meterEnv = selectPoints select <$> collectResourceMetrics meterEnv

selectPoints :: (MetricExport -> [(Attributes, NumberValue)]) -> [ResourceMetricsExport] -> [(Attributes, Int64)]
selectPoints select batches = [(attrs, value) | metric <- metricExports batches, (attrs, IntNumber value) <- select metric]
