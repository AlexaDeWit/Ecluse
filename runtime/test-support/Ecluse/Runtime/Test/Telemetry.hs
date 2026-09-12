-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | In-process telemetry collection without exporters or background readers.
module Ecluse.Runtime.Test.Telemetry (withTestTelemetry, gaugePoints, sumPoints) where

import OpenTelemetry.Attributes (Attributes)
import OpenTelemetry.Exporter.Metric (
    GaugeDataPoint (gaugeDataPointAttributes, gaugeDataPointValue),
    MetricExport (MetricExportGauge, MetricExportSum, megGaugePoints, megName, mesName, mesSumPoints),
    NumberValue (IntNumber),
    ResourceMetricsExport (resourceMetricsScopes),
    ScopeMetricsExport (scopeMetricsExports),
    SumDataPoint (sumDataPointAttributes, sumDataPointValue),
 )
import OpenTelemetry.MeterProvider (SdkMeterEnv, collectResourceMetrics)
import OpenTelemetry.Metric (createMeterProvider, defaultSdkMeterProviderOptions, shutdownMeterProvider)
import OpenTelemetry.Resource (emptyMaterializedResources)
import OpenTelemetry.Trace (createTracerProvider, emptyTracerProviderOptions, shutdownTracerProvider)
import UnliftIO (bracket)

import Ecluse.Runtime.Telemetry (Telemetry (TelemetryEnabled), TelemetryProviders (TelemetryProviders))

-- | Keep both SDK providers alive until all measurements and assertions finish.
withTestTelemetry :: (Telemetry -> SdkMeterEnv -> IO a) -> IO a
withTestTelemetry use =
    bracket (createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions) (\(meter, _) -> void (shutdownMeterProvider meter Nothing)) $ \(meter, meterEnv) ->
        bracket (createTracerProvider [] emptyTracerProviderOptions) (\tracer -> void (shutdownTracerProvider tracer Nothing)) $ \tracer ->
            use (TelemetryEnabled (TelemetryProviders tracer meter)) meterEnv

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

collectPoints :: (MetricExport -> [(Attributes, NumberValue)]) -> SdkMeterEnv -> IO [(Attributes, Int64)]
collectPoints select meterEnv = do
    batches <- collectResourceMetrics meterEnv
    pure
        [ (attrs, value)
        | batch <- batches
        , scope <- toList (resourceMetricsScopes batch)
        , metric <- toList (scopeMetricsExports scope)
        , (attrs, IntNumber value) <- select metric
        ]
