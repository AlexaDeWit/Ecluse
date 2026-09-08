-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | In-process telemetry and advisory slots for role composition tests.
module Ecluse.Composition.TelemetrySupport (
    withRoleTelemetry,
    newAdvisoryHandles,
    advisoryAgePoints,
) where

import Katip (LogEnv, closeScribes)
import OpenTelemetry.Attributes (Attributes)
import OpenTelemetry.Exporter.Metric (
    GaugeDataPoint (gaugeDataPointAttributes, gaugeDataPointValue),
    MetricExport (MetricExportGauge, megGaugePoints, megName),
    NumberValue (IntNumber),
    ResourceMetricsExport (resourceMetricsScopes),
    ScopeMetricsExport (scopeMetricsExports),
 )
import OpenTelemetry.MeterProvider (SdkMeterEnv, collectResourceMetrics)
import OpenTelemetry.Metric (createMeterProvider, defaultSdkMeterProviderOptions, shutdownMeterProvider)
import OpenTelemetry.Resource (emptyMaterializedResources)
import OpenTelemetry.Trace (createTracerProvider, emptyTracerProviderOptions, shutdownTracerProvider)
import UnliftIO (bracket)

import Ecluse.Core.Cve.Slot (newCveSlot)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Cve.Sync (CveSyncHandle (..))
import Ecluse.Runtime.Cve.Sync (SyncEnv (SyncEnv))
import Ecluse.Runtime.Telemetry (Telemetry (TelemetryEnabled), TelemetryProviders (TelemetryProviders))
import Ecluse.Runtime.Test.Cve (headOnlyFetch)
import Ecluse.Test.Log (newTestLogEnv)

-- | Keep the logger and both SDK providers alive until the role and its assertions finish.
withRoleTelemetry :: (LogEnv -> Telemetry -> SdkMeterEnv -> IO a) -> IO a
withRoleTelemetry use =
    bracket (createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions) (\(meter, _) -> void (shutdownMeterProvider meter Nothing)) $ \(meter, meterEnv) ->
        bracket (createTracerProvider [] emptyTracerProviderOptions) (\tracer -> void (shutdownTracerProvider tracer Nothing)) $ \tracer ->
            bracket newTestLogEnv (void . closeScribes) $ \logEnv ->
                use logEnv (TelemetryEnabled (TelemetryProviders tracer meter)) meterEnv

-- | Ready slots whose fetch reports no published artifact, so role tasks cannot change their generation.
newAdvisoryHandles :: [Ecosystem] -> IO [(Ecosystem, CveSyncHandle)]
newAdvisoryHandles ecosystems = forM ecosystems $ \eco -> do
    slot <- newCveSlot
    ready <- newTVarIO True
    let env = SyncEnv (headOnlyFetch (Right Nothing)) eco "unused.db" slot
    pure (eco, CveSyncHandle ready env)

-- | Collect the SDK's age observations without registering a callback in the test.
advisoryAgePoints :: SdkMeterEnv -> IO [(Attributes, Int64)]
advisoryAgePoints meterEnv = do
    batches <- collectResourceMetrics meterEnv
    pure
        [ (gaugeDataPointAttributes point, age)
        | batch <- batches
        , scope <- toList (resourceMetricsScopes batch)
        , MetricExportGauge{megName = name, megGaugePoints = points} <- toList (scopeMetricsExports scope)
        , name == "ecluse.advisory.database.age.seconds"
        , point <- toList points
        , IntNumber age <- [gaugeDataPointValue point]
        ]
