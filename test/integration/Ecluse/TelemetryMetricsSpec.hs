-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.TelemetryMetricsSpec (spec) where

import Test.Hspec

import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import OpenTelemetry.Metric.Core (forceFlushMeterProvider)

import Ecluse.Core.Telemetry.Metrics (
    CacheResult (Hit, Miss),
    Decision (Admit, Deny),
    MirrorResult (Published),
    ReasonClass (ReasonPolicy),
    StatusClass (Status2xx),
    Upstream (Public),
 )
import Ecluse.Integration.Collector (
    Collector (collectorEndpoint),
    awaitCollectorLine,
    withCollector,
    withSdkEnv,
 )
import Ecluse.Runtime.Telemetry (
    TelemetrySwitch (TelemetryOff, TelemetryOn),
    telemetryMeterProvider,
    withTelemetry,
 )
import Ecluse.Runtime.Telemetry.Instruments (
    newMetrics,
    recordCacheRequest,
    recordMirrorJobProcessed,
    recordRuleDenial,
    recordServeDecision,
    recordUpstreamFetch,
 )

{- | Drive @ecluse.*@ measurements through an in-process telemetry handle into a real OTLP
Collector container, then assert the Collector accepts the series. The assertion keys on the
catalogue metric /name/, never a unique label, because the catalogue enforces bounded labels.
-}
spec :: Spec
spec =
    around (withCollector ["metrics"]) $
        describe "metrics → OTLP collector" $ do
            it "delivers ecluse.* metrics to the collector when telemetry is on" $ \collector -> do
                driveMetrics collector TelemetryOn
                accepted <- awaitCollectorLine collector markerMetric 40
                accepted `shouldBe` True

            it "delivers nothing to the collector when telemetry is off" $ \collector -> do
                driveMetrics collector TelemetryOff
                accepted <- awaitCollectorLine collector markerMetric 8
                accepted `shouldBe` False

-- The catalogue metric whose name the assertion watches for in the collector's logs.
markerMetric :: Text
markerMetric = "ecluse.serve.decision"

{- Record a spread of @ecluse.*@ signals, then force-flush so the export does not wait on the
periodic reader's window. With telemetry off there is no provider to flush. -}
driveMetrics :: Collector -> TelemetrySwitch -> IO ()
driveMetrics collector switch = do
    logEnv <- initLogEnv (Namespace ["itest"]) (Environment "test")
    withSdkEnv (collectorEndpoint collector) metricsExporter $
        withTelemetry switch logEnv $ \telemetry -> do
            metrics <- newMetrics telemetry
            -- A representative spread across instrument kinds and bounded labels.
            recordServeDecision metrics Admit
            recordServeDecision metrics Deny
            recordRuleDenial metrics (Just "min-age") ReasonPolicy
            recordUpstreamFetch metrics Public Status2xx 0.012
            recordCacheRequest metrics Hit
            recordCacheRequest metrics Miss
            recordMirrorJobProcessed metrics Published
            whenJust (telemetryMeterProvider telemetry) $ \meterProvider ->
                void (forceFlushMeterProvider meterProvider Nothing)

-- The signal this spec exports, over the baseline that silences every other one.
metricsExporter :: [(String, String)]
metricsExporter = [("OTEL_METRICS_EXPORTER", "otlp"), ("OTEL_METRIC_EXPORT_INTERVAL", "200")]
