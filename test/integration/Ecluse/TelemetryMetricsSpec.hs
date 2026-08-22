-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.TelemetryMetricsSpec (spec) where

import Data.ByteString qualified as BS
import System.Environment (setEnv, unsetEnv)
import Test.Hspec
import UnliftIO (bracket)

import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import OpenTelemetry.Metric.Core (forceFlushMeterProvider)
import TestContainers (Container, containerAddress)
import TestContainers qualified as TC
import TestContainers.Docker (fromDockerfile, withLabels)
import TestContainers.Hspec (withContainers)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Telemetry.Metrics (
    CacheResult (Hit, Miss),
    Decision (Admit, Deny),
    MirrorResult (Published),
    ReasonClass (ReasonPolicy),
    StatusClass (Status2xx),
    Upstream (Public),
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
import Ecluse.Test.Container.Image (PinnedImageRef, mkPinnedImageRef, renderPinnedImageRef)
import Ecluse.Test.Containers (testContainerLabels)

{- | The integration tier for metrics. Drive @ecluse.*@ measurements through an
in-process telemetry handle into a real OTLP __Collector__ container (no Datadog SaaS),
then assert the Collector accepts the series. The Collector runs an OTLP\/HTTP receiver
into a @debug@ exporter at detailed verbosity. It therefore writes every received
metric, its name and labels, to its logs. The test records a spread of catalogue
signals, force-flushes the meter provider, then watches the Collector's logs for a known
metric name.

Two cases prove the wire and its gate. With telemetry __on__ the metric reaches the
Collector, so the SDK exported it and the Collector accepted it. With telemetry __off__
the SDK never initialises and exports nothing, so the name never appears. The metric
/name/ is the marker, never a label: a unique-per-run label would breach the
bounded-label discipline the catalogue enforces. The assertion therefore keys on the
catalogue name, which a fresh per-case container makes unambiguous. Gating and
Dockerised, the same tier as the tracing and mirror-queue tests. It needs a Docker
daemon and no external network beyond pulling the Collector image.
-}
spec :: Spec
spec =
    around withCollector $
        describe "metrics → OTLP collector" $ do
            it "delivers ecluse.* metrics to the collector when telemetry is on" $ \collector -> do
                driveMetrics collector TelemetryOn
                accepted <- awaitMetric collector markerMetric 40
                accepted `shouldBe` True

            it "delivers nothing to the collector when telemetry is off" $ \collector -> do
                driveMetrics collector TelemetryOff
                accepted <- awaitMetric collector markerMetric 8
                accepted `shouldBe` False

-- The catalogue metric whose name the assertion watches for in the collector's logs.
markerMetric :: Text
markerMetric = "ecluse.serve.decision"

{- Record a spread of @ecluse.*@ signals through an in-process telemetry handle pointed
at the collector. Then force-flush the meter provider, so the export does not wait on
the periodic reader's window. With telemetry off, 'newMetrics' builds against the no-op
meter and no provider exists to flush, so the handle emits nothing. -}
driveMetrics :: Collector -> TelemetrySwitch -> IO ()
driveMetrics collector switch = do
    logEnv <- initLogEnv (Namespace ["itest"]) (Environment "test")
    withSdkEnv (collectorEndpoint collector) $
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

{- Run an action with the SDK pointed at the collector through the standard @OTEL_*@
environment, then __restore the prior environment on exit__. The metrics exporter is on,
because the collector carries a metrics pipeline. Traces and logs are off, so the SDK
does not ship signals the collector has no pipeline for. The @setEnv@ call is
process-global, and the integration suite runs every spec in one process. Without the
restore these values (e.g. @OTEL_TRACES_EXPORTER=none@) would leak into a later spec.
This saves every key it sets and puts it back, or unsets it if it was absent. -}
withSdkEnv :: Text -> IO a -> IO a
withSdkEnv endpoint act = bracket saveKeys restoreKeys (const (apply >> act))
  where
    keys :: [String]
    keys =
        [ "OTEL_EXPORTER_OTLP_ENDPOINT"
        , "OTEL_EXPORTER_OTLP_PROTOCOL"
        , "OTEL_SERVICE_NAME"
        , "OTEL_METRICS_EXPORTER"
        , "OTEL_TRACES_EXPORTER"
        , "OTEL_LOGS_EXPORTER"
        , "OTEL_METRIC_EXPORT_INTERVAL"
        ]

    saveKeys :: IO [(String, Maybe String)]
    saveKeys = traverse (\k -> (k,) <$> lookupEnv k) keys

    restoreKeys :: [(String, Maybe String)] -> IO ()
    restoreKeys = traverse_ (\(k, mv) -> maybe (unsetEnv k) (setEnv k) mv)

    apply :: IO ()
    apply = do
        setEnv "OTEL_EXPORTER_OTLP_ENDPOINT" (toString endpoint)
        setEnv "OTEL_EXPORTER_OTLP_PROTOCOL" "http/protobuf"
        setEnv "OTEL_SERVICE_NAME" "ecluse-itest"
        setEnv "OTEL_METRICS_EXPORTER" "otlp"
        setEnv "OTEL_TRACES_EXPORTER" "none"
        setEnv "OTEL_LOGS_EXPORTER" "none"
        setEnv "OTEL_METRIC_EXPORT_INTERVAL" "200"

-- A running OTLP collector: the endpoint to export to, and its accumulated logs.
data Collector = Collector
    { collectorEndpoint :: Text
    , collectorLogs :: IORef [ByteString]
    }

-- The OTLP HTTP receiver port the collector serves on.
collectorPort :: TC.Port
collectorPort = 4318

-- The OTLP Collector image (version 0.119.0), pinned by its multi-arch index digest.
-- 'withCollector' resolves it to a 'PinnedImageRef' at startup. A mutable tag, which
-- could be re-pointed at a poisoned image, therefore aborts the suite rather than
-- reaching the @FROM@ line. This digest matches the e2e harness's collector pin. The
-- core distribution carries the OTLP receiver and the @debug@ exporter the assertion
-- reads.
collectorImage :: Text
collectorImage = "otel/opentelemetry-collector@sha256:3805724e26351df55a45032a793c9b64a2117ac9a58f13f070674a9723fab373"

{- A derived image that bakes the @--config env:OTELCOL_CONFIG@ command into the
collector. Version 0.5.3 of testcontainers appends @setCmd@ to @docker start@, which
rejects it, so the command belongs in the image rather than at run time. The config
itself still arrives through the (correctly applied) @--env@ on @docker create@. -}
collectorDockerfile :: PinnedImageRef -> Text
collectorDockerfile image =
    "FROM "
        <> renderPinnedImageRef image
        <> "\nCMD [\"--config\", \"env:OTELCOL_CONFIG\"]\n"
        <> "LABEL com.ecluse.test=integration\n"

{- The whole collector configuration as a single-line (flow-style) YAML document. It
passes through the @env:@ config provider, so the distroless image needs no shell, file,
or bind mount. It declares an OTLP\/HTTP receiver feeding a @debug@ exporter at detailed
verbosity, through a __metrics__ pipeline. The collector therefore writes every received
metric to the container logs. -}
collectorConfig :: Text
collectorConfig =
    "{receivers: {otlp: {protocols: {http: {endpoint: \"0.0.0.0:4318\"}}}}, "
        <> "exporters: {debug: {verbosity: detailed}}, "
        <> "service: {pipelines: {metrics: {receivers: [otlp], exporters: [debug]}}}}"

{- | Start an OTLP Collector container, follow its logs into a shared buffer the test
inspects, and tear it down after. The container is given the inline config and waits
until its OTLP port accepts connections before the body runs.
-}
withCollector :: (Collector -> IO ()) -> IO ()
withCollector action = do
    logsRef <- newIORef []
    labels <- testContainerLabels "integration"
    -- Resolve the pinned image at startup, failing the suite loudly (the harness's IO
    -- idiom, 'fail') if the literal is not digest-pinned.
    image <- either (fail . toString) pure (mkPinnedImageRef collectorImage)
    withContainers (collectorContainer labels logsRef image) $ \container -> do
        let (host, mappedPort) = containerAddress container collectorPort
        action
            Collector
                { collectorEndpoint = "http://" <> host <> ":" <> show mappedPort
                , collectorLogs = logsRef
                }

collectorContainer :: [(Text, Text)] -> IORef [ByteString] -> PinnedImageRef -> TC.TestContainer Container
collectorContainer labels logsRef image =
    TC.run $
        TC.containerRequest (fromDockerfile (collectorDockerfile image))
            & TC.setEnv [("OTELCOL_CONFIG", collectorConfig)]
            & TC.setExpose [collectorPort]
            & TC.withFollowLogs (accumulateLogs logsRef)
            & TC.setWaitingFor (TC.waitUntilTimeout 120 (TC.waitUntilMappedPortReachable collectorPort))
            & TC.setRm True
            & withLabels labels

-- Accumulate each emitted collector log line into the shared buffer (newest first).
accumulateLogs :: IORef [ByteString] -> TC.LogConsumer
accumulateLogs logsRef _pipe line = atomicModifyIORef' logsRef (\acc -> (line : acc, ()))

{- Poll the collector's accumulated logs for the metric name, up to @attempts@ times at
~250ms each. 'True' once a log line carries the name. The @debug@ exporter prints each
received metric's name, so the name surfaces once the collector accepts the metric. -}
awaitMetric :: Collector -> Text -> Int -> IO Bool
awaitMetric collectorHandle metric = go
  where
    metricBytes :: ByteString
    metricBytes = encodeUtf8 metric

    go :: Int -> IO Bool
    go attemptsLeft
        | attemptsLeft <= 0 = pure False
        | otherwise = do
            logs <- readIORef (collectorLogs collectorHandle)
            if any (metricBytes `BS.isInfixOf`) logs
                then pure True
                else threadDelay 250_000 >> go (attemptsLeft - 1)
