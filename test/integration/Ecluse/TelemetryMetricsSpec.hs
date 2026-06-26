module Ecluse.TelemetryMetricsSpec (spec) where

import Data.ByteString qualified as BS
import System.Environment (setEnv, unsetEnv)
import Test.Hspec
import UnliftIO (bracket)

import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import OpenTelemetry.Metric.Core (forceFlushMeterProvider)
import TestContainers (Container, containerAddress)
import TestContainers qualified as TC
import TestContainers.Docker (fromDockerfile)
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
import Ecluse.Telemetry (
    TelemetrySwitch (TelemetryOff, TelemetryOn),
    telemetryMeterProvider,
    withTelemetry,
 )
import Ecluse.Telemetry.Instruments (
    newMetrics,
    recordCacheRequest,
    recordMirrorJobProcessed,
    recordRuleDenial,
    recordServeDecision,
    recordUpstreamFetch,
 )

{- | The integration tier for metrics: drive @ecluse.*@ measurements through an
in-process telemetry handle into a real OTLP __Collector__ container (no Datadog SaaS)
and assert the series are accepted. The Collector runs an OTLP\/HTTP receiver into a
@debug@ exporter at detailed verbosity, so every received metric — its name and labels —
is written to its logs; the test records a spread of catalogue signals, force-flushes
the meter provider, then watches the Collector's logs for a known metric name.

Two cases prove the wire and its gate: with telemetry __on__ the metric reaches the
Collector (it was exported and accepted); with telemetry __off__ nothing is exported (no
SDK is initialised), so the name never appears. The metric /name/ is the marker, not a
label: a unique-per-run label would breach the bounded-label discipline this very slice
enforces, so the assertion keys on the catalogue name a fresh per-case container makes
unambiguous. Gating and Dockerised, the same tier as the tracing and mirror-queue tests;
it needs a Docker daemon and no external network beyond pulling the Collector image.
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

-- ── the metrics under export ─────────────────────────────────────────────────

{- Record a spread of @ecluse.*@ signals through an in-process telemetry handle pointed
at the collector, then force-flush the meter provider so the export does not wait on the
periodic reader's window. With telemetry off, 'newMetrics' builds against the no-op
meter and there is no provider to flush, so nothing is emitted. -}
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
environment — metrics exporter on (the collector carries a metrics pipeline), traces and
logs off so the SDK does not ship signals the collector has no pipeline for — and
__restore the prior environment on exit__. @setEnv@ is process-global and the integration
suite runs every spec in one process, so without this restore these values (e.g.
@OTEL_TRACES_EXPORTER=none@) would leak into a later spec; every key this sets is saved
and put back (or unset if it was absent). -}
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

-- ── the collector container ────────────────────────────────────────────────────

-- A running OTLP collector: the endpoint to export to, and its accumulated logs.
data Collector = Collector
    { collectorEndpoint :: Text
    , collectorLogs :: IORef [ByteString]
    }

-- The OTLP HTTP receiver port the collector serves on.
collectorPort :: TC.Port
collectorPort = 4318

-- A pinned OTLP Collector image. The core distribution carries the OTLP receiver and
-- the @debug@ exporter the assertion reads.
collectorImage :: Text
collectorImage = "otel/opentelemetry-collector:0.119.0"

{- A derived image that bakes the @--config env:OTELCOL_CONFIG@ command into the
collector. testcontainers 0.5.3 appends @setCmd@ to @docker start@ (which rejects it),
so the command is set in the image rather than at run time; the config itself still
arrives through the (correctly applied) @--env@ on @docker create@. -}
collectorDockerfile :: Text
collectorDockerfile =
    "FROM "
        <> collectorImage
        <> "\nCMD [\"--config\", \"env:OTELCOL_CONFIG\"]\n"

{- The whole collector configuration as a single-line (flow-style) YAML document,
passed through the @env:@ config provider so no shell, file, or bind mount is needed on
the distroless image: an OTLP\/HTTP receiver feeding a @debug@ exporter at detailed
verbosity through a __metrics__ pipeline, so every received metric is written to the
container logs. -}
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
    withContainers (collectorContainer logsRef) $ \container -> do
        let (host, mappedPort) = containerAddress container collectorPort
        action
            Collector
                { collectorEndpoint = "http://" <> host <> ":" <> show mappedPort
                , collectorLogs = logsRef
                }

collectorContainer :: IORef [ByteString] -> TC.TestContainer Container
collectorContainer logsRef =
    TC.run $
        TC.containerRequest (fromDockerfile collectorDockerfile)
            & TC.setEnv [("OTELCOL_CONFIG", collectorConfig)]
            & TC.setExpose [collectorPort]
            & TC.withFollowLogs (accumulateLogs logsRef)
            & TC.setWaitingFor (TC.waitUntilTimeout 120 (TC.waitUntilMappedPortReachable collectorPort))
            & TC.setRm True

-- Accumulate each emitted collector log line into the shared buffer (newest first).
accumulateLogs :: IORef [ByteString] -> TC.LogConsumer
accumulateLogs logsRef _pipe line = atomicModifyIORef' logsRef (\acc -> (line : acc, ()))

{- Poll the collector's accumulated logs for the metric name, up to @attempts@ times at
~250ms each. 'True' once a log line carries the name — the @debug@ exporter prints each
received metric's name, so it surfaces once the metric is accepted. -}
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
