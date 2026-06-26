module Ecluse.TelemetryTracingSpec (spec) where

import Data.ByteString qualified as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (setEnv)
import Test.Hspec

import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest)
import Network.Wai.Handler.Warp qualified as Warp
import OpenTelemetry.Trace (forceFlushTracerProvider)
import TestContainers (Container, containerAddress)
import TestContainers qualified as TC
import TestContainers.Docker (fromDockerfile)
import TestContainers.Hspec (withContainers)
import UnliftIO.Concurrent (threadDelay)

import Ecluse (npmServerConfig, unconfiguredCredentials, unconfiguredRegistry)
import Ecluse.Env (Env, newEnv, newWorkerHeartbeat)
import Ecluse.Queue (newInMemoryQueue)
import Ecluse.Server (tracedApplication)
import Ecluse.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Telemetry (
    Telemetry,
    TelemetrySwitch (TelemetryOff, TelemetryOn),
    telemetryTracerProvider,
    withTelemetry,
 )

{- | The integration tier for tracing: drive a request through an in-process Écluse
into a real OTLP __Collector__ container (no Datadog SaaS) and assert the spans are
accepted. The Collector runs an OTLP\/HTTP receiver into a @debug@ exporter at
detailed verbosity, so every received span is written to its logs; the test stamps a
unique marker into the request path (which the WAI server span records as an
attribute) and then watches the Collector's logs for that marker.

Two cases prove the wire and its gate: with telemetry __on__ the marker reaches the
Collector (the span was exported and accepted); with telemetry __off__ a fresh marker
never appears, so the instrumentation is genuinely inert. Gating and Dockerised, the
same tier as the mirror-queue tests; it needs a Docker daemon and no external network
beyond pulling the Collector image.
-}
spec :: Spec
spec =
    around withCollector $
        describe "tracing → OTLP collector" $ do
            it "delivers a request's server span to the collector when telemetry is on" $ \collector -> do
                marker <- freshMarker
                driveRequest collector TelemetryOn marker
                accepted <- awaitMarker collector marker 40
                accepted `shouldBe` True

            it "delivers nothing to the collector when telemetry is off" $ \collector -> do
                marker <- freshMarker
                driveRequest collector TelemetryOff marker
                accepted <- awaitMarker collector marker 8
                accepted `shouldBe` False

-- ── the request under trace ────────────────────────────────────────────────────

{- Drive one request through the in-process traced Écluse application, pointing the
SDK at the collector. With telemetry on, the WAI middleware opens a server span that
records the request path (carrying the unique marker) and the OTLP exporter ships it;
the tracer provider is force-flushed so the export does not wait on the batch window.
With telemetry off, 'tracedApplication' adds no middleware, so nothing is emitted. -}
driveRequest :: Collector -> TelemetrySwitch -> Text -> IO ()
driveRequest collector switch marker = do
    pointSdkAt (collectorEndpoint collector)
    withTelemetry switch $ \telemetry -> do
        env <- buildEnv telemetry
        app <- tracedApplication npmServerConfig env
        Warp.testWithApplication (pure app) $ \port -> do
            manager <- newManager defaultManagerSettings
            request <- parseRequest ("http://127.0.0.1:" <> show port <> "/" <> toString marker)
            _ <- httpLbs request manager
            pass
        whenJust (telemetryTracerProvider telemetry) $ \tracerProvider ->
            void (forceFlushTracerProvider tracerProvider Nothing)

-- Point the SDK's OTLP exporter at the collector via the standard environment, with
-- traces export ON and metrics and logs export off (the collector here carries only a
-- traces pipeline). Every signal's exporter is pinned explicitly — including
-- @OTEL_TRACES_EXPORTER@ — because @setEnv@ is process-global and the integration suite
-- runs every spec in one process: a sibling spec exporting a different signal (e.g. the
-- metrics spec, which sets @OTEL_TRACES_EXPORTER=none@) would otherwise leave traces
-- disabled here. Pinning all three makes this spec independent of run order.
pointSdkAt :: Text -> IO ()
pointSdkAt endpoint = do
    setEnv "OTEL_EXPORTER_OTLP_ENDPOINT" (toString endpoint)
    setEnv "OTEL_EXPORTER_OTLP_PROTOCOL" "http/protobuf"
    setEnv "OTEL_SERVICE_NAME" "ecluse-itest"
    setEnv "OTEL_TRACES_EXPORTER" "otlp"
    setEnv "OTEL_METRICS_EXPORTER" "none"
    setEnv "OTEL_LOGS_EXPORTER" "none"
    setEnv "OTEL_BSP_SCHEDULE_DELAY" "200"

{- A minimal composition root for the traced front door: the route under test
(@\/{marker}@) matches no mount and is the neutral @404@, so the registry, credential,
and cache handles are never exercised and the unconfigured placeholders suffice. The
telemetry handle is the one wired here. -}
buildEnv :: Telemetry -> IO Env
buildEnv telemetry = do
    manager <- newManager defaultManagerSettings
    privateManager <- newManager defaultManagerSettings
    queue <- newInMemoryQueue
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    heartbeat <- newWorkerHeartbeat
    newEnv unconfiguredRegistry queue unconfiguredCredentials manager privateManager metadataCache logEnv telemetry heartbeat

-- A fresh, unique, path-safe marker per case, so one case's spans never satisfy
-- another's assertion (in particular the off case's absence assertion).
freshMarker :: IO Text
freshMarker = do
    now <- getPOSIXTime
    pure ("ecltrace" <> show (round (now * 1_000_000) :: Integer))

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
passed through the @env:@ config provider so no shell, file, or bind mount is needed
on the distroless image: an OTLP\/HTTP receiver feeding a @debug@ exporter at detailed
verbosity, so every received span is written to the container logs. -}
collectorConfig :: Text
collectorConfig =
    "{receivers: {otlp: {protocols: {http: {endpoint: \"0.0.0.0:4318\"}}}}, "
        <> "exporters: {debug: {verbosity: detailed}}, "
        <> "service: {pipelines: {traces: {receivers: [otlp], exporters: [debug]}}}}"

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

{- Poll the collector's accumulated logs for the marker, up to @attempts@ times at
~250ms each. 'True' once a log line carries the marker — the @debug@ exporter prints
the server span's path attribute, so the marker surfaces once the span is accepted. -}
awaitMarker :: Collector -> Text -> Int -> IO Bool
awaitMarker collectorHandle marker = go
  where
    markerBytes :: ByteString
    markerBytes = encodeUtf8 marker

    go :: Int -> IO Bool
    go attemptsLeft
        | attemptsLeft <= 0 = pure False
        | otherwise = do
            logs <- readIORef (collectorLogs collectorHandle)
            if any (markerBytes `BS.isInfixOf`) logs
                then pure True
                else threadDelay 250_000 >> go (attemptsLeft - 1)
