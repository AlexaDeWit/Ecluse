-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.TelemetryTracingSpec (spec) where

import Data.Aeson (encode)
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (setEnv)
import Test.Hspec

import Katip (
    Environment (Environment),
    Item (..),
    Namespace (Namespace),
    Severity (InfoS),
    SimpleLogPayload,
    ThreadIdText (ThreadIdText),
    Verbosity (V2),
    initLogEnv,
    itemJson,
    logStr,
 )
import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest)
import Network.Wai.Handler.Warp qualified as Warp
import OpenTelemetry.Trace (
    defaultSpanArguments,
    forceFlushTracerProvider,
    inSpan',
    makeTracer,
    tracerOptions,
 )
import TestContainers (Container, containerAddress)
import TestContainers qualified as TC
import TestContainers.Docker (fromDockerfile, withLabels)
import TestContainers.Hspec (withContainers)
import UnliftIO.Concurrent (threadDelay)

import Ecluse (mountBindingFor)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Runtime.Env (Env)
import Ecluse.Runtime.Log (
    DdContext (..),
    DdSpan (DdSpan),
    ddField,
 )
import Ecluse.Runtime.Server (ServerConfig, mkServerConfig, tracedApplication)
import Ecluse.Runtime.Telemetry (
    Telemetry,
    TelemetrySwitch (TelemetryOff, TelemetryOn),
    telemetryTracerProvider,
    withTelemetry,
 )
import Ecluse.Runtime.Telemetry.Correlation (ddContextNow, ddIdentity)
import Ecluse.Runtime.Telemetry.Resolve (resolveTelemetry)
import Ecluse.Runtime.Test.Support (newTestEnvWith)
import Ecluse.Test.Container.Image (PinnedImageRef, mkPinnedImageRef, renderPinnedImageRef)
import Ecluse.Test.Containers (testContainerLabels)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Server.Mount (inertPackumentDeps)

{- | The integration tier for tracing. Drive a request through an in-process Écluse into
a real OTLP __Collector__ container (no Datadog SaaS), then assert the Collector accepts
the spans. The Collector runs an OTLP\/HTTP receiver into a @debug@ exporter at
detailed verbosity, so it writes every received span to its logs. The test stamps a
unique marker into the request path, which the WAI server span records as an attribute.
It then watches the Collector's logs for that marker.

Two cases prove the wire and its gate. With telemetry __on__ the marker reaches the
Collector, so the SDK exported the span and the Collector accepted it. With telemetry
__off__ a fresh marker never appears, so the instrumentation is genuinely inert. Gating
and Dockerised, the same tier as the mirror-queue tests. It needs a Docker daemon and no
external network beyond pulling the Collector image.
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

            -- The AC4 stitch end to end on a real span. With the SDK live, this opens
            -- a span, as the WAI middleware does per request. It builds the dd context
            -- within it exactly as runHandler does, and renders a JSONL log line from
            -- it. The line must carry a non-zero dd.trace_id / dd.span_id, which proves
            -- the active-span -> low-64 -> log-line correlation, the "verify against the
            -- Agent" crux. (Ecluse.LogSpec pins the id format itself.)
            it "stamps a non-zero dd.trace_id on a log line emitted within a span" $ \collector -> do
                ctx <- ddContextWithinSpan collector
                case ddSpan ctx of
                    Nothing -> expectationFailure "no active span was seen inside the span scope"
                    Just (DdSpan tid sid) -> do
                        tid `shouldSatisfy` isNonZeroDecimal
                        sid `shouldSatisfy` isNonZeroDecimal
                        -- And the id lands in the serialised JSONL payload under dd.trace_id.
                        decodeUtf8 (encode (itemJson V2 (ddLogItem ctx)))
                            `shouldSatisfy` T.isInfixOf ("\"trace_id\":\"" <> tid <> "\"")

{- Drive one request through the in-process traced Écluse application, pointing the SDK
at the collector. With telemetry on, the WAI middleware opens a server span that records
the request path, carrying the unique marker, and the OTLP exporter ships it. This
force-flushes the tracer provider, so the export does not wait on the batch window. With
telemetry off, 'tracedApplication' adds no middleware, so the application emits
nothing. -}
driveRequest :: Collector -> TelemetrySwitch -> Text -> IO ()
driveRequest collector switch marker = do
    pointSdkAt (collectorEndpoint collector)
    logEnv <- initLogEnv (Namespace ["itest"]) (Environment "test")
    withTelemetry switch logEnv $ \telemetry -> do
        env <- buildEnv telemetry
        app <- tracedApplication npmTestConfig env
        Warp.testWithApplication (pure app) $ \port -> do
            manager <- newManager defaultManagerSettings
            request <- parseRequest ("http://127.0.0.1:" <> show port <> "/" <> toString marker)
            _ <- httpLbs request manager
            pass
        whenJust (telemetryTracerProvider telemetry) $ \tracerProvider ->
            void (forceFlushTracerProvider tracerProvider Nothing)

-- Point the SDK's OTLP exporter at the collector via the standard environment. Traces
-- export is ON, and metrics and logs export off, because the collector here carries
-- only a traces pipeline. This pins every signal's exporter explicitly,
-- @OTEL_TRACES_EXPORTER@ included, because @setEnv@ is process-global and the
-- integration suite runs every spec in one process. A sibling spec exporting a
-- different signal (e.g. the metrics spec, which sets @OTEL_TRACES_EXPORTER=none@)
-- would otherwise leave traces disabled here. Pinning all three makes this spec
-- independent of run order.
pointSdkAt :: Text -> IO ()
pointSdkAt endpoint = do
    setEnv "OTEL_EXPORTER_OTLP_ENDPOINT" (toString endpoint)
    setEnv "OTEL_EXPORTER_OTLP_PROTOCOL" "http/protobuf"
    setEnv "OTEL_SERVICE_NAME" "ecluse-itest"
    setEnv "OTEL_TRACES_EXPORTER" "otlp"
    setEnv "OTEL_METRICS_EXPORTER" "none"
    setEnv "OTEL_LOGS_EXPORTER" "none"
    setEnv "OTEL_BSP_SCHEDULE_DELAY" "200"

{- The npm front door the traced application mounts: a bare npm mount, with no serve or
publish dependencies, assembled through the public binding resolver. It is the same
shape the composition root derives from configuration. -}
npmTestConfig :: ServerConfig
npmTestConfig = mkServerConfig (maybeToList (mountBindingFor Npm inertPackumentDeps Nothing))

{- A minimal composition root for the traced front door. The route under test
(@\/{marker}@) matches no mount and is the neutral @404@. Nothing exercises the
registry, credential, and cache handles, so the unconfigured placeholders suffice. The
telemetry handle is the one wired here. -}
buildEnv :: Telemetry -> IO Env
buildEnv telemetry = do
    manager <- newManager defaultManagerSettings
    privateManager <- newManager defaultManagerSettings
    queue <- newTestMemoryQueue
    newTestEnvWith queue (manager, privateManager) telemetry

-- A fresh, unique, path-safe marker per case, so one case's spans never satisfy
-- another's assertion (in particular the off case's absence assertion).
freshMarker :: IO Text
freshMarker = do
    now <- getPOSIXTime
    pure ("ecltrace" <> show (round (now * 1_000_000) :: Integer))

{- Open a real SDK span, as the WAI middleware does per request. Build the @dd@ context
within it through the same "Ecluse.Telemetry.Correlation" path 'runHandler' uses, and
return that context. The collector backs the SDK's exporter, because init needs a valid
endpoint and export is async. This asserts the in-process active-span stitch, never
delivery. -}
ddContextWithinSpan :: Collector -> IO DdContext
ddContextWithinSpan collector = do
    pointSdkAt (collectorEndpoint collector)
    logEnv <- initLogEnv (Namespace ["itest"]) (Environment "test")
    withTelemetry TelemetryOn logEnv $ \telemetry ->
        case telemetryTracerProvider telemetry of
            Nothing -> fail "telemetry on must provide a tracer provider"
            Just tracerProvider -> do
                let tracer = makeTracer tracerProvider "ecluse" tracerOptions
                inSpan' tracer "itest-correlation-span" defaultSpanArguments $ \_span ->
                    ddContextNow (ddIdentity (resolveTelemetry []))

-- A rendered Datadog id is an unsigned decimal. A real span's id is non-empty and
-- non-zero (the low-64 render of a random id is overwhelmingly non-zero).
isNonZeroDecimal :: Text -> Bool
isNonZeroDecimal t = not (T.null t) && T.all isDigit t && t /= "0"

{- A katip log 'Item' carrying the @dd@ object as its structured payload. A test can
then render a JSONL line off a 'DdContext' with no stdout dependency. The unit tier uses
the same technique. This fixture holds every non-payload field fixed. -}
ddLogItem :: DdContext -> Item SimpleLogPayload
ddLogItem ctx =
    Item
        { _itemApp = Namespace ["ecluse"]
        , _itemEnv = Environment "test"
        , _itemSeverity = InfoS
        , _itemThread = ThreadIdText "ThreadId 1"
        , _itemHost = "itest-host"
        , _itemProcess = 1
        , _itemPayload = ddField ctx
        , _itemMessage = logStr ("served" :: Text)
        , _itemTime = fixedTime
        , _itemNamespace = Namespace ["serve"]
        , _itemLoc = Nothing
        }

-- A fixed instant so the rendered line is deterministic across runs.
fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 6 26) 0

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
verbosity. The collector therefore writes every received span to the container logs. -}
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

{- Poll the collector's accumulated logs for the marker, up to @attempts@ times at
~250ms each. 'True' once a log line carries the marker. The @debug@ exporter prints the
server span's path attribute, so the marker surfaces once the Collector accepts the
span. -}
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
