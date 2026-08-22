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

{- | Tracing against a real OTLP Collector container. A request stamps a unique marker
into its path, and the test watches the Collector's @debug@ exporter logs for it:
present with telemetry on, absent with telemetry off. Needs a Docker daemon.
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

            -- Proves the active-span to log-line correlation: a log item built inside a real span
            -- carries a non-zero dd.trace_id and dd.span_id. Ecluse.LogSpec pins the id format.
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

{- Drive one request through the traced application against the collector. This
force-flushes the tracer provider, so the export does not wait on the batch window. -}
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

-- Point the SDK's exporter at the collector. Every signal's exporter is pinned here
-- because @setEnv@ is process-global, so a sibling spec cannot disable traces for this one.
pointSdkAt :: Text -> IO ()
pointSdkAt endpoint = do
    setEnv "OTEL_EXPORTER_OTLP_ENDPOINT" (toString endpoint)
    setEnv "OTEL_EXPORTER_OTLP_PROTOCOL" "http/protobuf"
    setEnv "OTEL_SERVICE_NAME" "ecluse-itest"
    setEnv "OTEL_TRACES_EXPORTER" "otlp"
    setEnv "OTEL_METRICS_EXPORTER" "none"
    setEnv "OTEL_LOGS_EXPORTER" "none"
    setEnv "OTEL_BSP_SCHEDULE_DELAY" "200"

{- A bare npm mount with no serve or publish dependencies, the same shape the
composition root derives from configuration. -}
npmTestConfig :: ServerConfig
npmTestConfig = mkServerConfig (maybeToList (mountBindingFor Npm inertPackumentDeps Nothing))

{- A minimal composition root for the traced front door. The route under test matches no
mount and is the neutral @404@, so placeholder registry, credential, and cache handles
suffice. -}
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

{- Build the @dd@ context inside a real SDK span, the way 'runHandler' does. This
asserts the in-process active-span stitch, never delivery. -}
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

{- A katip log 'Item' carrying the @dd@ object as its payload, so a test renders a JSONL
line off a 'DdContext' with no stdout dependency. -}
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
-- 'withCollector' rejects a non-digest ref at startup, so a re-pointed mutable tag
-- carrying a poisoned image never reaches the @FROM@ line.
collectorImage :: Text
collectorImage = "otel/opentelemetry-collector@sha256:3805724e26351df55a45032a793c9b64a2117ac9a58f13f070674a9723fab373"

{- Bakes the @--config env:OTELCOL_CONFIG@ command into the image. Version 0.5.3 of
testcontainers appends @setCmd@ to @docker start@, which rejects it. -}
collectorDockerfile :: PinnedImageRef -> Text
collectorDockerfile image =
    "FROM "
        <> renderPinnedImageRef image
        <> "\nCMD [\"--config\", \"env:OTELCOL_CONFIG\"]\n"
        <> "LABEL com.ecluse.test=integration\n"

{- The collector configuration as one flow-style YAML document. It arrives through the
@env:@ config provider, so the distroless image needs no shell, file, or bind mount. -}
collectorConfig :: Text
collectorConfig =
    "{receivers: {otlp: {protocols: {http: {endpoint: \"0.0.0.0:4318\"}}}}, "
        <> "exporters: {debug: {verbosity: detailed}}, "
        <> "service: {pipelines: {traces: {receivers: [otlp], exporters: [debug]}}}}"

{- | Start an OTLP Collector container with the inline config, follow its logs into a
buffer the test inspects, and tear it down after.
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

{- Poll the collector's logs for the marker, up to @attempts@ times at ~250ms each. The
@debug@ exporter prints the server span's path attribute, so the marker surfaces once
the Collector accepts the span. -}
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
