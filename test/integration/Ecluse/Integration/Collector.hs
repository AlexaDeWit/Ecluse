-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The OTLP Collector harness the telemetry integration specs share.

The tracing and metrics specs each export one signal into a real Collector container and
watch its @debug@ exporter output. They differ only in which pipeline the Collector runs
and which @OTEL_*@ exporter the SDK enables, so both are parameters here. 'withSdkEnv'
restores the process environment on exit: @setEnv@ is process-global and the suite runs
every spec in one process, so an unrestored exporter would steer a later spec.
-}
module Ecluse.Integration.Collector (
    -- * The container
    Collector (collectorEndpoint),
    withCollector,
    awaitCollectorLine,

    -- * Pointing the SDK at it
    withSdkEnv,
) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import System.Environment (setEnv, unsetEnv)
import TestContainers (Container, containerAddress)
import TestContainers qualified as TC
import TestContainers.Docker (fromDockerfile, withLabels)
import TestContainers.Hspec (withContainers)
import UnliftIO (bracket)

import Ecluse.Test.Container.Image (PinnedImageRef, collectorImage, renderPinnedImageRef)
import Ecluse.Test.Containers (testContainerLabels)
import Ecluse.Test.Poll (pollUntil)

-- | A running OTLP Collector: the endpoint to export to, and its accumulated log lines.
data Collector = Collector
    { collectorEndpoint :: Text
    , collectorLogs :: IORef [ByteString]
    }

{- | Start an OTLP Collector container running the named signal pipelines (@traces@, @metrics@)
and follow its logs into a buffer. The body runs once the OTLP port accepts connections.
-}
withCollector :: [Text] -> (Collector -> IO ()) -> IO ()
withCollector pipelines action = do
    logsRef <- newIORef []
    labels <- testContainerLabels "integration"
    -- The harness's IO idiom: an unpinned literal aborts the suite before it pulls.
    image <- either (fail . toString) pure collectorImage
    withContainers (collectorContainer labels logsRef image pipelines) $ \container -> do
        let (host, mappedPort) = containerAddress container collectorPort
        action
            Collector
                { collectorEndpoint = "http://" <> host <> ":" <> show mappedPort
                , collectorLogs = logsRef
                }

{- | Poll the Collector's accumulated logs for @needle@, up to @attempts@ times at ~250ms.
The @debug@ exporter at detailed verbosity prints a span's attributes and a metric's name.
-}
awaitCollectorLine :: Collector -> Text -> Int -> IO Bool
awaitCollectorLine collector needle attempts =
    pollUntil attempts 250_000 id (any (needleBytes `BS.isInfixOf`) <$> readIORef (collectorLogs collector))
  where
    needleBytes :: ByteString
    needleBytes = encodeUtf8 needle

{- | Point the SDK at the Collector through the standard @OTEL_*@ environment for the action,
then put every key it touched back. @signal@ names the exporter under test and its schedule knob.
-}
withSdkEnv :: Text -> [(String, String)] -> IO a -> IO a
withSdkEnv endpoint signal act = bracket saveKeys restoreKeys (const (apply >> act))
  where
    -- Every exporter is pinned, not only the one under test, so an ambient value cannot
    -- enable a signal a spec asserts is silent.
    settings :: [(String, String)]
    settings =
        [ ("OTEL_EXPORTER_OTLP_ENDPOINT", toString endpoint)
        , ("OTEL_EXPORTER_OTLP_PROTOCOL", "http/protobuf")
        , ("OTEL_SERVICE_NAME", "ecluse-itest")
        , ("OTEL_TRACES_EXPORTER", "none")
        , ("OTEL_METRICS_EXPORTER", "none")
        , ("OTEL_LOGS_EXPORTER", "none")
        ]
            <> signal

    saveKeys :: IO [(String, Maybe String)]
    saveKeys = traverse (\k -> (k,) <$> lookupEnv k) (ordNub (map fst settings))

    restoreKeys :: [(String, Maybe String)] -> IO ()
    restoreKeys = traverse_ (\(k, mv) -> maybe (unsetEnv k) (setEnv k) mv)

    apply :: IO ()
    apply = traverse_ (uncurry setEnv) settings

-- The OTLP HTTP receiver port the Collector serves on.
collectorPort :: TC.Port
collectorPort = 4318

collectorContainer :: [(Text, Text)] -> IORef [ByteString] -> PinnedImageRef -> [Text] -> TC.TestContainer Container
collectorContainer labels logsRef image pipelines =
    TC.run $
        TC.containerRequest (fromDockerfile (collectorDockerfile image))
            & TC.setEnv [("OTELCOL_CONFIG", collectorConfig pipelines)]
            & TC.setExpose [collectorPort]
            & TC.withFollowLogs (accumulateLogs logsRef)
            & TC.setWaitingFor (TC.waitUntilTimeout 120 (TC.waitUntilMappedPortReachable collectorPort))
            & TC.setRm True
            & withLabels labels

{- Bakes the @--config env:OTELCOL_CONFIG@ command into the image. Version 0.5.3 of
testcontainers appends @setCmd@ to @docker start@, which rejects it. -}
collectorDockerfile :: PinnedImageRef -> Text
collectorDockerfile image =
    "FROM "
        <> renderPinnedImageRef image
        <> "\nCMD [\"--config\", \"env:OTELCOL_CONFIG\"]\n"
        <> "LABEL com.ecluse.test=integration\n"

{- The whole Collector configuration as one flow-style YAML document. It arrives through
the @env:@ config provider, so the distroless image needs no shell, file, or bind mount. -}
collectorConfig :: [Text] -> Text
collectorConfig pipelines =
    "{receivers: {otlp: {protocols: {http: {endpoint: \"0.0.0.0:4318\"}}}}, "
        <> "exporters: {debug: {verbosity: detailed}}, "
        <> "service: {pipelines: {"
        <> T.intercalate ", " (map debugPipeline pipelines)
        <> "}}}"
  where
    debugPipeline name = name <> ": {receivers: [otlp], exporters: [debug]}"

-- Accumulate each emitted Collector log line into the shared buffer (newest first).
accumulateLogs :: IORef [ByteString] -> TC.LogConsumer
accumulateLogs logsRef _pipe line = atomicModifyIORef' logsRef (\acc -> (line : acc, ()))
