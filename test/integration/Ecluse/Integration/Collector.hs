-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Collector lifecycle and SDK environment isolation for telemetry integration tests.
module Ecluse.Integration.Collector (
    -- * The container
    Collector (collectorEndpoint),
    withCollector,
    awaitCollectorLine,
    collectorOutput,

    -- * Pointing the SDK at it
    withSdkEnv,
) where

import Data.ByteString qualified as BS
import TestContainers (Container, containerAddress)
import TestContainers qualified as TC
import TestContainers.Docker (fromDockerfile, withLabels)
import TestContainers.Hspec (withContainers)

import Ecluse.Test.Collector (collectorConfig)
import Ecluse.Test.Container.Image (PinnedImageRef, collectorImage, renderPinnedImageRef)
import Ecluse.Test.Containers (testContainerLabels)
import Ecluse.Test.Env (withEnvOverrides)
import Ecluse.Test.Poll (pollUntil)

-- | A running OTLP Collector: the endpoint to export to, and its accumulated log lines.
data Collector = Collector
    { collectorEndpoint :: Text
    , collectorLogs :: IORef [ByteString]
    }

-- | Run with the named signal pipelines once the OTLP port accepts connections.
withCollector :: [Text] -> (Collector -> IO ()) -> IO ()
withCollector pipelines action = do
    logsRef <- newIORef []
    labels <- testContainerLabels "integration"
    image <- either (fail . toString) pure collectorImage
    withContainers (collectorContainer labels logsRef image pipelines) $ \container -> do
        let (host, mappedPort) = containerAddress container collectorPort
        action
            Collector
                { collectorEndpoint = "http://" <> host <> ":" <> show mappedPort
                , collectorLogs = logsRef
                }

-- | Poll detailed debug output up to @attempts@ times, with 250 ms pauses.
awaitCollectorLine :: Collector -> Text -> Int -> IO Bool
awaitCollectorLine collector needle attempts =
    pollUntil attempts 250_000 id (any (needleBytes `BS.isInfixOf`) <$> readIORef (collectorLogs collector))
  where
    needleBytes :: ByteString
    needleBytes = encodeUtf8 needle

-- | The collector's output so far, one line per log line, in the order it printed them.
collectorOutput :: Collector -> IO Text
collectorOutput collector = decodeUtf8 . BS.intercalate "\n" . map (BS.dropWhileEnd (`elem` [10, 13])) . reverse <$> readIORef (collectorLogs collector)

-- | Set the SDK endpoint and signal exporter settings, restoring the environment on exit.
withSdkEnv :: Text -> [(String, String)] -> IO a -> IO a
withSdkEnv endpoint signal = withEnvOverrides settings
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

accumulateLogs :: IORef [ByteString] -> TC.LogConsumer
accumulateLogs logsRef _pipe line = atomicModifyIORef' logsRef (\acc -> (line : acc, ()))
