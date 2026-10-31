-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Telemetry.TracingIntegrationSpec (spec) where

import Data.Char (isDigit)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Test.Hspec

import Katip (Environment (Environment), LogEnv, Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest)
import Network.Wai.Handler.Warp qualified as Warp
import OpenTelemetry.Trace (forceFlushTracerProvider)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Integration.Collector (
    Collector (collectorEndpoint),
    awaitCollectorLine,
    collectorOutput,
    withCollector,
    withSdkEnv,
 )
import Ecluse.Runtime.Env (Env)
import Ecluse.Runtime.Server (ServerConfig, mkServerConfig, tracedApplication)
import Ecluse.Runtime.Telemetry (
    Telemetry,
    TelemetrySwitch (TelemetryOff, TelemetryOn),
    telemetryTracerProvider,
    withTelemetry,
 )
import Ecluse.Runtime.Test.Support (newTestEnvLogging)
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Collector (datadogIds, loggedDatadogIds, printedSpans)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (captureJsonLog)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Server.Mount (inertPackumentDeps)

-- | Observe request markers in the OTLP Collector logs with telemetry enabled and disabled.
spec :: Spec
spec =
    around (withCollector ["traces"]) $
        describe "tracing → OTLP collector" $ do
            it (caseLabel "integration/telemetry-traces/native-001" "delivers a request's server span to the collector when telemetry is on") $ \collector -> do
                marker <- freshMarker
                driveRequest collector TelemetryOn marker
                accepted <- awaitCollectorLine collector marker 40
                accepted `shouldBe` True

            it (caseLabel "integration/telemetry-traces/native-002" "delivers nothing to the collector when telemetry is off") $ \collector -> do
                marker <- freshMarker
                driveRequest collector TelemetryOff marker
                accepted <- awaitCollectorLine collector marker 8
                accepted `shouldBe` False

            -- The serve path stamps each line with the active span, so a line's ids name the
            -- server span the collector received. The propagator owns the id format.
            it (caseLabel "integration/telemetry-traces/native-003" "stamps a non-zero dd.trace_id on a log line emitted within a span") $ \collector -> do
                marker <- freshMarker
                (_, output) <- captureJsonLog $ \logEnv -> driveRequestLogging logEnv collector TelemetryOn ("/npm/" <> marker)
                let logged = loggedSpans output
                logged `shouldSatisfy` (not . null)
                for_ logged $ \(loggedTrace, loggedSpan) -> do
                    loggedTrace `shouldSatisfy` isNonZeroDecimal
                    loggedSpan `shouldSatisfy` isNonZeroDecimal
                exported <- pollUntil 40 250_000 (any (`elem` logged)) (mapMaybe datadogIds . printedSpans <$> collectorOutput collector)
                exported `shouldSatisfy` any (`elem` logged)

-- Drive one request through the traced application against the collector. This
-- force-flushes the tracer provider, so the export does not wait on the batch window.
driveRequest :: Collector -> TelemetrySwitch -> Text -> IO ()
driveRequest collector switch marker = do
    logEnv <- initLogEnv (Namespace ["itest"]) (Environment "test")
    driveRequestLogging logEnv collector switch ("/" <> marker)

-- 'driveRequest' at @path@, with the front door logging to @logEnv@.
driveRequestLogging :: LogEnv -> Collector -> TelemetrySwitch -> Text -> IO ()
driveRequestLogging logEnv collector switch path =
    withSdkEnv (collectorEndpoint collector) tracesExporter $
        withTelemetry switch logEnv $ \telemetry -> withSweeper $ \deadlines -> do
            env <- buildEnv logEnv deadlines telemetry
            app <- tracedApplication npmTestConfig env
            Warp.testWithApplication (pure app) $ \port -> do
                manager <- newManager defaultManagerSettings
                request <- parseRequest ("http://127.0.0.1:" <> show port <> toString path)
                _ <- httpLbs request manager
                pass
            whenJust (telemetryTracerProvider telemetry) $ \tracerProvider ->
                void (forceFlushTracerProvider tracerProvider Nothing)

-- The signal this spec exports, over the baseline that silences every other one.
tracesExporter :: [(String, String)]
tracesExporter = [("OTEL_TRACES_EXPORTER", "otlp"), ("OTEL_BSP_SCHEDULE_DELAY", "200")]

-- A bare npm mount with no serve or publish dependencies, the same shape the
-- composition root derives from configuration.
npmTestConfig :: ServerConfig
npmTestConfig = mkServerConfig (maybeToList (mountBindingFor Npm inertPackumentDeps Nothing))

-- A minimal composition root for the traced front door. The mount's origins are closed ports,
-- so placeholder registry, credential, and cache handles suffice.
buildEnv :: LogEnv -> Deadlines -> Telemetry -> IO Env
buildEnv logEnv deadlines telemetry = do
    manager <- newManager defaultManagerSettings
    privateManager <- newManager defaultManagerSettings
    queue <- newTestMemoryQueue
    newTestEnvLogging deadlines logEnv queue (manager, privateManager) telemetry

-- A fresh, unique, path-safe marker per case, so one case's spans never satisfy
-- another's assertion (in particular the off case's absence assertion).
freshMarker :: IO Text
freshMarker = do
    now <- getPOSIXTime
    pure ("ecltrace" <> show (round (now * 1_000_000) :: Integer))

-- Katip nests the @dd@ object under @data@.
loggedSpans :: Text -> [(Text, Text)]
loggedSpans = mapMaybe (loggedDatadogIds ["data"]) . lines

-- A rendered Datadog id is an unsigned decimal. A real span's id is non-empty and
-- non-zero (the low-64 render of a random id is overwhelmingly non-zero).
isNonZeroDecimal :: Text -> Bool
isNonZeroDecimal t = not (T.null t) && T.all isDigit t && t /= "0"
