-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.TelemetryScrapeSpec (spec) where

import Data.List (lookup)
import Data.Text qualified as T
import Test.Hspec

import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (
    defaultManagerSettings,
    httpLbs,
    newManager,
    responseBody,
    responseHeaders,
    responseStatus,
 )
import Network.HTTP.Types (hContentType, statusCode)
import Network.Wai.Handler.Warp qualified as Warp

import Ecluse.Core.Telemetry.Metrics (
    Decision (Admit, Deny),
    MirrorResult (Published),
    ReasonClass (ReasonPolicy),
 )
import Ecluse.Integration.Collector (withSdkEnv)
import Ecluse.Runtime.Env (Env)
import Ecluse.Runtime.Server (mkServerConfig, tracedApplication)
import Ecluse.Runtime.Telemetry (Telemetry, TelemetrySwitch (TelemetryOn), withTelemetry)
import Ecluse.Runtime.Telemetry.Instruments (
    newMetrics,
    recordMirrorJobProcessed,
    recordRuleDenial,
    recordServeDecision,
 )
import Ecluse.Runtime.Test.Support (newTestEnvWith)
import Ecluse.Test.Metrics (highCardinalityKeys)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Support (parseRequestOrFail)

{- | Scrape the front door's @\/metrics@ route over a real listener and assert what the
exposition carries. The transport is pull-based and in-process, so no backend takes part.
-}
spec :: Spec
spec = describe "Prometheus /metrics scrape" $ do
    beforeAll (scrape "prometheus") $ do
        it "renders the recorded ecluse.* series as Prometheus text exposition" $ \(status, contentType, body) -> do
            status `shouldBe` 200
            contentType `shouldBe` Just "text/plain; version=0.0.4; charset=utf-8"
            body `shouldSatisfy` T.isInfixOf "# TYPE ecluse_serve_decision counter"
            body `shouldSatisfy` T.isInfixOf "decision=\"admit\""
            body `shouldSatisfy` T.isInfixOf "decision=\"deny\""
            body `shouldSatisfy` T.isInfixOf "# TYPE ecluse_rule_denials counter"
            body `shouldSatisfy` T.isInfixOf "rule=\"min-age\""
            body `shouldSatisfy` T.isInfixOf "# TYPE ecluse_mirror_jobs_processed counter"
            body `shouldSatisfy` T.isInfixOf "result=\"published\""

        it "labels every series from the bounded vocabulary alone" $ \(_, _, body) ->
            -- The catalogue keeps package, version, scope, and a denial message off labels, so a
            -- scrape cannot explode into a series per package. A label key follows '{' or ','.
            forM_ highCardinalityKeys $ \key -> do
                body `shouldNotSatisfy` T.isInfixOf ("{" <> key <> "=\"")
                body `shouldNotSatisfy` T.isInfixOf ("," <> key <> "=\"")

    it "mounts no route while the metrics transport is OTLP push" $ do
        (status, _, _) <- scrape "otlp"
        status `shouldBe` 404

{- Record a spread of @ecluse.*@ signals through a live telemetry handle, then scrape the front
door over its own listener. @exporter@ is the @OTEL_METRICS_EXPORTER@ value under test. -}
scrape :: String -> IO (Int, Maybe Text, Text)
scrape exporter =
    withSdkEnv unusedEndpoint [("OTEL_METRICS_EXPORTER", exporter)] $ do
        logEnv <- initLogEnv (Namespace ["itest"]) (Environment "test")
        withTelemetry TelemetryOn logEnv $ \telemetry -> do
            metrics <- newMetrics telemetry
            recordServeDecision metrics Admit
            recordServeDecision metrics Deny
            recordRuleDenial metrics (Just "min-age") ReasonPolicy
            recordMirrorJobProcessed metrics Published
            env <- buildEnv telemetry
            app <- tracedApplication (mkServerConfig []) env
            Warp.testWithApplication (pure app) (getMetrics . scrapeUrl)

-- Fetch one scrape and project the parts the assertions read.
getMetrics :: Text -> IO (Int, Maybe Text, Text)
getMetrics url = do
    manager <- newManager defaultManagerSettings
    request <- parseRequestOrFail url
    response <- httpLbs request manager
    pure
        ( statusCode (responseStatus response)
        , decodeUtf8 <$> lookup hContentType (responseHeaders response)
        , decodeUtf8 (responseBody response)
        )

scrapeUrl :: Warp.Port -> Text
scrapeUrl port = "http://127.0.0.1:" <> show port <> "/metrics"

{- 'withSdkEnv' pins every exporter, and neither transport under test dials the OTLP endpoint
from this suite, so the value only has to parse. -}
unusedEndpoint :: Text
unusedEndpoint = "http://127.0.0.1:1"

-- A minimal composition root for the front door. The scrape route sits above dispatch, so no
-- mount, registry, or cache handle is ever reached.
buildEnv :: Telemetry -> IO Env
buildEnv telemetry = do
    manager <- newManager defaultManagerSettings
    queue <- newTestMemoryQueue
    newTestEnvWith queue (manager, manager) telemetry
