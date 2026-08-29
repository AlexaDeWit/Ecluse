-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.TelemetryScrapeSpec (spec) where

import Data.List (lookup)
import Data.Text qualified as T
import Test.Hspec

import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (
    HttpException,
    defaultManagerSettings,
    httpLbs,
    newManager,
    responseBody,
    responseHeaders,
    responseStatus,
 )
import Network.HTTP.Types (hContentType, statusCode)
import Network.Wai.Handler.Warp qualified as Warp
import UnliftIO (try)

import Ecluse.Core.Telemetry.Metrics (
    Decision (Admit, Deny),
    MirrorResult (Published),
    ReasonClass (ReasonPolicy),
 )
import Ecluse.Integration.Collector (withSdkEnv)
import Ecluse.Runtime.Env (Env)
import Ecluse.Runtime.Server (mkServerConfig, tracedApplication)
import Ecluse.Runtime.Telemetry (Telemetry, TelemetrySwitch (TelemetryOff, TelemetryOn), withTelemetry)
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
import Ecluse.Test.Wai (freePort)

-- | One fetched response, reduced to the parts the assertions read.
data Fetched = Fetched
    { fStatus :: Int
    , fContentType :: Maybe Text
    , fBody :: Text
    }
    deriving stock (Eq, Show)

{- | What one telemetry-on session under the scrape transport served, from both listeners, so
every assertion below reads one run rather than standing the SDK up per example.
-}
data ScrapeRun = ScrapeRun
    { runExposition :: Fetched
    , runListenerOther :: Fetched
    , runProxyMetrics :: Fetched
    , runProxyUnknown :: Fetched
    }

{- | Scrape the dedicated Prometheus listener and assert what it carries, and that the proxy's own
port never answers @\/metrics@. Pull-based and in-process, so no backend takes part.
-}
spec :: Spec
spec = describe "Prometheus scrape listener" $ do
    beforeAll driveScrapeRun $ do
        it "renders the recorded ecluse.* series as Prometheus text exposition" $ \run -> do
            let served = runExposition run
            fStatus served `shouldBe` 200
            fContentType served `shouldBe` Just "text/plain; version=0.0.4; charset=utf-8"
            fBody served `shouldSatisfy` T.isInfixOf "# TYPE ecluse_serve_decision counter"
            fBody served `shouldSatisfy` T.isInfixOf "decision=\"admit\""
            fBody served `shouldSatisfy` T.isInfixOf "decision=\"deny\""
            fBody served `shouldSatisfy` T.isInfixOf "# TYPE ecluse_rule_denials counter"
            fBody served `shouldSatisfy` T.isInfixOf "rule=\"min-age\""
            fBody served `shouldSatisfy` T.isInfixOf "# TYPE ecluse_mirror_jobs_processed counter"
            fBody served `shouldSatisfy` T.isInfixOf "result=\"published\""

        it "labels every series from the bounded vocabulary alone" $ \run ->
            -- Bounded labels answer cardinality, not exposure: they are what stops the exposition
            -- growing a series per package. A label key follows '{' or ','.
            forM_ highCardinalityKeys $ \key -> do
                fBody (runExposition run) `shouldNotSatisfy` T.isInfixOf ("{" <> key <> "=\"")
                fBody (runExposition run) `shouldNotSatisfy` T.isInfixOf ("," <> key <> "=\"")

        it "serves one path only, so the listener answers nothing else" $ \run ->
            fStatus (runListenerOther run) `shouldBe` 404

        it "leaves the proxy port with no /metrics to find, even under the scrape transport" $ \run ->
            -- Identical triples, so a client on the data port cannot tell the path apart from any
            -- other unmounted one and learn that an exposition exists.
            runProxyMetrics run `shouldBe` runProxyUnknown run

    it "starts no listener while telemetry is off, whatever the transport selects" $ do
        reached <- scrapeReachableWith TelemetryOff
        reached `shouldBe` False

    it "starts one while telemetry is on, which is what makes that a real difference" $ do
        reached <- scrapeReachableWith TelemetryOn
        reached `shouldBe` True

{- Record a spread of @ecluse.*@ signals through a live telemetry handle, then read both the
dedicated listener and the proxy's own port inside that one session. -}
driveScrapeRun :: IO ScrapeRun
driveScrapeRun = do
    scrapePort <- freePort
    withScrapeEnv scrapePort $ do
        logEnv <- initLogEnv (Namespace ["itest"]) (Environment "test")
        withTelemetry TelemetryOn logEnv $ \telemetry -> do
            recordSpread telemetry
            env <- buildEnv telemetry
            app <- tracedApplication (mkServerConfig []) env
            Warp.testWithApplication (pure app) $ \proxyPort ->
                ScrapeRun
                    <$> fetch (urlOn scrapePort "/metrics")
                    <*> fetch (urlOn scrapePort "/not-the-metrics-path")
                    <*> fetch (urlOn proxyPort "/metrics")
                    <*> fetch (urlOn proxyPort "/not-a-mount")

{- Whether the scrape listener answers under the given switch, with the scrape transport selected
either way. That is the composed join: the selection alone must not open a port. -}
scrapeReachableWith :: TelemetrySwitch -> IO Bool
scrapeReachableWith switch = do
    scrapePort <- freePort
    withScrapeEnv scrapePort $ do
        logEnv <- initLogEnv (Namespace ["itest"]) (Environment "test")
        withTelemetry switch logEnv $ \_telemetry -> do
            attempt <- try (fetch (urlOn scrapePort "/metrics"))
            pure $ case attempt of
                Left (_ :: HttpException) -> False
                Right served -> fStatus served == 200

recordSpread :: Telemetry -> IO ()
recordSpread telemetry = do
    metrics <- newMetrics telemetry
    recordServeDecision metrics Admit
    recordServeDecision metrics Deny
    recordRuleDenial metrics (Just "min-age") ReasonPolicy
    recordMirrorJobProcessed metrics Published

{- Point the SDK at the scrape transport on a port this run owns. 'withSdkEnv' restores every key
it sets, and the OTLP endpoint stays unused because no push exporter is selected. -}
withScrapeEnv :: Int -> IO a -> IO a
withScrapeEnv scrapePort =
    withSdkEnv
        "http://127.0.0.1:1"
        [ ("OTEL_METRICS_EXPORTER", "prometheus")
        , ("OTEL_EXPORTER_PROMETHEUS_HOST", "127.0.0.1")
        , ("OTEL_EXPORTER_PROMETHEUS_PORT", show scrapePort)
        ]

-- Fetch one URL and reduce the response to what the assertions read.
fetch :: Text -> IO Fetched
fetch url = do
    manager <- newManager defaultManagerSettings
    request <- parseRequestOrFail url
    response <- httpLbs request manager
    pure
        Fetched
            { fStatus = statusCode (responseStatus response)
            , fContentType = decodeUtf8 <$> lookup hContentType (responseHeaders response)
            , fBody = decodeUtf8 (responseBody response)
            }

urlOn :: Int -> Text -> Text
urlOn port path = "http://127.0.0.1:" <> show port <> path

-- A minimal composition root for the front door. No mount is configured, so every path on the
-- proxy port falls through to the health probes.
buildEnv :: Telemetry -> IO Env
buildEnv telemetry = do
    manager <- newManager defaultManagerSettings
    queue <- newTestMemoryQueue
    newTestEnvWith queue (manager, manager) telemetry
