-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Telemetry.ScrapeSpec (spec) where

import Prelude hiding (get)

import Data.Vector qualified as V
import System.Environment (setEnv, unsetEnv)
import Test.Hspec
import Test.Hspec.Wai
import UnliftIO (bracket)

import OpenTelemetry.Metric (createMeterProvider, defaultSdkMeterProviderOptions)
import OpenTelemetry.Resource (emptyMaterializedResources)

import Ecluse.Runtime.Telemetry.Scrape (
    MetricScrape (MetricScrape, runMetricScrape),
    ScrapeListener (ScrapeListener),
    metricScrapeFor,
    scrapeApplication,
    scrapeListenerFrom,
    scrapeListenerWarnings,
    scrapeSelected,
 )

{- | The scrape transport's selection, where its listener binds, and what that listener serves.
Driving it through a live meter belongs to the integration tier.
-}
spec :: Spec
spec = do
    selectionSpec
    listenerSpec
    handleSpec
    applicationSpec

selectionSpec :: Spec
selectionSpec = describe "scrapeSelected" $ do
    it "selects the scrape transport on the OpenTelemetry name for it" $
        withExporter (Just "prometheus") scrapeSelected `shouldReturn` True

    it "ignores case and surrounding space, as the SDK's own parse does" $
        withExporter (Just "  Prometheus  ") scrapeSelected `shouldReturn` True

    it "reads the first entry of a comma list, as the SDK's own parse does" $
        withExporter (Just "prometheus,otlp") scrapeSelected `shouldReturn` True

    it "leaves every push transport unselected" $ do
        withExporter (Just "otlp") scrapeSelected `shouldReturn` False
        withExporter (Just "console") scrapeSelected `shouldReturn` False
        withExporter (Just "none") scrapeSelected `shouldReturn` False

    it "leaves an unset or blank variable unselected, so OTLP push stays the default" $ do
        withExporter Nothing scrapeSelected `shouldReturn` False
        withExporter (Just "") scrapeSelected `shouldReturn` False

listenerSpec :: Spec
listenerSpec = describe "scrapeListenerFrom" $ do
    it "reaches the loopback alone on 9464 when neither variable is declared" $
        -- The exposition names the host and the process, so the default must not be routable.
        scrapeListenerFrom [] `shouldBe` ScrapeListener "localhost" 9464

    it "takes the host and port the operator declared" $
        scrapeListenerFrom
            [ ("OTEL_EXPORTER_PROMETHEUS_HOST", "0.0.0.0")
            , ("OTEL_EXPORTER_PROMETHEUS_PORT", "19464")
            ]
            `shouldBe` ScrapeListener "0.0.0.0" 19464

    it "counts a blank host as unset, so an empty variable cannot widen the bind" $
        scrapeListenerFrom [("OTEL_EXPORTER_PROMETHEUS_HOST", "   ")]
            `shouldBe` ScrapeListener "localhost" 9464

    it "keeps the default port when the declared one is not a number" $
        scrapeListenerFrom [("OTEL_EXPORTER_PROMETHEUS_PORT", "nine-thousand")]
            `shouldBe` ScrapeListener "localhost" 9464

    it "names an unusable port rather than defaulting in silence" $
        -- An operator who typo'd the port would otherwise scrape 9464 and never learn why.
        scrapeListenerWarnings [("OTEL_EXPORTER_PROMETHEUS_PORT", "nine-thousand")]
            `shouldBe` ["OTEL_EXPORTER_PROMETHEUS_PORT is not a port number (nine-thousand). Serving the scrape exposition on 9464 instead."]

    it "raises nothing when the port is absent, blank, or usable" $ do
        scrapeListenerWarnings [] `shouldBe` []
        scrapeListenerWarnings [("OTEL_EXPORTER_PROMETHEUS_PORT", "  ")] `shouldBe` []
        scrapeListenerWarnings [("OTEL_EXPORTER_PROMETHEUS_PORT", "19464")] `shouldBe` []

handleSpec :: Spec
handleSpec = describe "metricScrapeFor" $ do
    it "builds no handle while the transport is OTLP push" $ do
        (_, meterEnv) <- createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions
        scrape <- withExporter (Just "otlp") (metricScrapeFor meterEnv)
        isNothing scrape `shouldBe` True

    it "collects the meter's snapshot once the scrape transport is selected" $ do
        (_, meterEnv) <- createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions
        scrape <- withExporter (Just "prometheus") (metricScrapeFor meterEnv)
        case scrape of
            Nothing -> expectationFailure "expected a scrape handle under the prometheus selection"
            Just handle -> do
                batches <- runMetricScrape handle
                V.length batches `shouldBe` 1

applicationSpec :: Spec
applicationSpec =
    describe "scrapeApplication" $
        with (pure (scrapeApplication emptyScrape)) $ do
            it "answers /metrics as Prometheus text exposition" $
                get "/metrics"
                    `shouldRespondWith` 200
                        { matchHeaders = ["Content-Type" <:> "text/plain; version=0.0.4; charset=utf-8"]
                        }

            it "answers every other path with a 404, so the listener serves one path only" $
                get "/livez" `shouldRespondWith` 404

-- A handle over no series at all: the listener's surface is the assertion here, not its content.
emptyScrape :: MetricScrape
emptyScrape = MetricScrape (pure V.empty)

{- @OTEL_METRICS_EXPORTER@ is process-global and the suite runs every spec in one process, so
each case puts back whatever it found. -}
withExporter :: Maybe String -> IO a -> IO a
withExporter value act = bracket (lookupEnv exporterVar) apply (const (apply value >> act))
  where
    apply :: Maybe String -> IO ()
    apply = maybe (unsetEnv exporterVar) (setEnv exporterVar)

exporterVar :: String
exporterVar = "OTEL_METRICS_EXPORTER"
