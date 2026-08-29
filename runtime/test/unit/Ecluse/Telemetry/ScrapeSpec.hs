-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Telemetry.ScrapeSpec (spec) where

import Prelude hiding (get)

import Data.Vector qualified as V
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import System.Environment (setEnv, unsetEnv)
import Test.Hspec
import Test.Hspec.Wai
import UnliftIO (bracket)

import OpenTelemetry.Metric (createMeterProvider, defaultSdkMeterProviderOptions)
import OpenTelemetry.Resource (emptyMaterializedResources)

import Ecluse.Runtime.Telemetry.Scrape (
    MetricScrape (MetricScrape, runMetricScrape),
    metricScrapeFor,
    scrapeMiddleware,
    scrapeSelected,
 )

{- | The scrape transport's selection and its routing. What the exposition /says/ needs the live
instruments, so the rendered series belong to the integration tier.
-}
spec :: Spec
spec = do
    selectionSpec
    handleSpec
    routeSpec

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

routeSpec :: Spec
routeSpec = describe "scrapeMiddleware" $ do
    with (pure (scrapeMiddleware (Just emptyScrape) innerApplication)) $ do
        it "answers /metrics as Prometheus text exposition" $
            get "/metrics"
                `shouldRespondWith` 200
                    { matchHeaders = ["Content-Type" <:> "text/plain; version=0.0.4; charset=utf-8"]
                    }

        it "passes every other path through to the inner application" $
            get "/livez" `shouldRespondWith` "inner"

    with (pure (scrapeMiddleware Nothing innerApplication)) $
        it "mounts no route without a handle, so /metrics reaches the inner application" $
            get "/metrics" `shouldRespondWith` "inner"

-- A handle over no series at all: the route's shape is the assertion here, not its content.
emptyScrape :: MetricScrape
emptyScrape = MetricScrape (pure V.empty)

-- The application under the middleware, answering any path with a body the assertions recognise.
innerApplication :: Application
innerApplication _request respond = respond (responseLBS status200 [] "inner")

{- @OTEL_METRICS_EXPORTER@ is process-global and the suite runs every spec in one process, so
each case puts back whatever it found. -}
withExporter :: Maybe String -> IO a -> IO a
withExporter value act = bracket (lookupEnv exporterVar) apply (const (apply value >> act))
  where
    apply :: Maybe String -> IO ()
    apply = maybe (unsetEnv exporterVar) (setEnv exporterVar)

exporterVar :: String
exporterVar = "OTEL_METRICS_EXPORTER"
