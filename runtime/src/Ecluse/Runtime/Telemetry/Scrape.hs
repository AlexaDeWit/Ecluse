-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The pull-side metrics transport: a Prometheus @\/metrics@ scrape endpoint over the same
instruments the OTLP push pipeline ("Ecluse.Runtime.Telemetry") exports. A backend that scrapes
rather than receives selects it with @OTEL_METRICS_EXPORTER=prometheus@, which the SDK answers
with a no-op push exporter, leaving the endpoint to the application. The route renders the
meter's series at the instant of the scrape, so no export interval sits between a measurement
and its collection. The middleware belongs __outermost__ in the front door's stack
("Ecluse.Runtime.Server"), above the server-span wrapper: a scrape then opens no span and never
counts itself into the @http.server.*@ series it reports.
-}
module Ecluse.Runtime.Telemetry.Scrape (
    -- * The collection handle
    MetricScrape (..),
    metricScrapeFor,
    scrapeSelected,

    -- * The route
    scrapeMiddleware,
) where

import Data.Vector (Vector)
import Data.Vector qualified as V
import Network.Wai (Middleware)
import OpenTelemetry.Environment (MetricsExporterSelection (MetricsExporterPrometheus), lookupMetricsExporterSelection)
import OpenTelemetry.Exporter.Metric (ResourceMetricsExport)
import OpenTelemetry.Exporter.Prometheus.WAI (prometheusMiddleware)
import OpenTelemetry.MeterProvider (SdkMeterEnv, collectResourceMetrics)

{- | One on-demand collection of the meter's current series. An enabled telemetry handle carries
one only where the operator asked for the scrape transport.
-}
newtype MetricScrape = MetricScrape
    { runMetricScrape :: IO (Vector ResourceMetricsExport)
    }

{- | Whether @OTEL_METRICS_EXPORTER@ names the Prometheus transport. It reads the SDK's own parse
of the variable, so the route and the exporter the SDK resolves cannot disagree over one value.
-}
scrapeSelected :: IO Bool
scrapeSelected = (Just MetricsExporterPrometheus ==) <$> lookupMetricsExporterSelection

{- | Build the scrape handle over an SDK meter environment, or 'Nothing' when the operator left
the transport on OTLP push, where a @\/metrics@ path would answer nothing worth scraping.
-}
metricScrapeFor :: SdkMeterEnv -> IO (Maybe MetricScrape)
metricScrapeFor meterEnv = do
    selected <- scrapeSelected
    pure (if selected then Just (MetricScrape collect) else Nothing)
  where
    collect :: IO (Vector ResourceMetricsExport)
    collect = V.fromList <$> collectResourceMetrics meterEnv

{- | Answer @\/metrics@ with the Prometheus text exposition of the current series and pass every
other path through. 'Nothing' is the identity, so an unselected transport mounts no route.
-}
scrapeMiddleware :: Maybe MetricScrape -> Middleware
scrapeMiddleware = maybe id (prometheusMiddleware . runMetricScrape)
