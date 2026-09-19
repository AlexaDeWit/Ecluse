-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The pull-side metrics transport: a Prometheus exposition on its own listener, never on the
proxy's data port. A backend that scrapes selects it with @OTEL_METRICS_EXPORTER=prometheus@, which
the SDK answers with a no-op push exporter, leaving the endpoint to the application. The exposition
carries the whole OpenTelemetry resource, naming the host, the process, and any cloud or cluster
identity the SDK detected, so it never shares the port untrusted registry clients reach and binds
@localhost@ until an operator widens it. @OTEL_EXPORTER_PROMETHEUS_HOST@ and
@OTEL_EXPORTER_PROMETHEUS_PORT@ address it, as the OpenTelemetry specification defines them.
-}
module Ecluse.Runtime.Telemetry.Scrape (
    -- * The collection handle
    MetricScrape,
    metricScrapeFor,

    -- * The dedicated listener
    withScrapeListener,
) where

import Ecluse.Runtime.Telemetry.Scrape.Internal (
    MetricScrape,
    metricScrapeFor,
    withScrapeListener,
 )
