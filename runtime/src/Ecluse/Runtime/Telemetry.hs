-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The OpenTelemetry substrate: the tracer and meter providers the rest of the proxy hangs
spans and metrics on, behind the @ECLUSE_OBSERVABILITY__TELEMETRY@ master switch. Observability
is opt-in, so with that switch unset nothing is wired and the SDK is never initialised.
'withTelemetry' is the lifecycle bracket the composition root runs the proxy within: it builds
the providers from the standard @OTEL_*@ variables, runs the Prometheus scrape listener, and
tears both down along every exit path. It also wraps the OTLP exporters, because
@hs-opentelemetry 1.0.0.0@ drops a failed export silently.
-}
module Ecluse.Runtime.Telemetry (
    -- * Master switch
    TelemetrySwitch (..),
    parseTelemetrySwitch,

    -- * The telemetry handle
    Telemetry,
    telemetryDisabled,
    telemetryTracerProvider,
    telemetryMeterProvider,

    -- * Lifecycle
    withTelemetry,
) where

import Ecluse.Runtime.Telemetry.Internal (
    Telemetry,
    TelemetrySwitch (..),
    parseTelemetrySwitch,
    telemetryDisabled,
    telemetryMeterProvider,
    telemetryTracerProvider,
    withTelemetry,
 )
