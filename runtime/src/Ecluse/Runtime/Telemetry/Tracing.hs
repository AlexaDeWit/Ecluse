-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The request-lifecycle tracing layer over the OpenTelemetry substrate ("Ecluse.Runtime.Telemetry"):
the WAI server span, the data plane's http-client child spans, and the hand-added domain spans. Every
entry point takes the 'Telemetry' handle and is inert when telemetry is off, so the middleware is 'id',
an exchange opens untraced, and a domain-span bracket runs its body against no span. The data-plane
client span records no request or response header and the WAI instrumentation
never records @Authorization@, so a forwarded client token never reaches a span, while the
high-cardinality package, version, and denial message deliberately do.
-}
module Ecluse.Runtime.Telemetry.Tracing (
    -- * WAI server span
    telemetryWaiMiddleware,

    -- * The data plane's client spans
    dataPlaneTrace,

    -- * Domain spans
    JobSpanOutcome (..),

    -- * The core tracing ports
    tracingPortOf,
    workerTracingPortOf,
    advisorySyncTracingPortOf,
) where

import Ecluse.Runtime.Telemetry.Tracing.Internal (
    JobSpanOutcome (..),
    advisorySyncTracingPortOf,
    dataPlaneTrace,
    telemetryWaiMiddleware,
    tracingPortOf,
    workerTracingPortOf,
 )
