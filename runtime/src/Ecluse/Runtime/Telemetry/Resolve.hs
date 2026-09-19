-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Resolve the telemetry identity from the environment, collapsing the Datadog and the vanilla
OpenTelemetry dialect into one answer that logs and traces share. @DD_API_KEY@ and @DD_SITE@ are
never read, and the OTLP endpoint is an operator-declared destination used as given, so no key in
the environment can turn into off-cluster egress. @OTEL_RESOURCE_ATTRIBUTES@ is read with the W3C
baggage grammar the SDK itself uses, so a percent-encoded value decodes the same way for the @dd@
log object and for the span resource. @docs\/architecture\/observability.md@ describes the
configuration model. "Ecluse.Runtime.Telemetry.Resolve.Internal" implements it.
-}
module Ecluse.Runtime.Telemetry.Resolve (
    -- * The resolved telemetry identity
    ResolvedTelemetry (..),
    resolveTelemetry,
    declaredEnv,

    -- * Boot wiring
    prepareTelemetry,
) where

import Ecluse.Runtime.Telemetry.Resolve.Internal (
    ResolvedTelemetry (..),
    declaredEnv,
    prepareTelemetry,
    resolveTelemetry,
 )
