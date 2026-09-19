-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The live OpenTelemetry instruments over the pure @ecluse.*@ catalogue
("Ecluse.Core.Telemetry.Metrics"), and one typed @record*@ per signal. Each helper takes only the
bounded label values its metric carries, so the type enforces the cardinality rule at the call
site. With telemetry off, 'newMetrics' builds from the SDK's no-op meter provider, so the hot path
records unconditionally: no per-call branch, and the 'metricAttributes' a call passes is never
forced. @docs\/architecture\/observability.md@ describes the catalogue. "Ecluse.Runtime.Telemetry.Instruments.Internal" implements it.
-}
module Ecluse.Runtime.Telemetry.Instruments (
    -- * The instrument handle
    Metrics,
    newMetrics,

    -- * The core recording ports
    metricsPortOf,
    workerMetricsPortOf,
    dredgerMetricsPortOf,
    advisorySyncMetricsPortOf,
    advisoryCompileMetricsPortOf,

    -- * The signals the deferred reporters feed directly
    recordBreakerState,
    recordMirrorEnqueueFailure,
    recordCredentialRefresh,
    registerCredentialTokenTtl,

    -- * Advisory ages (observable)
    registerAdvisoryDatabaseAge,
    registerAdvisorySourceAge,
) where

import Ecluse.Runtime.Telemetry.Instruments.Internal (
    Metrics,
    advisoryCompileMetricsPortOf,
    advisorySyncMetricsPortOf,
    dredgerMetricsPortOf,
    metricsPortOf,
    newMetrics,
    recordBreakerState,
    recordCredentialRefresh,
    recordMirrorEnqueueFailure,
    registerAdvisoryDatabaseAge,
    registerAdvisorySourceAge,
    registerCredentialTokenTtl,
    workerMetricsPortOf,
 )
