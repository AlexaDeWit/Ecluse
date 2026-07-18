-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test doubles for the core serve-path and worker recording ports
("Ecluse.Core.Telemetry.Record", "Ecluse.Core.Telemetry.Span").

The core pipeline and the mirror worker record through abstract ports rather than a
telemetry backend, so a suite can drive them over inert or recording doubles with no
OpenTelemetry SDK. These are the shared doubles every suite reaches for: an inert metrics
port, a metrics port that captures the signals it is handed (to assert what was
recorded), and a pass-through tracing port that simply runs the bracketed body -- one set
for the serve path, one for the worker.
-}
module Ecluse.Test.Port (
    -- * Serve-path ports
    noopMetricsPort,
    recordingMetricsPort,
    recordingDivergenceMetricsPort,
    passthroughTracingPort,

    -- * Worker ports
    noopWorkerMetricsPort,
    recordingWorkerMetricsPort,
    passthroughWorkerTracingPort,
) where

import Ecluse.Core.Telemetry.Metrics (Decision, MirrorResult)
import Ecluse.Core.Telemetry.Record (MetricsPort (..), WorkerMetricsPort (..))
import Ecluse.Core.Telemetry.Span (TracingPort (..), WorkerTracingPort (..))

{- | A 'MetricsPort' whose every field discards its measurement -- the inert double for a
spec that drives the serve path but asserts nothing about metrics.
-}
noopMetricsPort :: MetricsPort
noopMetricsPort =
    MetricsPort
        { mpServeDecision = const pass
        , mpServeAdmissionInFlight = const pass
        , mpServeAdmissionQueued = pass
        , mpPublishBodyInFlightBytes = const pass
        , mpPublishBodyShed = pass
        , mpMergeDivergence = pass
        , mpRuleDenial = \_ _ -> pass
        , mpRuleEvalDuration = \_ _ -> pass
        , mpRuleEffectfulFailure = const pass
        , mpUpstreamFetch = \_ _ _ -> pass
        , mpUpstreamFetchError = \_ _ -> pass
        , mpCacheRequest = const pass
        , mpCacheEntries = const pass
        , mpCacheResidentBytes = const pass
        , mpVersionCacheResidentBytes = const pass
        , mpAssembledCacheResidentBytes = const pass
        , mpMirrorEnqueued = pass
        , mpPublicRelayAnomaly = const pass
        , mpRequestPerimeterFault = const pass
        , mpMirrorEnqueueFailure = pass
        }

{- | A 'MetricsPort' that captures the serve decisions it records, alongside a reader for
the decisions seen so far (in record order). Every other field is inert. Lets a spec
assert that the pipeline recorded the expected admit\/deny\/unavailable through the port.
-}
recordingMetricsPort :: IO (MetricsPort, IO [Decision])
recordingMetricsPort = do
    seen <- newTVarIO []
    let port = noopMetricsPort{mpServeDecision = \d -> atomically (modifyTVar' seen (<> [d]))}
    pure (port, readTVarIO seen)

{- | A 'MetricsPort' that counts the cross-upstream integrity divergences it is handed
(@ecluse.registry.merge.divergence@), alongside a reader for the running total. Every
other field is inert. Lets a spec assert the serve path metered a divergence.
-}
recordingDivergenceMetricsPort :: IO (MetricsPort, IO Int)
recordingDivergenceMetricsPort = do
    seen <- newTVarIO 0
    let port = noopMetricsPort{mpMergeDivergence = atomically (modifyTVar' seen (+ 1))}
    pure (port, readTVarIO seen)

{- | A 'TracingPort' that opens no span and simply runs the bracketed body -- the inert
double for a spec that drives the serve path's span sites without a tracer.
-}
passthroughTracingPort :: TracingPort
passthroughTracingPort =
    TracingPort
        { spanRuleEval = \_ _ action -> fst <$> action
        , spanMirrorEnqueue = \_ _ _ _ action -> action Nothing
        , spanPackumentGate = \_ action -> action
        , spanMetadataFetch = \_ action -> action
        , spanMetadataDecode = \_ action -> action
        }

{- | A 'WorkerMetricsPort' whose every field discards its measurement -- the inert double
for a spec that drives the worker loop but asserts nothing about metrics.
-}
noopWorkerMetricsPort :: WorkerMetricsPort
noopWorkerMetricsPort =
    WorkerMetricsPort
        { wmpMirrorJobProcessed = const pass
        , wmpMirrorPublishDuration = const pass
        }

{- | A 'WorkerMetricsPort' that captures the per-job results it records, alongside a
reader for the results seen so far (in record order). The publish-duration field is
inert. Lets a spec assert that the worker recorded the expected processed-job result
through the port.
-}
recordingWorkerMetricsPort :: IO (WorkerMetricsPort, IO [MirrorResult])
recordingWorkerMetricsPort = do
    seen <- newTVarIO []
    let port = noopWorkerMetricsPort{wmpMirrorJobProcessed = \r -> atomically (modifyTVar' seen (<> [r]))}
    pure (port, readTVarIO seen)

{- | A 'WorkerTracingPort' that opens no span and simply runs the bracketed body -- the
inert double for a spec that drives the worker's per-job span site without a tracer.
-}
passthroughWorkerTracingPort :: WorkerTracingPort
passthroughWorkerTracingPort =
    WorkerTracingPort
        { -- Open no span and establish no link: ignore the carried trace context and the
          -- outcome projection, just running the job body.
          wtpMirrorJobSpan = \_ _ _ _ action -> action
        }
