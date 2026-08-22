-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test doubles for the core serve-path and worker recording ports
("Ecluse.Core.Telemetry.Record", "Ecluse.Core.Telemetry.Span").

The core pipeline and the mirror worker record through abstract ports rather than a
telemetry backend, so a suite can drive them over inert or recording doubles with no
OpenTelemetry SDK. These are the shared doubles every suite reaches for: an inert metrics
port, a metrics port that captures the signals it is handed (to assert what was
recorded), and a pass-through tracing port that simply runs the bracketed body: one set
for the serve path, one for the worker, and one for the advisory sync task.
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

    -- * Advisory sync ports
    noopAdvisorySyncMetricsPort,
    recordingAdvisorySyncMetricsPort,
    passthroughAdvisorySyncTracingPort,
    recordingAdvisorySyncTracingPort,
) where

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Telemetry.Metrics (AdvisorySyncResult, Decision, MirrorResult)
import Ecluse.Core.Telemetry.Record (AdvisorySyncMetricsPort (..), MetricsPort (..), WorkerMetricsPort (..))
import Ecluse.Core.Telemetry.Span (AdvisorySyncTracingPort (..), TracingPort (..), WorkerTracingPort (..))

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

{- | An 'AdvisorySyncMetricsPort' whose every field discards its measurement: the inert
double for a spec that drives the advisory sync loop but asserts nothing about metrics.
-}
noopAdvisorySyncMetricsPort :: AdvisorySyncMetricsPort
noopAdvisorySyncMetricsPort =
    AdvisorySyncMetricsPort
        { asmpSyncAttempt = \_ _ -> pass
        , asmpSyncDuration = \_ _ _ -> pass
        }

{- | An 'AdvisorySyncMetricsPort' that captures every attempt and every latency sample it
is handed (in record order), alongside a reader for each. Lets a spec assert that one sync
attempt recorded exactly one attempt and one duration under the expected ecosystem and
result. The latency reader carries the seconds so a spec can check the sample is a real
measurement rather than a placeholder.
-}
recordingAdvisorySyncMetricsPort ::
    IO
        ( AdvisorySyncMetricsPort
        , IO [(Ecosystem, AdvisorySyncResult)]
        , IO [(Ecosystem, AdvisorySyncResult, Double)]
        )
recordingAdvisorySyncMetricsPort = do
    attempts <- newTVarIO []
    durations <- newTVarIO []
    let port =
            AdvisorySyncMetricsPort
                { asmpSyncAttempt = \eco result -> atomically (modifyTVar' attempts (<> [(eco, result)]))
                , asmpSyncDuration = \eco result seconds -> atomically (modifyTVar' durations (<> [(eco, result, seconds)]))
                }
    pure (port, readTVarIO attempts, readTVarIO durations)

{- | An 'AdvisorySyncTracingPort' that opens no span and simply runs the bracketed
attempt: the inert double for a spec that drives the sync loop without a tracer.
-}
passthroughAdvisorySyncTracingPort :: AdvisorySyncTracingPort
passthroughAdvisorySyncTracingPort =
    AdvisorySyncTracingPort
        { astpSyncAttemptSpan = \_ _ action -> action
        }

{- | An 'AdvisorySyncTracingPort' that records one entry per bracketed attempt, the
ecosystem and the result the attempt projected, alongside a reader for the entries seen so
far (in record order). It records __after__ the body returns, as the real bracket closes
its span, so a spec that waits on this reader also sees the attempt's metrics settled.
-}
recordingAdvisorySyncTracingPort :: IO (AdvisorySyncTracingPort, IO [(Ecosystem, AdvisorySyncResult)])
recordingAdvisorySyncTracingPort = do
    seen <- newTVarIO []
    let port =
            AdvisorySyncTracingPort
                { astpSyncAttemptSpan = \eco project action -> do
                    result <- action
                    atomically (modifyTVar' seen (<> [(eco, project result)]))
                    pure result
                }
    pure (port, readTVarIO seen)
