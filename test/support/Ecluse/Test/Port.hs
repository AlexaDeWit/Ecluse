-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test doubles for the core serve-path and worker recording ports
("Ecluse.Core.Telemetry.Record", "Ecluse.Core.Telemetry.Span").

The core pipeline and the mirror worker record through abstract ports, not a telemetry
backend. A suite can therefore drive them over inert or recording doubles with no
OpenTelemetry SDK. This module holds the shared doubles: an inert metrics port, a
recording metrics port, and a pass-through tracing port. A recording port exposes what
it captured, so a spec can assert on it. There is one set for the serve path, one for the
worker, one for the advisory sync task, and one for the advisory compile.
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
    RecordedSync (..),
    passthroughAdvisorySyncTracingPort,
    recordingAdvisorySyncTracingPort,

    -- * Advisory compile ports
    noopAdvisoryCompileMetricsPort,
    recordingAdvisoryCompileMetricsPort,
    RecordedCompile (..),
) where

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Telemetry.Metrics (AdvisoryCompileResult, AdvisoryDropCause, AdvisorySyncResult, Decision, MirrorResult)
import Ecluse.Core.Telemetry.Record (AdvisoryCompileMetricsPort (..), AdvisorySyncMetricsPort (..), MetricsPort (..), WorkerMetricsPort (..))
import Ecluse.Core.Telemetry.Span (AdvisorySyncTracingPort (..), TracingPort (..), WorkerTracingPort (..))

{- | A 'MetricsPort' that discards every measurement, for a spec that drives the serve
path but asserts nothing about metrics.
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

{- | A 'MetricsPort' that captures the serve decisions it records, with a reader for them in record
order. Every other field is inert.
-}
recordingMetricsPort :: IO (MetricsPort, IO [Decision])
recordingMetricsPort = do
    seen <- newTVarIO []
    let port = noopMetricsPort{mpServeDecision = \d -> atomically (modifyTVar' seen (<> [d]))}
    pure (port, readTVarIO seen)

{- | A 'MetricsPort' that counts the cross-upstream integrity divergences it receives
(@ecluse.registry.merge.divergence@), with a reader for the running total. Every other
field is inert. A spec asserts that the serve path metered a divergence.
-}
recordingDivergenceMetricsPort :: IO (MetricsPort, IO Int)
recordingDivergenceMetricsPort = do
    seen <- newTVarIO 0
    let port = noopMetricsPort{mpMergeDivergence = atomically (modifyTVar' seen (+ 1))}
    pure (port, readTVarIO seen)

{- | A 'TracingPort' that opens no span and runs the bracketed body, for a spec that
drives the serve path's span sites without a tracer.
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

{- | A 'WorkerMetricsPort' that discards every measurement, for a spec that drives the
worker loop but asserts nothing about metrics.
-}
noopWorkerMetricsPort :: WorkerMetricsPort
noopWorkerMetricsPort =
    WorkerMetricsPort
        { wmpMirrorJobProcessed = const pass
        , wmpMirrorPublishDuration = const pass
        }

{- | A 'WorkerMetricsPort' that captures the per-job results it records, with a reader for them in
record order. The publish-duration field is inert.
-}
recordingWorkerMetricsPort :: IO (WorkerMetricsPort, IO [MirrorResult])
recordingWorkerMetricsPort = do
    seen <- newTVarIO []
    let port = noopWorkerMetricsPort{wmpMirrorJobProcessed = \r -> atomically (modifyTVar' seen (<> [r]))}
    pure (port, readTVarIO seen)

{- | A 'WorkerTracingPort' that opens no span and runs the bracketed body, for a spec
that drives the worker's per-job span site without a tracer.
-}
passthroughWorkerTracingPort :: WorkerTracingPort
passthroughWorkerTracingPort =
    WorkerTracingPort
        { -- Ignore the carried trace context and the outcome projection: no span, no link.
          wtpMirrorJobSpan = \_ _ _ _ action -> action
        }

{- | An 'AdvisorySyncMetricsPort' that discards every measurement, for a spec that drives
the advisory sync loop but asserts nothing about metrics.
-}
noopAdvisorySyncMetricsPort :: AdvisorySyncMetricsPort
noopAdvisorySyncMetricsPort =
    AdvisorySyncMetricsPort
        { asmpSyncAttempt = \_ _ -> pass
        , asmpSyncDuration = \_ _ _ -> pass
        , asmpDatabaseAge = \_ _ -> pass
        }

-- | What one sync run recorded through 'recordingAdvisorySyncMetricsPort', each list in record order.
data RecordedSync = RecordedSync
    { rsAttempts :: [(Ecosystem, AdvisorySyncResult)]
    , rsDurations :: [(Ecosystem, AdvisorySyncResult, Double)]
    , rsAges :: [(Ecosystem, Int)]
    }
    deriving stock (Eq, Show)

{- | An 'AdvisorySyncMetricsPort' that captures every attempt, latency sample, and age sample it
receives, with one reader for the lot.
-}
recordingAdvisorySyncMetricsPort :: IO (AdvisorySyncMetricsPort, IO RecordedSync)
recordingAdvisorySyncMetricsPort = do
    seen <- newTVarIO (RecordedSync [] [] [])
    let port =
            AdvisorySyncMetricsPort
                { asmpSyncAttempt = \eco result -> bump seen (\r -> r{rsAttempts = rsAttempts r <> [(eco, result)]})
                , asmpSyncDuration = \eco result seconds -> bump seen (\r -> r{rsDurations = rsDurations r <> [(eco, result, seconds)]})
                , asmpDatabaseAge = \eco seconds -> bump seen (\r -> r{rsAges = rsAges r <> [(eco, seconds)]})
                }
    pure (port, readTVarIO seen)

{- | An 'AdvisoryCompileMetricsPort' that discards every measurement, for a spec that compiles an
artifact but asserts nothing about metrics.
-}
noopAdvisoryCompileMetricsPort :: AdvisoryCompileMetricsPort
noopAdvisoryCompileMetricsPort =
    AdvisoryCompileMetricsPort
        { acmpCompileAccepted = const pass
        , acmpCompileDropped = \_ _ -> pass
        , acmpCompileRun = const pass
        }

{- | What one compile pass recorded through 'recordingAdvisoryCompileMetricsPort', each list in
record order.
-}
data RecordedCompile = RecordedCompile
    { rcAccepted :: [Int]
    , rcDropped :: [(AdvisoryDropCause, Int)]
    , rcRuns :: [AdvisoryCompileResult]
    }
    deriving stock (Eq, Show)

{- | An 'AdvisoryCompileMetricsPort' that captures every tally and verdict it receives, with one
reader for the lot.
-}
recordingAdvisoryCompileMetricsPort :: IO (AdvisoryCompileMetricsPort, IO RecordedCompile)
recordingAdvisoryCompileMetricsPort = do
    seen <- newTVarIO (RecordedCompile [] [] [])
    let port =
            AdvisoryCompileMetricsPort
                { acmpCompileAccepted = \entries -> bump seen (\r -> r{rcAccepted = rcAccepted r <> [entries]})
                , acmpCompileDropped = \cause entries -> bump seen (\r -> r{rcDropped = rcDropped r <> [(cause, entries)]})
                , acmpCompileRun = \result -> bump seen (\r -> r{rcRuns = rcRuns r <> [result]})
                }
    pure (port, readTVarIO seen)

-- Append one measurement to a recorder's tally.
bump :: TVar a -> (a -> a) -> IO ()
bump seen f = atomically (modifyTVar' seen f)

{- | An 'AdvisorySyncTracingPort' that opens no span and runs the bracketed attempt, for
a spec that drives the sync loop without a tracer.
-}
passthroughAdvisorySyncTracingPort :: AdvisorySyncTracingPort
passthroughAdvisorySyncTracingPort =
    AdvisorySyncTracingPort
        { astpSyncAttemptSpan = \_ _ action -> action
        }

{- | An 'AdvisorySyncTracingPort' that records the ecosystem and projected result of each bracketed
attempt, with a reader for them in record order. It records __after__ the body returns, as the real
bracket closes its span, so a spec waiting on the reader also sees the attempt's metrics settled.
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
