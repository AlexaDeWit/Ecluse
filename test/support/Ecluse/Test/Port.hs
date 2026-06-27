{- | Test doubles for the core serve-path recording ports
("Ecluse.Core.Telemetry.Record", "Ecluse.Core.Telemetry.Span").

The core pipeline records through abstract ports rather than a telemetry backend, so a
suite can drive it over inert or recording doubles with no OpenTelemetry SDK. These are
the shared doubles every suite reaches for: an inert metrics port, a metrics port that
captures the serve decisions it is handed (to assert the pipeline recorded an admit or a
denial), and a pass-through tracing port that simply runs the bracketed body.
-}
module Ecluse.Test.Port (
    noopMetricsPort,
    recordingMetricsPort,
    passthroughTracingPort,
) where

import Ecluse.Core.Telemetry.Metrics (Decision)
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Core.Telemetry.Span (TracingPort (..))

{- | A 'MetricsPort' whose every field discards its measurement — the inert double for a
spec that drives the serve path but asserts nothing about metrics.
-}
noopMetricsPort :: MetricsPort
noopMetricsPort =
    MetricsPort
        { mpServeDecision = const pass
        , mpRuleDenial = \_ _ -> pass
        , mpRuleEvalDuration = \_ _ -> pass
        , mpRuleEffectfulFailure = const pass
        , mpUpstreamFetch = \_ _ _ -> pass
        , mpUpstreamFetchError = \_ _ -> pass
        , mpCacheRequest = const pass
        , mpCacheEntries = const pass
        , mpMirrorEnqueued = pass
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

{- | A 'TracingPort' that opens no span and simply runs the bracketed body — the inert
double for a spec that drives the serve path's span sites without a tracer.
-}
passthroughTracingPort :: TracingPort
passthroughTracingPort =
    TracingPort
        { spanRuleEval = \_ _ action -> fst <$> action
        , spanMirrorEnqueue = \_ _ _ action -> action
        }
