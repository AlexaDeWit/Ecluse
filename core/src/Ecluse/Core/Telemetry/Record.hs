{- | The metric-recording port: the abstract interface the core serve path records
through, decoupled from any telemetry backend.

"Ecluse.Core.Telemetry.Metrics" defines /what/ the @ecluse.*@ catalogue is (the names
and the closed set of bounded labels). This module defines the __recording interface__
over that catalogue as a record of @IO@ functions (the Handle pattern, as
"Ecluse.Core.Registry" and "Ecluse.Core.Queue" use): one field per signal the serve
path emits, each taking only the bounded label values its metric carries. The core
serve path records through this port and never names an OpenTelemetry instrument; the
application supplies the OTel-backed implementation behind it (see
@Ecluse.Telemetry.Instruments@). A test supplies an inert or recording double.

Only the signals the serve path emits are present — serve decisions, the rule gate,
the data-plane upstream fetch, the metadata cache, and mirror enqueue. The
worker-only and credential signals stay in the application instrument set; the port
carries exactly what the pipeline uses.
-}
module Ecluse.Core.Telemetry.Record (
    -- * The recording port
    MetricsPort (..),

    -- * Timing
    timedSeconds,
) where

import GHC.Clock (getMonotonicTime)

import Ecluse.Core.Telemetry.Metrics (
    CacheResult,
    Cause,
    Decision,
    ReasonClass,
    StatusClass,
    Tier,
    Upstream,
 )

{- | The metric-recording port — a record of functions over a backend whose closure
captures its instruments. Each field records one @ecluse.*@ signal under exactly the
bounded labels that signal carries; the closed label vocabularies come from
"Ecluse.Core.Telemetry.Metrics", so the bounded-cardinality discipline is enforced at
the call site by the types. All fields return 'IO', so a backend (and the core code
recording through it) stays decoupled from the application's effect stack.
-}
data MetricsPort = MetricsPort
    { mpServeDecision :: Decision -> IO ()
    -- ^ Record one serve decision (@ecluse.serve.decision@): admit, deny, or unavailable.
    , mpRuleDenial :: Maybe Text -> ReasonClass -> IO ()
    {- ^ Record one rule denial (@ecluse.rule.denials@) by reason class and, for a
    policy denial, the deciding rule. A non-policy refusal carries no rule.
    -}
    , mpRuleEvalDuration :: Tier -> Double -> IO ()
    -- ^ Record a rule-evaluation latency sample (@ecluse.rule.eval.duration@) by tier.
    , mpRuleEffectfulFailure :: Cause -> IO ()
    -- ^ Record one effectful-rule failure (@ecluse.rule.effectful.failures@) by cause.
    , mpUpstreamFetch :: Upstream -> StatusClass -> Double -> IO ()
    {- ^ Record an upstream metadata-fetch latency sample
    (@ecluse.upstream.fetch.duration@) by upstream and the response's status class.
    -}
    , mpUpstreamFetchError :: Upstream -> Cause -> IO ()
    {- ^ Record one upstream metadata-fetch error (@ecluse.upstream.fetch.errors@) by
    upstream and the bounded cause.
    -}
    , mpCacheRequest :: CacheResult -> IO ()
    {- ^ Record one metadata-cache lookup (@ecluse.metadata_cache.requests@) as a hit
    or a miss.
    -}
    , mpCacheEntries :: Int -> IO ()
    -- ^ Record the metadata cache's current occupancy (@ecluse.metadata_cache.entries@).
    , mpMirrorEnqueued :: IO ()
    -- ^ Record one mirror job enqueued (@ecluse.mirror.enqueued@).
    , mpMirrorEnqueueFailure :: IO ()
    -- ^ Record one mirror enqueue failure (@ecluse.mirror.enqueue.failures@).
    }

{- | Run an action and return its result alongside the wall-clock seconds it took,
measured on the monotonic clock so a system-clock step never yields a negative or
absurd duration. The seconds are what the latency histograms record (through
'mpRuleEvalDuration' \/ 'mpUpstreamFetch'). Pure of any backend, so it lives beside the
port the durations feed.
-}
timedSeconds :: (MonadIO m) => m a -> m (a, Double)
timedSeconds action = do
    start <- liftIO getMonotonicTime
    result <- action
    end <- liftIO getMonotonicTime
    pure (result, end - start)
