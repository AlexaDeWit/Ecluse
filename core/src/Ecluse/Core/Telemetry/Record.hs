{- | The metric-recording ports: the abstract interfaces the core serve path and
mirror worker record through, decoupled from any telemetry backend.

"Ecluse.Core.Telemetry.Metrics" defines /what/ the @ecluse.*@ catalogue is (the names
and the closed set of bounded labels). This module defines the __recording interfaces__
over that catalogue as records of @IO@ functions (the Handle pattern, as
"Ecluse.Core.Registry" and "Ecluse.Core.Queue" use): one field per signal a consumer
emits, each taking only the bounded label values its metric carries. A consumer records
through its port and never names an OpenTelemetry instrument; the application supplies
the OTel-backed implementations behind them (see @Ecluse.Telemetry.Instruments@). A test
supplies an inert or recording double.

Two ports are defined: 'MetricsPort' for the serve path (serve decisions, the rule gate,
the data-plane upstream fetch, the metadata cache, and mirror enqueue) and
'WorkerMetricsPort' for the mirror worker (jobs processed, publish latency). The
credential signals stay in the application instrument set; each port carries exactly the
signals its consumer emits.
-}
module Ecluse.Core.Telemetry.Record (
    -- * The serve-path recording port
    MetricsPort (..),

    -- * The worker recording port
    WorkerMetricsPort (..),

    -- * Timing
    timedSeconds,
) where

import GHC.Clock (getMonotonicTime)

import Ecluse.Core.Telemetry.Metrics (
    CacheResult,
    Cause,
    Decision,
    MirrorResult,
    ReasonClass,
    StatusClass,
    Tier,
    Upstream,
 )

{- | The metric-recording port -- a record of functions over a backend whose closure
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
    , mpCacheResidentBytes :: Int -> IO ()
    {- ^ Record the full-packument metadata cache's resident bytes
    (@ecluse.metadata_cache.resident_bytes@).
    -}
    , mpVersionCacheResidentBytes :: Int -> IO ()
    {- ^ Record the single-version metadata cache's resident bytes
    (@ecluse.metadata_cache.version.resident_bytes@).
    -}
    , mpMirrorEnqueued :: IO ()
    -- ^ Record one mirror job enqueued (@ecluse.mirror.enqueued@).
    , mpMirrorEnqueueFailure :: IO ()
    -- ^ Record one mirror enqueue failure (@ecluse.mirror.enqueue.failures@).
    }

{- | The mirror worker's metric-recording port -- the worker analogue of 'MetricsPort',
kept a separate record so the worker records exactly its own signals and the serve path
exactly its own (the two consumers share no field). Both fields return 'IO', so the
worker loop records through the port without naming a telemetry backend; the application
supplies the OTel-backed implementation (see @Ecluse.Telemetry.Instruments@) and a test
an inert or recording double.
-}
data WorkerMetricsPort = WorkerMetricsPort
    { wmpMirrorJobProcessed :: MirrorResult -> IO ()
    {- ^ Record one processed mirror job (@ecluse.mirror.jobs.processed@) by its
    terminal result (published, or failed).
    -}
    , wmpMirrorPublishDuration :: Double -> IO ()
    -- ^ Record one mirror publish-latency sample (@ecluse.mirror.publish.duration@).
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
