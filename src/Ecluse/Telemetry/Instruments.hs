{- | The runtime metric instruments and the typed emit helpers the hot path records
through — the IO layer over the pure @ecluse.*@ catalogue ("Ecluse.Telemetry.Metrics").

"Ecluse.Telemetry.Metrics" defines /what/ the catalogue is (the names and the closed
set of bounded labels); this module turns that catalogue into live OpenTelemetry
instruments and exposes one typed @record*@ per signal. Each helper takes only the
bounded label values its metric carries — never a free identifier — so the
bounded-label discipline is enforced at the call site by the type, and the attribute
set an instrument ever sees is drawn from a small fixed product of the label domains.

== Gating: inert when telemetry is off

'newMetrics' builds the instruments from the 'Telemetry' handle's meter provider when
telemetry is enabled, and from the SDK's __no-op meter provider__ when it is not. A
no-op instrument discards every measurement, so the @record*@ helpers are called
__unconditionally__ on the hot path and are genuinely inert when telemetry is off — no
per-call branch, no provider fabricated at the edge. The 'Metrics' handle is therefore
total: every signal has a real instrument whichever posture the proxy is in.

The catalogue and the cardinality rule are described in
@docs\/architecture\/observability.md@.
-}
module Ecluse.Telemetry.Instruments (
    -- * The instrument handle
    Metrics,
    newMetrics,

    -- * Timing
    timedSeconds,

    -- * Serve decision
    recordServeDecision,

    -- * Rule gate
    recordRuleDenial,
    recordRuleEvalDuration,
    recordRuleEffectfulFailure,
    recordBreakerState,

    -- * Upstream fetch (data plane)
    recordUpstreamFetch,
    recordUpstreamFetchError,

    -- * Metadata cache
    recordCacheRequest,
    recordCacheEntries,

    -- * Mirror
    recordMirrorEnqueued,
    recordMirrorEnqueueFailure,
    recordMirrorJobProcessed,
    recordMirrorPublishDuration,

    -- * Credentials
    recordCredentialRefresh,
    recordCredentialTokenTtl,
) where

import GHC.Clock (getMonotonicTime)

import OpenTelemetry.Metric.Core (
    Counter (counterAdd),
    Gauge (gaugeRecord),
    Histogram (histogramRecord),
    Meter,
    MeterProvider,
    defaultAdvisoryParameters,
    getMeter,
    meterCreateCounterInt64,
    meterCreateGaugeInt64,
    meterCreateHistogram,
    noopMeterProvider,
 )

import Ecluse.Telemetry (Telemetry, telemetryMeterProvider)
import Ecluse.Telemetry.Metrics (
    BreakerSource,
    BreakerState,
    CacheResult,
    Cause,
    CredentialResult,
    Decision,
    Label (LBreakerSource, LCacheResult, LCause, LCredentialResult, LDecision, LMirrorResult, LProvider, LReasonClass, LRule, LStatusClass, LTier, LUpstream),
    MetricName (..),
    MirrorResult,
    Provider,
    ReasonClass,
    StatusClass,
    Tier,
    Upstream,
    breakerStateCode,
    metricAttributes,
    metricName,
 )

-- ── the instrument handle ─────────────────────────────────────────────────────

{- | The live metric instruments, one per @ecluse.*@ signal, created against a single
meter. Opaque: built with 'newMetrics' and recorded through the @record*@ helpers.
Held in the composition root ("Ecluse.Env") so every layer records through the same
instruments.

@http.server.request.duration@ is __not__ here: the WAI instrumentation emits it from
the server-span meter ("Ecluse.Telemetry.Tracing"), so duplicating it would double the
series. Advisory-sync and breaker instruments the catalogue names are present; their
wiring is layered on as the subsystems that own them are built.
-}
data Metrics = Metrics
    { mServeDecision :: Counter Int64
    , mRuleDenials :: Counter Int64
    , mRuleEvalDuration :: Histogram
    , mRuleEffectfulFailures :: Counter Int64
    , mRuleBreakerState :: Gauge Int64
    , mUpstreamFetchDuration :: Histogram
    , mUpstreamFetchErrors :: Counter Int64
    , mMetadataCacheRequests :: Counter Int64
    , mMetadataCacheEntries :: Gauge Int64
    , mMirrorEnqueued :: Counter Int64
    , mMirrorEnqueueFailures :: Counter Int64
    , mMirrorJobsProcessed :: Counter Int64
    , mMirrorPublishDuration :: Histogram
    , mCredentialRefresh :: Counter Int64
    , mCredentialTokenTtlSeconds :: Gauge Int64
    }

{- | Build the metric instruments from a 'Telemetry' handle. When telemetry is enabled
the instruments are created on the handle's meter provider; when it is disabled they
are created on the SDK's no-op meter provider, so every recorded measurement is
discarded and the @record*@ helpers are inert.

Instruments are created once here (at composition) rather than per measurement, so the
hot path only records.
-}
newMetrics :: Telemetry -> IO Metrics
newMetrics telemetry = do
    let meterProvider :: MeterProvider
        meterProvider = fromMaybe noopMeterProvider (telemetryMeterProvider telemetry)
    meter <- getMeter meterProvider ecluseScope
    Metrics
        <$> counter meter ServeDecision "{decision}" "serve decisions by admit/deny/unavailable"
        <*> counter meter RuleDenials "{denial}" "rule denials by rule and reason class"
        <*> histogram meter RuleEvalDuration "rule-evaluation latency by tier"
        <*> counter meter RuleEffectfulFailures "{failure}" "effectful-rule failures by cause"
        <*> gauge meter RuleBreakerState "circuit-breaker state by source (0 closed, 1 half-open, 2 open)"
        <*> histogram meter UpstreamFetchDuration "upstream metadata-fetch latency by upstream and status class"
        <*> counter meter UpstreamFetchErrors "{error}" "upstream metadata-fetch errors by upstream and cause"
        <*> counter meter MetadataCacheRequests "{request}" "metadata-cache lookups by hit/miss"
        <*> gauge meter MetadataCacheEntries "metadata-cache occupancy"
        <*> counter meter MirrorEnqueued "{job}" "mirror jobs enqueued"
        <*> counter meter MirrorEnqueueFailures "{failure}" "mirror enqueue failures"
        <*> counter meter MirrorJobsProcessed "{job}" "mirror jobs processed by result"
        <*> histogram meter MirrorPublishDuration "mirror publish latency"
        <*> counter meter CredentialRefresh "{refresh}" "credential refreshes by result and provider"
        <*> gauge meter CredentialTokenTtlSeconds "remaining outbound-token lifetime by provider"
  where
    counter :: Meter -> MetricName -> Text -> Text -> IO (Counter Int64)
    counter meter name unit description =
        meterCreateCounterInt64 meter (metricName name) (Just unit) (Just description) defaultAdvisoryParameters

    histogram :: Meter -> MetricName -> Text -> IO Histogram
    histogram meter name description =
        meterCreateHistogram meter (metricName name) (Just "s") (Just description) defaultAdvisoryParameters

    gauge :: Meter -> MetricName -> Text -> IO (Gauge Int64)
    gauge meter name description =
        meterCreateGaugeInt64 meter (metricName name) Nothing (Just description) defaultAdvisoryParameters

-- The instrumentation scope the instruments are created under: this service's name,
-- so the metrics are attributed to Écluse (the same scope the hand-added spans use).
-- Kept polymorphic over 'IsString' so the @InstrumentationLibrary@ type need not be
-- named (it is not exported from the metric API surface).
ecluseScope :: (IsString s) => s
ecluseScope = "ecluse"

-- ── timing ─────────────────────────────────────────────────────────────────────

{- | Run an action and return its result alongside the wall-clock seconds it took,
measured on the monotonic clock so a system-clock step never yields a negative or
absurd duration. The seconds are what the latency histograms record.
-}
timedSeconds :: (MonadIO m) => m a -> m (a, Double)
timedSeconds action = do
    start <- liftIO getMonotonicTime
    result <- action
    end <- liftIO getMonotonicTime
    pure (result, end - start)

-- ── serve decision ───────────────────────────────────────────────────────────

-- | Record one serve decision (@ecluse.serve.decision@): admit, deny, or unavailable.
recordServeDecision :: (MonadIO m) => Metrics -> Decision -> m ()
recordServeDecision m decision =
    addOne (mServeDecision m) [LDecision decision]

-- ── rule gate ─────────────────────────────────────────────────────────────────

{- | Record one rule denial (@ecluse.rule.denials@) by reason class and, for a policy
denial, the deciding rule. A non-policy refusal (a missing-integrity or upstream cause)
carries the reason class alone — no rule attributed it, so none is labelled.
-}
recordRuleDenial :: (MonadIO m) => Metrics -> Maybe Text -> ReasonClass -> m ()
recordRuleDenial m rule reasonClass =
    addOne (mRuleDenials m) (maybe [] (\name -> [LRule name]) rule <> [LReasonClass reasonClass])

-- | Record a rule-evaluation latency sample (@ecluse.rule.eval.duration@) by tier.
recordRuleEvalDuration :: (MonadIO m) => Metrics -> Tier -> Double -> m ()
recordRuleEvalDuration m tier seconds =
    record (mRuleEvalDuration m) seconds [LTier tier]

-- | Record one effectful-rule failure (@ecluse.rule.effectful.failures@) by cause.
recordRuleEffectfulFailure :: (MonadIO m) => Metrics -> Cause -> m ()
recordRuleEffectfulFailure m cause =
    addOne (mRuleEffectfulFailures m) [LCause cause]

{- | Record the current circuit-breaker state (@ecluse.rule.breaker.state@) for a
source as the gauge's bounded ordinal (0 closed, 1 half-open, 2 open).
-}
recordBreakerState :: (MonadIO m) => Metrics -> BreakerSource -> BreakerState -> m ()
recordBreakerState m source breakerState =
    set (mRuleBreakerState m) (breakerStateCode breakerState) [LBreakerSource source]

-- ── upstream fetch (data plane) ───────────────────────────────────────────────

{- | Record an upstream metadata-fetch latency sample (@ecluse.upstream.fetch.duration@)
by which upstream was fetched and the response's status class.
-}
recordUpstreamFetch :: (MonadIO m) => Metrics -> Upstream -> StatusClass -> Double -> m ()
recordUpstreamFetch m upstream statusClass seconds =
    record (mUpstreamFetchDuration m) seconds [LUpstream upstream, LStatusClass statusClass]

{- | Record one upstream metadata-fetch error (@ecluse.upstream.fetch.errors@) by
which upstream and the bounded cause.
-}
recordUpstreamFetchError :: (MonadIO m) => Metrics -> Upstream -> Cause -> m ()
recordUpstreamFetchError m upstream cause =
    addOne (mUpstreamFetchErrors m) [LUpstream upstream, LCause cause]

-- ── metadata cache ────────────────────────────────────────────────────────────

-- | Record one metadata-cache lookup (@ecluse.metadata_cache.requests@) as a hit or miss.
recordCacheRequest :: (MonadIO m) => Metrics -> CacheResult -> m ()
recordCacheRequest m result =
    addOne (mMetadataCacheRequests m) [LCacheResult result]

-- | Record the metadata cache's current occupancy (@ecluse.metadata_cache.entries@).
recordCacheEntries :: (MonadIO m) => Metrics -> Int -> m ()
recordCacheEntries m entries =
    set (mMetadataCacheEntries m) (fromIntegral entries) []

-- ── mirror ─────────────────────────────────────────────────────────────────────

-- | Record one mirror job enqueued (@ecluse.mirror.enqueued@).
recordMirrorEnqueued :: (MonadIO m) => Metrics -> m ()
recordMirrorEnqueued m = addOne (mMirrorEnqueued m) []

-- | Record one mirror enqueue failure (@ecluse.mirror.enqueue.failures@).
recordMirrorEnqueueFailure :: (MonadIO m) => Metrics -> m ()
recordMirrorEnqueueFailure m = addOne (mMirrorEnqueueFailures m) []

-- | Record one processed mirror job (@ecluse.mirror.jobs.processed@) by its result.
recordMirrorJobProcessed :: (MonadIO m) => Metrics -> MirrorResult -> m ()
recordMirrorJobProcessed m result =
    addOne (mMirrorJobsProcessed m) [LMirrorResult result]

-- | Record a mirror publish latency sample (@ecluse.mirror.publish.duration@).
recordMirrorPublishDuration :: (MonadIO m) => Metrics -> Double -> m ()
recordMirrorPublishDuration m seconds =
    record (mMirrorPublishDuration m) seconds []

-- ── credentials ────────────────────────────────────────────────────────────────

-- | Record one credential refresh (@ecluse.credential.refresh@) by result and provider.
recordCredentialRefresh :: (MonadIO m) => Metrics -> Provider -> CredentialResult -> m ()
recordCredentialRefresh m provider result =
    addOne (mCredentialRefresh m) [LProvider provider, LCredentialResult result]

{- | Record an outbound token's remaining lifetime in whole seconds
(@ecluse.credential.token.ttl.seconds@) by provider, so a stuck refresh alarms as the
gauge decays towards zero.
-}
recordCredentialTokenTtl :: (MonadIO m) => Metrics -> Provider -> Int -> m ()
recordCredentialTokenTtl m provider seconds =
    set (mCredentialTokenTtlSeconds m) (fromIntegral seconds) [LProvider provider]

-- ── recording primitives ─────────────────────────────────────────────────────

-- Add one to a counter under the given bounded labels.
addOne :: (MonadIO m) => Counter Int64 -> [Label] -> m ()
addOne instrument labels = liftIO (counterAdd instrument 1 (metricAttributes labels))

-- Record a histogram measurement under the given bounded labels.
record :: (MonadIO m) => Histogram -> Double -> [Label] -> m ()
record instrument value labels = liftIO (histogramRecord instrument value (metricAttributes labels))

-- Set a gauge to a value under the given bounded labels (last value wins per collect).
set :: (MonadIO m) => Gauge Int64 -> Int64 -> [Label] -> m ()
set instrument value labels = liftIO (gaugeRecord instrument value (metricAttributes labels))
