{- | The @ecluse.*@ metric catalogue and its __bounded-label discipline__.

An inline proxy sees thousands of distinct packages, so the failure mode for metrics
is a __series explosion__: a single high-cardinality label (a package name, a version,
a denial message) multiplied across every package turns a handful of series into
millions. This module is the structural defence. It defines the catalogue of metric
__names__ and, crucially, the __closed set of label types__ a metric may carry -- every
one a small, fixed-domain enum.

== Bounded labels

The label vocabulary is a closed sum, 'Label', whose every constructor pairs a
bounded-domain key with a bounded value. High-cardinality identifiers -- @package@,
@version@, @scope@, and a denial @message@ -- have __no constructor here at all__, so
they cannot be made into a metric label: the type system forbids it. They live on
spans and the structured log line ("Ecluse.Log") instead, which is where a specific
decision is debugged. The one operator-bounded label is @rule@ (a rule's configured
name): a deployment defines a small, fixed set of rules, so it is bounded by
configuration rather than by an enum, and is the sole label carrying free text.

'renderLabel' projects a 'Label' to its @(key, value)@ wire pair, and 'metricAttributes'
materialises a label list into the OpenTelemetry 'Attributes' an instrument is recorded
with. The catalogue and the cardinality rule are described in
@docs\/architecture\/observability.md@.
-}
module Ecluse.Core.Telemetry.Metrics (
    -- * The metric-name catalogue
    MetricName (..),
    metricName,
    allMetricNames,

    -- * Label keys (the closed set)
    LabelKey (..),
    labelKeyName,
    allLabelKeys,
    highCardinalityKeys,

    -- * Bounded label values
    Decision (..),
    ReasonClass (..),
    Upstream (..),
    StatusClass (..),
    statusClassOf,
    Provider (..),
    Cause (..),
    Tier (..),
    CacheResult (..),
    MirrorResult (..),
    CredentialResult (..),
    BreakerSource (..),

    -- * Breaker state (a bounded gauge value, not a label)
    BreakerState (..),
    breakerStateCode,

    -- * Labels
    Label (..),
    labelKey,
    renderLabel,

    -- * Attribute construction
    metricAttributes,
) where

-- relude's prelude exports a Bounded/Enum-based `universe`; hide it so the
-- Generic-derived `Data.Universe.Class.universe` is the one in scope here.
import Prelude hiding (universe)

import OpenTelemetry.Attributes (
    Attributes,
    addAttributesFromBuilder,
    attr,
    defaultAttributeLimits,
    emptyAttributes,
 )

import Data.Universe.Class (Universe (..))
import Data.Universe.Generic (universeGeneric)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)

{- | The catalogue of metric instruments Écluse emits: the @ecluse.*@ domain signals
plus the OpenTelemetry HTTP server semantic convention. Each maps to its wire name
through 'metricName'; a typed enum so the catalogue is enumerable (and asserted whole
in the tests) rather than a scatter of string literals.

Queue backlog and DLQ depth are deliberately absent -- those are cloud-native metrics
(CloudWatch, Cloud Monitoring), not signals Écluse re-emits. Advisory-sync metrics are
deferred until the advisory subsystem exists.
-}
data MetricName
    = -- | @http.server.request.duration@ -- server request latency (histogram).
      HttpServerRequestDuration
    | -- | @ecluse.serve.decision@ -- admit\/deny\/unavailable (counter).
      ServeDecision
    | -- | @ecluse.rule.denials@ -- rule denials by rule and reason class (counter).
      RuleDenials
    | -- | @ecluse.rule.eval.duration@ -- rule-evaluation latency by tier (histogram).
      RuleEvalDuration
    | -- | @ecluse.rule.effectful.failures@ -- effectful-rule failures (counter).
      RuleEffectfulFailures
    | -- | @ecluse.rule.breaker.state@ -- effectful\/mint breaker state by source (gauge).
      RuleBreakerState
    | -- | @ecluse.serve.admission.in_flight@ -- in-flight metadata parses (up-down counter).
      ServeAdmissionInFlight
    | -- | @ecluse.serve.admission.queued@ -- admissions that waited for a slot (counter).
      ServeAdmissionQueued
    | -- | @ecluse.registry.merge.divergence@ -- cross-upstream integrity divergences detected in the packument merge (counter).
      MergeDivergence
    | -- | @ecluse.upstream.fetch.duration@ -- upstream fetch latency (histogram).
      UpstreamFetchDuration
    | -- | @ecluse.upstream.fetch.errors@ -- upstream fetch errors (counter).
      UpstreamFetchErrors
    | -- | @ecluse.metadata_cache.requests@ -- metadata-cache hit\/miss (counter).
      MetadataCacheRequests
    | -- | @ecluse.metadata_cache.entries@ -- metadata-cache occupancy (gauge).
      MetadataCacheEntries
    | -- | @ecluse.metadata_cache.resident_bytes@: full-packument cache resident bytes (gauge).
      MetadataCacheResidentBytes
    | -- | @ecluse.metadata_cache.version.resident_bytes@: single-version cache resident bytes (gauge).
      SingleVersionCacheResidentBytes
    | -- | @ecluse.metadata_cache.assembled.resident_bytes@: assembled-representation store resident bytes (gauge).
      AssembledCacheResidentBytes
    | -- | @ecluse.mirror.enqueued@ -- mirror jobs enqueued (counter).
      MirrorEnqueued
    | -- | @ecluse.mirror.enqueue.failures@ -- mirror enqueue failures (counter).
      MirrorEnqueueFailures
    | -- | @ecluse.mirror.jobs.processed@ -- mirror jobs processed by result (counter).
      MirrorJobsProcessed
    | -- | @ecluse.mirror.publish.duration@ -- mirror publish latency (histogram).
      MirrorPublishDuration
    | -- | @ecluse.credential.refresh@ -- credential refreshes by result and provider (counter).
      CredentialRefresh
    | -- | @ecluse.credential.token.ttl.seconds@ -- remaining token lifetime by provider (gauge).
      CredentialTokenTtlSeconds
    deriving stock (Eq, Generic, Ord, Show)

instance Universe MetricName where universe = universeGeneric

-- | The wire name of a 'MetricName'.
metricName :: MetricName -> Text
metricName = \case
    HttpServerRequestDuration -> "http.server.request.duration"
    ServeDecision -> "ecluse.serve.decision"
    RuleDenials -> "ecluse.rule.denials"
    RuleEvalDuration -> "ecluse.rule.eval.duration"
    RuleEffectfulFailures -> "ecluse.rule.effectful.failures"
    RuleBreakerState -> "ecluse.rule.breaker.state"
    ServeAdmissionInFlight -> "ecluse.serve.admission.in_flight"
    ServeAdmissionQueued -> "ecluse.serve.admission.queued"
    MergeDivergence -> "ecluse.registry.merge.divergence"
    UpstreamFetchDuration -> "ecluse.upstream.fetch.duration"
    UpstreamFetchErrors -> "ecluse.upstream.fetch.errors"
    MetadataCacheRequests -> "ecluse.metadata_cache.requests"
    MetadataCacheEntries -> "ecluse.metadata_cache.entries"
    MetadataCacheResidentBytes -> "ecluse.metadata_cache.resident_bytes"
    SingleVersionCacheResidentBytes -> "ecluse.metadata_cache.version.resident_bytes"
    AssembledCacheResidentBytes -> "ecluse.metadata_cache.assembled.resident_bytes"
    MirrorEnqueued -> "ecluse.mirror.enqueued"
    MirrorEnqueueFailures -> "ecluse.mirror.enqueue.failures"
    MirrorJobsProcessed -> "ecluse.mirror.jobs.processed"
    MirrorPublishDuration -> "ecluse.mirror.publish.duration"
    CredentialRefresh -> "ecluse.credential.refresh"
    CredentialTokenTtlSeconds -> "ecluse.credential.token.ttl.seconds"

-- | Every metric in the catalogue (the Generic-derived 'Universe' enumeration).
allMetricNames :: [MetricName]
allMetricNames = universe

{- | The closed set of metric label keys. Every label Écluse attaches is one of these
bounded-domain keys. High-cardinality identifiers (@package@, @version@, @scope@, a
denial @message@) are deliberately __absent__ -- see 'highCardinalityKeys' -- so they
can never become a metric label.
-}
data LabelKey
    = KeyDecision
    | KeyReasonClass
    | KeyRule
    | KeyEcosystem
    | KeyMount
    | KeyUpstream
    | KeyStatusClass
    | KeyResult
    | KeyProvider
    | KeyCause
    | KeyBreakerSource
    | KeyTier
    deriving stock (Eq, Generic, Ord, Show)

instance Universe LabelKey where universe = universeGeneric

-- | The wire name of a 'LabelKey'.
labelKeyName :: LabelKey -> Text
labelKeyName = \case
    KeyDecision -> "decision"
    KeyReasonClass -> "reason_class"
    KeyRule -> "rule"
    KeyEcosystem -> "ecosystem"
    KeyMount -> "mount"
    KeyUpstream -> "upstream"
    KeyStatusClass -> "status_class"
    KeyResult -> "result"
    KeyProvider -> "provider"
    KeyCause -> "cause"
    KeyBreakerSource -> "source"
    KeyTier -> "tier"

-- | Every label key in the closed set.
allLabelKeys :: [LabelKey]
allLabelKeys = universe

{- | The high-cardinality identifiers that must __never__ be metric labels: they live
on spans and the structured log line instead. The label-domain guard asserts none of
these is a 'LabelKey' wire name; there is, by construction, no 'Label' that produces one.
-}
highCardinalityKeys :: [Text]
highCardinalityKeys = ["package", "version", "scope", "message"]

-- | The serve decision (@ecluse.serve.decision@).
data Decision = Admit | Deny | Unavailable
    deriving stock (Eq, Generic, Show)

instance Universe Decision where universe = universeGeneric

{- | The bucketed class of a denial reason -- a bounded summary of
"Ecluse.Core.Server.Response.RejectReason", __not__ the rule name or the message (those are
high-cardinality and stay on the log line).
-}
data ReasonClass = ReasonPolicy | ReasonMissingIntegrity | ReasonUnavailable | ReasonLimit
    deriving stock (Eq, Generic, Show)

instance Universe ReasonClass where universe = universeGeneric

-- | Which upstream a data-plane fetch targeted.
data Upstream = Private | Public
    deriving stock (Eq, Generic, Show)

instance Universe Upstream where universe = universeGeneric

-- | The HTTP status class of an upstream response (the bounded summary of the code).
data StatusClass = Status2xx | Status3xx | Status4xx | Status5xx | StatusOther
    deriving stock (Eq, Generic, Show)

instance Universe StatusClass where universe = universeGeneric

-- | The outbound-credential provider a refresh\/ttl signal concerns.
data Provider = CodeArtifact | Static | Adc
    deriving stock (Eq, Generic, Show)

instance Universe Provider where universe = universeGeneric

-- | A bounded error class for a failure signal (never the exception text).
data Cause = Timeout | Connection | Decode | UpstreamStatus | OtherCause
    deriving stock (Eq, Generic, Show)

instance Universe Cause where universe = universeGeneric

-- | The rule-evaluation tier a duration is measured at.
data Tier = Structural | Effectful
    deriving stock (Eq, Generic, Show)

instance Universe Tier where universe = universeGeneric

-- | A metadata-cache lookup result.
data CacheResult = Hit | Miss
    deriving stock (Eq, Generic, Show)

instance Universe CacheResult where universe = universeGeneric

{- | A processed mirror job's result. The idempotent "already present" outcome (a
registry @409@) is __not__ a distinct value: the worker treats it as a success, so it is
counted as 'Published' -- a series that could never emit is not published.
-}
data MirrorResult = Published | Failed
    deriving stock (Eq, Generic, Show)

instance Universe MirrorResult where universe = universeGeneric

-- | A credential-refresh result.
data CredentialResult = Refreshed | RefreshFailed
    deriving stock (Eq, Generic, Show)

instance Universe CredentialResult where universe = universeGeneric

-- | Which circuit breaker a state gauge concerns.
data BreakerSource = EffectfulRule | CredentialMint
    deriving stock (Eq, Generic, Show)

instance Universe BreakerSource where universe = universeGeneric

{- | The circuit-breaker state, recorded as the value of the @ecluse.rule.breaker.state@
gauge (labelled by 'BreakerSource'). It is a bounded measurement, not a label.
-}
data BreakerState = Closed | HalfOpen | Open
    deriving stock (Eq, Generic, Show)

instance Universe BreakerState where universe = universeGeneric

{- | The gauge code for a breaker state: @0@ closed, @1@ half-open, @2@ open -- a small
ordinal so a dashboard can alarm on "not closed" without a high-cardinality label.
-}
breakerStateCode :: BreakerState -> Int64
breakerStateCode = \case
    Closed -> 0
    HalfOpen -> 1
    Open -> 2

{- | A single metric label: a bounded key paired with its bounded value. There is no
constructor for a package, version, scope, or message, so a high-cardinality identifier
cannot be turned into a label. 'LRule' carries a rule's configured name -- the one
operator-bounded label (a deployment defines a small, fixed rule set).
-}
data Label
    = LDecision Decision
    | LReasonClass ReasonClass
    | LRule Text
    | LEcosystem Ecosystem
    | LMount Ecosystem
    | LUpstream Upstream
    | LStatusClass StatusClass
    | LCacheResult CacheResult
    | LMirrorResult MirrorResult
    | LCredentialResult CredentialResult
    | LProvider Provider
    | LCause Cause
    | LBreakerSource BreakerSource
    | LTier Tier
    deriving stock (Eq, Show)

-- | The 'LabelKey' a 'Label' is filed under.
labelKey :: Label -> LabelKey
labelKey = \case
    LDecision{} -> KeyDecision
    LReasonClass{} -> KeyReasonClass
    LRule{} -> KeyRule
    LEcosystem{} -> KeyEcosystem
    LMount{} -> KeyMount
    LUpstream{} -> KeyUpstream
    LStatusClass{} -> KeyStatusClass
    LCacheResult{} -> KeyResult
    LMirrorResult{} -> KeyResult
    LCredentialResult{} -> KeyResult
    LProvider{} -> KeyProvider
    LCause{} -> KeyCause
    LBreakerSource{} -> KeyBreakerSource
    LTier{} -> KeyTier

-- | Project a 'Label' to its @(key, value)@ wire pair.
renderLabel :: Label -> (Text, Text)
renderLabel label = (labelKeyName (labelKey label), labelValue label)

labelValue :: Label -> Text
labelValue = \case
    LDecision d -> case d of
        Admit -> "admit"
        Deny -> "deny"
        Unavailable -> "unavailable"
    LReasonClass r -> case r of
        ReasonPolicy -> "policy"
        ReasonMissingIntegrity -> "missing_integrity"
        ReasonUnavailable -> "unavailable"
        ReasonLimit -> "limit"
    LRule name -> name
    LEcosystem eco -> ecosystemName eco
    LMount eco -> ecosystemName eco
    LUpstream u -> case u of
        Private -> "private"
        Public -> "public"
    LStatusClass s -> case s of
        Status2xx -> "2xx"
        Status3xx -> "3xx"
        Status4xx -> "4xx"
        Status5xx -> "5xx"
        StatusOther -> "other"
    LCacheResult c -> case c of
        Hit -> "hit"
        Miss -> "miss"
    LMirrorResult m -> case m of
        Published -> "published"
        Failed -> "failed"
    LCredentialResult c -> case c of
        Refreshed -> "refreshed"
        RefreshFailed -> "failed"
    LProvider p -> case p of
        CodeArtifact -> "codeartifact"
        Static -> "static"
        Adc -> "adc"
    LCause c -> case c of
        Timeout -> "timeout"
        Connection -> "connection"
        Decode -> "decode"
        UpstreamStatus -> "upstream_status"
        OtherCause -> "other"
    LBreakerSource b -> case b of
        EffectfulRule -> "effectful_rule"
        CredentialMint -> "credential_mint"
    LTier t -> case t of
        Structural -> "structural"
        Effectful -> "effectful"

{- | Classify an HTTP status code into its bounded 'StatusClass', so a status never
becomes a per-code label.
-}
statusClassOf :: Int -> StatusClass
statusClassOf code
    | code >= 200 && code < 300 = Status2xx
    | code >= 300 && code < 400 = Status3xx
    | code >= 400 && code < 500 = Status4xx
    | code >= 500 && code < 600 = Status5xx
    | otherwise = StatusOther

{- | Materialise a label list into the OpenTelemetry 'Attributes' an instrument is
recorded with. Every value is bounded, so the attribute set an instrument ever sees is
drawn from a small fixed product of the label domains -- never the unbounded space of
package identifiers.
-}
metricAttributes :: [Label] -> Attributes
metricAttributes labels =
    addAttributesFromBuilder
        defaultAttributeLimits
        emptyAttributes
        (foldMap (\label -> let (key, value) = renderLabel label in attr key value) labels)
