-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The bounded-label discipline for the @ecluse.*@ metrics. An inline proxy sees thousands of
distinct packages, so one high-cardinality label turns a handful of series into millions.
'Label' is a closed sum over bounded-domain keys and values: @package@, @version@, @scope@, and
a denial @message@ have no constructor, so the type keeps them off a metric, and they ride the
spans and the log line instead. @rule@ is the exception, bounded by a deployment's own rule set
rather than by an enum. The instruments these label are in
"Ecluse.Core.Telemetry.Catalogue".
-}
module Ecluse.Core.Telemetry.Metrics (
    -- * Label keys (the closed set)
    LabelKey (..),
    labelKeyName,

    -- * Bounded label values
    Decision (..),
    ReasonClass (..),
    Upstream (..),
    StatusClass (..),
    Provider (..),
    Cause (..),
    Tier (..),
    CacheResult (..),
    MirrorResult (..),
    SweepResult (..),
    SweepTarget (..),
    CredentialResult (..),
    AdvisorySyncResult (..),
    advisorySyncResultName,
    AdvisoryDropCause (..),
    AdvisoryCompileResult (..),
    BreakerSource (..),
    RequestFaultCause (..),
    RelayAnomaly (..),

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

-- relude's prelude exports a Bounded/Enum-based `universe`. Hide it so the
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

{- | The closed set of metric label keys. The high-cardinality identifiers (@package@,
@version@, @scope@, a denial @message@) have no key, so they can never become a label.
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
    | KeyTarget
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
    KeyTarget -> "target"
    KeyProvider -> "provider"
    KeyCause -> "cause"
    KeyBreakerSource -> "source"
    KeyTier -> "tier"

-- | The serve decision (@ecluse.serve.decision@).
data Decision = Admit | Deny | Unavailable
    deriving stock (Eq, Generic, Show)

instance Universe Decision where universe = universeGeneric

{- | The bucketed class of a denial reason. Not the rule name or the message, which are
high-cardinality and stay on the log line.
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

{- | The store a mirror-write credential's refresh\/ttl signal concerns: one value per store
tag the configuration admits, so a dashboard and a mount's declaration spell the same word.
-}
data Provider = ProviderRegistry | ProviderCodeArtifact | ProviderVerdaccio
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

{- | Why the request perimeter had to answer for an escaped fault. The unbounded detail rides
the perimeter's log line, never a label.
-}
data RequestFaultCause = RenderFault | UnclassifiedFault
    deriving stock (Eq, Generic, Show)

instance Universe RequestFaultCause where universe = universeGeneric

{- | What a public artifact relay passed through when it did not carry the admitted artifact:
a 2xx that does not look like one, or a non-success relayed verbatim.
-}
data RelayAnomaly = RelayOddShape | RelayNonSuccess
    deriving stock (Eq, Generic, Show)

instance Universe RelayAnomaly where universe = universeGeneric

-- | A metadata-cache lookup result.
data CacheResult = Hit | Miss
    deriving stock (Eq, Generic, Show)

instance Universe CacheResult where universe = universeGeneric

{- | A processed mirror job's result. The idempotent "already present" outcome (a registry
@409@) counts as 'Published', not as a distinct value.
-}
data MirrorResult
    = -- | The artifact reached the mirror target (an already-present version included).
      Published
    | -- | The job did not publish, and its message stays in the queue's own hands.
      Failed
    | {- | The worker retired the message itself, once it spent the queue's redelivery budget.
      The terminus when no dead-letter queue exists, so an operator alerts on it.
      -}
      Discarded
    deriving stock (Eq, Generic, Show)

instance Universe MirrorResult where universe = universeGeneric

-- | The bounded registry role of a sweep observation or operation.
data SweepTarget
    = -- | The source that receives mirrored packages.
      SweepMirror
    | -- | The associated cache used for private reads.
      SweepPrivate
    deriving stock (Eq, Generic, Ord, Show)

instance Universe SweepTarget where universe = universeGeneric

-- | The disposition of one observed version or logical preview selection.
data SweepResult
    = -- | The sweep evaluated the version.
      SweepExamined
    | -- | A named decisive deny removed it.
      SweepDeleted
    | -- | A named decisive deny would have removed it, under a dry run.
      SweepWouldDelete
    | -- | Nothing decisively denied it, so it stays.
      SweepKept
    | -- | A safety control held it back: the first-party belt, or the cycle's deletion cap.
      SweepGuardSkipped
    deriving stock (Eq, Generic, Show)

instance Universe SweepResult where universe = universeGeneric

-- | A credential-refresh result.
data CredentialResult = Refreshed | RefreshFailed
    deriving stock (Eq, Generic, Show)

instance Universe CredentialResult where universe = universeGeneric

{- | What one advisory sync attempt concluded. It labels the @ecluse.advisory.sync.*@ signals
and the sync span alike.
-}
data AdvisorySyncResult
    = -- | The sync verified a new artifact and swapped it into the read path.
      AdvisorySwapped
    | -- | The remote artifact matches the last seen one.
      AdvisoryUnchanged
    | -- | No artifact exists in the bucket yet.
      AdvisoryNonePublished
    | -- | The fetch itself did not deliver the object.
      AdvisoryFetchFailed
    | -- | Verification refused the downloaded artifact.
      AdvisoryRefused
    deriving stock (Eq, Generic, Show)

instance Universe AdvisorySyncResult where universe = universeGeneric

{- | The wire value of an advisory sync result. The metric label and the span attribute must
read identically, so the two signals join on it.
-}
advisorySyncResultName :: AdvisorySyncResult -> Text
advisorySyncResultName = \case
    AdvisorySwapped -> "swapped"
    AdvisoryUnchanged -> "unchanged"
    AdvisoryNonePublished -> "none_published"
    AdvisoryFetchFailed -> "fetch_failed"
    AdvisoryRefused -> "refused"

{- | Why a compile pass dropped one advisory entry. The entry's own name and bytes stay on the
drop log line, never a label.
-}
data AdvisoryDropCause
    = -- | The entry breached the per-advisory byte cap.
      DropOversize
    | -- | The entry's JSON did not decode.
      DropMalformed
    deriving stock (Eq, Generic, Show)

instance Universe AdvisoryDropCause where universe = universeGeneric

{- | What one compile pass concluded. A pass that never concluded, because a fetch or a
filesystem fault escaped it, records neither value.
-}
data AdvisoryCompileResult
    = -- | The pass finalised an artifact.
      CompileCompleted
    | -- | The pass abandoned the artifact over a systemic drop rate.
      CompileAborted
    deriving stock (Eq, Generic, Show)

instance Universe AdvisoryCompileResult where universe = universeGeneric

-- | Which circuit breaker a state gauge concerns.
data BreakerSource = EffectfulRule | CredentialMint
    deriving stock (Eq, Generic, Show)

instance Universe BreakerSource where universe = universeGeneric

{- | The circuit-breaker state, recorded as the @ecluse.rule.breaker.state@ gauge's value
(labelled by 'BreakerSource'). It is a bounded measurement, not a label.
-}
data BreakerState = Closed | HalfOpen | Open
    deriving stock (Eq, Generic, Show)

instance Universe BreakerState where universe = universeGeneric

{- | The gauge code for a breaker state. Closed is @0@, so a dashboard alarms on "not
closed" without a high-cardinality label.
-}
breakerStateCode :: BreakerState -> Int64
breakerStateCode = \case
    Closed -> 0
    HalfOpen -> 1
    Open -> 2

{- | A single metric label. No constructor takes a package, version, scope, or message. 'LRule'
is the one operator-bounded label, since a deployment defines a small, fixed rule set.
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
    | LSweepResult SweepResult
    | LSweepTarget SweepTarget
    | LCredentialResult CredentialResult
    | LAdvisorySyncResult AdvisorySyncResult
    | LAdvisoryCompileResult AdvisoryCompileResult
    | LAdvisoryDropCause AdvisoryDropCause
    | LProvider Provider
    | LCause Cause
    | LBreakerSource BreakerSource
    | LTier Tier
    | LPerimeterCause RequestFaultCause
    | LRelayAnomaly RelayAnomaly
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
    LSweepResult{} -> KeyResult
    LSweepTarget{} -> KeyTarget
    LCredentialResult{} -> KeyResult
    LAdvisorySyncResult{} -> KeyResult
    LAdvisoryCompileResult{} -> KeyResult
    LAdvisoryDropCause{} -> KeyCause
    LProvider{} -> KeyProvider
    LCause{} -> KeyCause
    LBreakerSource{} -> KeyBreakerSource
    LTier{} -> KeyTier
    LPerimeterCause{} -> KeyCause
    LRelayAnomaly{} -> KeyCause

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
        Discarded -> "discarded"
    LSweepTarget target -> case target of
        SweepMirror -> "mirrorTarget"
        SweepPrivate -> "privateUpstream"
    LSweepResult r -> case r of
        SweepExamined -> "examined"
        SweepDeleted -> "deleted"
        SweepWouldDelete -> "would_delete"
        SweepKept -> "kept"
        SweepGuardSkipped -> "guard_skipped"
    LCredentialResult c -> case c of
        Refreshed -> "refreshed"
        RefreshFailed -> "failed"
    LAdvisorySyncResult r -> advisorySyncResultName r
    LAdvisoryCompileResult r -> case r of
        CompileCompleted -> "completed"
        CompileAborted -> "aborted"
    LAdvisoryDropCause c -> case c of
        DropOversize -> "oversize"
        DropMalformed -> "malformed"
    LProvider p -> case p of
        ProviderRegistry -> "registry"
        ProviderCodeArtifact -> "codeArtifact"
        ProviderVerdaccio -> "verdaccio"
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
    LPerimeterCause c -> case c of
        RenderFault -> "render"
        UnclassifiedFault -> "unclassified"
    LRelayAnomaly a -> case a of
        RelayOddShape -> "odd_shape"
        RelayNonSuccess -> "non_success"

-- | Materialise bounded labels into the attributes recorded by an OpenTelemetry instrument.
metricAttributes :: [Label] -> Attributes
metricAttributes labels =
    addAttributesFromBuilder
        defaultAttributeLimits
        emptyAttributes
        (foldMap (\label -> let (key, value) = renderLabel label in attr key value) labels)
