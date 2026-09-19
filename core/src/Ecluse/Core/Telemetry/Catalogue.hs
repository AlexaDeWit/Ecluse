-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @ecluse.*@ instrument catalogue: which metrics this build emits, and their wire names.

The catalogue is closed and its 'Universe' instance enumerates it, so the runtime creates one
instrument per name and a new metric cannot reach a meter unregistered. What may label an
instrument is in "Ecluse.Core.Telemetry.Metrics". @docs\/architecture\/observability.md@ holds
the catalogue as prose.
-}
module Ecluse.Core.Telemetry.Catalogue (
    MetricName (..),
    metricName,
) where

-- relude's prelude exports a Bounded/Enum-based `universe`. Hide it so the
-- Generic-derived `Data.Universe.Class.universe` is the one in scope here.
import Prelude hiding (universe)

import Data.Universe.Class (Universe (..))
import Data.Universe.Generic (universeGeneric)

{- | The catalogue of metric instruments Écluse emits. Queue backlog and DLQ depth are absent
on purpose: those are cloud-native metrics, not signals Écluse re-emits.
-}
data MetricName
    = -- | @http.server.request.duration@: server request latency (histogram).
      HttpServerRequestDuration
    | -- | @ecluse.serve.decision@: admit\/deny\/unavailable (counter).
      ServeDecision
    | -- | @ecluse.rule.denials@: rule denials by rule and reason class (counter).
      RuleDenials
    | -- | @ecluse.rule.eval.duration@: rule-evaluation latency by tier (histogram).
      RuleEvalDuration
    | -- | @ecluse.rule.effectful.failures@: effectful-rule failures (counter).
      RuleEffectfulFailures
    | -- | @ecluse.rule.breaker.state@: effectful\/mint breaker state by source (gauge).
      RuleBreakerState
    | -- | @ecluse.serve.admission.in_flight@: in-flight metadata parses (up-down counter).
      ServeAdmissionInFlight
    | -- | @ecluse.serve.admission.queued@: admissions that waited for a slot (counter).
      ServeAdmissionQueued
    | -- | @ecluse.publish.body.in_flight_bytes@: bytes reserved for buffered publish bodies (up-down counter).
      PublishBodyInFlightBytes
    | -- | @ecluse.publish.body.shed@: publishes shed at the body-byte budget (counter).
      PublishBodyShed
    | -- | @ecluse.registry.merge.divergence@: cross-upstream integrity divergences detected in the packument merge (counter).
      MergeDivergence
    | -- | @ecluse.upstream.fetch.duration@: upstream fetch latency (histogram).
      UpstreamFetchDuration
    | -- | @ecluse.upstream.fetch.errors@: upstream fetch errors (counter).
      UpstreamFetchErrors
    | -- | @ecluse.metadata_cache.requests@: metadata-cache hit\/miss (counter).
      MetadataCacheRequests
    | -- | @ecluse.metadata_cache.entries@: metadata-cache occupancy (gauge).
      MetadataCacheEntries
    | -- | @ecluse.metadata_cache.resident_bytes@: full-packument cache resident bytes (gauge).
      MetadataCacheResidentBytes
    | -- | @ecluse.metadata_cache.version.resident_bytes@: single-version cache resident bytes (gauge).
      SingleVersionCacheResidentBytes
    | -- | @ecluse.metadata_cache.assembled.resident_bytes@: assembled-representation store resident bytes (gauge).
      AssembledCacheResidentBytes
    | -- | @ecluse.serve.perimeter.faults@: pre-commit handler escapes the request perimeter answered (counter).
      ServePerimeterFaults
    | -- | @ecluse.serve.relay.anomalies@: public relays that were not the admitted artifact (counter).
      ServeRelayAnomalies
    | -- | @ecluse.mirror.enqueued@: mirror jobs enqueued (counter).
      MirrorEnqueued
    | -- | @ecluse.mirror.enqueue.failures@: mirror enqueue failures (counter).
      MirrorEnqueueFailures
    | -- | @ecluse.mirror.jobs.processed@: mirror jobs processed by result (counter).
      MirrorJobsProcessed
    | -- | @ecluse.mirror.publish.duration@: mirror publish latency (histogram).
      MirrorPublishDuration
    | -- | @ecluse.dredger.versions@: mirror-store versions one sweep cycle disposed of, by result (counter).
      DredgerVersions
    | -- | @ecluse.credential.refresh@: credential refreshes by result and provider (counter).
      CredentialRefresh
    | -- | @ecluse.credential.token.ttl.seconds@: remaining token lifetime by provider (gauge).
      CredentialTokenTtlSeconds
    | -- | @ecluse.advisory.sync.attempts@: advisory sync attempts by ecosystem and result (counter).
      AdvisorySyncAttempts
    | -- | @ecluse.advisory.sync.duration@: advisory sync attempt latency by ecosystem and result (histogram).
      AdvisorySyncDuration
    | {- | @ecluse.advisory.database.age.seconds@: seconds since this ecosystem's last swap
      (gauge). It measures this process's own installation, not the data.
      -}
      AdvisoryDatabaseAgeSeconds
    | {- | @ecluse.advisory.source.age.seconds@: seconds since this ecosystem's serving artifact
      was published (gauge). It is the age the CVE-deny path expires on.
      -}
      AdvisorySourceAgeSeconds
    | -- | @ecluse.advisory.compile.accepted@: advisory entries a compile pass accepted (counter).
      AdvisoryCompileAccepted
    | -- | @ecluse.advisory.compile.dropped@: advisory entries a compile pass dropped, by cause (counter).
      AdvisoryCompileDropped
    | -- | @ecluse.advisory.compile.runs@: compile passes by ecosystem and result (counter).
      AdvisoryCompileRuns
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
    PublishBodyInFlightBytes -> "ecluse.publish.body.in_flight_bytes"
    PublishBodyShed -> "ecluse.publish.body.shed"
    MergeDivergence -> "ecluse.registry.merge.divergence"
    UpstreamFetchDuration -> "ecluse.upstream.fetch.duration"
    UpstreamFetchErrors -> "ecluse.upstream.fetch.errors"
    MetadataCacheRequests -> "ecluse.metadata_cache.requests"
    MetadataCacheEntries -> "ecluse.metadata_cache.entries"
    MetadataCacheResidentBytes -> "ecluse.metadata_cache.resident_bytes"
    SingleVersionCacheResidentBytes -> "ecluse.metadata_cache.version.resident_bytes"
    AssembledCacheResidentBytes -> "ecluse.metadata_cache.assembled.resident_bytes"
    ServePerimeterFaults -> "ecluse.serve.perimeter.faults"
    ServeRelayAnomalies -> "ecluse.serve.relay.anomalies"
    MirrorEnqueued -> "ecluse.mirror.enqueued"
    MirrorEnqueueFailures -> "ecluse.mirror.enqueue.failures"
    MirrorJobsProcessed -> "ecluse.mirror.jobs.processed"
    MirrorPublishDuration -> "ecluse.mirror.publish.duration"
    DredgerVersions -> "ecluse.dredger.versions"
    CredentialRefresh -> "ecluse.credential.refresh"
    CredentialTokenTtlSeconds -> "ecluse.credential.token.ttl.seconds"
    AdvisorySyncAttempts -> "ecluse.advisory.sync.attempts"
    AdvisorySyncDuration -> "ecluse.advisory.sync.duration"
    AdvisoryDatabaseAgeSeconds -> "ecluse.advisory.database.age.seconds"
    AdvisorySourceAgeSeconds -> "ecluse.advisory.source.age.seconds"
    AdvisoryCompileAccepted -> "ecluse.advisory.compile.accepted"
    AdvisoryCompileDropped -> "ecluse.advisory.compile.dropped"
    AdvisoryCompileRuns -> "ecluse.advisory.compile.runs"
