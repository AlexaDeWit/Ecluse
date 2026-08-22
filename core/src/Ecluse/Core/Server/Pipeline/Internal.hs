-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Internal guts of the serve pipeline ("Ecluse.Core.Server.Pipeline"), exposed for
tests without widening that module's two-handler public API. This is the @.Internal@
convention "Ecluse.Core.Credential.Refresh.Internal" also uses. Importing it opts out of
the public module's stability promise.

It holds the operator-facing warning helpers for the bad-upstream and misconfiguration
conditions the response-bound guards leave silent:

* an upstream whose body does not decode into a usable packument ('logDecodeFailure')
* an upstream whose packument self-reports a name for a /different/ package
  ('logNameMismatch')
* a mount whose configured base URL cannot be formed into a request
  ('logUpstreamUnformable')
* an upstream the transport could not reach ('logUpstreamUnreachable')

Each surfaces at a 'WarningS' through the ambient @katip@ context before the contribution
degrades. The serve path classifies the conditions themselves as a typed
'Ecluse.Core.Registry.Metadata.MetadataError'. This module only renders their warning
lines. Alongside them sit the pure integrity-floor admission and the metric-label
projections the serve path records.
-}
module Ecluse.Core.Server.Pipeline.Internal (
    logDecodeFailure,
    logNameMismatch,
    logUpstreamUnformable,
    logUpstreamUnreachable,

    -- * Integrity-floor admission (pure)
    admitByIntegrity,

    -- * Metric-label projections (pure)
    packumentServeDecision,
    serveDecisionClass,
    denialLabels,
    evalTier,
    transienceCause,

    -- * Metric emits (off a serve outcome)
    recordDenials,
    recordEffectfulFailures,

    -- * Denial audit trail (structured log)
    VersionVerdict (..),
    Metadata (..),
    DenialAudit (..),
    denialAuditPayload,
    logDenials,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Katip (KatipContext, Severity (WarningS), SimpleLogPayload, katipAddContext, logFM, ls, sl)

import Ecluse.Core.Cve (DbEtag (..))
import Ecluse.Core.Fault (TransportFault (tfCause, tfDetail))
import Ecluse.Core.Package (
    PackageDetails (pkgArtifacts),
    PackageInfo (infoDistTags, infoVersions),
    PackageName,
    renderPackageName,
 )
import Ecluse.Core.Package.Integrity (
    IntegrityFloor,
    VersionIntegrity (BelowFloor, MeetsFloor, NoIntegrity),
    classifyArtifacts,
 )
import Ecluse.Core.Registry (UrlFormationError, renderUrlFormationError)
import Ecluse.Core.Rules (PreparedRule (prepResilience), cveIdsInReason)
import Ecluse.Core.Rules.Types (Decision (Undecidable))
import Ecluse.Core.Security.Authority (authorityLabel)
import Ecluse.Core.Server.Response (
    PackumentStatus (PackumentForbidden, PackumentOk),
    RejectReason (BelowIntegrityFloor, ByPolicy, MissingIntegrity, Unavailable, UpstreamInvalid),
    Rejection (Rejection),
    RuleName (RuleName),
    ServeDecision (Admit, Reject),
    Transience (WillResolve, WontResolve),
    packumentStatus,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort, mpRuleDenial, mpRuleEffectfulFailure)
import Ecluse.Core.Version (renderVersion)

-- The @module@ tag this module's warnings carry. It is the operator-facing log filter
-- key, not the source module path. It is held stable at this value, so an operator's
-- saved filter keeps matching.
pipelineInternalModule :: Text
pipelineInternalModule = "Ecluse.Server.Pipeline.Internal"

{- | Log a parse failure at 'WarningS'. The upstream answered, but its body did not
decode into the typed view and raw document the serve path needs. This is the one
bad-upstream condition the response-bound guards leave silent. It takes the same
fail-closed degrade and the same @module@\/@package@ payload convention as the breach log
in "Ecluse.Core.Server.Pipeline". An operator therefore sees an undecodable upstream
distinctly rather than as silence. The line rides the ambient @katip@ context (the
request's, so it carries the trace-correlation @dd@).
-}
logDecodeFailure :: (KatipContext m) => PackageName -> m ()
logDecodeFailure name =
    katipAddContext payload $ logFM WarningS (ls message)
  where
    payload = sl "module" pipelineInternalModule <> sl "package" (renderPackageName name)
    message :: Text
    message = "refused an upstream metadata document: it did not decode into a usable packument"

{- | Log an upstream name mismatch at 'WarningS' before the contribution degrades. The
origin answered, but its packument self-reported a name for a different package than the
one requested. The serve path drops it as untrusted for this request. The structured
payload carries both names and the origin's authority: the high-cardinality identifiers
that belong on the log line. An operator can then tell a misconfigured or hostile
upstream from an ordinary outage. Same fail-closed degrade and payload convention as
'logDecodeFailure'.
-}
logNameMismatch :: (KatipContext m) => PackageName -> Text -> Text -> m ()
logNameMismatch requested origin reported =
    katipAddContext payload $ logFM WarningS (ls message)
  where
    payload =
        sl "module" pipelineInternalModule
            <> sl "package" (renderPackageName requested)
            <> sl "origin" (authorityLabel origin)
            <> sl "upstreamName" reported
    message :: Text
    message = "dropped an upstream contribution: its packument self-reported a name for a different package"

{- | Log an unformable upstream request URL at 'WarningS' before the contribution
degrades. The base URL configured for this origin is empty or could not be parsed into a
request, so no fetch could even be attempted. This is a __configuration__ fault. It
carries its own 'Ecluse.Core.Registry.Metadata.MetadataUrlUnformable', distinct from a
decode failure or a transient outage. An operator therefore sees a misconfigured mount,
not an upstream that merely appears unreachable. The structured payload carries the
origin's authority and the rendered URL fault. Same fail-closed degrade and payload
convention as 'logNameMismatch'.
-}
logUpstreamUnformable :: (KatipContext m) => PackageName -> Text -> UrlFormationError -> m ()
logUpstreamUnformable name origin urlErr =
    katipAddContext payload $ logFM WarningS (ls message)
  where
    payload =
        sl "module" pipelineInternalModule
            <> sl "package" (renderPackageName name)
            <> sl "origin" (authorityLabel origin)
            <> sl "urlError" (renderUrlFormationError urlErr)
    message :: Text
    message = "refused an upstream metadata fetch: the configured base URL could not be formed into a request"

{- | Log an unreachable upstream at 'WarningS' before the contribution degrades. The
transport failed before a usable body returned: a timeout, a refused connection, or a TLS
refusal, so the origin contributes nothing this request. The structured payload carries
the origin's authority, the bounded 'Ecluse.Core.Fault.TransportCause', and the rendered
detail. An operator can then tell an outage from a decode failure or a misconfigured
mount. Same fail-closed degrade and payload convention as 'logUpstreamUnformable'.
-}
logUpstreamUnreachable :: (KatipContext m) => PackageName -> Text -> TransportFault -> m ()
logUpstreamUnreachable name origin fault =
    katipAddContext payload $ logFM WarningS (ls message)
  where
    payload =
        sl "module" pipelineInternalModule
            <> sl "package" (renderPackageName name)
            <> sl "origin" (authorityLabel origin)
            <> sl "transportCause" (show (tfCause fault) :: Text)
            <> sl "transportDetail" (tfDetail fault)
    message :: Text
    message = "an upstream metadata fetch could not reach the origin; its contribution degrades this request"

{- | Apply an integrity-floor admission policy to a 'PackageInfo': keep the versions whose
strongest digest meets the floor, and project the rest to refusals. A version whose
digests are all weaker than the floor, or absent, cannot be tied to a floor-strength
tamper-evident fingerprint. The gate therefore drops it from the served listing rather
than serve a version a client could never safely verify.

Both gates use it: the public gate (@gatePublic@) with the hard-floored
'Ecluse.Core.Package.Integrity.MinIntegrity', and the trusted gate (@admitTrusted@) with
the loosenable 'Ecluse.Core.Package.Integrity.MinTrustedIntegrity'.

Returns the admissible 'PackageInfo', with @dist-tags@ pruned to the kept keys exactly as
@restrictToSurvivors@ prunes for the rules. Each kept version carries its own publish
time, so restricting the versions carries the times with it. It also returns the refusals
for the dropped versions: 'BelowIntegrityFloor' for a too-weak digest,
'MissingIntegrity' for none at all. Each refusal feeds the no-survivors status.
-}
admitByIntegrity ::
    (IntegrityFloor floor) =>
    floor ->
    -- The refusal projected for a present-but-too-weak digest ('BelowFloor') …
    ServeDecision ->
    -- … and for a version carrying no digest at all ('NoIntegrity'). The public and
    -- trusted gates pass their own context-worded decisions.
    ServeDecision ->
    PackageInfo ->
    (PackageInfo, [ServeDecision])
admitByIntegrity floorSpec belowFloorRefusal missingRefusal info =
    ( info
        { infoVersions = Map.restrictKeys (infoVersions info) admissibleKeys
        , infoDistTags = Map.filter ((`Set.member` admissibleKeys) . renderVersion) (infoDistTags info)
        }
    , refusals
    )
  where
    -- Classify each version against the floor exactly once: one walk of the
    -- up-to-100k-version map yields the admissible keys and both refusal buckets. The
    -- class map is itself the size of the version map, not small.
    classified :: Map Text VersionIntegrity
    classified = Map.map (classifyArtifacts floorSpec . pkgArtifacts) (infoVersions info)

    admissibleKeys :: Set Text
    admissibleKeys = Map.keysSet (Map.filter (== MeetsFloor) classified)

    -- The dropped versions projected to refusals in one pass over the class map.
    -- 'Map.foldr' visits keys in ascending order and each arm prepends, so the below-floor
    -- refusals precede the missing-integrity refusals, each in key order.
    refusals :: [ServeDecision]
    refusals = below <> missing
      where
        (below, missing) = Map.foldr bucket ([], []) classified
        bucket BelowFloor (b, m) = (belowFloorRefusal : b, m)
        bucket NoIntegrity (b, m) = (b, missingRefusal : m)
        bucket MeetsFloor acc = acc

{- | Classify a no-survivors packument outcome into the bounded @ecluse.serve.decision@
value: a forbidden set is a denial, any other non-served status a transient
unavailability. (The call site records a served set as an admit, not this function.)
-}
packumentServeDecision :: [ServeDecision] -> Metric.Decision
packumentServeDecision decisions = case packumentStatus decisions of
    PackumentForbidden -> Metric.Deny
    PackumentOk -> Metric.Admit
    _ -> Metric.Unavailable

{- | Classify a single artifact-path serve decision into the bounded metric decision. A
policy or integrity refusal is a denial. An upstream outage or an invalid response is an
unavailability.
-}
serveDecisionClass :: ServeDecision -> Metric.Decision
serveDecisionClass = \case
    Admit -> Metric.Admit
    Reject (Rejection reason _) -> case reason of
        ByPolicy{} -> Metric.Deny
        MissingIntegrity -> Metric.Deny
        BelowIntegrityFloor -> Metric.Deny
        Unavailable{} -> Metric.Unavailable
        UpstreamInvalid -> Metric.Unavailable

{- | Map a reject reason to the @ecluse.rule.denials@ labels: the deciding rule (only a
policy denial names one) and the bounded reason class.
-}
denialLabels :: RejectReason -> (Maybe Text, Metric.ReasonClass)
denialLabels = \case
    ByPolicy (RuleName name) -> (Just name, Metric.ReasonPolicy)
    MissingIntegrity -> (Nothing, Metric.ReasonMissingIntegrity)
    BelowIntegrityFloor -> (Nothing, Metric.ReasonMissingIntegrity)
    Unavailable _ -> (Nothing, Metric.ReasonUnavailable)
    UpstreamInvalid -> (Nothing, Metric.ReasonUnavailable)

{- | The rule-evaluation tier a duration is attributed to, from the mount's rule set. A
mount with any __resilient__ (effectful) rule takes the effectful tier. A purely pure rule
set reduces to the structural tier. The two tiers are one engine, so what marks a rule
effectful is the resilience policy the prepared rule carries, not a separate list.
-}
evalTier :: [PreparedRule] -> Metric.Tier
evalTier rules = if any (isJust . prepResilience) rules then Metric.Effectful else Metric.Structural

{- | Map an undecidable verdict's transience to the bounded
@ecluse.rule.effectful.failures@ cause. A retryable cause is a connection-class fault: the
source was unreachable now. A permanent one is the catch-all other.
-}
transienceCause :: Transience -> Metric.Cause
transienceCause = \case
    WillResolve _ -> Metric.Connection
    WontResolve -> Metric.OtherCause

{- | Record the @ecluse.rule.denials@ counter for each rejected decision, labelled by the
bounded reason class. A policy denial also names the deciding rule ('denialLabels'). An
admit records nothing.
-}
recordDenials :: MetricsPort -> [ServeDecision] -> IO ()
recordDenials metrics = traverse_ recordOne
  where
    recordOne :: ServeDecision -> IO ()
    recordOne = \case
        Admit -> pass
        Reject (Rejection reason _) ->
            let (rule, reasonClass) = denialLabels reason
             in mpRuleDenial metrics rule reasonClass

{- | Count each effectful-rule failure among a packument's per-version decisions. An
'Undecidable' is an effectful rule whose source could not be consulted, so it is the
effectful-failure signal. Its transience classifies it to a bounded cause
('transienceCause'). A decided version (allowed or denied) is not a failure.
-}
recordEffectfulFailures :: MetricsPort -> [Decision] -> IO ()
recordEffectfulFailures metrics = traverse_ recordOne
  where
    recordOne :: Decision -> IO ()
    recordOne = \case
        Undecidable transience _ -> mpRuleEffectfulFailure metrics (transienceCause transience)
        _ -> pass

{- | A per-version serve outcome that keeps the version, the decision's subject, so a
denial's audit line can name it. 'Ecluse.Core.Server.Pipeline.Packument.gatePublic'
preserves the version when it projects to 'ServeDecision' rather than dropping it.
-}
data VersionVerdict = VersionVerdict
    { vvVersion :: Text
    , vvDecision :: ServeDecision
    }
    deriving stock (Eq, Show)

{- | An extensible bag of audit fields folded into a denial line's JSON at emit time. It
lives here, at the audit boundary, deliberately __not__ on the pure
'Ecluse.Core.Rules.Types.Decision'. The rule engine carries no logging concern. New audit
data (a CVE id, an EPSS score) joins at this layer without threading a field through the
decision path.
-}
newtype Metadata = Metadata (Map Text Text)
    deriving stock (Eq, Show)

instance Semigroup Metadata where
    Metadata a <> Metadata b = Metadata (a <> b)

instance Monoid Metadata where
    mempty = Metadata Map.empty

{- | Everything one denial audit line records, typed and stable. The advisory 'DbEtag' is
the database active when the request was admitted, carried on the
'Ecluse.Core.Rules.Types.EvalContext'. The line names it as active at emit. It never
claims it as the database the decision was evaluated against, because a shadow swap may
land mid-request.
-}
data DenialAudit = DenialAudit
    { daPackage :: PackageName
    , daVersion :: Text
    , daRule :: Maybe Text
    , daReasonClass :: Metric.ReasonClass
    , daAdvisoryEtag :: Maybe DbEtag
    , daExtra :: Metadata
    }

-- | Render a 'DenialAudit' to the structured payload katip folds into the line's @data@ object.
denialAuditPayload :: DenialAudit -> SimpleLogPayload
denialAuditPayload da =
    sl "module" pipelineInternalModule
        <> sl "package" (renderPackageName (daPackage da))
        <> sl "version" (daVersion da)
        <> maybe mempty (sl "rule") (daRule da)
        <> sl "reason_class" (show (daReasonClass da) :: Text)
        <> maybe mempty (\(DbEtag e) -> sl "active_advisory_db_etag" e) (daAdvisoryEtag da)
        <> metadataPayload (daExtra da)
  where
    metadataPayload (Metadata m) = Map.foldrWithKey (\k v acc -> sl k v <> acc) mempty m

{- | The advisory ids a denial named, recovered from its rendered message and folded into
the audit line's 'Metadata' as a comma-joined @cve@ field. Empty for a non-CVE denial, so
the field appears only when an advisory drove the refusal. 'cveIdsInReason' recovers them
at this layer instead of threading them through the pure decision path, per the
'Metadata' contract.
-}
cveMetadata :: Text -> Metadata
cveMetadata message = case cveIdsInReason message of
    [] -> mempty
    ids -> Metadata (Map.singleton "cve" (T.intercalate ", " ids))

{- | Emit one audit log line per denied version, __denials only__ (an admit logs
nothing). Companion to 'recordDenials', which counts the same denials as metrics.
The 'DbEtag' is the advisory database active at emit, from the request's
'Ecluse.Core.Rules.Types.EvalContext'. The line answers "which database was live when this
verdict was logged", not "which database produced it".
-}
logDenials :: (KatipContext m) => PackageName -> Maybe DbEtag -> [VersionVerdict] -> m ()
logDenials pkg etag = traverse_ logOne
  where
    logOne vv = case vvDecision vv of
        Admit -> pass
        Reject (Rejection reason message) ->
            let (rule, reasonClass) = denialLabels reason
                audit = DenialAudit pkg (vvVersion vv) rule reasonClass etag (cveMetadata message)
             in katipAddContext (denialAuditPayload audit) $
                    logFM WarningS (ls ("denied" :: Text))
