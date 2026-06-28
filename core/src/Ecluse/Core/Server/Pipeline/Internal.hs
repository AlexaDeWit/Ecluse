{- | Internal guts of the serve pipeline ("Ecluse.Core.Server.Pipeline"), exposed for
tests without widening that module's two-handler public API — the @.Internal@ convention,
as "Ecluse.Core.Credential.Refresh.Internal" uses. Importing it opts out of the public
module's stability promise.

It holds the operator-facing warning helpers for two bad-upstream conditions the
response-bound guards leave silent — an upstream whose body does not decode into a
usable packument ('logDecodeFailure'), and one whose packument self-reports a name for
a /different/ package ('logNameMismatch') — each surfaced at a 'WarningS' through the
ambient @katip@ context before the contribution degrades. The conditions themselves are
classified on the serve path as a typed 'Ecluse.Core.Registry.Metadata.MetadataError';
this module only renders their warning lines. Alongside them sit the pure integrity-floor
admission and the metric-label projections the serve path records.
-}
module Ecluse.Core.Server.Pipeline.Internal (
    logDecodeFailure,
    logNameMismatch,

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
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Katip (KatipContext, Severity (WarningS), katipAddContext, logFM, ls, sl)

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
import Ecluse.Core.Rules (PreparedRule (prepResilience))
import Ecluse.Core.Rules.Types (Decision (Undecidable))
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

-- The @module@ tag this module's warnings carry. It is the operator-facing log
-- filter key, not the source module path, so it is held stable across the move into
-- ecluse-core: an operator's saved filter on this value keeps matching, and the only
-- change to these lines is the trace-correlation @dd@ object the ambient context adds.
pipelineInternalModule :: Text
pipelineInternalModule = "Ecluse.Server.Pipeline.Internal"

{- | Log a parse failure at 'WarningS' — the one bad-upstream condition the
response-bound guards leave silent: the upstream answered, but its body did not decode
into the typed view and raw document the serve path needs. Same fail-closed degrade and
the same @module@\/@package@ payload convention as the breach log in
"Ecluse.Core.Server.Pipeline", so an operator sees an undecodable upstream distinctly
rather than as silence. Emitted through the ambient @katip@ context (the request's, so
the line carries its trace-correlation @dd@).
-}
logDecodeFailure :: (KatipContext m) => PackageName -> m ()
logDecodeFailure name =
    katipAddContext payload $ logFM WarningS (ls message)
  where
    payload = sl "module" pipelineInternalModule <> sl "package" (renderPackageName name)
    message :: Text
    message = "refused an upstream metadata document: it did not decode into a usable packument"

{- | Log an upstream name mismatch at 'WarningS' before the contribution degrades: the
origin answered, but its packument self-reported a name for a different package than
the one requested, so it is dropped as untrusted for this request. The structured
payload carries both names and the origin (its base URL) — the high-cardinality
identifiers that belong on the log line — so an operator can tell a misconfigured or
hostile upstream from an ordinary outage. Same fail-closed degrade and payload
convention as 'logDecodeFailure'.
-}
logNameMismatch :: (KatipContext m) => PackageName -> Text -> Text -> m ()
logNameMismatch requested origin reported =
    katipAddContext payload $ logFM WarningS (ls message)
  where
    payload =
        sl "module" pipelineInternalModule
            <> sl "package" (renderPackageName requested)
            <> sl "origin" origin
            <> sl "upstreamName" reported
    message :: Text
    message = "dropped an upstream contribution: its packument self-reported a name for a different package"

-- ── integrity-floor admission ────────────────────────────────────────────────

{- | Apply an integrity-floor admission policy to a 'PackageInfo', keeping only the versions
whose strongest digest meets the floor and projecting the rest to refusals. A version
whose digests are all weaker than the floor (or absent) cannot be tied to a
floor-strength tamper-evident fingerprint, so it is dropped from the served listing rather
than served a client could never safely verify. Used by both gates: the public gate
(@gatePublic@) with the hard-floored 'Ecluse.Core.Package.Integrity.MinIntegrity', and the
trusted gate (@admitTrusted@) with the loosenable
'Ecluse.Core.Package.Integrity.MinTrustedIntegrity'. Returns the admissible 'PackageInfo'
(with @dist-tags@ pruned to the kept keys, exactly as @restrictToSurvivors@ prunes for the
rules; each kept version carries its own publish time, so restricting the versions carries
the times with it) and the refusals for the dropped versions: 'BelowIntegrityFloor' for a
too-weak digest, 'MissingIntegrity' for none at all, each feeding the no-survivors
status.
-}
admitByIntegrity ::
    (IntegrityFloor floor) =>
    floor ->
    -- The refusal projected for a present-but-too-weak digest ('BelowFloor') …
    ServeDecision ->
    -- … and for a version carrying no digest at all ('NoIntegrity'); the public and
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
    -- Classify each version against the floor exactly once — the up-to-100k-version map is
    -- walked a single time, and the admissible keys and both refusal buckets are read off
    -- the resulting class map (itself the size of the version map, not small).
    classified :: Map Text VersionIntegrity
    classified = Map.map (classifyArtifacts floorSpec . pkgArtifacts) (infoVersions info)

    admissibleKeys :: Set Text
    admissibleKeys = Map.keysSet (Map.filter (== MeetsFloor) classified)

    -- The dropped versions projected to refusals in one pass over the class map: 'Map.foldr'
    -- visits ascending-key order and each arm prepends, so the below-floor refusals precede
    -- the missing-integrity refusals, each in key order.
    refusals :: [ServeDecision]
    refusals = below <> missing
      where
        (below, missing) = Map.foldr bucket ([], []) classified
        bucket BelowFloor (b, m) = (belowFloorRefusal : b, m)
        bucket NoIntegrity (b, m) = (b, missingRefusal : m)
        bucket MeetsFloor acc = acc

-- ── metric-label projections ─────────────────────────────────────────────────

{- | Classify a no-survivors packument outcome into the bounded @ecluse.serve.decision@
value: a forbidden set is a denial, any other non-served status a transient
unavailability. (A served set is recorded as an admit at the call site, not here.)
-}
packumentServeDecision :: [ServeDecision] -> Metric.Decision
packumentServeDecision decisions = case packumentStatus decisions of
    PackumentForbidden -> Metric.Deny
    PackumentOk -> Metric.Admit
    _ -> Metric.Unavailable

{- | Classify a single artifact-path serve decision into the bounded metric decision: a
policy or integrity refusal is a denial, an upstream outage or invalid response an
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

{- | The rule-evaluation tier a duration is attributed to, from the mount's rule set:
a mount with any __resilient__ (effectful) rule is attributed to the effectful tier;
a purely-pure rule set reduces to the structural tier. The resilience policy a prepared
rule carries — not a separate list — is what distinguishes an effectful rule now that
the two tiers are one engine.
-}
evalTier :: [PreparedRule] -> Metric.Tier
evalTier rules = if any (isJust . prepResilience) rules then Metric.Effectful else Metric.Structural

{- | Map an undecidable verdict's transience to the bounded
@ecluse.rule.effectful.failures@ cause: a retryable cause is a connection-class fault (the
source was unreachable now), a permanent one the catch-all other.
-}
transienceCause :: Transience -> Metric.Cause
transienceCause = \case
    WillResolve _ -> Metric.Connection
    WontResolve -> Metric.OtherCause

{- | Record the @ecluse.rule.denials@ counter for each rejected decision, labelled by
the bounded reason class and — for a policy denial — the deciding rule name
('denialLabels'). An admit records nothing.
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

{- | Count each effectful-rule failure among a packument's per-version decisions: an
'Undecidable' is an effectful rule whose source could not be consulted, so it is the
effectful-failure signal, classified to a bounded cause by its transience
('transienceCause'). A decided version (allowed or denied) is not a failure.
-}
recordEffectfulFailures :: MetricsPort -> [Decision] -> IO ()
recordEffectfulFailures metrics = traverse_ recordOne
  where
    recordOne :: Decision -> IO ()
    recordOne = \case
        Undecidable transience _ -> mpRuleEffectfulFailure metrics (transienceCause transience)
        _ -> pass
