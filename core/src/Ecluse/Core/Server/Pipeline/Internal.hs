{- | Internal guts of the serve pipeline ("Ecluse.Core.Server.Pipeline"), exposed for
tests without widening that module's two-handler public API — the @.Internal@ convention,
as "Ecluse.Core.Credential.Refresh.Internal" uses. Importing it opts out of the public
module's stability promise.

It holds the degrade signalling for two bad-upstream conditions the response-bound
guards leave silent: 'PackumentUndecodable' (the upstream answered, but its body did
not decode into a usable packument) and 'PackumentNameMismatch' (the upstream
answered with a packument whose self-reported name is for a /different/ package).
Each is a typed throw raised at the fetch and caught by the origin fetcher's
@tryAny@, with a paired @log*@ surfacing it at a 'WarningS' through the ambient
@katip@ context before the contribution degrades.
-}
module Ecluse.Core.Server.Pipeline.Internal (
    PackumentUndecodable (..),
    PackumentNameMismatch (..),
    logDecodeFailure,
    logNameMismatch,

    -- * Metric-label projections (pure)
    fetchCause,
    packumentServeDecision,
    serveDecisionClass,
    denialLabels,
    evalTier,
    transienceCause,

    -- * Metric emits (off a serve outcome)
    recordDenials,
    recordEffectfulFailures,
) where

import Katip (KatipContext, Severity (WarningS), katipAddContext, logFM, ls, sl)
import Network.HTTP.Client qualified as HTTP

import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry.Npm (ResponseBoundExceeded (ResponseBoundExceeded))
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

{- | Raised when an upstream packument does not decode into both the typed view and the
raw document the serve path needs. A (typed) throw, not a stringly one, caught by the
origin fetcher's @tryAny@ and degraded to a missing contribution like a bound breach.
-}
data PackumentUndecodable = PackumentUndecodable
    deriving stock (Eq, Show)

instance Exception PackumentUndecodable

{- | Raised when an upstream answered with a packument whose self-reported top-level
@name@ is for a /different/ package than the one requested. The route name is the
validation authority, so a misreporting origin is untrusted for this request: its
contribution is dropped from the merge. A (typed) throw, not a stringly one, caught
by the origin fetcher's @tryAny@ and degraded like 'PackumentUndecodable' — but kept
a distinct type so the serve layer can render the terminal no-valid-origin status as
a @502@ (an upstream returned an invalid response), distinct from a genuine absence.
-}
data PackumentNameMismatch = PackumentNameMismatch
    deriving stock (Eq, Show)

instance Exception PackumentNameMismatch

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

-- ── metric-label projections ─────────────────────────────────────────────────

{- | Classify a caught metadata-fetch failure into the bounded
@ecluse.upstream.fetch.errors@ cause: an undecodable or name-mismatched body is a decode
fault, a transport error a connection fault, and a response-bound breach or anything else
the catch-all other. The cause is bounded by construction — never the exception text.
-}
fetchCause :: SomeException -> Metric.Cause
fetchCause err
    | Just PackumentUndecodable <- fromException err = Metric.Decode
    | Just PackumentNameMismatch <- fromException err = Metric.Decode
    | Just (ResponseBoundExceeded _) <- fromException err = Metric.OtherCause
    | Just (_ :: HTTP.HttpException) <- fromException err = Metric.Connection
    | otherwise = Metric.OtherCause

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
