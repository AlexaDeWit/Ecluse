{- | Internal guts of the serve pipeline ("Ecluse.Server.Pipeline"), exposed for tests
without widening that module's two-handler public API — the @.Internal@ convention, as
"Ecluse.Credential.Refresh.Internal" uses. Importing it opts out of the public
module's stability promise.

It holds the degrade signalling for two bad-upstream conditions the response-bound
guards leave silent: 'PackumentUndecodable' (the upstream answered, but its body did
not decode into a usable packument) and 'PackumentNameMismatch' (the upstream
answered with a packument whose self-reported name is for a /different/ package).
Each is a typed throw raised at the fetch and caught by the origin fetcher's
@tryAny@, with a paired @log*@ surfacing it at a 'WarningS' before the contribution
degrades.
-}
module Ecluse.Server.Pipeline.Internal (
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

import Katip (LogEnv, Severity (WarningS), logFM, ls, sl)
import Katip.Monadic (runKatipContextT)
import Network.HTTP.Client qualified as HTTP

import Ecluse.Log (moduleField)
import Ecluse.Package (PackageName, renderPackageName)
import Ecluse.Registry.Npm (ResponseBoundExceeded (ResponseBoundExceeded))
import Ecluse.Rules.Types (Decision (Undecidable))
import Ecluse.Server.Response (
    PackumentStatus (PackumentForbidden, PackumentOk),
    RejectReason (BelowIntegrityFloor, ByPolicy, MissingIntegrity, Unavailable, UpstreamInvalid),
    Rejection (Rejection),
    RuleName (RuleName),
    ServeDecision (Admit, Reject),
    Transience (WillResolve, WontResolve),
    packumentStatus,
 )
import Ecluse.Telemetry.Instruments (Metrics, recordRuleDenial, recordRuleEffectfulFailure)
import Ecluse.Telemetry.Metrics qualified as Metric

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

{- | Log a parse failure at 'WarningS' — the one bad-upstream condition the
response-bound guards leave silent: the upstream answered, but its body did not decode
into the typed view and raw document the serve path needs. Same fail-closed degrade and
the same @module@\/@package@ payload convention as the breach log in
"Ecluse.Server.Pipeline", so an operator sees an undecodable upstream distinctly rather
than as silence. The @module@ tag names this module's own path.
-}
logDecodeFailure :: LogEnv -> PackageName -> IO ()
logDecodeFailure logEnv name =
    runKatipContextT logEnv payload mempty $
        logFM WarningS (ls message)
  where
    payload = moduleField "Ecluse.Server.Pipeline.Internal" <> sl "package" (renderPackageName name)
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
logNameMismatch :: LogEnv -> PackageName -> Text -> Text -> IO ()
logNameMismatch logEnv requested origin reported =
    runKatipContextT logEnv payload mempty $
        logFM WarningS (ls message)
  where
    payload =
        moduleField "Ecluse.Server.Pipeline.Internal"
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

{- | The rule-evaluation tier a duration is attributed to, from the mount's effectful
rule list: a mount with effectful rules configured runs the effectful tier (the pure
tier first, the effectful tier only where it could change the outcome), otherwise the
evaluation reduces to the structural tier. Polymorphic in the rule element — only
emptiness is consulted — so it is exercised without constructing a rule.
-}
evalTier :: [a] -> Metric.Tier
evalTier effectfulRules = if null effectfulRules then Metric.Structural else Metric.Effectful

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
recordDenials :: Metrics -> [ServeDecision] -> IO ()
recordDenials metrics = traverse_ recordOne
  where
    recordOne :: ServeDecision -> IO ()
    recordOne = \case
        Admit -> pass
        Reject (Rejection reason _) ->
            let (rule, reasonClass) = denialLabels reason
             in recordRuleDenial metrics rule reasonClass

{- | Count each effectful-rule failure among a packument's per-version decisions: an
'Undecidable' is an effectful rule whose source could not be consulted, so it is the
effectful-failure signal, classified to a bounded cause by its transience
('transienceCause'). A decided version (allowed or denied) is not a failure.
-}
recordEffectfulFailures :: Metrics -> [Decision] -> IO ()
recordEffectfulFailures metrics = traverse_ recordOne
  where
    recordOne :: Decision -> IO ()
    recordOne = \case
        Undecidable transience _ -> recordRuleEffectfulFailure metrics (transienceCause transience)
        _ -> pass
