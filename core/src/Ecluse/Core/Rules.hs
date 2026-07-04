{- | The policy rules engine.

A rule set is evaluated against a single 'PackageDetails' snapshot to produce a
'Decision'. The model is __deny by default; the boot order decides__: the configured
rules are arranged once, at boot, into a single total order ('bootOrder') -- highest
precedence first, then rule name ascending -- and evaluation walks that order and
takes the __first decisive result__. A result is decisive iff it is 'Allow', 'Deny',
or an @'Unavailable' _ 'FailDeny' _@ (a fail-closed uncomputable check); 'NoDecision'
and @'Unavailable' _ 'FailNoDecision' _@ are non-decisive no-ops whose reasons are
collected, in boot order, for the deny-by-default audit trail. If no rule is decisive
the package is 'BlockedByDefault'.

__A rule is evaluation-agnostic data; how it is evaluated is a separate concern.__ The
closed built-in vocabulary ('Ecluse.Core.Rules.Types.Rule') says /what/ a rule is;
'evalRule' is the single dispatch that says /how/ each built-in rule decides, closing
over the boot-bound capabilities in 'RuleDeps'. The engine's runtime structure is the
'PreparedRule': it pairs a rule's boot-order identity (precedence and name) with the
raw per-version evaluator and an optional 'Resilience' policy. 'prepare' builds one
per configured rule; the pure built-ins carry no 'Resilience' and run directly, while
the effectful 'AllowIfRemediatesCve' carries a 'Resilience' (a per-attempt timeout,
bounded retry with backoff, and a per-source 'Ecluse.Core.Breaker.Breaker') applied by
the harness 'runEffectfulRule'. The order /is/ the tiebreak: there is no runtime
comparison of results.

The evaluator on a 'PreparedRule' is __not__ reachable from config: 'prepare' only
ever binds 'evalRule' over closed 'Rule' data, so untrusted config can express only
the built-in vocabulary. Supplying an arbitrary evaluator is a code-layer capability
(the engine's own tests today; a rule DSL or plugin later), never a config surface.

'evalRules' may evaluate effectful rules speculatively in parallel, but the result is
always __as-if sequential by boot order__: the winner is the earliest-in-order
decisive rule, never the first to return in wall-clock time, and once the winner is
known every still-running strictly-later evaluation is cancelled. The cheap pure
prefix is evaluated directly, so no IO an earlier decisive result would moot is ever
launched. Evaluation is 'IO'-typed (a rule's evaluator may do IO), so there is no pure
entry point. The rule data types live in "Ecluse.Core.Rules.Types".
-}
module Ecluse.Core.Rules (
    -- * The boot-bound rule capabilities
    RuleDeps (..),
    inertRuleDeps,

    -- * The built-in rule dispatch
    evalRule,

    -- * The engine's prepared rule
    PreparedRule (..),
    Resilience (..),
    prepare,

    -- * Boot-time ordering
    bootOrder,
    renderBootOrder,

    -- * Evaluation
    evalRules,
    renderDecision,
    renderDuration,

    -- * The resilience harness
    runEffectfulRule,
    EffectfulConfig (..),
    defaultEffectfulConfig,
    backoffPolicy,
    Breaker (..),
    newBreaker,
    BreakerReporter (..),
    noBreakerReporter,
) where

import Control.Retry (
    RetryPolicyM (RetryPolicyM),
    RetryStatus (rsIterNumber),
    retrying,
 )
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import UnliftIO (timeout, tryAny)
import UnliftIO.Async (Async, async, cancel, uninterruptibleCancel, wait)
import UnliftIO.Exception (bracket)

import Ecluse.Core.Breaker (
    Breaker (..),
    BreakerReporter (..),
    admit,
    initialBreaker,
    noBreakerReporter,
    recordFailure,
    recordSuccess,
    reportBreakerChange,
 )
import Ecluse.Core.Cve (AdvisoryRange (..), CveLookup (..), insideAffectedRange)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package
import Ecluse.Core.Rules.Types
import Ecluse.Core.Version (renderVersion)

{- | The boot-bound capabilities a rule's evaluation may consult, injected once at
the composition root and closed into the prepared rules by 'prepare'. This is the
capability counterpart of 'EvalContext': the context carries per-evaluation ambient
__data__ (the clock instant), while these are process-lifetime __capabilities__.

'rdWithCveLookup' is acquisition-bracketed rather than a bare read so its provider
can pin the advisory database generation for exactly one rule evaluation: the
background sync's atomic shadow-swap closes and prunes a superseded artifact only
once no evaluation still holds it. 'Nothing' means no advisory database is loaded
(none configured, or the first sync has not landed); the CVE rule abstains.
-}
data RuleDeps = RuleDeps
    { rdWithCveLookup :: forall a. (Maybe CveLookup -> IO a) -> IO a
    -- ^ Bracketed access to the current advisory database view, if one is loaded.
    , rdBreakerReporter :: BreakerReporter
    {- ^ The observer effectful rules report their breaker transitions to
    (@ecluse.rule.breaker.state@); 'noBreakerReporter' when unobserved.
    -}
    }

{- | Rule capabilities with no advisory database and no breaker observer: the
composition value before a CVE sync is configured, and the pure tests' default.
-}
inertRuleDeps :: RuleDeps
inertRuleDeps =
    RuleDeps
        { rdWithCveLookup = \use -> use Nothing
        , rdBreakerReporter = noBreakerReporter
        }

{- | Evaluate a single built-in rule against a single package version -- the one place
"how a rule decides" lives. The dispatch over the closed 'Rule' data: the pure
constructors reason over the 'PackageDetails' alone and 'pure' their result, never
yielding 'Unavailable'; 'AllowIfRemediatesCve' reads the advisory database through
the boot-bound 'RuleDeps' and does IO, relying on its 'Resilience' harness (attached
by 'prepare') to resolve a failing lookup to a fail-open 'Unavailable'.

'IO'-typed so the dispatch is uniform across the pure and effectful arms. The pure
arms are total -- a malformed rule or package yields a result, never an exception, so
hostile metadata cannot crash the gate.
-}
evalRule :: RuleDeps -> EvalContext -> Rule -> PackageDetails -> IO RuleResult
evalRule _ _ (AllowScope scope) pd =
    pure $ case pkgNamespace (pkgName pd) of
        Just s
            | s == scope ->
                Allow ("scope " <> renderScope scope <> " is allow-listed")
        _ ->
            NoDecision ("scope is not the allow-listed " <> renderScope scope)
evalRule _ ctx (AllowIfOlderThan minAge) pd =
    pure $ case pkgPublishedAt pd of
        Nothing -> NoDecision "publish time is unknown"
        Just publishedAt ->
            let age = diffUTCTime (ctxNow ctx) publishedAt
             in if age >= minAge
                    then
                        Allow
                            ( "published "
                                <> renderDuration age
                                <> " ago (at least "
                                <> renderDuration minAge
                                <> " old)"
                            )
                    else
                        NoDecision
                            ( "published only "
                                <> renderDuration age
                                <> " ago, minimum age is "
                                <> renderDuration minAge
                            )
evalRule _ _ DenyInstallTimeExecution pd =
    pure $ case pkgInstallCode pd of
        RunsCodeOnInstall how -> Deny ("runs code on install: " <> how)
        NoCodeOnInstall -> NoDecision "no install-time code execution"
        CodeExecUnknown -> NoDecision "install-time code execution not yet determined"
evalRule _ _ (DenyByIdentity ident) pd =
    pure $
        if matchesIdentity ident pd
            then Deny ("identity " <> ident <> " is revoked by operator")
            else NoDecision ("identity is not the revoked " <> ident)
evalRule _ _ (AllowByIdentity ident) pd =
    pure $
        if matchesIdentity ident pd
            then Allow ("identity " <> ident <> " is allow-listed by operator")
            else NoDecision ("identity is not the allow-listed " <> ident)
evalRule deps _ AllowIfRemediatesCve pd =
    rdWithCveLookup deps $ \case
        Nothing -> pure (NoDecision "no advisory database is loaded")
        Just cve -> remediationVerdict cve pd

-- The CVE rule's verdict against a loaded advisory database.
remediationVerdict :: CveLookup -> PackageDetails -> IO RuleResult
remediationVerdict cve pd = do
    fixes <- cveRemediationProbe cve name version
    if not fixes
        then pure (NoDecision "no advisory names this version as its fix")
        else do
            -- The probe hit, so the version is some advisory's exact fixed
            -- bound; fetch the package's ranges once to name what it fixes
            -- and to guard the lane.
            ranges <- cveAdvisoriesFor cve name
            pure (classifyRanges (pkgEcosystem (pkgName pd)) version ranges)
  where
    name = renderPackageName (pkgName pd)
    version = renderVersion (pkgVersion pd)

-- Classify the fetched ranges: a version still inside *any* advisory's affected
-- range (an unfixed one included) must not fast-track; otherwise credit the
-- advisories that name this version as their exact fixed bound.
classifyRanges :: Ecosystem -> Text -> [AdvisoryRange] -> RuleResult
classifyRanges eco version ranges =
    case (remediated, stillOpen) of
        (_, _ : _) ->
            NoDecision
                ("fixes " <> T.intercalate ", " remediated <> " but is still affected by " <> T.intercalate ", " stillOpen)
        ([], []) ->
            -- Unreachable under one acquisition (the probe and the
            -- fetch see the same artifact), kept total.
            NoDecision "no advisory names this version as its fix"
        (ids, []) -> Allow ("remediates " <> T.intercalate ", " ids)
  where
    remediated = ordNub [arCveId ar | ar <- ranges, arFixed ar == Just version]
    stillOpen = ordNub [arCveId ar | ar <- ranges, insideAffectedRange eco version ar]

-- The one identity test the by-identity twins share: the exact rendered package
-- name, or the exact package@version.
matchesIdentity :: Text -> PackageDetails -> Bool
matchesIdentity ident pd =
    let pkgStr = renderPackageName (pkgName pd)
        pkgAtVer = pkgStr <> "@" <> renderVersion (pkgVersion pd)
     in ident == pkgStr || ident == pkgAtVer

{- | A rule prepared for the engine to evaluate: its boot-order identity (precedence
and name), an optional 'Resilience' policy, and the raw per-version evaluator the
engine runs. This is the engine's __one__ runtime structure -- and its only injection
point.

For a configured rule 'prepare' builds it: the name from the rule data ('ruleName'),
the evaluator from 'evalRule', and (today) no 'Resilience'. Because the evaluator is a
plain function field -- not a closed 'Rule' -- it is also where an arbitrary evaluator
can be supplied without widening the closed 'Rule' vocabulary: the engine's own tests
build a 'PreparedRule' directly with a fake evaluator (one that throws, hangs, or
returns a chosen 'RuleResult') and a chosen name to exercise the resilience harness
and the parallel walk. That escape hatch is a code-layer capability; config only ever
reaches the closed data path through 'prepare', so it cannot supply one.

It declares no allow\/deny "direction": admit vs block is simply what 'prepEval'
returns. With @'prepResilience' = 'Nothing'@ the rule runs directly; with a
'Resilience' it is wrapped by 'runEffectfulRule'.
-}
data PreparedRule = PreparedRule
    { prepName :: Text
    -- ^ The stable, human-facing name; the boot-order tiebreak and the credited identity.
    , prepPrecedence :: Int
    -- ^ The precedence at which this rule competes; higher wins in the boot order.
    , prepResilience :: Maybe Resilience
    -- ^ The resilience policy, or 'Nothing' for a rule run directly.
    , prepEval :: EvalContext -> PackageDetails -> IO RuleResult
    {- ^ The rule's raw verdict for one version. For a resilient rule it may perform IO
    that fails or hangs; 'runEffectfulRule' wraps it.
    -}
    }

{- | The resilience policy wrapped around an effectful rule's IO: the timeout\/retry\/
breaker knobs, the per-source circuit-breaker state, its observer, and the
__failure alignment__ an exhausted evaluation resolves to (fail-closed 'FailDeny' or
fail-open 'FailNoDecision'). The alignment rides on the prepared rule, folding away the
separate failure-policy the two-tier design once carried.
-}
data Resilience = Resilience
    { resConfig :: EffectfulConfig
    -- ^ The per-attempt timeout, retry budget\/backoff, and breaker threshold\/cooldown.
    , resAlignment :: FailureAlignment
    -- ^ Whether an exhausted evaluation fails closed ('FailDeny') or open ('FailNoDecision').
    , resBreaker :: TVar Breaker
    -- ^ This rule's per-source circuit-breaker state, shared across evaluations.
    , resBreakerReporter :: BreakerReporter
    {- ^ The observer this rule's breaker reports its state transitions to
    (@ecluse.rule.breaker.state@). Inert ('Ecluse.Core.Breaker.noBreakerReporter') for an
    unobserved rule; the composition root installs the live one.
    -}
    }

{- | Prepare a resolved policy ('PrecededRule's) into the engine's runtime rules: each
rule's name comes from its data ('ruleName'), its evaluator from 'evalRule' closed
over the boot-bound 'RuleDeps', and its 'Resilience' from whether the rule needs one.
The pure built-ins carry no 'Resilience' (@'prepResilience' = 'Nothing'@) and run
directly; 'AllowIfRemediatesCve' is prepared with a __fail-open__ 'Resilience'
('FailNoDecision'), so a lookup that fails or hangs abstains -- the version falls back
to the ordinary quarantine -- and never admits on an unconfirmable claim.

'IO'-typed because preparing a resilient rule allocates its per-source breaker
('newBreaker') -- once, at the composition root, shared across evaluations.
-}
prepare :: RuleDeps -> [PrecededRule] -> IO [PreparedRule]
prepare deps = traverse (prepareRule deps)

-- Prepare one configured rule: attach the fail-open 'Resilience' (allocating its
-- breaker) to the effectful CVE rule; the pure rules run directly.
prepareRule :: RuleDeps -> PrecededRule -> IO PreparedRule
prepareRule deps (PrecededRule prec rule) = do
    resilience <- resilienceFor deps rule
    pure
        PreparedRule
            { prepName = ruleName rule
            , prepPrecedence = prec
            , prepResilience = resilience
            , prepEval = \ctx -> evalRule deps ctx rule
            }

-- The resilience a rule needs: the effectful CVE rule carries the fail-open
-- policy (allocating its per-source breaker); the pure rules carry none.
resilienceFor :: RuleDeps -> Rule -> IO (Maybe Resilience)
resilienceFor deps = \case
    AllowIfRemediatesCve -> do
        breaker <- newBreaker
        pure $
            Just
                Resilience
                    { resConfig = defaultEffectfulConfig
                    , resAlignment = FailNoDecision
                    , resBreaker = breaker
                    , resBreakerReporter = rdBreakerReporter deps
                    }
    _ -> pure Nothing

{- | Arrange a rule set into the single total order evaluation walks: __highest
precedence first, then rule name ascending__ as the deterministic tiebreak. A pure
function of the rules' precedences and names, independent of the order they were
configured in -- so shuffling the configured set yields the same order and hence the
same 'Decision'. The order /is/ the tiebreak; there is no runtime comparison of
results.
-}
bootOrder :: [PreparedRule] -> [PreparedRule]
bootOrder = sortOn (\r -> bootKey (prepPrecedence r) (prepName r))

-- The single boot-order comparator key: precedence descending (highest first), then
-- name ascending. Both 'bootOrder' and the engine order through this one key, so the
-- tiebreak is expressed exactly once.
bootKey :: Int -> Text -> (Down Int, Text)
bootKey prec name = (Down prec, name)

{- | Render the boot order as one diagnostic line per rule, in evaluation order, so
an operator sees at boot exactly how their policy will resolve. Empty for an empty
rule set.
-}
renderBootOrder :: [PreparedRule] -> [Text]
renderBootOrder rules = zipWith line [1 :: Int ..] (bootOrder rules)
  where
    line i r =
        "rule "
            <> show i
            <> ": "
            <> prepName r
            <> " (precedence "
            <> show (prepPrecedence r)
            <> ")"

{- | Evaluate a package version against a rule set in 'IO': walk the boot order and
take the __first decisive result__, else 'BlockedByDefault' with every non-decisive
reason gathered in boot order.

The engine evaluates effectful rules speculatively in parallel but the decision is
always __as-if sequential by boot order__ -- the earliest-in-order decisive rule wins,
never the first to return in wall-clock time. A rule with no 'Resilience' is evaluated
directly; a contiguous run of resilient rules is launched concurrently, then awaited
in boot order, and the moment the earliest decisive one is known every still-running
strictly-later evaluation is cancelled. No IO an earlier decisive result would moot is
ever launched, because a resilient run is started only once every rule before it is
known non-decisive.
-}
evalRules :: EvalContext -> [PreparedRule] -> PackageDetails -> IO Decision
evalRules ctx rules pd = step (bootOrder rules) []
  where
    -- 'reasons' accumulates non-decisive reasons in reverse boot order; the final
    -- deny-by-default list is reversed back into boot order.
    step :: [PreparedRule] -> [Reason] -> IO Decision
    step [] reasons = pure (BlockedByDefault (reverse reasons))
    step (r : rs) reasons
        | isNothing (prepResilience r) = do
            -- A direct rule is zero-cost: run it in place. Reaching it means every
            -- earlier rule was non-decisive, so no speculated IO has been mooted.
            res <- prepEval r ctx pd
            case decisive (prepName r) res of
                Just d -> pure d
                Nothing -> step rs (reasonOf res : reasons)
        | otherwise =
            -- A maximal contiguous block of resilient rules: launch it concurrently
            -- and resolve it in boot order. Stopping the block at the next direct rule
            -- keeps the "no mooted IO" guarantee -- a later direct rule is evaluated, and
            -- may decide, before any resilient rule beyond it is launched.
            let (block, rest) = span (isJust . prepResilience) (r : rs)
             in evalBlock ctx pd block >>= \case
                    Left d -> pure d
                    Right blockReasons -> step rest (reverse blockReasons <> reasons)

-- Launch a contiguous resilient block concurrently, then await in boot order:
-- 'Left' the earliest decisive winner (with every strictly-later evaluation
-- cancelled), or 'Right' the block's non-decisive reasons in boot order. 'bracket'
-- guarantees every launched evaluation is cancelled on any exit.
evalBlock :: EvalContext -> PackageDetails -> [PreparedRule] -> IO (Either Decision [Reason])
evalBlock ctx pd block =
    bracket
        (traverse (\r -> async (runEffectfulRule ctx r pd)) block)
        (traverse_ uninterruptibleCancel)
        (\asyncs -> awaitInOrder (zip block asyncs) [])

-- Await a launched block's evaluations in boot order; a decisive winner cancels
-- every strictly-later one.
awaitInOrder :: [(PreparedRule, Async RuleResult)] -> [Reason] -> IO (Either Decision [Reason])
awaitInOrder [] reasons = pure (Right (reverse reasons))
awaitInOrder ((r, a) : rest) reasons = do
    res <- wait a
    case decisive (prepName r) res of
        Just d -> do
            traverse_ (cancel . snd) rest
            pure (Left d)
        Nothing -> awaitInOrder rest (reasonOf res : reasons)

-- Map a rule result to the 'Decision' it credits if decisive, or 'Nothing' if it is a
-- no-op (the only runtime classification -- there is no comparison of competing
-- results, the boot order having already settled who wins).
decisive :: Text -> RuleResult -> Maybe Decision
decisive name = \case
    Allow reason -> Just (Admitted name reason)
    Deny reason -> Just (Blocked name reason)
    Unavailable transience FailDeny reason -> Just (Undecidable transience reason)
    NoDecision _ -> Nothing
    Unavailable _ FailNoDecision _ -> Nothing

-- The audit reason carried by any result, gathered for the deny-by-default trail.
reasonOf :: RuleResult -> Reason
reasonOf = \case
    Allow reason -> reason
    Deny reason -> reason
    NoDecision reason -> reason
    Unavailable _ _ reason -> reason

{- | Run one prepared rule through its resilience policy. A rule with no 'Resilience'
(@'prepResilience' = 'Nothing'@) runs directly. A resilient rule's IO runs under its
circuit-breaker gate, a per-attempt timeout, and bounded retry with backoff: a clean
verdict ('Allow'\/'Deny'\/'NoDecision') resets the breaker and is returned; an
exhausted rule (timeout, exception, the breaker open, or the rule self-reporting
'Unavailable' on every attempt) advances the breaker and resolves to @'Unavailable'
transience alignment reason@ -- the alignment from the rule's 'Resilience' (fail-closed
or fail-open), the transience from the last failing attempt.

The breaker timing reads the 'EvalContext' clock, so it is deterministic under test.
Total -- it never throws; a rule failure becomes a result.
-}
runEffectfulRule :: EvalContext -> PreparedRule -> PackageDetails -> IO RuleResult
runEffectfulRule ctx rule pd = case prepResilience rule of
    Nothing -> prepEval rule ctx pd
    Just res -> do
        let now = ctxNow ctx
        admitted <- admitProbe res now
        if not admitted
            then -- Breaker open and still cooling down: fast-fail without running the
            -- rule's IO, the cheap path a sustained outage stays on. An open breaker
            -- is an infrastructural outage, so it is transient.
                pure (exhausted res (prepName rule) (transientCause (resConfig res)) "the rule source circuit breaker is open")
            else do
                result <- attemptWithRetry res (prepEval rule ctx) pd
                settleOutcome res (prepName rule) now result

{- Settle a finished retry run against the breaker: a clean verdict resets the
breaker and is returned; an exhausted run advances the breaker and resolves to
the rule's aligned 'Unavailable'. -}
settleOutcome :: Resilience -> Text -> UTCTime -> Either Transience RuleResult -> IO RuleResult
settleOutcome res name now = \case
    Right outcome -> do
        commitBreaker res recordSuccess
        pure outcome
    Left transience -> do
        commitBreaker res (tripOnFailure (resConfig res) now)
        pure (exhausted res name transience "the rule could not be evaluated")

{- Attempt the rule's IO under the per-attempt timeout, retrying with backoff until
the retry budget is spent. 'Right' a clean verdict on success; 'Left' the 'Transience'
of the last failing attempt when every attempt failed (an exception, a timeout, or
the rule yielding its own 'Unavailable'). 'retrying' returns the final value the
action produced, so the surfaced transience is the last attempt's: a permanent
('WontResolve') self-report on that attempt is honoured through to the serve mapping,
while any other failure stays transient. A rule that returns
'Allow'\/'Deny'\/'NoDecision' is taken at face value and not retried. -}
attemptWithRetry :: Resilience -> (PackageDetails -> IO RuleResult) -> PackageDetails -> IO (Either Transience RuleResult)
attemptWithRetry res evalAt pd =
    retrying (backoffPolicy (ecBackoff (resConfig res))) shouldRetry (\_ -> attemptOnce res evalAt pd)
  where
    shouldRetry _ = pure . isLeft

{- | An 'ecBackoff' schedule compiled to a "Control.Retry" policy: the retry at
iteration n waits the n-th delay (microseconds) before it, and the policy stops
(yields 'Nothing') once the schedule is exhausted -- so the list's length is the retry
budget. @[]@ admits no retry (a single attempt); @[a, b]@ admits up to two. Inspect
the resulting delays without sleeping with 'Control.Retry.simulatePolicy'.
-}
backoffPolicy :: [Int] -> RetryPolicyM IO
backoffPolicy backoffs = RetryPolicyM (\rs -> pure (backoffs !!? rsIterNumber rs))

{- One attempt: run the rule's IO under the timeout, catching any exception. 'Right'
a clean verdict; 'Left' the 'Transience' to surface should the retry budget be
exhausted on this attempt. A timeout, an exception, or a rule reporting its own
transient unavailability is 'WillResolve' (an infrastructural outage the configured
'RetryAfter' applies to); a rule reporting its own /permanent/ ('WontResolve')
unavailability keeps that distinction so an internal\/parse fault is not later
dressed up as retryable. Either way a self-reported 'Unavailable' still counts as a
failed attempt -- the harness retries and trips the breaker rather than trusting a
single self-report -- only the transience it carries on exhaustion differs. -}
attemptOnce :: Resilience -> (PackageDetails -> IO RuleResult) -> PackageDetails -> IO (Either Transience RuleResult)
attemptOnce res evalAt pd = do
    result <- tryAny (timeout (ecTimeout (resConfig res)) (evalAt pd))
    pure $ case result of
        Left _ -> Left transient -- the rule's IO threw
        Right Nothing -> Left transient -- the attempt timed out
        Right (Just (Unavailable WontResolve _ _)) -> Left WontResolve -- a permanent self-report, honoured
        Right (Just (Unavailable (WillResolve _) _ _)) -> Left transient -- a transient self-report
        Right (Just clean) -> Right clean
  where
    transient = transientCause (resConfig res)

{- The result an exhausted rule resolves to: @'Unavailable' transience alignment@ --
'WillResolve' for an infrastructural failure (a timeout, an exception, an open
breaker) or a self-reported transient, 'WontResolve' for a self-reported permanent
fault; the alignment is the rule's own (fail-closed 'FailDeny' or fail-open
'FailNoDecision'). The reason is carried for the audit trail. -}
exhausted :: Resilience -> Text -> Transience -> Text -> RuleResult
exhausted res name transience reason = Unavailable transience (resAlignment res) (name <> ": " <> reason)

{- The transient 'Transience' an infrastructural failure (a timeout, an exception, an
open breaker) surfaces: retryable, carrying the rule's configured 'RetryAfter'. -}
transientCause :: EffectfulConfig -> Transience
transientCause cfg = WillResolve (ecRetryAfter cfg)

{- The breaker admission gate: defer the decision to 'Ecluse.Core.Breaker.admit' and
commit the breaker state it returns, reporting any change (a half-open recovery probe).
See 'Ecluse.Core.Breaker.admit' for the admission policy. -}
admitProbe :: Resilience -> UTCTime -> IO Bool
admitProbe res now = do
    (permitted, old, new) <- atomically $ do
        st <- readTVar (resBreaker res)
        let (p, st') = admit now st
        writeTVar (resBreaker res) st'
        pure (p, st, st')
    reportBreakerChange (resBreakerReporter res) old new
    pure permitted

{- Commit a breaker fold to this rule's breaker and report any observable state change
it makes (a trip, a reset). Reads the breaker before and after in one transaction so the
report reflects exactly the transition committed. -}
commitBreaker :: Resilience -> (Breaker -> Breaker) -> IO ()
commitBreaker res step = do
    (old, new) <- atomically $ do
        st <- readTVar (resBreaker res)
        let st' = step st
        writeTVar (resBreaker res) st'
        pure (st, st')
    reportBreakerChange (resBreakerReporter res) old new

{- Advance the breaker on a failed evaluation per this rule's configured threshold
and cooldown ('Ecluse.Core.Breaker.recordFailure'). -}
tripOnFailure :: EffectfulConfig -> UTCTime -> Breaker -> Breaker
tripOnFailure cfg = recordFailure (ecBreakerThreshold cfg) (ecBreakerCooldown cfg)

{- | The resilience knobs around an effectful rule's IO: a per-attempt timeout,
how many retries to make on failure with the backoff before each, and the breaker
threshold and cooldown. The breaker clock is the 'EvalContext'.
-}
data EffectfulConfig = EffectfulConfig
    { ecTimeout :: Int
    {- ^ The per-attempt timeout in microseconds. An attempt that does not return
    within it is treated as a failure (a transient, retryable cause).
    -}
    , ecBackoff :: [Int]
    {- ^ The backoff delays in microseconds, one per retry, applied __before__ the
    corresponding retry attempt. Its length is the retry budget: @[]@ means the
    single initial attempt only, @[100, 200]@ means up to two retries after it.
    -}
    , ecBreakerThreshold :: Int
    -- ^ Consecutive exhausted-rule failures that trip the breaker.
    , ecBreakerCooldown :: NominalDiffTime
    {- ^ How long the breaker stays open (fast-failing the rule) before a single
    half-open probe is allowed to test recovery.
    -}
    , ecRetryAfter :: Maybe RetryAfter
    {- ^ The @Retry-After@ delay to suggest to a client when this rule's
    unavailability surfaces on a concrete-artifact request; 'Nothing' suggests none.
    -}
    }

{- | Sensible defaults for the resilience knobs: a 2-second per-attempt timeout, two
retries at 100ms then 250ms, and a breaker tripping after 5 consecutive failures and
cooling for 30 seconds. The caller supplies the rule's IO; the knobs are policy with
these defaults.
-}
defaultEffectfulConfig :: EffectfulConfig
defaultEffectfulConfig =
    EffectfulConfig
        { ecTimeout = 2_000_000
        , ecBackoff = [100_000, 250_000]
        , ecBreakerThreshold = 5
        , ecBreakerCooldown = 30
        , ecRetryAfter = Nothing
        }

-- | A fresh, healthy breaker (no failures recorded) in a new 'TVar'.
newBreaker :: IO (TVar Breaker)
newBreaker = newTVarIO initialBreaker

{- | A human-readable summary of a decision, suitable for logs and the denial
response body.
-}
renderDecision :: PackageDetails -> Decision -> Text
renderDecision pd decision =
    let subject = renderPackageName (pkgName pd) <> "@" <> renderVersion (pkgVersion pd)
     in case decision of
            Admitted name reason ->
                subject <> " was approved by " <> name <> ": " <> reason
            Blocked name reason ->
                subject <> " was denied by " <> name <> ": " <> reason
            BlockedByDefault reasons ->
                subject
                    <> " was denied (no rule allowed it)"
                    <> if null reasons
                        then ""
                        else ": " <> T.intercalate "; " reasons
            Undecidable _ reason ->
                subject <> " could not be evaluated: " <> reason

{- | Render a duration as an approximate, human-friendly string for use in
decision messages. Always non-negative.

>>> renderDuration 604800
"7 days"

>>> renderDuration 90
"1 minute"
-}
renderDuration :: NominalDiffTime -> Text
renderDuration d =
    let secs = max 0 (round (realToFrac d :: Double)) :: Integer
     in pick units secs
  where
    units :: [(Text, Integer)]
    units =
        [ ("day", 86400)
        , ("hour", 3600)
        , ("minute", 60)
        ]
    pick [] secs = plural secs "second"
    pick ((unit, size) : rest) secs
        | secs >= size = plural (secs `div` size) unit
        | otherwise = pick rest secs
    plural n unit = show n <> " " <> unit <> (if n == 1 then "" else "s")
