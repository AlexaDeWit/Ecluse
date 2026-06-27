{- | The policy rules engine.

A rule set is evaluated against a single 'PackageDetails' snapshot to produce a
'Decision'. The model is __deny by default; the boot order decides__: the configured
rules are arranged once, at boot, into a single total order ('bootOrder') — highest
precedence first, then rule name ascending — and evaluation walks that order and
takes the __first decisive result__. A result is decisive iff it is 'Allow', 'Deny',
or an @'Unavailable' _ 'FailDeny' _@ (a fail-closed uncomputable check); 'NoDecision'
and @'Unavailable' _ 'FailNoDecision' _@ are non-decisive no-ops whose reasons are
collected, in boot order, for the deny-by-default audit trail. If no rule is decisive
the package is 'BlockedByDefault'.

There is __one rule representation__ ('Rule') and __one engine__. A pure rule lifts
into the rule shape at no cost ('liftPureRule', evaluating directly); an effectful
rule carries a 'Resilience' policy (a per-attempt timeout, bounded retry with
backoff, and a per-source 'Ecluse.Core.Breaker.Breaker') applied by the harness
'runEffectfulRule'. The order /is/ the tiebreak: there is no runtime comparison of
results, so the duplicated winner-selection the two-tier design once carried is gone.

'evalRules' may evaluate effectful rules speculatively in parallel, but the result is
always __as-if sequential by boot order__: the winner is the earliest-in-order
decisive rule, never the first to return in wall-clock time, and once the winner is
known every still-running strictly-later evaluation is cancelled. The cheap pure
prefix is evaluated directly, so no IO an earlier pure decisive result would moot is
ever launched. 'evalRulesPure' is the pure entry point over the configuration
vocabulary. The rule data types live in "Ecluse.Core.Rules.Types".
-}
module Ecluse.Core.Rules (
    -- * The uniform rule
    Rule (..),
    Resilience (..),
    liftPureRule,
    liftPolicy,
    pureRuleName,
    evalPureRule,

    -- * Boot-time ordering
    bootOrder,
    renderBootOrder,

    -- * Evaluation
    evalRules,
    evalRulesPure,
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
import Ecluse.Core.Package

-- 'PrecededRule' carries its own @rulePrecedence@ field (config's resolved-policy
-- element); the engine 'Rule' record below carries one too. They are distinct fields
-- on distinct types, so the config one is hidden here and read by pattern match.
import Ecluse.Core.Rules.Types hiding (rulePrecedence)
import Ecluse.Core.Version (renderVersion)

-- ── the uniform rule ────────────────────────────────────────────────────────────

{- | The engine's __one__ rule representation: a rule carries only its definition —
the precedence it competes at, a stable name (its identity, and the boot-order
tiebreak), the IO that evaluates it for one version, and an optional 'Resilience'
policy.

It declares no allow\/deny "direction": admit vs block is simply what 'ruleEval'
returns. A pure rule lifts in via 'liftPureRule' with @ruleResilience = Nothing@ and
runs directly; an effectful rule sets a 'Resilience' and is wrapped by
'runEffectfulRule'. @m@ is the monad evaluation runs in — 'IO' for 'evalRules'.
-}
data Rule m = Rule
    { rulePrecedence :: Int
    -- ^ The precedence at which this rule competes; higher wins in the boot order.
    , ruleName :: Text
    -- ^ The stable, human-facing name; the boot-order tiebreak and the credited identity.
    , ruleEval :: EvalContext -> PackageDetails -> m RuleResult
    {- ^ The rule's verdict for one version. For a resilient rule it may perform IO
    that fails or hangs; 'runEffectfulRule' wraps it.
    -}
    , ruleResilience :: Maybe Resilience
    -- ^ The resilience policy, or 'Nothing' for a pure rule run directly.
    }

{- | The resilience policy wrapped around an effectful rule's IO: the timeout\/retry\/
breaker knobs, the per-source circuit-breaker state, its observer, and the
__failure alignment__ an exhausted evaluation resolves to (fail-closed 'FailDeny' or
fail-open 'FailNoDecision'). The alignment rides on the rule, folding away the
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

{- | Lift a configured pure rule ('PrecededRule') into the engine's rule shape: it
evaluates directly ('evalPureRule') and carries no 'Resilience', so it runs at no
cost — pure and effectful rules thereafter share one representation and one engine.
-}
liftPureRule :: (Applicative m) => PrecededRule -> Rule m
liftPureRule (PrecededRule prec pr) =
    Rule
        { rulePrecedence = prec
        , ruleName = pureRuleName pr
        , ruleEval = \ctx pd -> pure (evalPureRule ctx pr pd)
        , ruleResilience = Nothing
        }

-- | Lift a whole resolved pure policy into the engine's rule list.
liftPolicy :: (Applicative m) => [PrecededRule] -> [Rule m]
liftPolicy = map liftPureRule

-- | A stable, human-facing name for a pure rule (for logs and denial messages).
pureRuleName :: PureRule -> Text
pureRuleName = \case
    AllowScope{} -> "AllowScope"
    AllowIfPublishedBefore{} -> "AllowIfPublishedBefore"
    DenyInstallTimeExecution -> "DenyInstallTimeExecution"

{- | Evaluate a single pure rule against a single package version. Total — a
malformed rule or package yields a result, never an exception, so hostile metadata
cannot crash the gate. A pure rule never yields 'Unavailable'.
-}
evalPureRule :: EvalContext -> PureRule -> PackageDetails -> RuleResult
evalPureRule _ (AllowScope scope) pd =
    case pkgNamespace (pkgName pd) of
        Just s
            | s == scope ->
                Allow ("scope " <> renderScope scope <> " is allow-listed")
        _ ->
            NoDecision ("scope is not the allow-listed " <> renderScope scope)
evalPureRule ctx (AllowIfPublishedBefore minAge) pd =
    case pkgPublishedAt pd of
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
evalPureRule _ DenyInstallTimeExecution pd =
    case pkgInstallCode pd of
        RunsCodeOnInstall how -> Deny ("runs code on install: " <> how)
        NoCodeOnInstall -> NoDecision "no install-time code execution"
        CodeExecUnknown -> NoDecision "install-time code execution not yet determined"

-- ── boot-time ordering ──────────────────────────────────────────────────────────

{- | Arrange a rule set into the single total order evaluation walks: __highest
precedence first, then rule name ascending__ as the deterministic tiebreak. A pure
function of the rules' precedences and names, independent of the order they were
configured in — so shuffling the configured set yields the same order and hence the
same 'Decision'. The order /is/ the tiebreak; there is no runtime comparison of
results.
-}
bootOrder :: [Rule m] -> [Rule m]
bootOrder = sortOn (\r -> bootKey (rulePrecedence r) (ruleName r))

-- The single boot-order comparator key: precedence descending (highest first), then
-- name ascending. Both the pure entry point and the IO engine order through this one
-- key, so the tiebreak is expressed exactly once.
bootKey :: Int -> Text -> (Down Int, Text)
bootKey prec name = (Down prec, name)

{- | Render the boot order as one diagnostic line per rule, in evaluation order, so
an operator sees at boot exactly how their policy will resolve. Empty for an empty
rule set.
-}
renderBootOrder :: [Rule m] -> [Text]
renderBootOrder rules = zipWith line [1 :: Int ..] (bootOrder rules)
  where
    line i r =
        "rule "
            <> show i
            <> ": "
            <> ruleName r
            <> " (precedence "
            <> show (rulePrecedence r)
            <> ")"

-- ── evaluation ──────────────────────────────────────────────────────────────────

{- | Evaluate a package version against a rule set in 'IO': walk the boot order and
take the __first decisive result__, else 'BlockedByDefault' with every non-decisive
reason gathered in boot order.

The engine evaluates effectful rules speculatively in parallel but the decision is
always __as-if sequential by boot order__ — the earliest-in-order decisive rule wins,
never the first to return in wall-clock time. The cheap pure prefix is evaluated
directly; a contiguous run of effectful rules is launched concurrently, then awaited
in boot order, and the moment the earliest decisive one is known every still-running
strictly-later evaluation is cancelled. No IO an earlier pure decisive result would
moot is ever launched, because an effectful run is started only once every rule
before it is known non-decisive.
-}
evalRules :: EvalContext -> [Rule IO] -> PackageDetails -> IO Decision
evalRules ctx rules pd = step (bootOrder rules) []
  where
    -- 'reasons' accumulates non-decisive reasons in reverse boot order; the final
    -- deny-by-default list is reversed back into boot order.
    step :: [Rule IO] -> [Reason] -> IO Decision
    step [] reasons = pure (BlockedByDefault (reverse reasons))
    step (r : rs) reasons
        | isNothing (ruleResilience r) = do
            -- A pure rule is zero-cost: run it directly. Reaching it means every
            -- earlier rule was non-decisive, so no speculated IO has been mooted.
            res <- ruleEval r ctx pd
            case decisive (ruleName r) res of
                Just d -> pure d
                Nothing -> step rs (reasonOf res : reasons)
        | otherwise =
            -- A maximal contiguous block of effectful rules: launch it concurrently
            -- and resolve it in boot order. Stopping the block at the next pure rule
            -- keeps the "no mooted IO" guarantee — a later pure rule is evaluated, and
            -- may decide, before any effectful rule beyond it is launched.
            let (block, rest) = span (isJust . ruleResilience) (r : rs)
             in evalBlock block >>= \case
                    Left d -> pure d
                    Right blockReasons -> step rest (reverse blockReasons <> reasons)

    -- Launch a contiguous effectful block concurrently, then await in boot order:
    -- 'Left' the earliest decisive winner (with every strictly-later evaluation
    -- cancelled), or 'Right' the block's non-decisive reasons in boot order. 'bracket'
    -- guarantees every launched evaluation is cancelled on any exit.
    evalBlock :: [Rule IO] -> IO (Either Decision [Reason])
    evalBlock block =
        bracket
            (traverse (\r -> async (runEffectfulRule ctx r pd)) block)
            (traverse_ uninterruptibleCancel)
            (\asyncs -> awaitInOrder (zip block asyncs) [])

    awaitInOrder :: [(Rule IO, Async RuleResult)] -> [Reason] -> IO (Either Decision [Reason])
    awaitInOrder [] reasons = pure (Right (reverse reasons))
    awaitInOrder ((r, a) : rest) reasons = do
        res <- wait a
        case decisive (ruleName r) res of
            Just d -> do
                traverse_ (cancel . snd) rest
                pure (Left d)
            Nothing -> awaitInOrder rest (reasonOf res : reasons)

{- | Evaluate a package version against a __resolved pure policy__, with no IO. The
pure entry point: it walks the same boot order 'evalRules' does (lifting each pure
rule in) and takes the first decisive result, else 'BlockedByDefault' with the
non-decisive reasons in boot order. Used where the rule set is known pure (the
packument filter's typed decision).
-}
evalRulesPure :: EvalContext -> [PrecededRule] -> PackageDetails -> Decision
evalRulesPure ctx prs pd = go ordered []
  where
    ordered = sortOn (\(PrecededRule prec pr) -> bootKey prec (pureRuleName pr)) prs
    go [] reasons = BlockedByDefault (reverse reasons)
    go (PrecededRule _ pr : rest) reasons =
        let res = evalPureRule ctx pr pd
         in case decisive (pureRuleName pr) res of
                Just d -> d
                Nothing -> go rest (reasonOf res : reasons)

-- Map a rule result to the 'Decision' it credits if decisive, or 'Nothing' if it is a
-- no-op (the only runtime classification — there is no comparison of competing
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

-- ── the resilience harness ──────────────────────────────────────────────────────

{- | Run one rule through its resilience policy. A pure rule (@ruleResilience =
Nothing@) runs directly. A resilient rule's IO runs under its circuit-breaker gate,
a per-attempt timeout, and bounded retry with backoff: a clean verdict
('Allow'\/'Deny'\/'NoDecision') resets the breaker and is returned; an exhausted rule
(timeout, exception, the breaker open, or the rule self-reporting 'Unavailable' on
every attempt) advances the breaker and resolves to @'Unavailable' transience
alignment reason@ — the alignment from the rule's 'Resilience' (fail-closed or
fail-open), the transience from the last failing attempt.

The breaker timing reads the 'EvalContext' clock, so it is deterministic under test.
Total — it never throws; a rule failure becomes a result.
-}
runEffectfulRule :: EvalContext -> Rule IO -> PackageDetails -> IO RuleResult
runEffectfulRule ctx rule pd = case ruleResilience rule of
    Nothing -> ruleEval rule ctx pd
    Just res -> do
        let now = ctxNow ctx
        admitted <- admitProbe res now
        if not admitted
            then -- Breaker open and still cooling down: fast-fail without running the
            -- rule's IO, the cheap path a sustained outage stays on. An open breaker
            -- is an infrastructural outage, so it is transient.
                pure (exhausted res (ruleName rule) (transientCause (resConfig res)) "the rule source circuit breaker is open")
            else do
                result <- attemptWithRetry res (ruleEval rule ctx) pd
                case result of
                    Right outcome -> do
                        commitBreaker res recordSuccess
                        pure outcome
                    Left transience -> do
                        commitBreaker res (tripOnFailure (resConfig res) now)
                        pure (exhausted res (ruleName rule) transience "the rule could not be evaluated")

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
(yields 'Nothing') once the schedule is exhausted — so the list's length is the retry
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
failed attempt — the harness retries and trips the breaker rather than trusting a
single self-report — only the transience it carries on exhaustion differs. -}
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

{- The result an exhausted rule resolves to: @'Unavailable' transience alignment@ —
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

-- ── the breaker gate ────────────────────────────────────────────────────────────

{- The breaker admission gate: defer the decision to 'Ecluse.Core.Breaker.admit' and
commit the breaker state it returns, reporting any change (a half-open recovery probe).
While open and cooling down it denies; once the cooldown elapses it moves to half-open
and admits a single probe; a closed or half-open breaker always admits. -}
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

-- ── resilience knobs ────────────────────────────────────────────────────────────

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

-- ── rendering ───────────────────────────────────────────────────────────────────

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
