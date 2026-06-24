{- | The effectful rule tier, layered on the pure one in "Ecluse.Rules".

A pure rule reasons over the 'PackageDetails' an adapter already fetched; an
__effectful__ rule may do IO to learn its signal — consult a synced advisory index,
fetch and parse a gemspec, call an external policy check. That IO can fail or hang,
so each effectful rule is wrapped in a resilience harness before its verdict is
trusted: a __timeout budget__, __bounded retry with backoff__, and a per-source
__circuit breaker__. When a rule the evaluator /needed/ still cannot be consulted,
it is __fail-closed__ — the version is 'Ecluse.Rules.Types.Undecidable', not
admitted — unless the rule opts into 'OnAbstain', where availability beats safety.

== Tier is performance, not precedence

The two tiers are a __performance ordering__: the pure tier runs first because it is
cheap, and an effectful rule is consulted only where it could still change the
outcome. Once the pure tier yields a winner at precedence /P/, an effectful rule
ranked below /P/ cannot outrank it and is __skipped__ (its IO never runs), and the
effectful tier is skipped __entirely__ when no effectful rule is ranked at or above
/P/. Precedence — not tier — still decides who wins: the surviving effectful
candidates compete against the pure winner under the same
@(precedence, deny-before-allow)@ comparator the pure tier uses, so a higher-ranked
effectful deny overrides a lower pure allow and vice versa.

== Resilience

Each rule carries an 'EffectfulConfig': a timeout per attempt, a bounded retry
count with a backoff schedule, and a breaker threshold/cooldown. The 'ecBackoff'
schedule is compiled to a "Control.Retry" 'RetryPolicyM': the n-th retry waits the
n-th delay, and the list's length /is/ the retry budget, so @[]@ is a single attempt
and the schedule runs out rather than retrying forever. The breaker is the shared
"Ecluse.Breaker" state machine, kept per source as a 'TVar' so repeated failures of
one advisory source fast-fail without latency or hammering, while a half-open probe
tests recovery. The breaker clock is read from the
'Ecluse.Rules.Types.EvalContext', so its timing is deterministic under test, and a
test that asserts the retry /schedule/ does so without sleeping via
'Control.Retry.simulatePolicy' over the same policy the harness runs.

See @docs\/architecture\/rules-engine.md@ → "Effectful-rule failure".
-}
module Ecluse.Rules.Effectful (
    -- * Effectful rules
    EffectfulRule (..),
    PrecededEffectfulRule (..),
    FailurePolicy (..),

    -- * Resilience
    EffectfulConfig (..),
    defaultEffectfulConfig,
    backoffPolicy,
    Breaker (..),
    newBreaker,

    -- * Evaluation
    evalRulesEffectful,
    runEffectfulRule,
) where

import Control.Retry (
    RetryPolicyM (RetryPolicyM),
    RetryStatus (rsIterNumber),
    retrying,
 )
import Data.List (maximumBy)
import Data.Time (NominalDiffTime, UTCTime)
import UnliftIO (timeout, tryAny)

import Ecluse.Breaker (Breaker (..), admit, initialBreaker, recordFailure, recordSuccess)
import Ecluse.Package (PackageDetails)
import Ecluse.Rules (evalRulesWithPrecedence)
import Ecluse.Rules.Types (
    Decision (Approved, ApprovedEffectful, DeniedEffectful, Undecidable),
    EvalContext (ctxNow),
    PrecededRule,
    RetryAfter,
    RuleOutcome (Abstain, Allow, Deny, Unavailable),
    Transience (WillResolve),
 )

-- ── effectful rules ───────────────────────────────────────────────────────────

{- | What to do when an effectful rule cannot be consulted after the harness has
exhausted its timeout, retries, and the breaker. The default across the engine is
__fail-closed__; a rule opts out only where availability must beat safety.
-}
data FailurePolicy
    = {- | __Fail closed.__ An exhausted rule yields 'Unavailable' (the version is
      not admitted). This is the default: a never-vetted package is not let in just
      because the scanner is down.
      -}
      OnUnavailable
    | {- | __Fail open.__ An exhausted rule 'Abstain's instead, yielding the floor to
      other rules. For a rule (an /allow/ direction, say) where a missing signal
      should not block availability — it simply does not fire.
      -}
      OnAbstain
    deriving stock (Eq, Show)

{- | An effectful rule: its name, the IO that learns its verdict for one version,
the resilience knobs, the failure policy, and its per-source circuit breaker.

The breaker is a 'TVar' shared across every evaluation of this rule (it is the one
source's health), so repeated failures trip it once and fast-fail subsequent
evaluations until the cooldown elapses. 'erEval' is the only part that touches a
network or other unreliable resource; everything else is policy.
-}
data EffectfulRule = EffectfulRule
    { erName :: Text
    -- ^ A stable, human-facing name for logs and the audit trail.
    , erEval :: PackageDetails -> IO RuleOutcome
    {- ^ The rule's verdict for one version. May perform IO that fails or hangs;
    the harness wraps it. Should yield 'Allow'\/'Deny'\/'Abstain' on success and
    raise an exception (or yield 'Unavailable') when its source is unreachable.
    -}
    , erConfig :: EffectfulConfig
    -- ^ The timeout, retry, and breaker knobs for this rule.
    , erOnError :: FailurePolicy
    -- ^ Whether an exhausted evaluation fails closed ('OnUnavailable') or open ('OnAbstain').
    , erBreaker :: TVar Breaker
    -- ^ This rule's per-source circuit-breaker state, shared across evaluations.
    }

{- | An 'EffectfulRule' paired with the integer precedence at which it competes
(higher wins), mirroring 'Ecluse.Rules.Types.PrecededRule' for the pure tier. The
precedence is what decides whether the rule could still change the outcome and so
whether its IO runs at all.
-}
data PrecededEffectfulRule = PrecededEffectfulRule
    { perPrecedence :: Int
    -- ^ The precedence at which this effectful rule competes; higher wins.
    , perRule :: EffectfulRule
    -- ^ The rule itself.
    }

-- ── resilience ────────────────────────────────────────────────────────────────

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
cooling for 30 seconds. The caller supplies the rule's 'erEval'; the knobs are policy
with these defaults.
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

-- ── the resilience harness ────────────────────────────────────────────────────

{- | Run one effectful rule through its resilience harness: the circuit breaker
gates the attempt, then the rule's IO runs under a per-attempt timeout with bounded
retry and backoff. A clean verdict ('Allow'\/'Deny'\/'Abstain') resets the breaker
and is returned; an exhausted rule (timeout, exception, the breaker open, or the
rule itself yielding 'Unavailable' on every attempt) advances the breaker and
resolves per the rule's 'erOnError' — 'Unavailable' (fail-closed) or 'Abstain'
(fail-open).

The breaker timing reads the 'EvalContext' clock, so it is deterministic under test.
Total — it never throws; a rule failure becomes an outcome.
-}
runEffectfulRule :: EvalContext -> EffectfulRule -> PackageDetails -> IO RuleOutcome
runEffectfulRule ctx rule pd = do
    let now = ctxNow ctx
    admitted <- atomically (admitProbe (erBreaker rule) now)
    if not admitted
        then -- Breaker open and still cooling down: fast-fail without running the
        -- rule's IO, the cheap path a sustained outage stays on.
            pure (exhausted rule "the rule source circuit breaker is open")
        else do
            result <- attemptWithRetry rule pd
            case result of
                Just outcome -> do
                    atomically (modifyTVar' (erBreaker rule) recordSuccess)
                    pure outcome
                Nothing -> do
                    atomically (modifyTVar' (erBreaker rule) (tripOnFailure (erConfig rule) now))
                    pure (exhausted rule "the rule could not be evaluated")

{- Attempt the rule's IO under the per-attempt timeout, retrying with backoff until
the retry budget is spent. 'Just' a clean verdict on success; 'Nothing' when every
attempt failed (an exception, a timeout, or the rule yielding its own 'Unavailable').
A rule that returns 'Allow'\/'Deny'\/'Abstain' is taken at face value and not
retried — 'retrying' stops as soon as the attempt yields 'Just'. -}
attemptWithRetry :: EffectfulRule -> PackageDetails -> IO (Maybe RuleOutcome)
attemptWithRetry rule pd =
    retrying (backoffPolicy (ecBackoff (erConfig rule))) shouldRetry (\_ -> attemptOnce rule pd)
  where
    shouldRetry _ = pure . isNothing

{- | An 'ecBackoff' schedule compiled to a "Control.Retry" policy: the retry at
iteration n waits the n-th delay (microseconds) before it, and the policy stops
(yields 'Nothing') once the schedule is exhausted — so the list's length is the retry
budget. @[]@ admits no retry (a single attempt); @[a, b]@ admits up to two. Inspect
the resulting delays without sleeping with 'Control.Retry.simulatePolicy'.
-}
backoffPolicy :: [Int] -> RetryPolicyM IO
backoffPolicy backoffs = RetryPolicyM (\rs -> pure (backoffs !!? rsIterNumber rs))

{- One attempt: run the rule's IO under the timeout, catching any exception. 'Just'
a clean verdict; 'Nothing' on a timeout, an exception, or the rule yielding
'Unavailable' (which counts as a failed attempt so the harness retries and trips the
breaker rather than trusting an unavailability the rule reported itself). -}
attemptOnce :: EffectfulRule -> PackageDetails -> IO (Maybe RuleOutcome)
attemptOnce rule pd = do
    result <- tryAny (timeout (ecTimeout (erConfig rule)) (erEval rule pd))
    pure $ case result of
        Left _ -> Nothing -- the rule's IO threw
        Right Nothing -> Nothing -- the attempt timed out
        Right (Just (Unavailable _ _)) -> Nothing -- the rule reported unavailability
        Right (Just clean) -> Just clean

{- The outcome an exhausted rule resolves to, per its failure policy: 'Unavailable'
(fail-closed, the default) carrying the configured 'Transience' — always transient
('WillResolve'), since an exhausted source is expected to recover — or 'Abstain'
(fail-open). The reason is carried for the audit trail either way. -}
exhausted :: EffectfulRule -> Text -> RuleOutcome
exhausted rule reason = case erOnError rule of
    OnUnavailable -> Unavailable (WillResolve (ecRetryAfter (erConfig rule))) detail
    OnAbstain -> Abstain detail
  where
    detail = erName rule <> ": " <> reason

-- ── the breaker gate ──────────────────────────────────────────────────────────

{- The breaker admission gate: defer the decision to 'Ecluse.Breaker.admit' and
commit the breaker state it returns. While open and cooling down it denies; once the
cooldown elapses it moves to half-open and admits a single probe; a closed or
half-open breaker always admits. -}
admitProbe :: TVar Breaker -> UTCTime -> STM Bool
admitProbe breaker now = do
    st <- readTVar breaker
    let (permitted, st') = admit now st
    writeTVar breaker st'
    pure permitted

{- Advance the breaker on a failed evaluation per this rule's configured threshold
and cooldown ('Ecluse.Breaker.recordFailure'). -}
tripOnFailure :: EffectfulConfig -> UTCTime -> Breaker -> Breaker
tripOnFailure cfg = recordFailure (ecBreakerThreshold cfg) (ecBreakerCooldown cfg)

-- ── evaluation ────────────────────────────────────────────────────────────────

{- | Evaluate a package version against both tiers, the effectful tier layered on
the pure one. Returns the same 'Decision' the pure 'Ecluse.Rules.evalRules' would,
unless an effectful rule that could still change the outcome takes a position.

The pure tier runs first. Its winning precedence /P/ then bounds the effectful work:
only effectful rules ranked at or above /P/ are consulted — a lower-ranked one
cannot outrank the pure winner — and the effectful tier is skipped entirely (no IO)
when none qualifies, so a rule set with no effectful rules, or none ranked high
enough, behaves __exactly__ as the pure tier. The qualifying effectful rules are run
through 'runEffectfulRule' (timeout, retry, breaker), and the resulting candidates
compete against the pure winner under the same @(precedence, deny-before-allow)@
comparator. An effectful 'Unavailable' that wins is __fail-closed__ to 'Undecidable'.
-}
evalRulesEffectful ::
    EvalContext ->
    [PrecededRule] ->
    [PrecededEffectfulRule] ->
    PackageDetails ->
    IO Decision
evalRulesEffectful ctx pureRules effectfulRules pd = do
    let (pureWinPrec, pureDecision) = evalRulesWithPrecedence ctx pureRules pd
        -- Performance ordering: an effectful rule below the pure winner cannot
        -- outrank it, so only rules at or above the winner's precedence are
        -- consulted. With no pure winner (deny-by-default) every effectful rule
        -- could still change the outcome, so all qualify.
        qualifying = filter (qualifies pureWinPrec) effectfulRules
    if null qualifying
        then pure pureDecision
        else do
            candidates <- traverse evalOne qualifying
            pure (selectDecision pureWinPrec pureDecision candidates)
  where
    qualifies :: Maybe Int -> PrecededEffectfulRule -> Bool
    qualifies pureWinPrec per = case pureWinPrec of
        Nothing -> True
        Just p -> perPrecedence per >= p

    evalOne :: PrecededEffectfulRule -> IO ECandidate
    evalOne per = do
        let rule = perRule per
        outcome <- runEffectfulRule ctx rule pd
        pure (ECandidate (perPrecedence per) (erName rule) outcome)

{- An effectful candidate: the precedence it competed at, the rule's name (for the
denial\/audit message), and the harness-resolved outcome. -}
data ECandidate = ECandidate Int Text RuleOutcome

{- Select the overall decision from the pure winner and the effectful candidates,
under the shared @(precedence, deny-before-allow)@ comparator. The pure winner is
seeded as the incumbent at its precedence; each effectful candidate that took a
position (anything but 'Abstain') competes against it, an 'Unavailable' ranking with
the denies (it is fail-closed). The highest-ranked position wins; if none of the
effectful candidates outranks the pure winner, the pure decision stands. -}
selectDecision :: Maybe Int -> Decision -> [ECandidate] -> Decision
selectDecision pureWinPrec pureDecision candidates =
    case nonEmpty (pureEntry <> positions) of
        Nothing -> pureDecision
        Just entries -> entryDecision (maximumBy (comparing rank) entries)
  where
    -- The pure winner as a ranked entry, so the effectful candidates compete
    -- against it directly. A deny-by-default has no precedence floor of its own, so
    -- it seeds nothing and any effectful position wins it.
    pureEntry :: [Entry]
    pureEntry = case pureWinPrec of
        Nothing -> []
        Just p -> [Entry p (pureIsDeny pureDecision) pureDecision]

    positions :: [Entry]
    positions = mapMaybe entryOf candidates

    -- An effectful candidate becomes a ranked entry unless it abstained (no
    -- position). An 'Unavailable' ranks as a deny (fail-closed) and resolves to an
    -- 'Undecidable' decision if it wins.
    entryOf :: ECandidate -> Maybe Entry
    entryOf (ECandidate prec name outcome) = case outcome of
        Abstain _ -> Nothing
        Allow reason -> Just (Entry prec False (ApprovedEffectful name reason))
        Deny reason -> Just (Entry prec True (DeniedEffectful name reason))
        Unavailable transience reason -> Just (Entry prec True (Undecidable transience reason))

    rank :: Entry -> (Int, Bool)
    rank e = (entryPrecedence e, entryIsDeny e)

    -- The pure tier never yields an effectful decision, so only its own approval is
    -- the allow case; everything else (denial, deny-by-default, undecidable) ranks
    -- as a deny so an equal-precedence effectful allow does not displace a pure deny.
    pureIsDeny :: Decision -> Bool
    pureIsDeny = \case
        Approved{} -> False
        _ -> True

{- A ranked competitor in the cross-tier selection: its precedence, whether it ranks
as a deny (so a deny beats an allow at equal precedence), and the decision it
resolves to if it wins. -}
data Entry = Entry
    { entryPrecedence :: Int
    , entryIsDeny :: Bool
    , entryDecision :: Decision
    }
