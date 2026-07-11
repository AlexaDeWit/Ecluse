{- | The policy rules engine.

A rule set is evaluated against a single 'PackageDetails' snapshot to produce a
'Decision'. The model is __deny by default; the boot order decides__: the configured
rules are arranged once, at boot, into a single total order ('bootOrder') -- highest
precedence first, then rule name ascending -- and evaluation walks that order and
takes the __first decisive result__. A result is decisive iff the rule returned a
decisive verdict ('Allow', 'Deny', or a fail-closed 'CannotVet'), or the harness
resolved a faulted evaluation fail-closed (@'Unavailable' _ 'FailDeny' _@); a
non-decisive verdict ('NoDecision', a fail-open 'CannotVet') or a fail-open fault
(@'Unavailable' _ 'FailNoDecision' _@) is a non-decisive no-op whose reason is
collected, in boot order, for the deny-by-default audit trail. If no rule is decisive
the package is 'BlockedByDefault'.

__A rule is evaluation-agnostic data; how it is evaluated is a separate concern.__ The
closed built-in vocabulary ('Ecluse.Core.Rules.Types.Rule') says /what/ a rule is;
'evalRule' is the single dispatch that says /how/ each built-in rule decides, closing
over the boot-bound capabilities in 'RuleDeps'. The engine's runtime structure is the
'PreparedRule': it pairs a rule's boot-order identity (precedence and name) with the
raw per-version evaluator and an optional 'Resilience' policy. 'prepare' builds one
per configured rule; the pure built-ins carry no 'Resilience' and run directly, while
the effectful CVE rules carry a 'Resilience' (a per-attempt timeout, bounded retry
with backoff, and a per-source 'Ecluse.Core.Breaker.Breaker') applied by the harness
'runEffectfulRule'. The order /is/ the tiebreak: there is no runtime
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
entry point. The rule data types live in "Ecluse.Core.Rules.Types"; the resilience
harness lives in "Ecluse.Core.Rules.Effectful".
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

import Data.Text qualified as T
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import UnliftIO (tryAny)
import UnliftIO.Async (Async, async, cancel, uninterruptibleCancel, wait)
import UnliftIO.Exception (bracket)

import Ecluse.Core.Breaker (
    Breaker (..),
    BreakerReporter (..),
    noBreakerReporter,
 )
import Ecluse.Core.Cve (AdvisoryRange (..), CveLookup (..), DbEtag, insideAffectedRange, severityAtLeast)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package
import Ecluse.Core.Rules.Effectful (
    EffectfulConfig (..),
    Resilience (..),
    backoffPolicy,
    defaultEffectfulConfig,
    newBreaker,
    runResilient,
 )
import Ecluse.Core.Rules.Types
import Ecluse.Core.Text (displayExceptionT)
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
    , rdCurrentAdvisoryEtag :: IO (Maybe DbEtag)
    {- ^ A non-pinning read of the active advisory database's 'DbEtag' for the
    per-request 'EvalContext'. 'Nothing' when none is loaded. Distinct from
    'rdWithCveLookup': it snapshots identity for the audit trail without holding
    a generation open, so it never delays a shadow-swap.
    -}
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
        , rdCurrentAdvisoryEtag = pure Nothing
        , rdBreakerReporter = noBreakerReporter
        }

{- | Evaluate a single built-in rule against a single package version -- the one place
"how a rule decides" lives. The dispatch over the closed 'Rule' data: the pure
constructors reason over the 'PackageDetails' alone and 'pure' their 'RuleVerdict';
'AllowIfRemediatesCve' and 'DenyIfCve' read the advisory database through the
boot-bound 'RuleDeps' and do IO. A rule returns only a __verdict__ -- it never
manufactures an 'Unavailable'; a genuine lookup fault surfaces as an exception, which
the 'Resilience' harness (attached by 'prepare') catches and resolves.

'IO'-typed so the dispatch is uniform across the pure and effectful arms. The pure
arms are total -- a malformed rule or package yields a verdict, never an exception, so
hostile metadata cannot crash the gate.
-}
evalRule :: RuleDeps -> EvalContext -> Rule -> PackageDetails -> IO RuleVerdict
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
evalRule deps _ (DenyIfCve params) pd =
    rdWithCveLookup deps $ \case
        Nothing -> pure (noAdvisoryDbVerdict params)
        Just cve -> denyVerdict params cve pd

{- The deny rule's verdict when no advisory database is loaded: pre-first-sync, a sync
yet to land its first artifact, or the rule enabled with no advisory bucket configured
at all. This is a __deterministic, in-process absence__, so it is a 'CannotVet'
verdict, not a fault: the harness takes it at face value and never retries it or trips
the breaker on it (no in-process retry could load a database). The rule's own alignment
decides what the absence means -- a fail-open rule skips ('CannotVet' 'FailNoDecision',
a no-op), a fail-closed rule refuses the version it cannot vet ('CannotVet' 'FailDeny',
decisive → 'Undecidable', a retryable 503, so readiness gating and the operator's
dashboards surface a misconfiguration loudly). -}
noAdvisoryDbVerdict :: DenyIfCveParams -> RuleVerdict
noAdvisoryDbVerdict params = CannotVet (dicOnUnavailable params) "DenyIfCve: no advisory database loaded"

{- The deny rule's verdict against a loaded advisory database: deny the version if
any advisory that affects it meets the configured severity threshold, naming the
advisories for the audit trail; otherwise abstain. An unscored advisory clears the
threshold (fail-closed, so npm malware -- unscored -- is denied). -}
denyVerdict :: DenyIfCveParams -> CveLookup -> PackageDetails -> IO RuleVerdict
denyVerdict params cve pd = do
    ranges <- cveAdvisoriesFor cve name
    let blocking =
            ordNub
                [ arCveId ar
                | ar <- ranges
                , insideAffectedRange eco version ar
                , severityAtLeast (dicMinSeverity params) (arSeverity ar)
                ]
    pure $ case blocking of
        [] -> NoDecision "no advisory at or above the severity threshold affects this version"
        ids -> Deny ("affected by " <> T.intercalate ", " ids <> " (CVSS >= " <> show (dicMinSeverity params) <> ")")
  where
    eco = pkgEcosystem (pkgName pd)
    name = renderPackageName (pkgName pd)
    version = renderVersion (pkgVersion pd)

-- The CVE rule's verdict against a loaded advisory database.
remediationVerdict :: CveLookup -> PackageDetails -> IO RuleVerdict
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
classifyRanges :: Ecosystem -> Text -> [AdvisoryRange] -> RuleVerdict
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
returns a chosen 'RuleVerdict') and a chosen name to exercise the resilience harness
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
    , prepEval :: EvalContext -> PackageDetails -> IO RuleVerdict
    {- ^ The rule's raw verdict for one version. For a resilient rule it may perform IO
    that fails or hangs; 'runEffectfulRule' wraps it.
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
    AllowIfRemediatesCve -> effectful FailNoDecision
    -- The deny rule aligns per its config: fail-closed refuses a version it cannot
    -- vet, fail-open skips itself. The same alignment governs a lookup that throws
    -- or times out (here) and a database that is not loaded ('noAdvisoryDbVerdict').
    DenyIfCve params -> effectful (dicOnUnavailable params)
    _ -> pure Nothing
  where
    effectful alignment = do
        breaker <- newBreaker
        pure $
            Just
                Resilience
                    { resConfig = defaultEffectfulConfig
                    , resAlignment = alignment
                    , resBreaker = breaker
                    , resBreakerReporter = rdBreakerReporter deps
                    , resClock = getCurrentTime
                    }

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

__Never throws.__ An effectful rule's faults are absorbed by its resilience
harness ('runEffectfulRule'); a direct rule that throws anyway -- an invariant
break, since a direct rule declares no effects -- is absorbed here as a
fail-closed 'Undecidable' naming the rule. Either way one request's evaluation
resolves to a 'Decision', never a serve-path escape.
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
            evaluated <- tryAny (prepEval r ctx pd)
            case evaluated of
                Left escape ->
                    -- A direct rule declares no effects, so a throw here is an
                    -- invariant break -- absorbed fail-closed as 'Undecidable'
                    -- (the retryable 503), symmetric with the effectful
                    -- harness's fail-deny 'Unavailable', with the rule named in
                    -- the reason the audit trail carries. Absorbing (rather
                    -- than propagating) keeps the engine's totality claim
                    -- constructive: no rule, however written, can turn one
                    -- request's evaluation into a serve-path escape.
                    pure (Undecidable (WillResolve Nothing) (prepName r <> ": the rule threw: " <> displayExceptionT escape))
                Right verdict -> do
                    let res = Decided verdict
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
awaitInOrder :: [(PreparedRule, Async RuleEvaluation)] -> [Reason] -> IO (Either Decision [Reason])
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
-- results, the boot order having already settled who wins). A deterministic
-- 'CannotVet' and a harness 'Unavailable' fault credit the same 'Undecidable'; the
-- 'CannotVet' carries no transience of its own, so it is a plain retryable 503.
decisive :: Text -> RuleEvaluation -> Maybe Decision
decisive name = \case
    Decided (Allow reason) -> Just (Admitted name reason)
    Decided (Deny reason) -> Just (Blocked name reason)
    Decided (NoDecision _) -> Nothing
    Decided (CannotVet FailDeny reason) -> Just (Undecidable (WillResolve Nothing) reason)
    Decided (CannotVet FailNoDecision _) -> Nothing
    Unavailable transience FailDeny reason -> Just (Undecidable transience reason)
    Unavailable _ FailNoDecision _ -> Nothing

-- The audit reason carried by any result, gathered for the deny-by-default trail.
reasonOf :: RuleEvaluation -> Reason
reasonOf (Unavailable _ _ reason) = reason
reasonOf (Decided verdict) = case verdict of
    Allow reason -> reason
    Deny reason -> reason
    NoDecision reason -> reason
    CannotVet _ reason -> reason

{- | Run one prepared rule through its resilience policy. A rule with no 'Resilience'
(@'prepResilience' = 'Nothing'@) runs directly, its verdict wrapped 'Decided'. A
resilient rule's IO runs under its circuit-breaker gate, a per-attempt timeout, and
bounded retry with backoff: any 'RuleVerdict' the rule returns -- a deterministic
'CannotVet' included -- resets the breaker and is returned 'Decided', taken at face
value and never retried; only a __fault__ the harness observes (a timeout, an
exception, or the breaker already open) advances the breaker and resolves to
@'Unavailable' transience alignment reason@, the alignment from the rule's 'Resilience'
(fail-closed or fail-open).

The breaker timing reads the injected resilience clock ('resClock'), read fresh at each
breaker decision, so it is deterministic under test and independent of the request
snapshot 'ctxNow' the age rules hold constant. Reading it again after the retry run means
a tripped breaker's cooldown starts when the failure commits, not when the run began.
Total -- it never throws; a rule failure becomes a result.
-}
runEffectfulRule :: EvalContext -> PreparedRule -> PackageDetails -> IO RuleEvaluation
runEffectfulRule ctx rule pd = case prepResilience rule of
    Nothing -> Decided <$> prepEval rule ctx pd
    Just res -> runResilient res (prepName rule) (prepEval rule ctx) pd

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

{- | Render a duration as an approximate, human-friendly string for a decision
message: its two most-significant non-zero units, so a value just short of a
threshold reads differently from the threshold itself (@89s@ is @"1 minute 29
seconds"@, not the bare @"1 minute"@ a @90s@ minimum also rendered to). A long
duration stays compact, since its lesser units are zero and dropped. Always
non-negative.

>>> renderDuration 604800
"7 days"

>>> renderDuration 90
"1 minute 30 seconds"
-}
renderDuration :: NominalDiffTime -> Text
renderDuration d = case take 2 (durationComponents secs) of
    [] -> "0 seconds"
    parts -> T.unwords (map renderDurationPart parts)
  where
    secs = max 0 (round (nominalDiffTimeToSeconds d)) :: Integer

{- | The unit ladder 'durationComponents' decomposes a second count against, the
largest unit first. @second@ (size 1) is the floor, so any remainder is fully
consumed and the smallest component is always whole seconds.
-}
durationLadder :: [(Text, Integer)]
durationLadder =
    [ ("day", 86400)
    , ("hour", 3600)
    , ("minute", 60)
    , ("second", 1)
    ]

{- | The non-zero @(unit, count)@ components of a non-negative second count, the
largest unit first: @90@ is @[("minute", 1), ("second", 30)]@, and @604800@ is
@[("day", 7)]@ (a single component, its lesser units being zero). 'renderDuration'
keeps the two most significant.
-}
durationComponents :: Integer -> [(Text, Integer)]
durationComponents = go durationLadder
  where
    go [] _ = []
    go ((unit, size) : rest) r =
        let (q, r') = r `divMod` size
         in [(unit, q) | q > 0] <> go rest r'

-- Render one @(unit, count)@ component, pluralising the unit (@1 minute@, @30 seconds@).
renderDurationPart :: (Text, Integer) -> Text
renderDurationPart (unit, n) = show n <> " " <> unit <> (if n == 1 then "" else "s")
