-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The policy rules engine.

A rule set evaluates one 'PackageDetails' snapshot and produces a 'Decision'. The
model is __deny by default, and the boot order decides__. 'bootOrder' arranges the
configured rules once, at boot, into one total order: highest precedence first, then
rule name ascending. Evaluation walks that order and takes the __first decisive
result__. If no rule is decisive the package is 'BlockedByDefault'.

A result is decisive in exactly two cases. The first is a decisive verdict from the
rule: 'Allow', 'Deny', or a fail-closed 'CannotVet'. The second is a faulted evaluation
the harness resolved fail-closed (@'Unavailable' _ 'FailDeny' _@). Everything else is a
non-decisive no-op: a 'NoDecision' verdict, a fail-open 'CannotVet', or a fail-open
fault (@'Unavailable' _ 'FailNoDecision' _@). The engine collects a no-op's reason, in
boot order, for the deny-by-default audit trail.

__A rule is evaluation-agnostic data. Evaluating a rule is a separate concern.__ The
closed built-in vocabulary ('Ecluse.Core.Rules.Types.Rule') says /what/ a rule is.
'evalRule' is the single dispatch that says /how/ each built-in rule decides, closing
over the boot-bound capabilities in 'RuleDeps'.

The engine's one runtime structure is the 'PreparedRule'. It pairs a rule's boot-order
identity (precedence and name) with the raw per-version evaluator and an optional
'Resilience' policy. 'prepare' builds one per configured rule. The pure built-ins carry
no 'Resilience' and run directly. The effectful CVE rules carry a 'Resilience' that the
harness 'runEffectfulRule' applies: a per-attempt timeout, bounded retry with backoff,
and a per-source 'Ecluse.Core.Breaker.Breaker'. The order /is/ the tiebreak, and nothing
compares results at runtime.

Config cannot reach the evaluator on a 'PreparedRule'. 'prepare' only ever binds
'evalRule' over closed 'Rule' data, so untrusted config expresses only the built-in
vocabulary. Supplying an arbitrary evaluator is a code-layer capability, never a config
surface. The engine's own tests use it.

'evalRules' may evaluate effectful rules speculatively in parallel, but the result is
always __as-if sequential by boot order__. The winner is the earliest-in-order decisive
rule, never the first to return in wall-clock time. Once the winner is known, the engine
cancels every still-running strictly-later evaluation. The engine evaluates the cheap
pure prefix directly, so it never launches IO that an earlier decisive result would
moot. Evaluation is 'IO'-typed, because a rule's evaluator may do IO, so there is no
pure entry point. The rule data types live in "Ecluse.Core.Rules.Types", and the
resilience harness lives in "Ecluse.Core.Rules.Effectful".
-}
module Ecluse.Core.Rules (
    -- * The boot-bound rule capabilities
    RuleDeps (..),

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
    cveIdsInReason,

    -- * The resilience harness
    runEffectfulRule,
    EffectfulConfig (..),
    defaultEffectfulConfig,
    backoffPolicy,
    Breaker (..),
    newBreaker,
    BreakerReporter (..),
    noBreakerReporter,
    FaultReporter (..),
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
    FaultReporter (..),
    Resilience (..),
    backoffPolicy,
    defaultEffectfulConfig,
    newBreaker,
    runResilient,
 )
import Ecluse.Core.Rules.Types
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Core.Version (renderVersion)

{- | The boot-bound capabilities a rule's evaluation may consult, closed into the prepared
rules by 'prepare'.

'rdWithCveLookup' brackets its acquisition so the provider can pin the advisory database
generation for one rule evaluation, which keeps the background sync's shadow-swap from
pruning an artifact an evaluation still holds. 'Nothing' means no advisory database is
loaded: the operator configured none, or the first sync is still pending.
-}
data RuleDeps = RuleDeps
    { rdWithCveLookup :: forall a. (Maybe CveLookup -> IO a) -> IO a
    -- ^ Bracketed access to the current advisory database view, if one is loaded.
    , rdCurrentAdvisoryEtag :: IO (Maybe DbEtag)
    {- ^ A non-pinning read of the active advisory database's 'DbEtag', or 'Nothing' when none
    is loaded. It holds no generation open, so it never delays a shadow-swap.
    -}
    , rdBreakerReporter :: BreakerReporter
    {- ^ The observer that effectful rules report their breaker transitions to, as
    @ecluse.rule.breaker.state@. 'noBreakerReporter' when unobserved.
    -}
    , rdFaultReporter :: FaultReporter
    {- ^ The observer that effectful rules report an exhausted evaluation's fault detail to,
    or 'noFaultReporter' when unobserved. The detail stays in the operator log and never
    reaches the client-facing message.
    -}
    }

{- | Evaluate one built-in rule against one package version.

A rule returns only a verdict and never manufactures an 'Unavailable': a genuine lookup
fault surfaces as an exception, which the 'Resilience' harness that 'prepare' attaches
resolves. The pure arms are total, so hostile metadata cannot crash the gate.
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

{- The rule's verdict when no advisory database is loaded. The absence is deterministic
and in-process, so it is a 'CannotVet' verdict and not a fault: no in-process retry could
load a database, so the harness never retries it and never trips the breaker on it. -}
noAdvisoryDbVerdict :: DenyIfCveParams -> RuleVerdict
noAdvisoryDbVerdict params = CannotVet (dicOnUnavailable params) "DenyIfCve: no advisory database loaded"

{- The rule's verdict against a loaded advisory database. An unscored advisory clears the
severity threshold, which is fail-closed: npm malware carries no score, and the rule
denies it. -}
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

{- | Recover the advisory ids a 'DenyIfCve' denial named, reading them back out of the
reason 'denyVerdict' rendered, between @"affected by "@ and @" (CVSS"@. A message with no
such segment yields @[]@. 'Ecluse.Core.RulesSpec' round-trips this against 'denyVerdict',
so rewording either one fails the build.
-}
cveIdsInReason :: Text -> [Text]
cveIdsInReason message
    | T.null afterCvss = []
    | otherwise = filter (not . T.null) (map T.strip (T.splitOn ", " ids))
  where
    -- 'stripPrefix' drops the marker without an O(n) 'Data.Text.length' on it (STAN-0208).
    -- An absent marker leaves the body empty, so the guard yields @[]@.
    (_, afterAffected) = T.breakOn "affected by " message
    body = fromMaybe "" (T.stripPrefix "affected by " afterAffected)
    (ids, afterCvss) = T.breakOn " (CVSS" body

-- The CVE rule's verdict against a loaded advisory database.
remediationVerdict :: CveLookup -> PackageDetails -> IO RuleVerdict
remediationVerdict cve pd = do
    fixes <- cveRemediationProbe cve name version
    if not fixes
        then pure (NoDecision "no advisory names this version as its fix")
        else do
            -- The probe hit, so the version is some advisory's exact fixed bound.
            ranges <- cveAdvisoriesFor cve name
            pure (classifyRanges (pkgEcosystem (pkgName pd)) version ranges)
  where
    name = renderPackageName (pkgName pd)
    version = renderVersion (pkgVersion pd)

-- A version still inside any advisory's affected range, an unfixed one included, must not
-- fast-track. Otherwise credit the advisories that name it as their exact fixed bound.
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

{- | A rule prepared for the engine to evaluate, and the engine's one runtime structure.

'prepEval' is a plain function field rather than a closed 'Rule', so code such as the
engine's own tests can supply an arbitrary evaluator. Config reaches this only through
'prepare', so config can never supply one.
-}
data PreparedRule = PreparedRule
    { prepName :: Text
    {- ^ The stable, human-facing name. It is the boot-order tiebreak and the credited
    identity.
    -}
    , prepPrecedence :: Int
    -- ^ The precedence at which this rule competes. Higher wins in the boot order.
    , prepResilience :: Maybe Resilience
    -- ^ The resilience policy, or 'Nothing' for a rule run directly.
    , prepEval :: EvalContext -> PackageDetails -> IO RuleVerdict
    {- ^ The rule's raw verdict for one version. For a resilient rule it may do IO that
    fails or hangs, and 'runEffectfulRule' wraps it.
    -}
    }

{- | Prepare a resolved policy into the engine's runtime rules, closing each evaluator over
the boot-bound 'RuleDeps'.

'AllowIfRemediatesCve' gets a __fail-open__ 'Resilience' ('FailNoDecision'), so a lookup
that fails or hangs abstains and the rule never admits on an unconfirmable claim.
'IO'-typed because a resilient rule allocates its per-source breaker here, once.
-}
prepare :: RuleDeps -> [PrecededRule] -> IO [PreparedRule]
prepare deps = traverse (prepareRule deps)

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

-- The resilience a rule needs. The effectful CVE rule carries the fail-open policy,
-- allocating its per-source breaker. The pure rules carry none.
resilienceFor :: RuleDeps -> Rule -> IO (Maybe Resilience)
resilienceFor deps = \case
    AllowIfRemediatesCve -> effectful FailNoDecision
    -- The deny rule aligns per its config. The same alignment governs a lookup that throws or
    -- times out (here) and a database that is not loaded ('noAdvisoryDbVerdict').
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
                    , resFaultReporter = rdFaultReporter deps
                    , resClock = getCurrentTime
                    }

{- | Arrange a rule set into the one total order evaluation walks: highest precedence
first, then rule name ascending as the deterministic tiebreak. The configured order never
enters, so shuffling the set yields the same 'Decision'.
-}
bootOrder :: [PreparedRule] -> [PreparedRule]
bootOrder = sortOn (\r -> bootKey (prepPrecedence r) (prepName r))

-- Both 'bootOrder' and the engine order through this one key, so the tiebreak lives in
-- exactly one place.
bootKey :: Int -> Text -> (Down Int, Text)
bootKey prec name = (Down prec, name)

{- | Render the boot order as one line per rule, in evaluation order, so an operator sees
at boot how their policy will resolve.
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

{- | Evaluate a package version against a rule set. Take the first decisive result in boot
order, or 'BlockedByDefault' with every non-decisive reason in boot order.

The engine speculates on resilient rules in parallel, but decides as-if sequentially by
boot order: the earliest decisive rule wins, never the first to return in wall-clock time.
It never launches IO an earlier decisive result would moot.

__Never throws.__ A direct rule that throws breaks its no-effects declaration, and this
absorbs the break fail-closed as an 'Undecidable' naming the rule, so no rule can turn one
request's evaluation into a serve-path escape.
-}
evalRules :: EvalContext -> [PreparedRule] -> PackageDetails -> IO Decision
evalRules ctx rules pd = step (bootOrder rules) []
  where
    -- 'reasons' accumulates non-decisive reasons in reverse boot order. The final
    -- deny-by-default list reverses them back into boot order.
    step :: [PreparedRule] -> [Reason] -> IO Decision
    step [] reasons = pure (BlockedByDefault (reverse reasons))
    step (r : rs) reasons
        | isNothing (prepResilience r) = do
            -- A direct rule is zero-cost, so run it in place. Reaching it means every
            -- earlier rule was non-decisive, so it moots no speculated IO.
            evaluated <- tryAny (prepEval r ctx pd)
            case evaluated of
                Left escape ->
                    -- A direct rule declares no effects, so a throw here is an invariant break.
                    -- Absorb it fail-closed as 'Undecidable' (the retryable 503) so no rule reaches
                    -- the serve path.
                    pure (Undecidable (WillResolve Nothing) (prepName r <> ": the rule threw: " <> displayExceptionT escape))
                Right verdict -> do
                    let res = Decided verdict
                    case decisive (prepName r) res of
                        Just d -> pure d
                        Nothing -> step rs (reasonOf res : reasons)
        | otherwise =
            -- Stopping the block at the next direct rule keeps the "no mooted IO" guarantee: that
            -- rule runs, and may decide, before the engine launches any resilient rule beyond it.
            let (block, rest) = span (isJust . prepResilience) (r : rs)
             in evalBlock ctx pd block >>= \case
                    Left d -> pure d
                    Right blockReasons -> step rest (reverse blockReasons <> reasons)

-- Launch a contiguous resilient block concurrently, then await in boot order. 'Left' is
-- the earliest decisive winner, 'Right' the block's non-decisive reasons in boot order.
evalBlock :: EvalContext -> PackageDetails -> [PreparedRule] -> IO (Either Decision [Reason])
evalBlock ctx pd block =
    bracket
        (traverse (\r -> async (runEffectfulRule ctx r pd)) block)
        (traverse_ uninterruptibleCancel)
        (\asyncs -> awaitInOrder (zip block asyncs) [])

-- Await a launched block's evaluations in boot order. A decisive winner cancels
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

-- Map a rule result to the 'Decision' it credits, or 'Nothing' if it is a no-op. Nothing
-- compares competing results, the boot order having already settled who wins. A
-- 'CannotVet' carries no transience of its own, so it credits a plain retryable 503.
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

{- | Run one prepared rule through its resilience policy. A rule with no 'Resilience' runs
directly and comes back 'Decided'.

A resilient rule runs under its breaker gate, a per-attempt timeout, and bounded retry.
Any 'RuleVerdict' it returns, a deterministic 'CannotVet' included, resets the breaker and
is never retried. Only a fault the harness observes advances the breaker: a timeout, an
exception, or an already-open breaker. Total: a rule failure becomes a result, never a
throw.
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

{- | Render a duration for a decision message as its two most-significant non-zero units.
Two units keep a near-threshold value from reading like the threshold, so @89s@ is
@"1 minute 29 seconds"@ and not the @"1 minute"@ a @90s@ minimum also renders to. Always
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

{- | The unit ladder 'durationComponents' decomposes a second count against, the largest
unit first. The floor is @second@ (size 1), so nothing remains below it and the smallest
component is always whole seconds.
-}
durationLadder :: [(Text, Integer)]
durationLadder =
    [ ("day", 86400)
    , ("hour", 3600)
    , ("minute", 60)
    , ("second", 1)
    ]

{- | The non-zero @(unit, count)@ components of a non-negative second count, the largest
unit first. Zero components drop out, so @604800@ is @[("day", 7)]@ alone.
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
