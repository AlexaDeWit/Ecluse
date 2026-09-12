-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The policy engine denies by default and decides in boot order.
Effectful evaluation may overlap, but precedence governs the result.
-}
module Ecluse.Core.Rules (
    -- * The boot-bound rule capabilities
    RuleDeps (..),

    -- * The built-in rule dispatch
    evalRule,

    -- * The engine's prepared rule
    PreparedRule (..),
    AdvisoryGate (..),
    Resilience (..),
    prepare,

    -- * Boot-time ordering
    bootOrder,
    renderBootOrder,

    -- * Evaluation
    evalRules,
    renderDecision,
    renderDuration,
    renderIneligible,
    cveIdsInReason,

    -- * The resilience harness
    runEffectfulRule,
    FaultReporter (..),
) where

import Data.Text qualified as T
import Data.Text.Short qualified as TS
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import UnliftIO (tryAny)
import UnliftIO.Async (Async, async, cancel, uninterruptibleCancel, wait)
import UnliftIO.Exception (bracket)

import Ecluse.Core.Breaker (BreakerReporter (..))
import Ecluse.Core.Cve (AdvisoryRange (..), CveLookup (..), DbEtag, MissingScorePolicy (..), insideAffectedRange, scoreAtLeast)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore))
import Ecluse.Core.Package
import Ecluse.Core.Rules.Effectful (
    FaultReporter (..),
    Resilience (..),
    defaultEffectfulConfig,
    newBreaker,
    runResilient,
 )
import Ecluse.Core.Rules.Freshness (AdvisoryAge (..), AdvisoryFreshness (AdvisoryAging, AdvisoryFresh, AdvisoryStale, AdvisoryUndated))
import Ecluse.Core.Rules.Types
import Ecluse.Core.Text (displayExceptionT, renderIso8601Utc)
import Ecluse.Core.Version (renderVersion)

-- | Pin one advisory generation for an evaluation, or supply 'Nothing' before the first sync.
data RuleDeps = RuleDeps
    { rdWithCveLookup :: forall a. (Maybe (DbEtag, CveLookup) -> IO a) -> IO a
    -- ^ Bracketed access to the lookup and ETag acquired together, if a database is loaded.
    , rdCurrentAdvisoryEtag :: IO (Maybe DbEtag)
    {- ^ A non-pinning read of the active advisory database's 'DbEtag', or 'Nothing' when none
    is loaded. It holds no generation open, so it never delays a shadow-swap.
    -}
    , rdBreakerReporter :: BreakerReporter
    {- ^ The observer that effectful rules report their breaker transitions to, as
    @ecluse.rule.breaker.state@. 'Ecluse.Core.Breaker.noBreakerReporter' when unobserved.
    -}
    , rdFaultReporter :: FaultReporter
    -- ^ Reports exhausted faults to the operator log without exposing them to clients.
    , rdAdvisoryFreshness :: IO AdvisoryFreshness
    {- ^ How old the serving artifact's push is, read again at every evaluation. The wall clock
    alone ages it, so an unchanged artifact expires in a warm process.
    -}
    }

{- | Lookup faults escape to the resilience policy attached by 'prepare'. A rule that reads a fact
nothing supplied refuses rather than abstaining, so the fold stops at it.
-}
evalRule :: RuleDeps -> EvalContext -> Rule -> RuleEvidence -> IO RuleVerdict
evalRule _ _ (AllowScope scope) ev =
    pure $ case pkgNamespace (evName ev) of
        Just s
            | s == scope ->
                Allow ("scope " <> renderScope scope <> " is allow-listed")
        _ ->
            NoDecision ("scope is not the allow-listed " <> renderScope scope)
evalRule _ ctx (AllowIfOlderThan minAge) ev =
    pure $ case evPublishedAt ev of
        Unread -> needsFact "AllowIfOlderThan" "the publish time"
        Known Nothing -> NoDecision "publish time is unknown"
        Known (Just publishedAt) ->
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
evalRule _ _ DenyInstallTimeExecution ev =
    pure $ case evInstallCode ev of
        Unread -> needsFact "DenyInstallTimeExecution" "the install-time execution signal"
        Known (RunsCodeOnInstall how) -> Deny Nothing ("runs code on install: " <> how)
        Known NoCodeOnInstall -> NoDecision "no install-time code execution"
        Known CodeExecUnknown -> NoDecision "install-time code execution not yet determined"
evalRule _ _ (DenyByIdentity ident) ev =
    pure $
        if matchesIdentity ident ev
            then Deny Nothing ("identity " <> ident <> " is revoked by operator")
            else NoDecision ("identity is not the revoked " <> ident)
evalRule _ _ (AllowByIdentity ident) ev =
    pure $
        if matchesIdentity ident ev
            then Allow ("identity " <> ident <> " is allow-listed by operator")
            else NoDecision ("identity is not the allow-listed " <> ident)
evalRule deps _ AllowIfRemediatesCve ev =
    rdWithCveLookup deps $ \case
        Nothing -> pure (NoDecision "no advisory database is loaded")
        Just (_, cve) -> remediationVerdict cve ev
evalRule deps _ (DenyIfCve params) ev =
    rdWithCveLookup deps $ \case
        Nothing -> pure (noAdvisoryDbVerdict "DenyIfCve" (dicOnUnavailable params))
        Just (etag, cve) -> advisoryDenyVerdict etag DenyMissingScore "CVSS" (dicMinCvss params) arSeverity cve ev
evalRule deps _ (DenyIfEpss params) ev =
    rdWithCveLookup deps $ \case
        Nothing -> pure (noAdvisoryDbVerdict "DenyIfEpss" (dieOnUnavailable params))
        Just (etag, cve) -> advisoryDenyVerdict etag AbstainMissingScore "EPSS" (dieMinEpss params) arEpss cve ev

{- The verdict when the evidence carries no reading of a fact the rule consults. It is fail-closed so
the fold stops here, rather than letting a lower-precedence rule decide past an unresolved one. -}
needsFact :: Text -> Text -> RuleVerdict
needsFact rule fact = CannotVet FailDeny (rule <> ": " <> fact <> " is not available")

{- The verdict when no advisory database is loaded. It is a 'CannotVet' verdict and not a
fault, because no in-process retry could load one, so the harness never retries it. -}
noAdvisoryDbVerdict :: Text -> FailureAlignment -> RuleVerdict
noAdvisoryDbVerdict rule alignment = CannotVet alignment (rule <> ": no advisory database loaded")

advisoryDenyVerdict :: DbEtag -> MissingScorePolicy -> Text -> Double -> (AdvisoryRange -> Maybe Double) -> CveLookup -> RuleEvidence -> IO RuleVerdict
advisoryDenyVerdict etag missing metric threshold scoreOf cve ev = do
    ranges <- cveAdvisoriesFor cve name
    let blocking =
            ordNub
                [ arCveId ar
                | ar <- ranges
                , insideAffectedRange eco version ar
                , scoreAtLeast missing threshold (scoreOf ar)
                ]
    pure $ case blocking of
        [] -> NoDecision ("no advisory at or above the " <> metric <> " threshold affects this version")
        ids -> Deny (Just etag) ("affected by " <> T.intercalate ", " ids <> " (" <> metric <> " >= " <> show threshold <> ")")
  where
    eco = pkgEcosystem (evName ev)
    name = TS.toText (pkgCanonical (evName ev))
    version = renderVersion (evVersion ev)

-- | Read the advisory identifiers from a scored denial reason, or return none.
cveIdsInReason :: Text -> [Text]
cveIdsInReason message
    | T.null afterThreshold = []
    | otherwise = filter (not . T.null) (map T.strip (T.splitOn ", " ids))
  where
    -- 'stripPrefix' drops the marker without an O(n) 'Data.Text.length' on it (STAN-0208).
    -- An absent marker leaves the body empty, so the guard yields @[]@.
    (_, afterAffected) = T.breakOn "affected by " message
    body = fromMaybe "" (T.stripPrefix "affected by " afterAffected)
    (ids, afterThreshold) = T.breakOn " (" body

-- The CVE rule's verdict against a loaded advisory database.
remediationVerdict :: CveLookup -> RuleEvidence -> IO RuleVerdict
remediationVerdict cve ev = do
    fixes <- cveRemediationProbe cve name version
    if not fixes
        then pure (NoDecision "no advisory names this version as its fix")
        else do
            -- The probe hit, so the version is some advisory's exact fixed bound.
            ranges <- cveAdvisoriesFor cve name
            pure (classifyRanges (pkgEcosystem (evName ev)) version ranges)
  where
    name = TS.toText (pkgCanonical (evName ev))
    version = renderVersion (evVersion ev)

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
    remediated = ordNub [arCveId ar | ar <- ranges, arUpperBound ar == FixedBefore version]
    stillOpen = ordNub [arCveId ar | ar <- ranges, insideAffectedRange eco version ar]

-- The one identity test the by-identity twins share: the exact rendered package
-- name, or the exact package@version.
matchesIdentity :: Text -> RuleEvidence -> Bool
matchesIdentity ident ev =
    let pkgStr = renderPackageName (evName ev)
        pkgAtVer = pkgStr <> "@" <> renderVersion (evVersion ev)
     in ident == pkgStr || ident == pkgAtVer

-- | Config obtains evaluators only through 'prepare', never from arbitrary code.
data PreparedRule = PreparedRule
    { prepName :: Text
    {- ^ The stable, human-facing name. It is the boot-order tiebreak and the credited
    identity.
    -}
    , prepPrecedence :: Int
    -- ^ The precedence at which this rule competes. Higher wins in the boot order.
    , prepResilience :: Maybe Resilience
    -- ^ The resilience policy, or 'Nothing' for a rule run directly.
    , prepAdvisoryGate :: Maybe AdvisoryGate
    {- ^ The push-age gate an advisory-reading rule answers to, or 'Nothing' for a rule that
    reads no advisory database.
    -}
    , prepEval :: EvalContext -> RuleEvidence -> IO RuleVerdict
    {- ^ The rule's raw verdict for one version. For a resilient rule it may do IO that
    fails or hangs, and 'runEffectfulRule' wraps it.
    -}
    }

{- | One advisory-reading rule's push-age gate: the reading, and what an expired push resolves the
rule to. Expiry is unavailability a rule may not waive, so a deny's alignment is fixed here.
-}
data AdvisoryGate = AdvisoryGate
    { agAlignment :: FailureAlignment
    -- ^ The alignment an expired push resolves under, which configuration cannot change.
    , agFreshness :: IO AdvisoryFreshness
    -- ^ The push-age reading, taken fresh for every evaluation.
    }

-- | Allocate each effectful rule's breaker once. Unconfirmed remediation claims abstain.
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
            , prepAdvisoryGate = advisoryGateFor deps rule
            , prepEval = \ctx -> evalRule deps ctx rule
            }

{- The gate each advisory-reading rule carries. Expired evidence refuses on the deny rules and
abstains on the remediation allow, so the deny's refusal is what the version meets. -}
advisoryGateFor :: RuleDeps -> Rule -> Maybe AdvisoryGate
advisoryGateFor deps = \case
    DenyIfCve{} -> gate FailDeny
    DenyIfEpss{} -> gate FailDeny
    AllowIfRemediatesCve -> gate FailNoDecision
    AllowScope{} -> Nothing
    AllowIfOlderThan{} -> Nothing
    DenyInstallTimeExecution -> Nothing
    DenyByIdentity{} -> Nothing
    AllowByIdentity{} -> Nothing
  where
    gate alignment = Just AdvisoryGate{agAlignment = alignment, agFreshness = rdAdvisoryFreshness deps}

-- The resilience a rule needs. The effectful CVE rule carries the fail-open policy,
-- allocating its per-source breaker. The pure rules carry none.
resilienceFor :: RuleDeps -> Rule -> IO (Maybe Resilience)
resilienceFor deps = \case
    AllowIfRemediatesCve -> effectful FailNoDecision
    -- A deny rule aligns per its config. The same alignment governs a lookup that throws or
    -- times out (here) and a database that is not loaded ('noAdvisoryDbVerdict').
    DenyIfCve params -> effectful (dicOnUnavailable params)
    DenyIfEpss params -> effectful (dieOnUnavailable params)
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

-- | Sort by descending precedence, then ascending rule name, independently of configuration order.
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

-- | Decide in boot order despite concurrent lookups. Unexpected direct-rule faults refuse admission.
evalRules :: EvalContext -> [PreparedRule] -> RuleEvidence -> IO Decision
evalRules ctx rules ev = step (bootOrder rules) []
  where
    -- 'reasons' accumulates non-decisive reasons in reverse boot order. The final
    -- deny-by-default list reverses them back into boot order.
    step :: [PreparedRule] -> [Reason] -> IO Decision
    step [] reasons = pure (BlockedByDefault (reverse reasons))
    step (r : rs) reasons
        | isNothing (prepResilience r) = do
            -- A direct rule is zero-cost, so run it in place; reaching it moots no speculated
            -- IO. It still goes through the one runner, so no rule can skip its own gate.
            evaluated <- tryAny (runEffectfulRule ctx r ev)
            case evaluated of
                Left escape ->
                    -- A direct-rule exception breaks its contract and must refuse admission.
                    pure (Undecidable (WillResolve Nothing) (prepName r <> ": the rule threw: " <> displayExceptionT escape))
                Right res ->
                    case decisive (prepName r) res of
                        Just d -> pure d
                        Nothing -> step rs (reasonOf res : reasons)
        | otherwise =
            -- Stopping the block at the next direct rule keeps the "no mooted IO" guarantee: that
            -- rule runs, and may decide, before the engine launches any resilient rule beyond it.
            let (block, rest) = span (isJust . prepResilience) (r : rs)
             in evalBlock ctx ev block >>= \case
                    Left d -> pure d
                    Right blockReasons -> step rest (reverse blockReasons <> reasons)

-- Launch a contiguous resilient block concurrently, then await in boot order. 'Left' is
-- the earliest decisive winner, 'Right' the block's non-decisive reasons in boot order.
evalBlock :: EvalContext -> RuleEvidence -> [PreparedRule] -> IO (Either Decision [Reason])
evalBlock ctx ev block =
    bracket
        (traverse (\r -> async (runEffectfulRule ctx r ev)) block)
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

-- 'CannotVet' has no transience evidence, so it produces a plain retryable refusal.
decisive :: Text -> RuleEvaluation -> Maybe Decision
decisive name = \case
    Decided (Allow reason) -> Just (Admitted name reason)
    Decided (Deny etag reason) -> Just (Blocked name etag reason)
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
    Deny _ reason -> reason
    NoDecision reason -> reason
    CannotVet _ reason -> reason

{- | Apply the push-age gate, then resilience. The gate runs ahead of breaker admission, which an
open breaker would otherwise skip past. Direct-rule exceptions remain the caller's responsibility.
-}
runEffectfulRule :: EvalContext -> PreparedRule -> RuleEvidence -> IO RuleEvaluation
runEffectfulRule ctx rule ev =
    expiredEvidence rule >>= \case
        Just verdict -> pure (Decided verdict)
        Nothing -> case prepResilience rule of
            Nothing -> Decided <$> prepEval rule ctx ev
            Just res -> runResilient res (prepName rule) (prepEval rule ctx) ev

-- The verdict an ineligible push resolves a gated rule to, or nothing while its evidence holds.
expiredEvidence :: PreparedRule -> IO (Maybe RuleVerdict)
expiredEvidence rule = case prepAdvisoryGate rule of
    Nothing -> pure Nothing
    Just gate -> agFreshness gate <&> fmap (refuseOn gate) . renderIneligible
  where
    refuseOn gate why = CannotVet (agAlignment gate) (prepName rule <> ": " <> why)

{- | Why a push is not eligible evidence, or 'Nothing' while it is. A serving generation the store
gave no publication time for reads as unverified, because its age cannot be established.
-}
renderIneligible :: AdvisoryFreshness -> Maybe Text
renderIneligible = \case
    AdvisoryFresh -> Nothing
    AdvisoryAging{} -> Nothing
    AdvisoryStale observed -> Just (renderExpiredPush observed)
    AdvisoryUndated -> Just "the object store reported no publication time for the serving advisory artifact"

-- An expired push: its age, the maximum it passed, and when it landed, so an operator can
-- tell an update outage from a maximum set too short.
renderExpiredPush :: AdvisoryAge -> Text
renderExpiredPush observed =
    "the advisory push is "
        <> renderDuration (advisoryAge observed)
        <> " old, past the maximum of "
        <> renderDuration (advisoryMaxAge observed)
        <> " (pushed at "
        <> renderIso8601Utc (advisoryPushedAt observed)
        <> ")"

{- | A human-readable summary of a decision, suitable for logs and the denial
response body.
-}
renderDecision :: RuleEvidence -> Decision -> Text
renderDecision ev decision =
    let subject = renderPackageName (evName ev) <> "@" <> renderVersion (evVersion ev)
     in case decision of
            Admitted name reason ->
                subject <> " was approved by " <> name <> ": " <> reason
            Blocked name _ reason ->
                subject <> " was denied by " <> name <> ": " <> reason
            BlockedByDefault reasons ->
                subject
                    <> " was denied (no rule allowed it)"
                    <> if null reasons
                        then ""
                        else ": " <> T.intercalate "; " reasons
            Undecidable _ reason ->
                subject <> " could not be evaluated: " <> reason

-- | Keep two non-zero units to distinguish near-threshold durations. Negative values render as zero.
renderDuration :: NominalDiffTime -> Text
renderDuration d = case take 2 (durationComponents secs) of
    [] -> "0 seconds"
    parts -> T.unwords (map renderDurationPart parts)
  where
    secs = max 0 (round (nominalDiffTimeToSeconds d)) :: Integer

durationLadder :: [(Text, Integer)]
durationLadder =
    [ ("day", 86400)
    , ("hour", 3600)
    , ("minute", 60)
    , ("second", 1)
    ]

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
