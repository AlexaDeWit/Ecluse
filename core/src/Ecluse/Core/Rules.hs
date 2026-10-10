-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The policy engine denies by default and decides in boot order. One advisory read per package
serves every advisory rule an evaluator reaches, so a request's decisions read one generation.
-}
module Ecluse.Core.Rules (
    -- * The boot-bound rule capabilities
    RuleDeps (..),
    AdvisoryDatabase (..),
    withCveLookup,

    -- * The built-in rule dispatch
    VerdictSource (..),
    AdvisoryAlignment (..),
    verdictSource,
    AdvisoryRows,
    readAdvisories,

    -- * The engine's prepared rule
    PreparedRule (..),
    RuleEval (..),
    PackageRead (..),
    Resilience (..),
    prepare,
    prepResilience,

    -- * Boot-time ordering
    bootOrder,
    renderBootOrder,

    -- * Evaluation
    newEvaluator,
    evalRules,
    pushInability,

    -- * Observing the advisory source
    SourceHealth (..),
    SourceReporter (..),
    noSourceReporter,
) where

import Data.Text.Short qualified as TS
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import UnliftIO (tryAny)
import UnliftIO.MVar (modifyMVar)

import Ecluse.Core.Breaker (BreakerReporter (..))
import Ecluse.Core.Cve (AdvisoryRange (..), CveLookup (..), MissingScorePolicy (..), PackageAdvisories, affecting, fixedAt, keepAdvisories, packageAdvisories, scoreAtLeast)
import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Package
import Ecluse.Core.Rules.Effectful (
    ReadFault (..),
    Resilience (..),
    defaultEffectfulConfig,
    newBreaker,
    runResilient,
 )
import Ecluse.Core.Rules.Freshness (AdvisoryFreshness (AdvisoryAging, AdvisoryFresh, AdvisoryStale, AdvisoryUndated))
import Ecluse.Core.Rules.Outage (SourceHealth (..), SourceReporter (..), noSourceReporter)
import Ecluse.Core.Rules.Types
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Core.Version (renderVersion)

-- | One ecosystem's boot-bound rule capabilities: its advisory database and the rules' observers.
data RuleDeps = RuleDeps
    { rdAdvisoryDatabase :: AdvisoryDatabase
    , rdCurrentAdvisoryEtag :: IO (Maybe DbEtag)
    {- ^ A non-pinning read of the active 'DbEtag'. It holds no generation open, so it never
    delays a shadow-swap.
    -}
    , rdBreakerReporter :: BreakerReporter
    -- ^ Where advisory rules report breaker transitions, as @ecluse.rule.breaker.state@.
    , rdSourceReporter :: SourceReporter
    {- ^ Where each advisory rule a request reaches reports whether it could consult the source, so
    an outage is observed as a transition rather than once per request.
    -}
    , rdAdvisoryFreshness :: IO AdvisoryFreshness
    {- ^ How old the serving artifact's push is, read again for every package read. The wall
    clock alone ages it, so an unchanged artifact expires in a warm process.
    -}
    }

-- | An ecosystem's advisory database, as configuration fixes it at boot.
data AdvisoryDatabase
    = NoAdvisoryDatabase
    | -- | Bracketed access to the lookup and ETag acquired together, 'Nothing' until a generation loads.
      AdvisoryDatabase (forall a. (Maybe (DbEtag, CveLookup) -> IO a) -> IO a)

-- | Borrow the loaded generation, or 'Nothing' when none is configured or none has loaded yet.
withCveLookup :: RuleDeps -> (Maybe (DbEtag, CveLookup) -> IO a) -> IO a
withCveLookup deps use = case rdAdvisoryDatabase deps of
    NoAdvisoryDatabase -> use Nothing
    AdvisoryDatabase borrow -> borrow use

-- | One package's advisory rows and the generation that served them, or 'Nothing' while none is loaded.
type AdvisoryRows = Maybe (DbEtag, PackageAdvisories)

{- | Pin a generation and read one package's rows through it, their bounds parsed once for all its
versions. A query fault escapes to the caller.
-}
readAdvisories :: RuleDeps -> PackageName -> IO AdvisoryRows
readAdvisories deps name =
    withCveLookup deps (traverse (\(etag, cve) -> (etag,) . packageAdvisories (pkgEcosystem name) <$> cveAdvisoriesFor cve (TS.toText (pkgCanonical name))))

-- | Where a built-in rule's verdict comes from.
data VerdictSource
    = -- | One version's evidence and the request context.
      FromEvidence (EvalContext -> RuleEvidence -> RuleVerdict)
    | -- | The package's advisory rows, with how the rule resolves when it cannot read them.
      FromAdvisories AdvisoryAlignment (AdvisoryRows -> RuleEvidence -> RuleVerdict)

-- | How an advisory rule resolves when it cannot read: on an expired push, and on a faulted read.
data AdvisoryAlignment = AdvisoryAlignment
    { onExpiredPush :: FailureAlignment
    -- ^ Fixed per rule, since expiry is unavailability configuration cannot waive.
    , onFaultedRead :: FailureAlignment
    -- ^ The rule's configured alignment for a timeout, a spent retry budget, or an open breaker.
    }

{- | The single dispatch over the closed vocabulary. A rule that reads a fact nothing supplied
refuses rather than abstaining, so the fold stops at it.
-}
verdictSource :: Rule -> VerdictSource
verdictSource = \case
    AllowScope scope -> FromEvidence $ \_ ev -> case pkgNamespace (evName ev) of
        Just s | s == scope -> Allow (ScopeAllowListed scope)
        _ -> NoDecision (ScopeNotAllowListed scope)
    AllowIfOlderThan minAge -> FromEvidence $ \ctx ev -> case evPublishedAt ev of
        Unread -> CannotVet FailDeny PublishTimeUnread
        Known Nothing -> NoDecision PublishTimeUnknown
        Known (Just publishedAt) -> ageVerdict minAge (diffUTCTime (ctxNow ctx) publishedAt)
    DenyInstallTimeExecution -> FromEvidence $ \_ ev -> case evInstallCode ev of
        Unread -> CannotVet FailDeny InstallSignalUnread
        Known (RunsCodeOnInstall how) -> Deny Nothing (RunsOnInstall how)
        Known NoCodeOnInstall -> NoDecision NothingRunsOnInstall
        Known CodeExecUnknown -> NoDecision InstallCodeUndetermined
    DenyByIdentity ident -> FromEvidence $ \_ ev ->
        if matchesIdentity ident ev
            then Deny Nothing (IdentityRevoked ident)
            else NoDecision (IdentityNotRevoked ident)
    AllowByIdentity ident -> FromEvidence $ \_ ev ->
        if matchesIdentity ident ev
            then Allow (IdentityAllowListed ident)
            else NoDecision (IdentityNotAllowListed ident)
    -- Expiry abstains on the remediation allow, so a deny's refusal is what the version meets.
    AllowIfRemediatesCve -> FromAdvisories (AdvisoryAlignment FailNoDecision FailNoDecision) $ \case
        Nothing -> const (NoDecision NoDatabaseToRemediate)
        Just (_, advisories) -> classifyRanges advisories
    -- A deny's configured alignment governs a faulted read and an unloaded database alike. No
    -- in-process retry could load a database, so its absence is a verdict and never a fault.
    DenyIfCve params -> FromAdvisories (AdvisoryAlignment FailDeny (dicOnUnavailable params)) $ \case
        Nothing -> const (CannotVet (dicOnUnavailable params) NoDatabaseLoaded)
        Just (etag, advisories) -> advisoryDenyVerdict etag DenyMissingScore Cvss (dicMinCvss params) arSeverity advisories
    DenyIfEpss params -> FromAdvisories (AdvisoryAlignment FailDeny (dieOnUnavailable params)) $ \case
        Nothing -> const (CannotVet (dieOnUnavailable params) NoDatabaseLoaded)
        Just (etag, advisories) -> advisoryDenyVerdict etag AbstainMissingScore Epss (dieMinEpss params) arEpss advisories

{- The minimum-age verdict for a version whose publish time the evidence carries. The quarantine
holds a new version until the registry has had time to yank a malicious publish. -}
ageVerdict :: NominalDiffTime -> NominalDiffTime -> RuleVerdict
ageVerdict minAge age
    | age >= minAge = Allow (PublishedLongEnough age minAge)
    | otherwise = NoDecision (PublishedTooRecently age minAge)

-- The score filter and the abstention no version changes are built once per read.
advisoryDenyVerdict :: DbEtag -> MissingScorePolicy -> AdvisoryScore -> Double -> (AdvisoryRange -> Maybe Double) -> PackageAdvisories -> RuleEvidence -> RuleVerdict
advisoryDenyVerdict etag missing score threshold scoreOf advisories = \ev ->
    case ordNub (map arCveId (affecting scored (evVersion ev))) of
        [] -> unaffected
        affected : more -> Deny (Just etag) (AffectedBy score threshold (mkAdvisoryIds (affected :| more)))
  where
    scored = keepAdvisories (scoreAtLeast missing threshold . scoreOf) advisories
    unaffected = NoDecision (NotAffectedAtThreshold score)

-- A version still inside any advisory's affected range, an unfixed one included, must not fast-track.
classifyRanges :: PackageAdvisories -> RuleEvidence -> RuleVerdict
classifyRanges advisories ev =
    case (remediated, stillOpen) of
        ([], _) -> NoDecision FixesNoAdvisory
        (fixed : fixes, []) -> Allow (Remediates (mkAdvisoryIds (fixed :| fixes)))
        (fixed : fixes, open : others) -> NoDecision (FixesButStillAffected (mkAdvisoryIds (fixed :| fixes)) (mkAdvisoryIds (open :| others)))
  where
    remediated = ordNub (map arCveId (fixedAt advisories (evVersion ev)))
    stillOpen = ordNub (map arCveId (affecting advisories (evVersion ev)))

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
    -- ^ The stable, human-facing name: the boot-order tiebreak and the credited identity.
    , prepPrecedence :: Int
    -- ^ The precedence at which this rule competes. Higher wins in the boot order.
    , prepEval :: RuleEval
    -- ^ How the rule reaches the versions an evaluator decides.
    }

-- | How a prepared rule reaches the versions an evaluator decides.
data RuleEval
    = -- | Each version's verdict on its own. A throw refuses admission.
      PerVersion (EvalContext -> RuleEvidence -> IO RuleVerdict)
    | -- | A verdict for each version from the evaluator's one advisory read of the package.
      PerPackage PackageRead

{- | An advisory rule: how to make the evaluator's shared read when this rule reaches it first, and
how the rule decides from that read.
-}
data PackageRead = PackageRead
    { prFreshness :: IO AdvisoryFreshness
    -- ^ The push-age reading, taken ahead of the breaker.
    , prResilience :: Maybe Resilience
    -- ^ The policy around the read, or 'Nothing' where no database is configured and the read does no IO.
    , prRows :: PackageName -> IO AdvisoryRows
    -- ^ The package's rows in one pinned generation.
    , prAlignment :: AdvisoryAlignment
    -- ^ How this rule resolves an ineligible push and a faulted read.
    , prVerdict :: AdvisoryRows -> RuleEvidence -> RuleVerdict
    -- ^ This rule's verdict for each version, from the rows.
    , prReporter :: SourceReporter
    -- ^ Where this rule reports whether the read let it consult the source.
    }

-- | The resilience policy a prepared rule's package read runs under, if any.
prepResilience :: PreparedRule -> Maybe Resilience
prepResilience rule = case prepEval rule of
    PerVersion _ -> Nothing
    PerPackage packageRead -> prResilience packageRead

{- | Allocate each advisory rule's breaker once. With no advisory database configured, an advisory
rule's read does no IO and returns the rule's fixed verdict, so it runs without resilience.
-}
prepare :: RuleDeps -> [PrecededRule] -> IO [PreparedRule]
prepare deps = traverse (prepareRule deps)

prepareRule :: RuleDeps -> PrecededRule -> IO PreparedRule
prepareRule deps (PrecededRule prec _ rule) = do
    eval <- case verdictSource rule of
        FromEvidence verdict -> pure (PerVersion (\ctx -> pure . verdict ctx))
        FromAdvisories alignment verdict -> PerPackage <$> advisoryRead deps alignment verdict
    pure PreparedRule{prepName = ruleName rule, prepPrecedence = prec, prepEval = eval}

advisoryRead :: RuleDeps -> AdvisoryAlignment -> (AdvisoryRows -> RuleEvidence -> RuleVerdict) -> IO PackageRead
advisoryRead deps alignment verdict = do
    resilience <- case rdAdvisoryDatabase deps of
        NoAdvisoryDatabase -> pure Nothing
        AdvisoryDatabase _ -> Just <$> newResilience deps
    pure
        PackageRead
            { prFreshness = rdAdvisoryFreshness deps
            , prResilience = resilience
            , prRows = readAdvisories deps
            , prAlignment = alignment
            , prVerdict = verdict
            , prReporter = rdSourceReporter deps
            }

newResilience :: RuleDeps -> IO Resilience
newResilience deps = do
    breaker <- newBreaker
    pure
        Resilience
            { resConfig = defaultEffectfulConfig
            , resBreaker = breaker
            , resBreakerReporter = rdBreakerReporter deps
            , resClock = getCurrentTime
            }

-- | Sort by descending precedence, then ascending rule name, independently of configuration order.
bootOrder :: [PreparedRule] -> [PreparedRule]
bootOrder = sortOn (\r -> bootKey (prepPrecedence r) (prepName r))

-- 'bootOrder' and 'renderBootOrder' order through this one key, so the logged order is the
-- evaluated one.
bootKey :: Int -> Text -> (Down Int, Text)
bootKey prec name = (Down prec, name)

{- | One line per configured rule, in the order 'bootOrder' evaluates the rules prepared from them,
with the phases each applies at.
-}
renderBootOrder :: [PrecededRule] -> [Text]
renderBootOrder rules = zipWith line [1 :: Int ..] (sortOn (\r -> bootKey (rulePrecedence r) (ruleName (prRule r))) rules)
  where
    line i r =
        "rule "
            <> show i
            <> ": "
            <> ruleName (prRule r)
            <> " (precedence "
            <> show (rulePrecedence r)
            <> ", "
            <> renderReach (ruleReach r)
            <> ")"

renderReach :: RuleReach -> Text
renderReach = \case
    AdmissionAndRevocation -> "applies at admission and revocation"
    AdmissionOnly -> "applies at admission only"

{- | An evaluator for one request's versions, in boot order. Its advisory rules must come from one
'prepare', since the first reached reads the package once for them all. A throw refuses.
-}
newEvaluator :: EvalContext -> [PreparedRule] -> IO (RuleEvidence -> IO Decision)
newEvaluator ctx rules = do
    shared <- newPackageCell
    bound <- traverse (\rule -> (rule,) <$> bindRule ctx shared rule) (bootOrder rules)
    pure (\ev -> stepRules ev bound [])

-- | Decide one version through a fresh 'newEvaluator'.
evalRules :: EvalContext -> [PreparedRule] -> RuleEvidence -> IO Decision
evalRules ctx rules ev = newEvaluator ctx rules >>= ($ ev)

-- One rule as an evaluator runs it for each version: its evaluation, or what it threw.
type VersionEval = RuleEvidence -> IO (Either Text RuleEvaluation)

bindRule :: EvalContext -> PackageCell (Either Text AdvisoryRead) -> PreparedRule -> IO VersionEval
bindRule ctx shared rule = case prepEval rule of
    PerVersion eval -> pure (caught . fmap Decided . eval ctx)
    PerPackage packageRead -> do
        own <- newPackageCell
        pure $ \ev -> fmap ($ ev) <$> heldFor own (evName ev) (decideFromRead (prepName rule) packageRead shared ev)

caught :: IO a -> IO (Either Text a)
caught = fmap (first displayExceptionT) . tryAny

-- A result held for the last package decided. Another package computes its own, never borrowing it.
newtype PackageCell a = PackageCell (MVar (Maybe (PackageName, a)))

newPackageCell :: IO (PackageCell a)
newPackageCell = PackageCell <$> newMVar Nothing

heldFor :: PackageCell a -> PackageName -> IO a -> IO a
heldFor (PackageCell held) package compute =
    modifyMVar held $ \case
        Just (known, value) | known == package -> pure (Just (known, value), value)
        _ -> (\value -> (Just (package, value), value)) <$> compute

-- The evaluator's one advisory read of a package: its rows, or why it has none.
data AdvisoryRead
    = RowsRead AdvisoryRows
    | PushIneligible Inability
    | ReadGivenUp ReadFault

{- One rule's evaluation of every version from the shared read, made by this rule if it is the first
reached. The rule reports once per package whether the read let it consult the source. -}
decideFromRead :: Text -> PackageRead -> PackageCell (Either Text AdvisoryRead) -> RuleEvidence -> IO (Either Text (RuleEvidence -> RuleEvaluation))
decideFromRead name packageRead shared reaching =
    heldFor shared package (caught (readShared packageRead package)) >>= \case
        Left escape -> pure (Left escape)
        Right advisory ->
            caught (resolveRead packageRead advisory <$ reportSource (prReporter packageRead) (readHealth name packageRead advisory reaching))
  where
    package = evName reaching

{- The push-age gate, then the rows under the reading rule's resilience. The gate runs ahead of
breaker admission, which an open breaker would otherwise skip past. -}
readShared :: PackageRead -> PackageName -> IO AdvisoryRead
readShared packageRead package =
    prFreshness packageRead >>= \freshness -> case pushInability freshness of
        Just why -> pure (PushIneligible why)
        Nothing -> case prResilience packageRead of
            Nothing -> RowsRead <$> prRows packageRead package
            Just res -> either ReadGivenUp RowsRead <$> runResilient res (prRows packageRead package)

-- One rule's evaluation of each version, under its own alignments. A fault resolves all alike.
resolveRead :: PackageRead -> AdvisoryRead -> RuleEvidence -> RuleEvaluation
resolveRead packageRead = \case
    RowsRead rows -> Decided . prVerdict packageRead rows
    PushIneligible why -> const (Decided (CannotVet (onExpiredPush alignment) why))
    ReadGivenUp fault -> const (Unavailable (rfTransience fault) (onFaultedRead alignment) (rfReason fault))
  where
    alignment = prAlignment packageRead

-- Whether the read let one rule consult the source: only a rule that could not vet says it did not.
readHealth :: Text -> PackageRead -> AdvisoryRead -> RuleEvidence -> SourceHealth
readHealth name packageRead advisory reaching = case advisory of
    RowsRead rows -> case prVerdict packageRead rows reaching of
        CannotVet _ why -> SourceUnavailable name why
        Allow _ -> SourceAnswered name
        Deny _ _ -> SourceAnswered name
        NoDecision _ -> SourceAnswered name
    PushIneligible why -> SourceUnavailable name why
    ReadGivenUp fault -> SourceUnavailable name (rfDetail fault)

-- 'passed' holds each non-decisive evaluation in reverse boot order. The deny-by-default trail
-- and an admission's skipped-check evidence both read it back.
stepRules :: RuleEvidence -> [(PreparedRule, VersionEval)] -> [Passed] -> IO Decision
stepRules _ [] passed = pure (BlockedByDefault (map passedReason (reverse passed)))
stepRules ev ((rule, eval) : rest) passed =
    eval ev >>= \case
        -- An evaluation that throws breaks its contract and must refuse admission.
        Left escape -> pure (Undecidable (WillResolve Nothing) (RuleUnable (prepName rule) (RuleThrew escape)))
        Right res -> case decisive (prepName rule) res of
            Just d -> pure (withEvidence passed (map fst rest) d)
            Nothing -> stepRules ev rest (Passed (prepName rule) res : passed)

-- One non-decisive evaluation as the fold keeps it, so the trail and the evidence read one record.
data Passed = Passed Text RuleEvaluation

-- The audit reason one passed evaluation leaves in the deny-by-default trail, its rule named
-- where the rule could not vet.
passedReason :: Passed -> Reason
passedReason (Passed name res) = case res of
    Unavailable _ _ why -> RuleUnable name why
    Decided (CannotVet _ why) -> RuleUnable name why
    Decided (Allow reason) -> reason
    Decided (Deny _ reason) -> reason
    Decided (NoDecision reason) -> reason

-- The evidence a fail-open inability leaves. A fail-closed one is decisive, so it never passes.
skippedOf :: Passed -> Maybe SkippedCheck
skippedOf (Passed name res) = case res of
    Decided (CannotVet alignment why) -> skipped alignment why
    Unavailable _ alignment why -> skipped alignment why
    Decided _ -> Nothing
  where
    skipped FailNoDecision why = Just (SkippedUnavailable name why)
    skipped FailDeny _ = Nothing

-- Only an admission carries evidence: the checks that could not vet ahead of it, in boot order,
-- then the ones it pre-empted.
withEvidence :: [Passed] -> [PreparedRule] -> Decision -> Decision
withEvidence passed unreached = \case
    Admitted name reason _ -> Admitted name reason (reverse (mapMaybe skippedOf passed) <> map (Unreached . prepName) unreached)
    other -> other

-- 'CannotVet' has no transience evidence, so it produces a plain retryable refusal. An admission's
-- evidence is attached by the fold, which alone knows what it passed and pre-empted.
decisive :: Text -> RuleEvaluation -> Maybe Decision
decisive name = \case
    Decided (Allow reason) -> Just (Admitted name reason [])
    Decided (Deny etag reason) -> Just (Blocked name etag reason)
    Decided (NoDecision _) -> Nothing
    Decided (CannotVet FailDeny why) -> Just (Undecidable (WillResolve Nothing) (RuleUnable name why))
    Decided (CannotVet FailNoDecision _) -> Nothing
    Unavailable transience FailDeny why -> Just (Undecidable transience (RuleUnable name why))
    Unavailable _ FailNoDecision _ -> Nothing

{- | Why a push is not eligible evidence, or 'Nothing' while it is. A serving generation the store
gave no publication time for reads as unverified, because its age cannot be established.
-}
pushInability :: AdvisoryFreshness -> Maybe Inability
pushInability = \case
    AdvisoryFresh -> Nothing
    AdvisoryAging{} -> Nothing
    AdvisoryStale observed -> Just (PushPastMaximum observed)
    AdvisoryUndated -> Just PushUndated
