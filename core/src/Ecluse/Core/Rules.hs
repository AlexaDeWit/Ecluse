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
    AdvisoryGeneration (..),
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

import Data.Fixed (Fixed (MkFixed))
import Data.Text qualified as T
import Data.Text.Short qualified as TS
import Data.Time (NominalDiffTime, UTCTime (UTCTime), diffDays, diffTimeToPicoseconds, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixDayLength)
import UnliftIO (evaluate, tryAny)
import UnliftIO.MVar (withMVar)

import Ecluse.Core.Breaker (BreakerReporter (..))
import Ecluse.Core.Cve (AdvisoryRange (..), CveLookup (..), MissingScorePolicy (..), PackageAdvisories, advisoryIdsOf, affecting, hasAdvisories, keepAdvisories, packageAdvisories, scoreAtLeast)
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
import Ecluse.Core.Rules.Remediation (remediationVerdict)
import Ecluse.Core.Rules.Types
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Core.Version (renderVersion)

-- | One ecosystem's boot-bound rule capabilities: its advisory database and the rules' observers.
data RuleDeps = RuleDeps
    { rdAdvisoryDatabase :: AdvisoryDatabase
    , rdCurrentAdvisoryEtag :: IO (Maybe DbEtag)
    -- ^ Read the active ETag without pinning or delaying a swap.
    , rdBreakerReporter :: BreakerReporter
    -- ^ Where advisory rules report breaker transitions, as @ecluse.rule.breaker.state@.
    , rdSourceReporter :: SourceReporter
    -- ^ Report source availability as outage transitions for each reached advisory rule.
    , rdAdvisoryFreshness :: IO AdvisoryFreshness
    -- ^ The live slot age for the Dredger's final deletion check. Advisory reads use their borrowed age.
    }

-- | An ecosystem's advisory database, as configuration fixes it at boot.
data AdvisoryDatabase
    = NoAdvisoryDatabase
    | -- | Hold one generation's age reading and lookup access together through the caller's action.
      AdvisoryDatabase (forall a. (AdvisoryGeneration -> IO a) -> IO a)

-- | The age and lookup capabilities borrowed from one advisory generation.
data AdvisoryGeneration = AdvisoryGeneration
    { agFreshness :: IO AdvisoryFreshness
    -- ^ Assess the borrowed publication timestamp against the existing clock and maximum age.
    , agLookup :: forall a. (Maybe (DbEtag, CveLookup) -> IO a) -> IO a
    -- ^ Access the borrowed lookup. Query failures belong to the caller's resilience policy.
    }

-- | Borrow the loaded generation, or 'Nothing' when none is configured or none has loaded yet.
withCveLookup :: RuleDeps -> (Maybe (DbEtag, CveLookup) -> IO a) -> IO a
withCveLookup deps use = case rdAdvisoryDatabase deps of
    NoAdvisoryDatabase -> use Nothing
    AdvisoryDatabase borrow -> borrow (\AdvisoryGeneration{agLookup} -> agLookup use)

-- | One package's advisory rows and the generation that served them, or 'Nothing' while none is loaded.
type AdvisoryRows = Maybe (DbEtag, PackageAdvisories)

-- | Parse one borrowed generation's package rows once for all versions. Query faults escape.
readAdvisories :: PackageName -> Maybe (DbEtag, CveLookup) -> IO AdvisoryRows
readAdvisories name =
    traverse (\(etag, cve) -> (etag,) . packageAdvisories (pkgEcosystem name) <$> cveAdvisoriesFor cve (TS.toText (pkgCanonical name)))

-- | Where a built-in rule's verdict comes from.
data VerdictSource
    = -- | One version's evidence and the request context.
      FromEvidence (EvalContext -> RuleEvidence -> RuleVerdict)
    | -- | The package's advisory rows, with how the rule resolves when it cannot read them.
      FromAdvisories AdvisoryAlignment (AdvisoryRows -> RuleEvidence -> RuleVerdict)

-- | How an advisory rule resolves when it cannot read: on an expired push, and on a faulted read.
data AdvisoryAlignment = AdvisoryAlignment
    { onExpiredPush :: FailureAlignment
    -- ^ A deny refuses an expired push under any configuration. The remediation allow takes its one choice here too.
    , onFaultedRead :: FailureAlignment
    -- ^ The rule's configured alignment for a timeout, a spent retry budget, or an open breaker.
    }

-- | Dispatch the closed rule vocabulary. An unread required fact refuses instead of abstaining.
verdictSource :: Rule -> VerdictSource
verdictSource = \case
    AllowScope scope -> FromEvidence $ \_ ev -> case pkgNamespace (evName ev) of
        Just s | s == scope -> Allow (ScopeAllowListed scope)
        _ -> NoDecision (ScopeNotAllowListed scope)
    AllowIfOlderThan minAge -> FromEvidence $ \ctx ev -> case evPublishedAt ev of
        Unread -> CannotVet FailDeny PublishTimeUnread
        Known Nothing -> NoDecision PublishTimeUnknown
        Known (Just publishedAt) -> ageVerdict minAge (ageSince (ctxNow ctx) publishedAt)
    DenyInstallTimeExecution -> FromEvidence $ \_ ev -> case evInstallCode ev of
        Unread -> CannotVet FailDeny InstallSignalUnread
        Known (RunsCodeOnInstall how) -> Deny Nothing (RunsOnInstall how)
        Known NoCodeOnInstall -> NoDecision NothingRunsOnInstall
        Known CodeExecUnknown -> NoDecision InstallCodeUndetermined
    DenyExternalDependencies -> FromEvidence $ \_ ev -> case evDependencySources ev of
        Unread -> CannotVet FailDeny DependencySourcesUnread
        Known (FetchesOutsideRegistry which) -> Deny Nothing (FetchesOutside which)
        Known RegistryDependencies -> NoDecision DependenciesThroughRegistry
        Known DependencySourcesUnknown -> NoDecision DependencySourcesUndetermined
    DenyIfYanked -> availabilityVerdict Yanked VersionYanked VersionNotYanked
    DenyIfDeprecated -> availabilityVerdict Deprecated VersionDeprecated VersionNotDeprecated
    DenyByIdentity ident -> FromEvidence $ \_ ev ->
        if matchesIdentity ident ev
            then Deny Nothing (IdentityRevoked ident)
            else NoDecision (IdentityNotRevoked ident)
    AllowByIdentity ident -> FromEvidence $ \_ ev ->
        if matchesIdentity ident ev
            then Allow (IdentityAllowListed ident)
            else NoDecision (IdentityNotAllowListed ident)
    -- The remediation allow's one choice governs every unavailability, an expired push included.
    AllowIfRemediatesCve params ->
        let alignment = aircOnUnavailable params
            decide = remediationVerdict (aircRemaining params)
         in FromAdvisories (AdvisoryAlignment alignment alignment) $ \case
                Nothing -> const (CannotVet alignment NoDatabaseLoaded)
                Just (_, advisories) -> decide advisories
    -- No in-process retry could load a database, so its absence is a verdict and never a fault.
    DenyIfCve params -> FromAdvisories (AdvisoryAlignment FailDeny (dicOnUnavailable params)) $ \case
        Nothing -> const (CannotVet (dicOnUnavailable params) NoDatabaseLoaded)
        Just (etag, advisories) -> advisoryDenyVerdict etag DenyMissingScore Cvss (dicMinCvss params) arSeverity advisories
    DenyIfKnownMalicious alignment -> FromAdvisories (AdvisoryAlignment FailDeny alignment) $ \case
        Nothing -> const (CannotVet alignment NoDatabaseLoaded)
        Just (etag, advisories) ->
            affectedVerdict etag KnownMalicious NoKnownMaliciousAdvisory (keepAdvisories isMalicious advisories)
    DenyIfEpss params -> FromAdvisories (AdvisoryAlignment FailDeny (dieOnUnavailable params)) $ \case
        Nothing -> const (CannotVet (dieOnUnavailable params) NoDatabaseLoaded)
        Just (etag, advisories) -> advisoryDenyVerdict etag AbstainMissingScore Epss (dieMinEpss params) arEpss advisories

availabilityVerdict :: Availability -> Reason -> Reason -> VerdictSource
availabilityVerdict flag denied absent = FromEvidence $ \_ ev -> case evAvailability ev of
    Unread -> CannotVet FailDeny AvailabilityUnread
    Known actual
        | actual == flag -> Deny Nothing denied
        | otherwise -> NoDecision absent

-- The quarantine gives the registry time to yank a malicious publish.
ageVerdict :: NominalDiffTime -> NominalDiffTime -> RuleVerdict
ageVerdict minAge age
    | age >= minAge = Allow (PublishedLongEnough age minAge)
    | otherwise = NoDecision (PublishedTooRecently age minAge)

-- Equal to 'diffUTCTime' in exact Pico arithmetic, without its two POSIX conversions through
-- picosecond products past 2^64. The time of day clamps at 'posixDayLength' as it does there.
ageSince :: UTCTime -> UTCTime -> NominalDiffTime
ageSince (UTCTime nowDay nowTime) (UTCTime day time) =
    secondsToNominalDiffTime (MkFixed (diffDays nowDay day * dayPicos + clamped nowTime - clamped time))
  where
    clamped t = min dayPicos (diffTimeToPicoseconds t)

dayPicos :: Integer
dayPicos = diffTimeToPicoseconds (realToFrac posixDayLength)

-- The score filter and the abstention no version changes are built once per read.
advisoryDenyVerdict :: DbEtag -> MissingScorePolicy -> AdvisoryScore -> Double -> (AdvisoryRange -> Maybe Double) -> PackageAdvisories -> RuleEvidence -> RuleVerdict
advisoryDenyVerdict etag missing score threshold scoreOf advisories =
    affectedVerdict etag (AffectedBy score threshold) (NotAffectedAtThreshold score) scored
  where
    scored = keepAdvisories (scoreAtLeast missing threshold . scoreOf) advisories

isMalicious :: AdvisoryRange -> Bool
isMalicious = maybe False (not . T.null) . T.stripPrefix "MAL-" . arCveId

-- Filtering happens once per package. With no segment left, every version shares one abstention.
affectedVerdict :: DbEtag -> (AdvisoryIds -> Reason) -> Reason -> PackageAdvisories -> RuleEvidence -> RuleVerdict
affectedVerdict etag affectedReason absentReason advisories
    | hasAdvisories advisories = \ev -> case advisoryIdsOf (affecting advisories (evVersion ev)) of
        [] -> absent
        affected : more -> Deny (Just etag) (affectedReason (mkAdvisoryIds (affected :| more)))
    | otherwise = const absent
  where
    absent = NoDecision absentReason

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
    = -- | Each version's verdict. It can run more than once for a version and must answer alike. A throw refuses admission.
      PerVersion (EvalContext -> RuleEvidence -> IO RuleVerdict)
    | -- | A verdict for each version from the evaluator's one advisory read of the package.
      PerPackage PackageRead

-- | A rule's bracketed advisory read and its verdict over the shared result.
data PackageRead = PackageRead
    { prRead :: forall a. PackageName -> (AdvisoryFreshness -> IO AdvisoryRows -> IO a) -> IO a
    -- ^ Hold the assessed generation open while the caller gates its age and reads its rows.
    , prResilience :: Maybe Resilience
    -- ^ The policy around the read, or 'Nothing' where no database is configured and the read does no IO.
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

-- | Allocate each advisory rule's breaker once. Unconfigured databases need no resilience.
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
            { prRead = withAdvisoryRead deps
            , prResilience = resilience
            , prAlignment = alignment
            , prVerdict = verdict
            , prReporter = rdSourceReporter deps
            }

withAdvisoryRead :: RuleDeps -> PackageName -> (AdvisoryFreshness -> IO AdvisoryRows -> IO a) -> IO a
withAdvisoryRead deps package use = case rdAdvisoryDatabase deps of
    AdvisoryDatabase borrow ->
        borrow (\generation -> agFreshness generation >>= \freshness -> use freshness (agLookup generation (readAdvisories package)))
    NoAdvisoryDatabase -> rdAdvisoryFreshness deps >>= \freshness -> use freshness (pure Nothing)

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

-- | Render each rule and its phases in the order 'bootOrder' evaluates them.
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

-- | Decide a request's versions in boot order, catching throws. Advisory rules sharing one 'prepare' read one generation.
newEvaluator :: EvalContext -> [PreparedRule] -> IO (RuleEvidence -> IO Decision)
newEvaluator ctx rules = do
    shared <- newPackageCell
    bound <- traverse (\rule -> (rule,) <$> bindRule ctx shared rule) (bootOrder rules)
    -- One handler per version. A throw decides the version again with a handler per rule. The fast pass
    -- forces the evidence first, so evidence that throws leaves no held result for the second pass to meet.
    pure (\ev -> tryAny (evaluate ev *> stepRules forceEvaluation ev bound []) >>= either (const (stepRules caughtEvaluation ev bound [])) pure)

-- | Decide one version through a fresh 'newEvaluator'.
evalRules :: EvalContext -> [PreparedRule] -> RuleEvidence -> IO Decision
evalRules ctx rules ev = newEvaluator ctx rules >>= ($ ev)

-- How the fold runs one rule's evaluation. Each way forces it with 'forceEvaluation'.
type RunEvaluation = IO RuleEvaluation -> IO RuleEvaluation

-- One rule's evaluation of a version, as the action the fold runs.
type VersionEval = RuleEvidence -> IO (IO RuleEvaluation)

-- A rule's closures are built once here, not per version. 'eval ctx' is bound outside the version
-- lambda because a prepared rule builds its verdict function for each context it is given.
bindRule :: EvalContext -> PackageCell (Either Text AdvisoryRead) -> PreparedRule -> IO VersionEval
bindRule ctx shared rule = case prepEval rule of
    PerVersion eval -> do
        let decide = eval ctx
        pure (\ev -> pure (Decided <$!> decide ev))
    PerPackage packageRead -> do
        own <- newPackageCell
        let resolve = heldFor own (decideFromRead (prepName rule) packageRead shared)
        pure $ \ev ->
            resolve ev <&> \case
                Left escape -> pure (threw escape)
                Right decideVersion -> pure (decideVersion ev)

-- Strict fields evaluate the verdict payloads. Optional payloads need their own evaluation.
forceEvaluation :: IO RuleEvaluation -> IO RuleEvaluation
forceEvaluation action = do
    result <- action >>= evaluate
    case result of
        Decided (Deny etag _) -> traverse_ evaluate etag
        Unavailable (WillResolve retry) _ _ -> traverse_ evaluate retry
        _ -> pass
    pure result

-- The rule's own handler around 'forceEvaluation'. Both passes force alike, so the fast pass never forces less.
caughtEvaluation :: RunEvaluation
caughtEvaluation = fmap (either threw id) . caught . forceEvaluation

-- An evaluation that throws breaks its contract, so it refuses admission as a fail-closed inability.
threw :: Text -> RuleEvaluation
threw = Decided . CannotVet FailDeny . RuleThrew

caught :: IO a -> IO (Either Text a)
caught = fmap (first displayExceptionT) . tryAny

-- A result held for the last package decided. Another package computes its own, never borrowing it.
-- The lock serialises computing. A hit reads the latest result without it.
data PackageCell a = PackageCell (MVar ()) (IORef (Maybe (PackageName, a)))

newPackageCell :: IO (PackageCell a)
newPackageCell = PackageCell <$> newMVar () <*> newIORef Nothing

-- A miss looks again under the lock, so concurrent callers for one package compute it once.
heldFor :: PackageCell a -> (RuleEvidence -> IO a) -> RuleEvidence -> IO a
heldFor (PackageCell lock latest) compute ev =
    readIORef latest >>= \case
        Just (known, value) | known == evName ev -> pure value
        _ -> withMVar lock $ \() ->
            readIORef latest >>= \case
                Just (known, value) | known == evName ev -> pure value
                _ -> compute ev >>= \value -> value <$ atomicWriteIORef latest (Just (evName ev, value))

-- The evaluator's one advisory read of a package: its rows, or why it has none.
data AdvisoryRead
    = RowsRead AdvisoryRows
    | PushIneligible Inability
    | ReadGivenUp ReadFault

-- One rule's evaluation of every version from the shared read, made by this rule if it is the first
-- reached. The rule reports once per package whether the read let it consult the source.
decideFromRead :: Text -> PackageRead -> PackageCell (Either Text AdvisoryRead) -> RuleEvidence -> IO (Either Text (RuleEvidence -> RuleEvaluation))
decideFromRead name packageRead shared reaching =
    heldFor shared (caught . readShared packageRead . evName) reaching >>= \case
        Left escape -> pure (Left escape)
        Right advisory -> do
            let resolved = resolveRead packageRead advisory
            health <- caught (evaluate (readHealth name advisory (resolved reaching)))
            caught (resolved <$ reportSource (prReporter packageRead) (either (SourceUnavailable name . RuleThrew) id health))

-- The push-age gate, then the rows under the reading rule's resilience. The gate runs ahead of
-- breaker admission, which an open breaker would otherwise skip past.
readShared :: PackageRead -> PackageName -> IO AdvisoryRead
readShared packageRead package =
    prRead packageRead package $ \freshness rows -> case pushInability freshness of
        Just why -> pure (PushIneligible why)
        Nothing -> case prResilience packageRead of
            Nothing -> RowsRead <$> rows
            Just res -> either ReadGivenUp RowsRead <$> runResilient res rows

-- One rule's evaluation of each version, under its own alignments. A fault resolves all alike.
resolveRead :: PackageRead -> AdvisoryRead -> RuleEvidence -> RuleEvaluation
resolveRead packageRead = \case
    RowsRead rows -> Decided . prVerdict packageRead rows
    PushIneligible why -> const (Decided (CannotVet (onExpiredPush alignment) why))
    ReadGivenUp fault -> const (Unavailable (rfTransience fault) (onFaultedRead alignment) (rfReason fault))
  where
    alignment = prAlignment packageRead

-- Whether the read let one rule consult the source: only a rule that could not vet says it did not.
readHealth :: Text -> AdvisoryRead -> RuleEvaluation -> SourceHealth
readHealth name advisory resolved = case advisory of
    ReadGivenUp fault -> SourceUnavailable name (rfDetail fault)
    _ -> case resolved of
        Decided (CannotVet _ why) -> SourceUnavailable name why
        Unavailable _ _ why -> SourceUnavailable name why
        Decided _ -> SourceAnswered name

-- 'passed' holds each non-decisive evaluation in reverse boot order. The deny-by-default trail
-- and an admission's skipped-check evidence both read it back.
stepRules :: RunEvaluation -> RuleEvidence -> [(PreparedRule, VersionEval)] -> [Passed] -> IO Decision
stepRules _ _ [] passed = pure (BlockedByDefault (map passedReason (reverse passed)))
stepRules run ev ((rule, eval) : rest) passed =
    eval ev >>= run >>= \res -> case decisive (prepName rule) res of
        Just d -> pure (withEvidence passed (map fst rest) d)
        Nothing -> stepRules run ev rest (Passed (prepName rule) res : passed)

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
-- then the ones it pre-empted. 'decisive' builds it with none, so it stands when none is found.
withEvidence :: [Passed] -> [PreparedRule] -> Decision -> Decision
withEvidence passed unreached decided = case decided of
    Admitted name reason _ | not (null evidence) -> Admitted name reason evidence
    _ -> decided
  where
    evidence = reverse (mapMaybe skippedOf passed) <> map (Unreached . prepName) unreached

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

-- | Return why expired or undated evidence is unavailable, or 'Nothing' while it is eligible.
pushInability :: AdvisoryFreshness -> Maybe Inability
pushInability = \case
    AdvisoryFresh -> Nothing
    AdvisoryAging{} -> Nothing
    AdvisoryStale observed -> Just (PushPastMaximum observed)
    AdvisoryUndated -> Just PushUndated
