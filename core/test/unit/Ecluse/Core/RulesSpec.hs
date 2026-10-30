-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Policy decisions over package evidence and injected capabilities.
Advisory regressions preserve ecosystem identity and display spelling.
-}
module Ecluse.Core.RulesSpec (spec) where

import Prelude hiding (universe)

import Data.Aeson (Value (String))
import Data.Text qualified as T
import Data.Text.Short qualified as TS
import Data.Time (Day (..), DiffTime, NominalDiffTime, UTCTime (..), addDays, addUTCTime, diffUTCTime, nominalDay, picosecondsToDiffTime)
import Data.Universe.Class (Universe (universe))
import Database.SQLite.Simple (Connection, Only, query, withConnection)
import Hedgehog (Gen, forAll, forAllWith, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Control.Concurrent (yield)
import UnliftIO.Async (mapConcurrently)
import UnliftIO.Exception (evaluate, finally, impureThrow, throwIO, tryAny)

import Ecluse.Core.Breaker (Breaker, admit, initialBreaker, recordFailure)
import Ecluse.Core.Cve (AdvisoryRange (..), CveDb (..), CveLookup (..), MissingScorePolicy (..), scoreAtLeast)
import Ecluse.Core.Cve.Internal (toRange)
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssOptional))
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore, LastAffected, Unbounded))
import Ecluse.Core.Package
import Ecluse.Core.Version (Version, canonicalPep440, mkVersion, renderVersion)
import Ecluse.Test.Cve (fakeCveLookup, namesFix, referenceInside, unscoredEpssCases)
import Ecluse.Test.Osv (CorpusVersion (CorpusV2), RangeRow, mkValidDbWithRows)
import Ecluse.Test.Osv.Withdrawal (maliciousZip, withdrawalZip)
import Ecluse.Test.OsvDb (openCveDb, withFixtureOsvDb, withOsvZipDb, withServedArtifact)
import Ecluse.Test.Package (sampleDetails, scopedNpm, unscopedNpm, unscopedPyPI, v1_0_0)
import Ecluse.Test.Rules (
    admissionOnly,
    admittedBy,
    atDefaultPrecedence,
    atPrecedence,
    blockedBy,
    constRule,
    differentialRemediation,
    evalRule,
    inertRuleDeps,
    isAllow,
    isBlockedByDefault,
    isCannotVet,
    isDeny,
    isNoDecision,
    isUndecidable,
    mapPackageRead,
    mapResilience,
    packageRule,
    remediationWith,
    sentences,
    servingRuleDeps,
    shippedRemediation,
    staticAdvisoryDatabase,
    verdictSentence,
    withAdvisoryFreshness,
    withInstallScripts,
 )
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

import Ecluse.Core.Rules
import Ecluse.Core.Rules.Effectful (EffectfulConfig (ecBackoff, ecBreakerCooldown, ecBreakerThreshold))
import Ecluse.Core.Rules.Freshness
import Ecluse.Core.Rules.Render (renderDecision, renderInability, renderReason)
import Ecluse.Core.Rules.Types
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Rules.FrozenEvaluator (frozenEvaluator, frozenPrepare)
import Ecluse.Rules.Remediation.Support (genEcosystem, genRemediation, genRows, genTenths, referenceRemediation)
import Ecluse.Rules.Support (SourceState (Faulting, Loaded), ctx, ctxAt, normaliseVerdictIds, now, pkg, sixDayLimit, sourceStateDeps)

listed :: Maybe Text -> RuleEvidence
listed mScope = identityEvidence (mkPackageName Npm (mkScope <$> mScope) "thing") v1_0_0

-- | Evidence whose installable dependency names a git source.
fetchesOutside :: RuleEvidence -> RuleEvidence
fetchesOutside ev = ev{evDependencySources = Known (FetchesOutsideRegistry externalReason)}

externalReason :: Text
externalReason = "dependency 'left-pad' in dependencies names a git source"

at :: Int -> Rule -> PrecededRule
at = atPrecedence

decideWith :: RuleDeps -> [PrecededRule] -> RuleEvidence -> IO Decision
decideWith deps prs ev = prepare deps prs >>= \prepared -> evalRules ctx prepared ev

decide :: [PrecededRule] -> RuleEvidence -> IO Decision
decide = decideWith inertRuleDeps

shouldDecide :: IO RuleVerdict -> (RuleVerdict, Text) -> Expectation
shouldDecide evaluated (verdict, sentence) = do
    actual <- evaluated
    actual `shouldBe` verdict
    verdictSentence actual `shouldBe` sentence

-- GHC can append a runtime backtrace after the exception's own diagnostic line.
shouldHaveExceptionCause :: Text -> Text -> Expectation
shouldHaveExceptionCause actual expected = do
    let (cause, diagnosticContext) = T.breakOn "\n" actual
    cause `shouldBe` expected
    unless (T.null diagnosticContext) $
        diagnosticContext `shouldSatisfy` T.isPrefixOf "\nHasCallStack backtrace:\n"

shouldMatchDecision :: Decision -> Decision -> Expectation
shouldMatchDecision actual expected = case (actual, expected) of
    (Undecidable transience (RuleUnable name (RuleThrew detail)), Undecidable _ (RuleUnable _ (RuleThrew cause))) -> do
        Undecidable transience (RuleUnable name (RuleThrew cause)) `shouldBe` expected
        detail `shouldHaveExceptionCause` cause
    _ -> actual `shouldBe` expected

shouldReturnDecisions :: IO [Decision] -> [Decision] -> Expectation
shouldReturnDecisions evaluated expected = do
    actual <- evaluated
    length actual `shouldBe` length expected
    for_ (zip actual expected) (uncurry shouldMatchDecision)

-- | What a poisoned verdict throws.
thrownVerdict :: TestContractEscape
thrownVerdict = TestContractEscape "pure verdict"

-- | Verdicts that throw from each position the evaluator must force, so it refuses by the rule's name.
poisonedVerdicts :: [(String, RuleVerdict)]
poisonedVerdicts =
    [ ("the whole verdict", impureThrow thrownVerdict)
    , ("an allow reason", Allow (impureThrow thrownVerdict))
    , ("a deny reason", Deny Nothing (impureThrow thrownVerdict))
    , ("an abstention reason", NoDecision (impureThrow thrownVerdict))
    , ("an inability", CannotVet FailNoDecision (impureThrow thrownVerdict))
    , ("an advisory generation", Deny (Just (impureThrow thrownVerdict)) FixesNoAdvisory)
    , ("a later advisory identifier", Allow (Remediates (mkAdvisoryIds ("GHSA-first" :| [impureThrow thrownVerdict]))))
    ]

-- | Rule capabilities whose advisory database is the given fake's rows.
depsWith :: [(Text, AdvisoryRange)] -> RuleDeps
depsWith rows = servingRuleDeps (DbEtag "etag-1") (fakeCveLookup rows)

-- | Rule capabilities with a database configured and no generation loaded, as before the first sync.
unloadedDeps :: RuleDeps
unloadedDeps = inertRuleDeps{rdAdvisoryDatabase = staticAdvisoryDatabase (pure AdvisoryFresh) (\use -> use Nothing)}

-- | Rule capabilities with a database configured whose every lookup throws.
faultingDeps :: Text -> RuleDeps
faultingDeps detail = inertRuleDeps{rdAdvisoryDatabase = staticAdvisoryDatabase (pure AdvisoryFresh) (\_ -> throwIO (TestContractEscape detail))}

-- | An exact fix for @thing\@1.0.0@ with no other advisory affecting that version.
fixRows :: [(Text, AdvisoryRange)]
fixRows = [("thing", AdvisoryRange "GHSA-fixed-0001" Nothing (Just "0") (FixedBefore "1.0.0") Nothing)]

-- | A range covering @[0, 2.0.0)@ with the CVSS and EPSS scores under test.
affecting :: Maybe Double -> Maybe Double -> [(Text, AdvisoryRange)]
affecting severity epss = [("thing", AdvisoryRange "GHSA-affect-0001" severity (Just "0") (FixedBefore "2.0.0") epss)]

denyCveAt :: Double -> Rule
denyCveAt threshold = DenyIfCve (DenyIfCveParams threshold FailDeny)

denyEpssAt :: Double -> Rule
denyEpssAt threshold = DenyIfEpss (DenyIfEpssParams threshold FailDeny)

-- | The remediation rule with both choices set to abstain, as the fast lane decided before its policy choices.
skippingRemediation :: Rule
skippingRemediation = remediationWith FailNoDecision AbstainOnRemaining

-- | A partial fix of @thing\@1.0.0@ that loose limits admit, though every advisory deny blocks its remaining MAL advisory.
genLooseFixOfBlocked :: Gen ([AdvisoryRange], Rule)
genLooseFixOfBlocked = do
    fixedCvss <- genTenths
    remainingCvss <- (/ 10) . fromInteger <$> Gen.integral (Range.linear 70 100)
    remainingEpss <- (/ 100) . fromInteger <$> Gen.integral (Range.linear 50 99)
    alignment <- Gen.element [FailDeny, FailNoDecision]
    let rows =
            [ AdvisoryRange "GHSA-a" (Just fixedCvss) (Just "0") (FixedBefore "1.0.0") (Just 0.9)
            , AdvisoryRange "MAL-d" (Just remainingCvss) (Just "0") Unbounded (Just remainingEpss)
            ]
    pure (rows, remediationWith alignment (Differential (DifferentialLimits 10 1 NoGapRequired NoGapRequired)))

-- | A partial fix of @thing\@1.0.0@: a severe advisory it fixes, and a minor one that still affects it.
partialFixRows :: Double -> [(Text, AdvisoryRange)]
partialFixRows remainingCvss =
    [ ("thing", AdvisoryRange "GHSA-fixed-0001" (Just 9.8) (Just "0") (FixedBefore "1.0.0") (Just 0.5))
    , ("thing", AdvisoryRange "GHSA-open-0002" (Just remainingCvss) (Just "0.5.0") Unbounded (Just 0.005))
    ]

genScope :: Gen Text
genScope = Gen.text (Range.linear 1 12) Gen.alpha

genAgeDays :: Gen Integer
genAgeDays = Gen.integral (Range.linear 0 3650)

genPrecedence :: Gen Int
genPrecedence = Gen.int (Range.linear 0 1000)

-- | A rule that decides the scoped, old fixture package with install scripts.
genFiringRule :: Text -> Gen Rule
genFiringRule scopeTxt =
    Gen.element
        [ AllowScope (mkScope scopeTxt)
        , AllowIfOlderThan (7 * nominalDay)
        , DenyInstallTimeExecution
        ]

-- Compare decisions independently of the order in which abstention reasons arrived.
canonical :: Decision -> Decision
canonical (BlockedByDefault reasons) = BlockedByDefault (sortOn renderReason reasons)
canonical d = d

-- | Capabilities whose serving artifact was pushed the given age before 'now'.
pushedAgo :: NominalDiffTime -> RuleDeps -> RuleDeps
pushedAgo age =
    withAdvisoryFreshness (pure (assessAdvisoryAge sixDayLimit now (PublishedAt (addUTCTime (negate age) now))))

-- | Capabilities serving a generation the store gave no publication time for.
undated :: RuleDeps -> RuleDeps
undated = withAdvisoryFreshness (pure (assessAdvisoryAge sixDayLimit now UndatedGeneration))

-- | Capabilities whose serving artifact is three days past the six-day maximum.
expired :: RuleDeps -> RuleDeps
expired = pushedAgo (9 * nominalDay)

-- | Open the rule's breaker at 'now' to check that its age gate runs first.
openBreakerOn :: PreparedRule -> IO PreparedRule
openBreakerOn rule = case prepResilience rule of
    Nothing -> pure rule
    Just res -> do
        tripped <- newTVarIO (trippedBreaker (resConfig res))
        pure (mapResilience (\held -> held{resBreaker = tripped, resClock = pure now}) rule)

trippedBreaker :: EffectfulConfig -> Breaker
trippedBreaker cfg = foldl' recordOne initialBreaker [1 .. ecBreakerThreshold cfg]
  where
    recordOne breaker _ = case admit now breaker of
        (Just ticket, claimed) -> recordFailure (ecBreakerThreshold cfg) (ecBreakerCooldown cfg) now ticket claimed
        (Nothing, unchanged) -> unchanged

expirySpec :: Spec
expirySpec = describe "an expired advisory push" $ do
    for_ [DenyIfKnownMalicious FailNoDecision, DenyIfCve (DenyIfCveParams 8.0 FailNoDecision), DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision)] $ \rule ->
        it (toString (ruleName rule <> " refuses under onUnavailable: skip, past a lower age allow")) $
            decideWith
                (expired (depsWith (affecting (Just 9.8) (Just 0.9))))
                (map atDefaultPrecedence [rule, AllowIfOlderThan (7 * nominalDay)])
                (pkg Nothing 99)
                >>= (`shouldSatisfy` isUndecidable)

    for_ [DenyIfKnownMalicious FailNoDecision, DenyIfCve (DenyIfCveParams 8.0 FailNoDecision), DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision)] $ \rule ->
        it (toString (ruleName rule <> " refuses on a serving generation with no publication time")) $
            decideWith
                (undated (depsWith (affecting (Just 9.8) (Just 0.9))))
                (map atDefaultPrecedence [rule, AllowIfOlderThan (7 * nominalDay)])
                (pkg Nothing 99)
                >>= (`shouldSatisfy` isUndecidable)

    for_ [(FailNoDecision, "abstains", isBlockedByDefault), (FailDeny, "refuses", isUndecidable)] $ \(alignment, outcome, expected) -> do
        it ("the remediation allow " <> outcome <> " under onUnavailable " <> show alignment <> " when the push carries no publication time") $
            decideWith (undated (depsWith fixRows)) [atDefaultPrecedence (differentialRemediation alignment)] (pkg Nothing 0)
                >>= (`shouldSatisfy` expected)
        it ("the remediation allow " <> outcome <> " under onUnavailable " <> show alignment <> " rather than admitting on expired evidence") $
            decideWith (expired (depsWith fixRows)) [atDefaultPrecedence (differentialRemediation alignment)] (pkg Nothing 0)
                >>= (`shouldSatisfy` expected)

    it "is eligible at an age equal to the maximum" $
        decideWith
            (pushedAgo (6 * nominalDay) (depsWith (affecting (Just 9.8) Nothing)))
            [atDefaultPrecedence (denyCveAt 8.0)]
            (pkg Nothing 0)
            >>= \d -> blockedBy d `shouldBe` Just "DenyIfCve"

    it "expires on the clock alone, with the same generation still serving" $ do
        let deps = depsWith (affecting (Just 9.8) Nothing)
            policy = [atDefaultPrecedence (denyCveAt 8.0)]
        decideWith (pushedAgo (5 * nominalDay) deps) policy (pkg Nothing 0)
            >>= \d -> blockedBy d `shouldBe` Just "DenyIfCve"
        decideWith (pushedAgo (7 * nominalDay) deps) policy (pkg Nothing 0)
            >>= (`shouldSatisfy` isUndecidable)

    it "leaves a higher-precedence identity allow to decide" $
        decideWith
            (expired (depsWith (affecting (Just 9.8) Nothing)))
            (map atDefaultPrecedence [AllowByIdentity "thing@1.0.0", denyCveAt 8.0])
            (pkg Nothing 0)
            >>= \d -> admittedBy d `shouldBe` Just "AllowByIdentity"

    it "skips a remediation allow ranked above the deny, so the deny's refusal takes effect" $
        decideWith
            (expired (depsWith fixRows))
            [at 300 skippingRemediation, atDefaultPrecedence (denyCveAt 8.0)]
            (pkg Nothing 0)
            >>= (`shouldSatisfy` isUndecidable)

    it "keeps same-precedence ordering, so the earlier name reports the refusal" $ do
        decision <-
            decideWith
                (expired (depsWith (affecting (Just 9.8) (Just 0.9))))
                (map atDefaultPrecedence [denyEpssAt 0.5, denyCveAt 8.0])
                (pkg Nothing 0)
        decision `shouldSatisfy` isUndecidable
        sentences decision `shouldSatisfy` all (T.isPrefixOf "DenyIfCve: ")

    it "refuses ahead of an already-open breaker, which would otherwise skip the rule" $ do
        prepared <-
            prepare
                (expired (depsWith (affecting (Just 9.8) Nothing)))
                [atDefaultPrecedence (DenyIfCve (DenyIfCveParams 8.0 FailNoDecision))]
        opened <- traverse openBreakerOn prepared
        decision <- evalRules ctx opened (pkg Nothing 0)
        decision `shouldSatisfy` isUndecidable
        sentences decision `shouldSatisfy` all (T.isInfixOf "past the maximum")

-- | Capabilities whose source reports collect in the returned ref, newest first.
observedDeps :: RuleDeps -> IO (RuleDeps, IORef [SourceHealth])
observedDeps deps = do
    captured <- newIORef []
    pure (deps{rdSourceReporter = noSourceReporter{reportSource = \h -> modifyIORef' captured (h :)}}, captured)

-- | The reports so far, oldest first.
reported :: IORef [SourceHealth] -> IO [SourceHealth]
reported = fmap reverse . readIORef

-- | The issue's reproduction: the shipped quarantine beside an opt-in advisory deny set to skip.
skipPolicy :: [PrecededRule]
skipPolicy = map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), DenyIfCve (DenyIfCveParams 8.0 FailNoDecision)]

evidenceSpec :: Spec
evidenceSpec = describe "skipped-check evidence on an admission" $ do
    it "keeps the deny check the admission skipped for want of an advisory database" $ do
        -- The age rule admits an old version while DenyIfCve, set to skip, could not vet it. The
        -- admission says so, rather than reading as if every configured check passed.
        decision <- decideWith inertRuleDeps skipPolicy (pkg Nothing 30)
        admittedBy decision `shouldBe` Just "AllowIfOlderThan"
        skippedChecks decision `shouldBe` [SkippedUnavailable "DenyIfCve" NoDatabaseLoaded]
        renderDecision (pkg Nothing 30) decision
            `shouldSatisfy` T.isSuffixOf "(skipped for unavailability: DenyIfCve (no advisory database loaded))"

    it "keeps a skipped lookup fault, with the generic reason the client also sees" $ do
        decision <- decideWith (faultingDeps "advisory database exploded") skipPolicy (pkg Nothing 30)
        skippedChecks decision `shouldBe` [SkippedUnavailable "DenyIfCve" EvaluationFailed]
        renderDecision (pkg Nothing 30) decision
            `shouldSatisfy` T.isSuffixOf "(skipped for unavailability: DenyIfCve (the rule could not be evaluated))"

    it "keeps a skip behind an open breaker" $ do
        prepared <- prepare (faultingDeps "down") skipPolicy
        opened <- traverse openBreakerOn prepared
        decision <- evalRules ctx opened (pkg Nothing 30)
        skippedChecks decision `shouldBe` [SkippedUnavailable "DenyIfCve" SourceBreakerOpen]
        renderDecision (pkg Nothing 30) decision
            `shouldSatisfy` T.isSuffixOf "(skipped for unavailability: DenyIfCve (the rule source circuit breaker is open))"

    it "keeps the remediation check an expired push skipped under onUnavailable: skip" $ do
        decision <- decideWith (expired (depsWith fixRows)) (map atDefaultPrecedence [differentialRemediation FailNoDecision, AllowIfOlderThan (7 * nominalDay)]) (pkg Nothing 30)
        admittedBy decision `shouldBe` Just "AllowIfOlderThan"
        case skippedChecks decision of
            [SkippedUnavailable "AllowIfRemediatesCve" cause] -> renderInability cause `shouldSatisfy` T.isInfixOf "past the maximum"
            other -> expectationFailure ("expected one skipped check, got " <> show other)

    it "carries no evidence once the database answers, so a later allow reads clean" $ do
        decision <- decideWith (depsWith []) skipPolicy (pkg Nothing 30)
        admittedBy decision `shouldBe` Just "AllowIfOlderThan"
        skippedChecks decision `shouldBe` []

    it "records a check the winning allow pre-empted as unreached, never as passed or skipped" $ do
        -- An operator ranking the deny below the quarantine allow made the choice, so the record
        -- says the check never ran rather than claiming an unavailability.
        decision <- decideWith inertRuleDeps [at 50 (DenyIfCve (DenyIfCveParams 8.0 FailNoDecision)), atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))] (pkg Nothing 30)
        skippedChecks decision `shouldBe` [Unreached "DenyIfCve"]

    it "lists skipped checks in boot order ahead of the unreached ones" $ do
        decision <-
            decideWith
                inertRuleDeps
                (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision), AllowScope (mkScope "myorg"), DenyIfCve (DenyIfCveParams 8.0 FailNoDecision), shippedRemediation])
                (pkg (Just "myorg") 30)
        admittedBy decision `shouldBe` Just "AllowScope"
        skippedChecks decision
            `shouldBe` [ SkippedUnavailable "DenyIfCve" NoDatabaseLoaded
                       , SkippedUnavailable "DenyIfEpss" NoDatabaseLoaded
                       , Unreached "AllowIfRemediatesCve"
                       , Unreached "AllowIfOlderThan"
                       ]

    it "attaches no evidence to a refusal: a fail-closed inability is decisive, not skipped" $ do
        decision <- decideWith inertRuleDeps (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), denyCveAt 8.0]) (pkg Nothing 30)
        decision `shouldSatisfy` isUndecidable
        skippedChecks decision `shouldBe` []

    it "keeps the remediation check skipped before the first sync under onUnavailable: skip" $ do
        decision <- decideWith unloadedDeps (map atDefaultPrecedence [differentialRemediation FailNoDecision, AllowIfOlderThan (7 * nominalDay)]) (pkg Nothing 30)
        admittedBy decision `shouldBe` Just "AllowIfOlderThan"
        skippedChecks decision `shouldBe` [SkippedUnavailable "AllowIfRemediatesCve" NoDatabaseLoaded]

    it "does not turn an unavailable check into an admission by itself" $
        -- Nothing else admits a young version, so the skipped deny leaves deny-by-default standing.
        decideWith inertRuleDeps skipPolicy (pkg Nothing 1) >>= (`shouldSatisfy` isBlockedByDefault)

sourceHealthSpec :: Spec
sourceHealthSpec = describe "advisory source health reporting" $ do
    for_ [FailDeny, FailNoDecision] $ \alignment ->
        for_ [DenyIfCve (DenyIfCveParams 8 alignment), DenyIfKnownMalicious alignment, differentialRemediation alignment] $ \rule ->
            describe (toString (ruleName rule) <> " under " <> show alignment) $ do
                let name = ruleName rule
                    policy = map atDefaultPrecedence [rule, AllowIfOlderThan (7 * nominalDay)]
                    malRows = [("thing", AdvisoryRange "MAL-health-control" (Just 9.8) (Just "0") (FixedBefore "2.0.0") Nothing)]
                it "reports an absent database as unavailable before and after source configuration" $
                    for_ [inertRuleDeps, unloadedDeps] $ \absent -> do
                        (deps, reports) <- observedDeps absent
                        void (decideWith deps policy (pkg Nothing 30))
                        reported reports `shouldReturn` [SourceUnavailable name NoDatabaseLoaded]
                it "reports an answered read when no advisory matches" $ do
                    (deps, reports) <- observedDeps (depsWith [])
                    decision <- decideWith deps policy (pkg Nothing 30)
                    admittedBy decision `shouldBe` Just "AllowIfOlderThan"
                    reported reports `shouldReturn` [SourceAnswered name]
                when (ruleDenies rule) $
                    it "reports an answered read when the named MAL control denies" $ do
                        (deps, reports) <- observedDeps (depsWith malRows)
                        decision <- decideWith deps policy (pkg Nothing 30)
                        blockedBy decision `shouldBe` Just name
                        reported reports `shouldReturn` [SourceAnswered name]
                it "reports an expired push as unavailable with its cause" $ do
                    (deps, reports) <- observedDeps (expired (depsWith malRows))
                    void (decideWith deps policy (pkg Nothing 30))
                    reported reports >>= \case
                        [SourceUnavailable reportedName cause] -> do
                            reportedName `shouldBe` name
                            renderInability cause `shouldSatisfy` T.isInfixOf "past the maximum"
                        other -> expectationFailure ("expected one unavailability, got " <> show other)
                it "reports a lookup fault once with its detail" $ do
                    (deps, reports) <- observedDeps (faultingDeps "advisory database exploded")
                    void (decideWith deps policy (pkg Nothing 30))
                    reported reports >>= \case
                        [SourceUnavailable reportedName detail] -> do
                            reportedName `shouldBe` name
                            renderInability detail `shouldSatisfy` T.isInfixOf "advisory database exploded"
                        other -> expectationFailure ("expected one unavailability, got " <> show other)
                it "reports an open breaker once and applies its alignment to every version" $ do
                    (deps, reports) <- observedDeps (depsWith malRows)
                    prepared <- prepare deps policy
                    opened <- traverse openBreakerOn prepared
                    decideVersion <- newEvaluator ctx opened
                    decisions <- traverse decideVersion [(pkg Nothing 30){evVersion = mkVersion Npm version} | version <- ["1.0.0", "1.1.0"]]
                    for_ decisions $ \decision -> case alignment of
                        FailDeny -> decision `shouldSatisfy` isUndecidable
                        FailNoDecision -> do
                            admittedBy decision `shouldBe` Just "AllowIfOlderThan"
                            skippedChecks decision `shouldBe` [SkippedUnavailable name SourceBreakerOpen]
                    reported reports `shouldReturn` [SourceUnavailable name SourceBreakerOpen]

    it "reports nothing for a pure rule, which reads no source" $ do
        (deps, reports) <- observedDeps inertRuleDeps
        void (decideWith deps (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), DenyInstallTimeExecution]) (pkg Nothing 30))
        reported reports `shouldReturn` []

    it "reports the remediation allow answered once its rows load" $ do
        (deps, reports) <- observedDeps (depsWith fixRows)
        decision <- decideWith deps [atDefaultPrecedence shippedRemediation] (pkg Nothing 0)
        admittedBy decision `shouldBe` Just "AllowIfRemediatesCve"
        reported reports `shouldReturn` [SourceAnswered "AllowIfRemediatesCve"]

-- | Exact verdicts and sentences shared by unconfigured databases and databases awaiting their first sync.
noDatabaseVerdicts :: [(Text, Rule, RuleVerdict, Text)]
noDatabaseVerdicts =
    [ ("AllowIfRemediatesCve set to deny", shippedRemediation, CannotVet FailDeny NoDatabaseLoaded, "AllowIfRemediatesCve: no advisory database loaded")
    , ("AllowIfRemediatesCve set to skip", differentialRemediation FailNoDecision, CannotVet FailNoDecision NoDatabaseLoaded, "AllowIfRemediatesCve: no advisory database loaded")
    , ("DenyIfCve set to deny", denyCveAt 8.0, CannotVet FailDeny NoDatabaseLoaded, "DenyIfCve: no advisory database loaded")
    , ("DenyIfCve set to skip", DenyIfCve (DenyIfCveParams 8.0 FailNoDecision), CannotVet FailNoDecision NoDatabaseLoaded, "DenyIfCve: no advisory database loaded")
    , ("DenyIfEpss set to deny", denyEpssAt 0.5, CannotVet FailDeny NoDatabaseLoaded, "DenyIfEpss: no advisory database loaded")
    , ("DenyIfEpss set to skip", DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision), CannotVet FailNoDecision NoDatabaseLoaded, "DenyIfEpss: no advisory database loaded")
    ]

-- | The decision a policy of one rule reaches from that rule's verdict.
soleDecision :: Text -> RuleVerdict -> Decision
soleDecision name = \case
    Allow reason -> Admitted name reason []
    Deny etag reason -> Blocked name etag reason
    NoDecision reason -> BlockedByDefault [reason]
    CannotVet FailDeny why -> Undecidable (WillResolve Nothing) (RuleUnable name why)
    CannotVet FailNoDecision why -> BlockedByDefault [RuleUnable name why]

noDatabaseSpec :: Spec
noDatabaseSpec = describe "an advisory rule with no database configured" $ do
    it "is prepared to run directly, with no timeout, retry, or breaker" $ do
        rules <- prepare inertRuleDeps [atDefaultPrecedence rule | (_, rule, _, _) <- noDatabaseVerdicts]
        map (isJust . prepResilience) rules `shouldBe` (False <$ noDatabaseVerdicts)

    for_ noDatabaseVerdicts $ \(label, rule, verdict, sentence) ->
        it (toString (label <> " keeps the verdict it reaches before the first sync")) $
            for_ [inertRuleDeps, unloadedDeps] $ \deps -> do
                evalRule deps ctx rule (pkg Nothing 0) `shouldReturn` verdict
                decision <- decideWith deps [atDefaultPrecedence rule] (pkg Nothing 0)
                decision `shouldBe` soleDecision (ruleName rule) verdict
                sentences decision `shouldBe` [sentence]

    it "decides the shipped policy as it does before the first sync" $ do
        let shipped = map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), shippedRemediation]
        for_ [pkg Nothing 1, pkg Nothing 30] $ \ev -> do
            unconfigured <- decideWith inertRuleDeps shipped ev
            decideWith unloadedDeps shipped ev >>= (`shouldBe` unconfigured)

-- | Reference verdict from per-version SQL fix probes and repeated range parsing.
perVersionVerdict :: DbEtag -> (Text -> Text -> IO Bool) -> CveLookup -> Rule -> RuleEvidence -> IO RuleVerdict
perVersionVerdict etag probe cve rule ev = case rule of
    AllowIfRemediatesCve params ->
        probe name version >>= \case
            False -> pure (NoDecision FixesNoAdvisory)
            True -> referenceRemediation (aircRemaining params) ((== FixedBefore version) . arUpperBound) (referenceInside eco version) <$> cveAdvisoriesFor cve name
    DenyIfCve params -> deny DenyMissingScore Cvss (dicMinCvss params) arSeverity <$> cveAdvisoriesFor cve name
    DenyIfKnownMalicious _ -> denyMal <$> cveAdvisoriesFor cve name
    DenyIfEpss params -> deny AbstainMissingScore Epss (dieMinEpss params) arEpss <$> cveAdvisoriesFor cve name
    other -> fail ("not an advisory rule: " <> show other)
  where
    eco = pkgEcosystem (evName ev)
    name = TS.toText (pkgCanonical (evName ev))
    version = renderVersion (evVersion ev)
    denyMal ranges =
        case mkAdvisoryIds <$> nonEmpty (ordNub [arCveId ar | ar <- ranges, referenceInside eco version ar, Just suffix <- [T.stripPrefix "MAL-" (arCveId ar)], not (T.null suffix)]) of
            Nothing -> NoDecision NoKnownMaliciousAdvisory
            Just ids -> Deny (Just etag) (KnownMalicious ids)
    deny missing score threshold scoreOf ranges =
        case mkAdvisoryIds <$> nonEmpty (ordNub [arCveId ar | ar <- ranges, referenceInside eco version ar, scoreAtLeast missing threshold (scoreOf ar)]) of
            Nothing -> NoDecision (NotAffectedAtThreshold score)
            Just ids -> Deny (Just etag) (AffectedBy score threshold ids)

-- | The exact @fixed_version@ match the reference probes with, in SQL on the artifact.
sqlFixProbe :: Connection -> Text -> Text -> IO Bool
sqlFixProbe conn name version =
    not . null <$> (query conn "SELECT 1 FROM package_vulnerability_ranges WHERE package_name = ? AND fixed_version = ? LIMIT 1" (name, version) :: IO [Only Int])

-- | Each version's verdict from the one read a prepared advisory rule makes for its package.
perRequestVerdicts :: RuleDeps -> Rule -> PackageName -> [RuleEvidence] -> IO [RuleVerdict]
perRequestVerdicts deps rule name versions =
    prepare deps [atDefaultPrecedence rule] >>= \case
        [PreparedRule{prepEval = PerPackage packageRead}] -> prRead packageRead name (\_ rows -> (\loaded -> map (prVerdict packageRead loaded) versions) <$> rows)
        _ -> fail "expected one prepared package read"

-- | Every advisory rule, at thresholds and limits on, between, and past the fixtures' scores.
referenceParityRules :: [Rule]
referenceParityRules =
    DenyIfKnownMalicious FailDeny
        : [remediationWith FailNoDecision remaining | remaining <- AbstainOnRemaining : map Differential parityLimits]
            <> [DenyIfCve (DenyIfCveParams threshold alignment) | threshold <- [0, 5.0, 7.0, 9.9], alignment <- [FailDeny, FailNoDecision]]
            <> [DenyIfEpss (DenyIfEpssParams threshold FailDeny) | threshold <- [0, 0.25, 0.5, 0.95, 1]]
  where
    parityLimits =
        [ defaultDifferentialLimits
        , DifferentialLimits 10 1 NoGapRequired NoGapRequired
        , DifferentialLimits 5.0 0.5 (GapOfAtLeast 0.5) (GapOfAtLeast 0.1)
        ]

-- | Ranges, points and version spelling variants, including unparseable bounds and missing scores.
edgeCaseRows :: [RangeRow]
edgeCaseRows =
    [ ("range-pkg", "GHSA-r-0001", Just "1.0.0", Just "1.2.0", Nothing, Just 9.8, Just 0.9)
    , ("range-pkg", "GHSA-r-0001", Just "1.5.0", Just "1.6.0", Nothing, Just 9.8, Just 0.9)
    , ("range-pkg", "GHSA-r-0002", Nothing, Just "2.0.0", Nothing, Just 5.0, Just 0.1)
    , ("range-pkg", "GHSA-r-0003", Just "0", Just "1.2.0", Nothing, Just 7.0, Just 0.5)
    , ("range-pkg", "GHSA-r-0004", Just "2.1.0", Nothing, Just "2.3.0", Nothing, Nothing)
    , ("range-pkg", "GHSA-r-0005", Just "3.0.0", Just "3.0.1", Just "3.0.0", Just 8.0, Just 0.7)
    , ("range-pkg", "MAL-r-0006", Just "4.0.0", Nothing, Just "4.0.0", Nothing, Nothing)
    , ("range-pkg", "MAL-r-0007", Just "weird", Nothing, Just "weird", Nothing, Nothing)
    , ("range-pkg", "GHSA-r-0008", Just "5.0.0-alpha.1", Just "5.0.0-rc.1", Nothing, Just 6.9, Nothing)
    , ("fix-pkg", "GHSA-f-0001", Nothing, Just "1.0.0", Nothing, Just 9.8, Just 0.95)
    , ("fix-pkg", "GHSA-f-0002", Nothing, Just "v2.0.0", Nothing, Just 4.0, Nothing)
    , ("fix-pkg", "GHSA-f-0003", Nothing, Just "3.0.0+build.7", Nothing, Nothing, Just 0.2)
    , ("fix-pkg", "GHSA-f-0004", Just "4.0.0", Just "not.a.version", Nothing, Just 9.0, Nothing)
    , ("fix-pkg", "GHSA-f-0005", Just "1.0.0", Just "1.0.0", Nothing, Just 2.0, Just 0.01)
    , ("partial-pkg", "GHSA-p-0001", Nothing, Just "2.0.0", Nothing, Just 9.8, Just 0.5)
    , ("partial-pkg", "GHSA-p-0002", Just "1.5.0", Just "3.0.0", Nothing, Just 4.2, Just 0.005)
    , ("unfixed-pkg", "GHSA-u-0001", Just "1.0.0", Nothing, Nothing, Just 10.0, Just 0.5)
    , ("@scope/pkg", "GHSA-s-0001", Nothing, Just "1.0.0", Nothing, Just 3.9, Just 0.25)
    , ("bound-pkg", "GHSA-b-0001", Just "garbage", Just "6.0.0", Nothing, Just 9.0, Just 0.6)
    , ("bound-pkg", "GHSA-b-0001", Just "10.0.0", Just "10.2.0", Nothing, Just 9.0, Just 0.6)
    , ("bound-pkg", "GHSA-b-0002", Just "6.1.0", Nothing, Just "bogus", Just 8.0, Just 0.3)
    , ("bound-pkg", "GHSA-b-0003", Just "v7.0.0", Just "v7.1.0", Nothing, Just 7.5, Just 0.55)
    , ("bound-pkg", "MAL-b-0004", Just "v8.0.0", Nothing, Just "v8.0.0", Nothing, Nothing)
    , ("bound-pkg", "GHSA-b-0005", Just "9.0.0-beta.2", Nothing, Just "9.0.0+meta.1", Just 6.0, Just 0.8)
    , ("bound-pkg", "GHSA-b-0006", Just "9.5.0+meta", Nothing, Just "9.5.0+meta", Just 9.9, Nothing)
    , ("open-pkg", "MAL-o-0001", Nothing, Nothing, Nothing, Nothing, Nothing)
    ]

-- | Versions around every fixture bound, plus spellings only an exact text match tells apart.
spreadVersions :: [Text]
spreadVersions =
    [ "0"
    , "0.0.1"
    , "0.9.9"
    , "1.0.0"
    , "1.0.1"
    , "1.1.0"
    , "1.2.0"
    , "1.2.1"
    , "1.5.0"
    , "1.5.9"
    , "1.6.0"
    , "2.0.0"
    , "v2.0.0"
    , "2.0.1"
    , "2.2.0"
    , "2.3.0"
    , "2.3.1"
    , "2.5.0"
    , "3.0.0"
    , "3.0.1"
    , "3.0.0+build.7"
    , "3.9.9"
    , "4.0.0"
    , "4.0.1"
    , "5.0.0-alpha.1"
    , "5.0.0-beta"
    , "5.0.0-rc.1"
    , "5.0.0"
    , "weird"
    , "not.a.version"
    , "v1.0.0"
    , "1.0.0+build.1"
    , "6.0.0"
    , "6.5.0"
    , "7.0.5"
    , "v7.0.5"
    , "8.0.0"
    , "9.0.0-beta.1"
    , "9.0.0"
    , "9.0.1"
    , "9.5.0"
    , "9.5.0+other"
    , "10.0.0"
    , "10.1.0"
    ]

-- | The versions to decide for one package: the spread, and every bound its rows carry.
versionsFor :: CveLookup -> PackageName -> IO [Text]
versionsFor cve name = do
    ranges <- cveAdvisoriesFor cve (TS.toText (pkgCanonical name))
    pure (ordNub (spreadVersions <> concatMap bounds ranges))
  where
    bounds ar =
        maybeToList (arIntroduced ar) <> case arUpperBound ar of
            FixedBefore fixed -> [fixed]
            LastAffected lastAffected -> [lastAffected]
            Unbounded -> []

-- | Compare shared reads against per-version reads and evaluators, returning the checked verdicts.
agreesOn :: Ecosystem -> (Text -> Text -> IO Bool) -> CveLookup -> [PackageName] -> IO [RuleVerdict]
agreesOn eco probe cve names =
    fmap (concat . concat) . forM (names <> [mkPackageName eco Nothing "no-such-package"]) $ \name -> do
        -- A version built under another ecosystem's grammar reaches the matcher's re-parse.
        versions <- (\vs -> [(v, builtUnder) | v <- ordNub (map (servedSpelling eco) vs), builtUnder <- universe]) <$> versionsFor cve name
        let evidence = [completeEvidence (sampleDetails name (mkVersion builtUnder v)) | (v, builtUnder) <- versions]
        compared <- forM referenceParityRules $ \rule -> do
            perVersion <- traverse (perVersionVerdict parityEtag probe cve rule) evidence
            perRequest <- perRequestVerdicts deps rule name evidence
            zip versions perRequest `shouldBe` zip versions perVersion
            pure perVersion
        rules <- prepare deps (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), shippedRemediation, denyCveAt 7.0, denyEpssAt 0.5])
        shared <- newEvaluator ctx rules >>= \decideShared -> traverse decideShared evidence
        fresh <- traverse (evalRules ctx rules) evidence
        zip versions shared `shouldBe` zip versions fresh
        pure compared
  where
    deps = servingRuleDeps parityEtag cve

parityEtag :: DbEtag
parityEtag = DbEtag "reference-parity"

-- | A version as each ecosystem's projection spells it: canonical PEP 440 for PyPI, the published text for npm.
servedSpelling :: Ecosystem -> Text -> Text
servedSpelling = \case
    PyPI -> \raw -> maybe raw renderVersion (canonicalPep440 raw)
    _ -> id

-- | One hand-built row as a compiled PyPI artifact holds it, each bound in canonical PEP 440 spelling.
pypiRange :: RangeRow -> (Text, AdvisoryRange)
pypiRange (package, advisory, introduced, fixed, lastAffected, cvss, epss) =
    (package, toRange (advisory, spelled <$> introduced, spelled <$> fixed, spelled <$> lastAffected, cvss, epss))
  where
    spelled = servedSpelling PyPI

-- | Open an accepted artifact and lend the reference's fix probe and the lookup, then close both.
withOpened :: FilePath -> ((Text -> Text -> IO Bool) -> CveLookup -> IO a) -> IO a
withOpened path use =
    openCveDb Npm EpssOptional path >>= \case
        Left rejection -> fail ("reference-parity artifact rejected: " <> show rejection)
        Right db -> withConnection path (\conn -> use (sqlFixProbe conn) (cveDbLookup db)) `finally` cveDbClose db

-- | The names an artifact holds rows for, as npm packages.
coveredPackages :: CveLookup -> IO [PackageName]
coveredPackages cve = map unscopedNpm <$> cveCoveredNames cve

-- | The verdicts reach an admission, a denial, a still-affected fix, both differential outcomes, and a version no advisory fixes.
reachesEveryOutcome :: [RuleVerdict] -> Expectation
reachesEveryOutcome verdicts = do
    verdicts `shouldSatisfy` any isAllow
    verdicts `shouldSatisfy` any (\case Allow RemediatesWithinLimits{} -> True; _ -> False)
    verdicts `shouldSatisfy` any (\case NoDecision RemediationBeyondLimits{} -> True; _ -> False)
    verdicts `shouldSatisfy` any isDeny
    verdicts `shouldSatisfy` any (\case NoDecision FixesButStillAffected{} -> True; _ -> False)
    verdicts `shouldSatisfy` elem (NoDecision FixesNoAdvisory)
    map verdictSentence verdicts `shouldSatisfy` elem "no advisory names this version as its fix"

referenceParitySpec :: Spec
referenceParitySpec = describe "one read per request against a read per version, over a real artifact" $ do
    it "agrees on hand-built rows covering every bound shape and fix spelling" $
        withSystemTempDirectory "ecluse-rules-reference-parity" $ \dir -> do
            let path = dir </> "osv.db"
            mkValidDbWithRows path [] edgeCaseRows
            verdicts <- withOpened path $ \probe cve -> do
                names <- coveredPackages cve
                agreesOn Npm probe cve (scopedNpm "scope" "pkg" : names)
            reachesEveryOutcome verdicts

    it "agrees on the hand-built rows under PyPI names, each bound and version spelled as PyPI serves it" $ do
        let cve = fakeCveLookup [pypiRange row | row@(package, _, _, _, _, _, _) <- edgeCaseRows, not ("@" `T.isPrefixOf` package)]
        names <- map unscopedPyPI <$> cveCoveredNames cve
        agreesOn PyPI (namesFix cve) cve names >>= reachesEveryOutcome

    it "agrees on the compiled corpus" $
        withFixtureOsvDb CorpusV2 $
            \path -> withOpened path $ \probe cve -> void (coveredPackages cve >>= agreesOn Npm probe cve)

    for_ [("active", Nothing), ("withdrawn", Just (String "2024-05-14T20:15:44Z"))] $ \(label, withdrawn) ->
        it ("agrees on a compiled archive whose advisory is " <> label) $ do
            archive <- withdrawalZip withdrawn
            withOsvZipDb Npm archive $ \path ->
                withOpened path (\probe cve -> void (agreesOn Npm probe cve (map unscopedNpm ["withdrawal-only", "withdrawal-overlap", "corpus-vuln"])))

-- | One evaluator under comparison: how it prepares a policy, and the decider it builds per request.
data Evaluator = Evaluator (RuleDeps -> [PrecededRule] -> IO [PreparedRule]) (EvalContext -> [PreparedRule] -> IO (RuleEvidence -> IO Decision))

liveEvaluator, frozen :: Evaluator
liveEvaluator = Evaluator prepare newEvaluator
frozen = Evaluator frozenPrepare frozenEvaluator

-- | What one request did: each version's outcome, the packages its shared reads read, and its source reports, in order.
data Observed = Observed
    { obOutcomes :: [Outcome]
    , obReads :: [Either Text PackageName]
    , obReports :: [SourceHealth]
    }
    deriving stock (Eq, Show)

-- | Decide versions through a fresh evaluator. Retries are off, so a faulting read returns at once.
observe :: Evaluator -> ((RuleEvidence -> IO Outcome) -> [RuleEvidence] -> IO [Outcome]) -> RuleDeps -> [PrecededRule] -> ([PreparedRule] -> [PreparedRule]) -> [RuleEvidence] -> IO Observed
observe (Evaluator prepareWith evaluatorWith) decideAll deps policy adjust evidence = do
    readPackages <- newIORef []
    reports <- newIORef []
    let reporting = deps{rdSourceReporter = noSourceReporter{reportSource = \health -> atomicModifyIORef'_ reports (health :)}}
    rules <- adjust . map (recordReads readPackages . withoutRetries) <$> prepareWith reporting policy
    outcomes <- evaluatorWith ctx rules >>= \decideVersion -> decideAll (settle decideVersion) evidence
    readOrder <- reverse <$> readIORef readPackages
    reportOrder <- reverse <$> readIORef reports
    pure (Observed outcomes readOrder (map healthCause reportOrder))

-- | One version's outcome: its decision, a throw that left the evaluator, or a throw the decision hides from a reader.
data Outcome = Reached Decision | Escaped Text | Hidden Text
    deriving stock (Eq, Show)

-- | Decide one version, then force the whole decision as a reader that renders it would.
settle :: (RuleEvidence -> IO Decision) -> RuleEvidence -> IO Outcome
settle decideVersion ev =
    tryAny (decideVersion ev) >>= \case
        Left escaped -> pure (Escaped (causeLine escaped))
        Right decision ->
            tryAny (evaluate (length (show @String decision))) <&> \case
                Left hidden -> Hidden (causeLine hidden)
                Right _ -> Reached (decisionCause decision)

-- Yielding mid-read lets concurrent callers reach a held cell's lock while the read holds it. A
-- poisoned record's name is recorded as its cause, without moving where the read forces it.
recordReads :: IORef [Either Text PackageName] -> PreparedRule -> PreparedRule
recordReads readPackages = mapPackageRead $ \packageRead ->
    packageRead
        { prRead = \package use -> do
            named <- first causeLine <$> tryAny (evaluate package)
            atomicModifyIORef'_ readPackages (named :) >> yield >> prRead packageRead package use
        }

-- | How many versions reach an advisory rule, each decided alone by a fresh frozen evaluator.
reachingAdvisoryRules :: RuleDeps -> [PrecededRule] -> [RuleEvidence] -> IO Int
reachingAdvisoryRules deps policy = fmap (length . filter (not . null . obReads)) . traverse (observe frozen traverse deps policy id . one)

withoutRetries :: PreparedRule -> PreparedRule
withoutRetries = mapResilience (\res -> res{resConfig = (resConfig res){ecBackoff = []}})

-- GHC can append a runtime backtrace after the exception's own diagnostic line.
causeOf :: Text -> Text
causeOf = fst . T.breakOn "\n"

causeLine :: SomeException -> Text
causeLine = causeOf . displayExceptionT

decisionCause :: Decision -> Decision
decisionCause = \case
    Undecidable transience (RuleUnable name (RuleThrew detail)) -> Undecidable transience (RuleUnable name (RuleThrew (causeOf detail)))
    other -> other

healthCause :: SourceHealth -> SourceHealth
healthCause = \case
    SourceUnavailable name (RuleThrew detail) -> SourceUnavailable name (RuleThrew (causeOf detail))
    other -> other

-- | Which prepared rule throws, for which release, and from which of 'poisonedVerdicts'.
data Poison = Poison Int Text Int
    deriving stock (Show)

poisonAt :: Ecosystem -> Maybe Poison -> [PreparedRule] -> [PreparedRule]
poisonAt eco poison rules = case poison of
    Nothing -> rules
    Just (Poison index release position) ->
        let target = index `mod` max 1 (length rules)
            poisoned = maybe id (poisonRule (mkVersion eco release) . snd) (poisonedVerdicts !!? position)
         in zipWith (\i rule -> if i == target then poisoned rule else rule) [0 ..] rules

-- | The rule returns the poisoned verdict for one version and decides every other version as before.
poisonRule :: Version -> RuleVerdict -> PreparedRule -> PreparedRule
poisonRule version verdict rule = case prepEval rule of
    PerVersion eval -> rule{prepEval = PerVersion (\c ev -> if evVersion ev == version then pure verdict else eval c ev)}
    PerPackage packageRead -> rule{prepEval = PerPackage packageRead{prVerdict = \rows ev -> if evVersion ev == version then verdict else prVerdict packageRead rows ev}}

-- | A per-version rule at a precedence that returns one of 'poisonedVerdicts' for a release and abstains on every other.
data Bomb = Bomb Int Text Int
    deriving stock (Show)

bombRule :: Ecosystem -> Bomb -> PreparedRule
bombRule eco (Bomb prec release position) =
    maybe id (poisonRule (mkVersion eco release) . snd) (poisonedVerdicts !!? position) (constRule "PureBomb" (NoDecision FixesNoAdvisory)){prepPrecedence = prec}

genBomb :: [Text] -> Gen Bomb
genBomb pool = Bomb <$> genPrecedence <*> Gen.element pool <*> Gen.int (Range.constant 0 (length poisonedVerdicts - 1))

-- | Which listed version's evidence throws: its publish time, which only the quarantine reads, or the whole record.
data EvidencePoison = PoisonedPublishTime Int | PoisonedRecord Int
    deriving stock (Show)

poisonEvidence :: Maybe EvidencePoison -> [RuleEvidence] -> [RuleEvidence]
poisonEvidence poison evidence = case poison of
    Nothing -> evidence
    Just (PoisonedPublishTime index) -> poisonedAt index (\ev -> ev{evPublishedAt = Known (Just (impureThrow thrownVerdict))})
    Just (PoisonedRecord index) -> poisonedAt index (const (impureThrow thrownVerdict))
  where
    poisonedAt index change = zipWith (\i ev -> if i == index `mod` max 1 (length evidence) then change ev else ev) [0 ..] evidence

-- | One rule of a generated policy, at its default precedence or another.
genPolicyRule :: [Text] -> Gen PrecededRule
genPolicyRule pool = do
    rule <-
        Gen.choice
            [ DenyIfKnownMalicious <$> genAlignment
            , DenyIfCve <$> (DenyIfCveParams <$> Gen.element [0, 5.0, 9.9] <*> genAlignment)
            , DenyIfEpss <$> (DenyIfEpssParams <$> Gen.element [0, 0.5, 0.95] <*> genAlignment)
            , genRemediation
            , Gen.element (AllowIfOlderThan (7 * nominalDay) : DenyInstallTimeExecution : DenyByIdentity "other" : [AllowByIdentity ("thing@" <> release) | release <- take 1 pool])
            ]
    Gen.frequency [(3, pure (atDefaultPrecedence rule)), (1, (`at` rule) <$> genPrecedence)]

genAlignment :: Gen FailureAlignment
genAlignment = Gen.element [FailDeny, FailNoDecision]

-- | Rows for @thing@ and @other@: none, none that names a fix, or any.
genPackageRows :: [Text] -> Gen [(Text, AdvisoryRange)]
genPackageRows pool = concat <$> traverse (\name -> map (name,) <$> Gen.frequency [(1, pure []), (1, filter (not . namesAFix) <$> genRows pool), (2, genRows pool)]) ["thing", "other"]

namesAFix :: AdvisoryRange -> Bool
namesAFix row = case arUpperBound row of
    FixedBefore _ -> True
    _ -> False

-- | Mostly a loaded source, so the advisory rules decide from rows.
genSourceState :: Gen SourceState
genSourceState = Gen.frequency [(3, pure Loaded), (1, Gen.enumBounded)]

-- | A source whose reads never fault, so no breaker opens on a read count the schedule decides.
genSteadySource :: Gen SourceState
genSteadySource = Gen.filter (/= Faulting) genSourceState

-- | A request's versions of the named packages, interleaved, either side of the quarantine.
genListing :: Range.Range Int -> Ecosystem -> [Text] -> [Text] -> Gen [RuleEvidence]
genListing count eco names pool = Gen.list count $ do
    name <- Gen.element names
    release <- Gen.element pool
    age <- Gen.element [0, 30]
    scripts <- Gen.bool
    let ev = (pkg Nothing age){evName = mkPackageName eco Nothing name, evVersion = mkVersion eco release}
    pure (if scripts then withInstallScripts ev else ev)

-- | Whether a rule's filter keeps a row: a MAL record, a score at the threshold, or a named fix.
keptBy :: Rule -> AdvisoryRange -> Bool
keptBy rule row = case rule of
    DenyIfKnownMalicious _ -> maybe False (not . T.null) (T.stripPrefix "MAL-" (arCveId row))
    DenyIfCve params -> scoreAtLeast DenyMissingScore (dicMinCvss params) (arSeverity row)
    DenyIfEpss params -> scoreAtLeast AbstainMissingScore (dieMinEpss params) (arEpss row)
    AllowIfRemediatesCve _ -> namesAFix row
    _ -> False

isRuleThrew :: Outcome -> Bool
isRuleThrew = \case
    Reached (Undecidable _ (RuleUnable _ RuleThrew{})) -> True
    _ -> False

isEscaped :: Outcome -> Bool
isEscaped = \case
    Escaped _ -> True
    _ -> False

-- | Each rule's boot-order key, and whether the rule reads the package.
type RuleKey = ((Down Int, Text), Bool)

policyKeys :: [PrecededRule] -> [RuleKey]
policyKeys = map (\r -> ((Down (rulePrecedence r), ruleName (prRule r)), readsPackage (prRule r)))
  where
    readsPackage rule = case verdictSource rule of
        FromAdvisories{} -> True
        FromEvidence _ -> False

bombKeys :: Maybe Bomb -> [RuleKey]
bombKeys = maybe [] (\(Bomb prec _ _) -> [((Down prec, "PureBomb"), False)])

-- | Whether a per-version rule refused a version by throwing after an advisory rule had passed it.
perVersionThrowAfterRead :: [RuleKey] -> Outcome -> Bool
perVersionThrowAfterRead keys = \case
    Reached (Undecidable _ (RuleUnable name RuleThrew{})) -> or [advisory < thrower | (thrower@(_, rule), False) <- keys, rule == name, (advisory, True) <- keys]
    _ -> False

-- | Whether an advisory rule refused a version by throwing.
advisoryRuleThrew :: [RuleKey] -> Outcome -> Bool
advisoryRuleThrew keys = \case
    Reached (Undecidable _ (RuleUnable name RuleThrew{})) -> or [rule == name | ((_, rule), True) <- keys]
    _ -> False

-- | Whether the deny-by-default trail holds an advisory rule's abstention.
holdsAdvisoryAbstention :: Outcome -> Bool
holdsAdvisoryAbstention = \case
    Reached (BlockedByDefault reasons) -> any (\case FixesNoAdvisory -> True; NoKnownMaliciousAdvisory -> True; NotAffectedAtThreshold _ -> True; _ -> False) reasons
    _ -> False

evaluatorParitySpec :: Spec
evaluatorParitySpec = describe "the evaluator against the one frozen at f5f368d31" $ do
    -- Constant ranges and a two-package majority keep reads of more than one run common. A size-scaled range
    -- leaves the early tests of each size cycle with no rule or a single version. Fewer tests let either 1% label
    -- fall under its bar by chance.
    modifyMaxSuccess (const 600) . it "decides, reads and reports as it did over one or two packages in turn, npm and PyPI" $
        hedgehog $ do
            (eco, pool) <- forAll genEcosystem
            standing <- forAll genSourceState
            rows <- forAll (genPackageRows pool)
            policy <- forAll (Gen.list (Range.constant 0 6) (genPolicyRule pool))
            poison <- forAll (Gen.maybe (Poison <$> Gen.int (Range.constant 0 5) <*> Gen.element pool <*> Gen.int (Range.constant 0 (length poisonedVerdicts - 1))))
            evidencePoison <- forAll (Gen.maybe (Gen.choice [PoisonedPublishTime <$> Gen.int (Range.constant 0 9), PoisonedRecord <$> Gen.int (Range.constant 0 9)]))
            names <- forAll (Gen.frequency [(1, pure ["thing"]), (2, pure ["thing", "other"])])
            evidence <- poisonEvidence evidencePoison <$> forAll (genListing (Range.constant 1 10) eco names pool)
            let inTurn evaluator = liftIO (observe evaluator traverse (sourceStateDeps standing (depsWith rows)) policy (poisonAt eco poison) evidence)
            live <- inTurn liveEvaluator
            asItStood <- inTurn frozen
            H.cover 10 "the request reads more than one run of packages" (length (obReads asItStood) > 1)
            let keys = policyKeys policy
                wholeRecordThrows = case evidencePoison of
                    Just PoisonedRecord{} -> True
                    _ -> False
                admissions = [skippedChecks decision | Reached decision@Admitted{} <- obOutcomes asItStood]
            H.cover 10 "a rule's throw refuses a version" (any isRuleThrew (obOutcomes asItStood))
            H.cover 2 "a throw leaves the evaluator" (any isEscaped (obOutcomes asItStood))
            H.cover 1 "a per-version rule's throw refuses a version an advisory rule read" (any (perVersionThrowAfterRead keys) (obOutcomes asItStood))
            H.cover 2 "a throw inside a PerPackage verdict refuses a version" (not wholeRecordThrows && any (advisoryRuleThrew keys) (obOutcomes asItStood))
            H.cover 10 "an advisory rule's abstention reaches the trail" (any holdsAdvisoryAbstention (obOutcomes asItStood))
            H.cover 1 "an admission carries no evidence" (any null admissions)
            H.cover 2 "an admission carries evidence" (not (all null admissions))
            live === asItStood

    modifyMaxSuccess (const 200) . it "decides one package's versions concurrently as it did in turn, reading the package at most once" $
        hedgehog $ do
            (eco, pool) <- forAll genEcosystem
            standing <- forAll genSourceState
            rows <- forAll (genPackageRows pool)
            policy <- forAll (Gen.list (Range.linear 0 6) (genPolicyRule pool))
            bomb <- forAll (Gen.maybe (genBomb pool))
            timePoison <- forAll (Gen.maybe (PoisonedPublishTime <$> Gen.int (Range.constant 0 9)))
            evidence <- poisonEvidence timePoison <$> forAll (genListing (Range.linear 1 10) eco ["thing"] pool)
            let deps = sourceStateDeps standing (depsWith rows)
                withBomb = (<> maybeToList (bombRule eco <$> bomb))
            concurrent <- liftIO (observe liveEvaluator mapConcurrently deps policy withBomb evidence)
            asItStood <- liftIO (observe frozen traverse deps policy withBomb evidence)
            reaching <- liftIO (reachingAdvisoryRules deps policy evidence)
            H.cover 30 "two or more versions reach an advisory rule" (reaching >= 2)
            H.cover 10 "a rule's throw refuses a version" (any isRuleThrew (obOutcomes asItStood))
            H.cover 2 "a per-version rule's throw refuses a version an advisory rule read" (any (perVersionThrowAfterRead (policyKeys policy <> bombKeys bomb)) (obOutcomes asItStood))
            obOutcomes concurrent === obOutcomes asItStood
            obReads concurrent === obReads asItStood
            H.assert (length (obReads asItStood) <= 1)
            sortOn (show @Text) (obReports concurrent) === sortOn (show @Text) (obReports asItStood)

    modifyMaxSuccess (const 200) . it "decides two packages' versions concurrently as it did in turn, with reads and reports bounded" $
        hedgehog $ do
            (eco, pool) <- forAll genEcosystem
            standing <- forAll genSteadySource
            rows <- forAll (genPackageRows pool)
            policy <- forAll (Gen.list (Range.linear 0 6) (genPolicyRule pool))
            bomb <- forAll (Gen.maybe (genBomb pool))
            timePoison <- forAll (Gen.maybe (PoisonedPublishTime <$> Gen.int (Range.constant 0 9)))
            evidence <- poisonEvidence timePoison <$> forAll (genListing (Range.linear 1 10) eco ["thing", "other"] pool)
            let deps = sourceStateDeps standing (depsWith rows)
                withBomb = (<> maybeToList (bombRule eco <$> bomb))
                -- A throw decides its version twice, and each pass can read a package for each rule.
                bound = 2 * length evidence * (length policy + 1)
                distinctReports = sortNub . map (show @Text) . obReports
            concurrent <- liftIO (observe liveEvaluator mapConcurrently deps policy withBomb evidence)
            asItStood <- liftIO (observe frozen traverse deps policy withBomb evidence)
            reaching <- liftIO (reachingAdvisoryRules deps policy evidence)
            H.cover 30 "two or more versions reach an advisory rule" (reaching >= 2)
            H.cover 10 "both packages are read" (length (sortNub (obReads asItStood)) == 2)
            H.cover 10 "a rule's throw refuses a version" (any isRuleThrew (obOutcomes asItStood))
            H.cover 2 "a per-version rule's throw refuses a version an advisory rule read" (any (perVersionThrowAfterRead (policyKeys policy <> bombKeys bomb)) (obOutcomes asItStood))
            obOutcomes concurrent === obOutcomes asItStood
            sortNub (obReads concurrent) === sortNub (obReads asItStood)
            distinctReports concurrent === distinctReports asItStood
            H.assert (length (obReads concurrent) <= bound && length (obReports concurrent) <= bound)

    modifyMaxSuccess (const 300) . it "decides each version as the per-version reference, often with no row past the rule's filter, npm and PyPI" $
        hedgehog $ do
            (eco, pool) <- forAll genEcosystem
            rule <- forAll (Gen.choice [pure (DenyIfKnownMalicious FailDeny), denyCveAt <$> Gen.element [5.0, 9.9], denyEpssAt <$> Gen.element [0.5, 0.95], genRemediation])
            generated <- forAll (genRows pool)
            unmatched <- forAll Gen.bool
            let rows = if unmatched then filter (not . keptBy rule) generated else generated
                name = mkPackageName eco Nothing "thing"
                cve = fakeCveLookup (map ("thing",) rows)
                evidence = [identityEvidence name (mkVersion eco release) | release <- pool]
            H.cover 30 "the rule's filter keeps no row" (not (any (keptBy rule) rows))
            H.cover 10 "the rule's filter keeps a row" (any (keptBy rule) rows)
            perVersion <- liftIO (traverse (perVersionVerdict parityEtag (namesFix cve) cve rule) evidence)
            perRequest <- liftIO (perRequestVerdicts (servingRuleDeps parityEtag cve) rule name evidence)
            map normaliseVerdictIds perRequest === map normaliseVerdictIds perVersion

-- | The PyPI counterpart of 'pkg': @Flask_Thing\@1.0.0@, published the given days before 'now'.
pypiPkg :: Integer -> RuleEvidence
pypiPkg ageDays = (pkg Nothing ageDays){evName = unscopedPyPI "Flask_Thing"}

-- | Each built-in verdict and its exact reason sentence for both ecosystems.
ruleSentences :: [(String, RuleDeps, Rule, RuleEvidence, RuleVerdict, Text)]
ruleSentences =
    [ ("AllowScope on its scope", inertRuleDeps, AllowScope myorg, pkg (Just "myorg") 0, Allow (ScopeAllowListed myorg), "scope @myorg is allow-listed")
    , ("AllowScope on another scope", inertRuleDeps, AllowScope myorg, pkg (Just "other") 0, NoDecision (ScopeNotAllowListed myorg), "scope is not the allow-listed @myorg")
    , ("AllowScope on a PyPI project", inertRuleDeps, AllowScope myorg, pypiPkg 0, NoDecision (ScopeNotAllowListed myorg), "scope is not the allow-listed @myorg")
    , ("AllowIfOlderThan past the minimum", inertRuleDeps, quarantineRule, pkg Nothing 30, Allow (PublishedLongEnough (30 * nominalDay) (7 * nominalDay)), "published 30 days ago (at least 7 days old)")
    , ("AllowIfOlderThan at the minimum", inertRuleDeps, quarantineRule, pkg Nothing 7, Allow (PublishedLongEnough (7 * nominalDay) (7 * nominalDay)), "published 7 days ago (at least 7 days old)")
    , ("AllowIfOlderThan short of the minimum", inertRuleDeps, quarantineRule, pkg Nothing 1, NoDecision (PublishedTooRecently nominalDay (7 * nominalDay)), "published only 1 day ago, minimum age is 7 days")
    , ("AllowIfOlderThan on a version published today", inertRuleDeps, quarantineRule, pkg Nothing 0, NoDecision (PublishedTooRecently 0 (7 * nominalDay)), "published only 0 seconds ago, minimum age is 7 days")
    , ("AllowIfOlderThan on a version published in the future", inertRuleDeps, quarantineRule, pkg Nothing (-2), NoDecision (PublishedTooRecently (negate (2 * nominalDay)) (7 * nominalDay)), "published only 0 seconds ago, minimum age is 7 days")
    , ("AllowIfOlderThan on a PyPI project past the minimum", inertRuleDeps, quarantineRule, pypiPkg 30, Allow (PublishedLongEnough (30 * nominalDay) (7 * nominalDay)), "published 30 days ago (at least 7 days old)")
    , ("AllowIfOlderThan on a PyPI project short of the minimum", inertRuleDeps, quarantineRule, pypiPkg 1, NoDecision (PublishedTooRecently nominalDay (7 * nominalDay)), "published only 1 day ago, minimum age is 7 days")
    , ("AllowIfOlderThan with no publish time", inertRuleDeps, quarantineRule, unpublished (pkg Nothing 0), NoDecision PublishTimeUnknown, "publish time is unknown")
    , ("AllowIfOlderThan with the publish time unread", inertRuleDeps, quarantineRule, listed Nothing, CannotVet FailDeny PublishTimeUnread, "the publish time is not available")
    , ("DenyInstallTimeExecution on an install hook", inertRuleDeps, DenyInstallTimeExecution, withInstallScripts (pkg Nothing 0), Deny Nothing (RunsOnInstall "postinstall hook"), "runs code on install: postinstall hook")
    , ("DenyInstallTimeExecution on a PyPI project that runs code", inertRuleDeps, DenyInstallTimeExecution, withInstallScripts (pypiPkg 0), Deny Nothing (RunsOnInstall "postinstall hook"), "runs code on install: postinstall hook")
    , ("DenyInstallTimeExecution with no install code", inertRuleDeps, DenyInstallTimeExecution, pkg Nothing 0, NoDecision NothingRunsOnInstall, "no install-time code execution")
    , ("DenyInstallTimeExecution with the signal undetermined", inertRuleDeps, DenyInstallTimeExecution, (pkg Nothing 0){evInstallCode = Known CodeExecUnknown}, NoDecision InstallCodeUndetermined, "install-time code execution not yet determined")
    , ("DenyInstallTimeExecution with the signal unread", inertRuleDeps, DenyInstallTimeExecution, listed Nothing, CannotVet FailDeny InstallSignalUnread, "the install-time execution signal is not available")
    , ("DenyExternalDependencies on a dependency from outside the registry", inertRuleDeps, DenyExternalDependencies, fetchesOutside (pkg Nothing 0), Deny Nothing (FetchesOutside externalReason), "installs a dependency from outside the registry: dependency 'left-pad' in dependencies names a git source")
    , ("DenyExternalDependencies with every dependency through the registry", inertRuleDeps, DenyExternalDependencies, pkg Nothing 0, NoDecision DependenciesThroughRegistry, "every installable dependency resolves through the registry")
    , ("DenyExternalDependencies with the dependency sources undetermined", inertRuleDeps, DenyExternalDependencies, (pkg Nothing 0){evDependencySources = Known DependencySourcesUnknown}, NoDecision DependencySourcesUndetermined, "dependency sources not yet determined")
    , ("DenyExternalDependencies with the dependency sources unread", inertRuleDeps, DenyExternalDependencies, listed Nothing, CannotVet FailDeny DependencySourcesUnread, "the dependency sources are not available")
    , ("DenyByIdentity on its identity", inertRuleDeps, DenyByIdentity "thing@1.0.0", pkg Nothing 0, Deny Nothing (IdentityRevoked "thing@1.0.0"), "identity thing@1.0.0 is revoked by operator")
    , ("DenyByIdentity on another identity", inertRuleDeps, DenyByIdentity "other", pkg Nothing 0, NoDecision (IdentityNotRevoked "other"), "identity is not the revoked other")
    , ("DenyByIdentity on a PyPI project", inertRuleDeps, DenyByIdentity "Flask_Thing@1.0.0", pypiPkg 0, Deny Nothing (IdentityRevoked "Flask_Thing@1.0.0"), "identity Flask_Thing@1.0.0 is revoked by operator")
    , ("AllowByIdentity on its identity", inertRuleDeps, AllowByIdentity "@myorg/thing", pkg (Just "myorg") 0, Allow (IdentityAllowListed "@myorg/thing"), "identity @myorg/thing is allow-listed by operator")
    , ("AllowByIdentity on another identity", inertRuleDeps, AllowByIdentity "other", pkg Nothing 0, NoDecision (IdentityNotAllowListed "other"), "identity is not the allow-listed other")
    , ("AllowByIdentity on a PyPI project", inertRuleDeps, AllowByIdentity "Flask_Thing", pypiPkg 0, Allow (IdentityAllowListed "Flask_Thing"), "identity Flask_Thing is allow-listed by operator")
    , ("DenyIfCve below its threshold", depsWith (affecting (Just 5.0) Nothing), denyCveAt 8.0, pkg Nothing 0, NoDecision (NotAffectedAtThreshold Cvss), "no advisory at or above the CVSS threshold affects this version")
    , ("DenyIfEpss below its threshold", depsWith (affecting Nothing (Just 0.1)), denyEpssAt 0.5, pkg Nothing 0, NoDecision (NotAffectedAtThreshold Epss), "no advisory at or above the EPSS threshold affects this version")
    , ("DenyIfCve on a PyPI project", depsWith pypiAffecting, denyCveAt 8.0, pypiPkg 0, Deny (Just (DbEtag "etag-1")) (AffectedBy Cvss 8.0 (mkAdvisoryIds ("GHSA-affect-0001" :| []))), "affected by GHSA-affect-0001 (CVSS >= 8.0)")
    , ("DenyIfEpss on a PyPI project", depsWith pypiAffecting, denyEpssAt 0.5, pypiPkg 0, Deny (Just (DbEtag "etag-1")) (AffectedBy Epss 0.5 (mkAdvisoryIds ("GHSA-affect-0001" :| []))), "affected by GHSA-affect-0001 (EPSS >= 0.5)")
    , ("AllowIfRemediatesCve on a PyPI project", depsWith [("flask-thing", snd row) | row <- fixRows], shippedRemediation, pypiPkg 0, Allow (Remediates (mkAdvisoryIds ("GHSA-fixed-0001" :| []))), "remediates GHSA-fixed-0001")
    , ("AllowIfRemediatesCve on a PyPI partial fix inside the limits", depsWith (pypiRows (partialFixRows 4.2)), shippedRemediation, pypiPkg 0, Allow (RemediatesWithinLimits (partialComparison 4.2)), "remediates GHSA-fixed-0001 with GHSA-open-0002 remaining inside the limits" <> partialFacts "4.2")
    , ("AllowIfRemediatesCve on a PyPI partial fix outside the limits", depsWith (pypiRows (partialFixRows 7.0)), shippedRemediation, pypiPkg 0, NoDecision (RemediationBeyondLimits (partialComparison 7.0) (mkLimitShortfalls (RemainingAboveCeiling Cvss :| []))), "fixes GHSA-fixed-0001 with GHSA-open-0002 remaining outside the limits: remaining CVSS above its ceiling" <> partialFacts "7.0")
    , ("AllowIfRemediatesCve on an npm partial fix inside the limits", depsWith (partialFixRows 4.2), shippedRemediation, pkg Nothing 0, Allow (RemediatesWithinLimits (partialComparison 4.2)), "remediates GHSA-fixed-0001 with GHSA-open-0002 remaining inside the limits" <> partialFacts "4.2")
    , ("AllowIfRemediatesCve on an npm partial fix outside the limits", depsWith (partialFixRows 7.0), shippedRemediation, pkg Nothing 0, NoDecision (RemediationBeyondLimits (partialComparison 7.0) (mkLimitShortfalls (RemainingAboveCeiling Cvss :| []))), "fixes GHSA-fixed-0001 with GHSA-open-0002 remaining outside the limits: remaining CVSS above its ceiling" <> partialFacts "7.0")
    ]
  where
    myorg = mkScope "myorg"
    quarantineRule = AllowIfOlderThan (7 * nominalDay)
    unpublished ev = ev{evPublishedAt = Known Nothing}
    pypiAffecting = pypiRows (affecting (Just 9.8) (Just 0.9))
    pypiRows rows = [("flask-thing", snd row) | row <- rows]
    partialComparison remainingCvss =
        ScoreComparison
            (mkAdvisoryIds ("GHSA-fixed-0001" :| []))
            (mkAdvisoryIds ("GHSA-open-0002" :| []))
            (ScoreSides (Scored 9.8) (Scored remainingCvss))
            (ScoreSides (Scored 0.5) (Scored 0.005))
            defaultDifferentialLimits
    partialFacts remainingCvss =
        " (remaining CVSS " <> remainingCvss <> ", CVSS ceiling 6.9 inclusive, fixed CVSS 9.8, CVSS gap at least 1.0, remaining EPSS 0.005, EPSS ceiling 0.01 exclusive, fixed EPSS 0.5)"

ruleSentenceSpec :: Spec
ruleSentenceSpec = describe "the sentence each built-in rule's reason reads as" $ do
    for_ ruleSentences $ \(label, deps, rule, ev, verdict, sentence) ->
        it label $
            evalRule deps ctx rule ev `shouldDecide` (verdict, sentence)

    for_ [(AllowIfOlderThan (7 * nominalDay), "AllowIfOlderThan: the publish time is not available"), (DenyInstallTimeExecution, "DenyInstallTimeExecution: the install-time execution signal is not available"), (DenyExternalDependencies, "DenyExternalDependencies: the dependency sources are not available")] $ \(rule, sentence) ->
        it (toString (ruleName rule <> " names itself in the refusal an unread fact decides")) $ do
            decision <- decide [atDefaultPrecedence rule] (listed Nothing)
            decision `shouldSatisfy` isUndecidable
            sentences decision `shouldBe` [sentence]

quarantineAgeSpec :: Spec
quarantineAgeSpec = describe "the quarantine age" $
    modifyMaxSuccess (const 2000) . it "equals diffUTCTime and decides as a verdict built on it, npm and PyPI" $
        hedgehog $ do
            instant <- forAll genInstant
            published <- forAll (Gen.choice [genInstant, genPublishedNear instant])
            let age = diffUTCTime instant published
            minAge <- forAll (Gen.element [age, succ age, pred age, 7 * nominalDay])
            evidence <- forAllWith (show . pkgEcosystem . evName) (Gen.element [pkg Nothing 0, pypiPkg 0])
            verdict <- liftIO (evalRule inertRuleDeps (ctxAt instant) (AllowIfOlderThan minAge) evidence{evPublishedAt = Known (Just published)})
            H.cover 20 "the version is old enough" (isAllow verdict)
            H.cover 20 "the version is too recent" (isNoDecision verdict)
            H.cover 5 "the version is published after the instant" (published > instant)
            H.cover 20 "a time of day falls in a leap second" (any ((>= 86_400) . utctDayTime) [instant, published])
            H.cover 10 "a day falls before day 0" (any ((< 0) . toModifiedJulianDay . utctDay) [instant, published])
            reportedAge verdict === Just age
            verdict === referenceAgeVerdict minAge age

-- The quarantine verdict over 'diffUTCTime', the reference for the rule's own age arithmetic.
referenceAgeVerdict :: NominalDiffTime -> NominalDiffTime -> RuleVerdict
referenceAgeVerdict minAge age
    | age >= minAge = Allow (PublishedLongEnough age minAge)
    | otherwise = NoDecision (PublishedTooRecently age minAge)

reportedAge :: RuleVerdict -> Maybe NominalDiffTime
reportedAge = \case
    Allow (PublishedLongEnough age _) -> Just age
    NoDecision (PublishedTooRecently age _) -> Just age
    _ -> Nothing

-- An instant over a wide range of days either side of day 0.
genInstant :: Gen UTCTime
genInstant = UTCTime . ModifiedJulianDay <$> Gen.integral (Range.linearFrom 61_000 (-1_000_000_000_000) 1_000_000_000_000) <*> genTimeOfDay

-- A publish instant up to ten years before the given one, or up to a month after it.
genPublishedNear :: UTCTime -> Gen UTCTime
genPublishedNear (UTCTime day _) = UTCTime . (`addDays` day) <$> Gen.integral (Range.linearFrom 0 (-3650) 30) <*> genTimeOfDay

-- Picoseconds from midnight to the end of a leap second, often past 86,400 s, where 'diffUTCTime' clamps.
genTimeOfDay :: Gen DiffTime
genTimeOfDay =
    picosecondsToDiffTime
        <$> Gen.choice
            [ Gen.integral (Range.constant 0 leapSecondEnd)
            , Gen.integral (Range.constant dayEnd leapSecondEnd)
            , Gen.element [0, dayEnd - 1, dayEnd, dayEnd + 1, leapSecondEnd]
            ]
  where
    dayEnd = 86_400_000_000_000_000
    leapSecondEnd = dayEnd + 1_000_000_000_000

spec :: Spec
spec = do
    knownMaliciousSpec
    availabilitySpec
    expirySpec
    evidenceSpec
    sourceHealthSpec
    noDatabaseSpec
    referenceParitySpec
    evaluatorParitySpec
    ruleSentenceSpec
    quarantineAgeSpec
    describe "advisory package identity" $ do
        for_ [denyCveAt 0, denyEpssAt 0] $ \rule ->
            it (toString (ruleName rule <> " queries the canonical PyPI name")) $ do
                let pd = pypiPkg 0
                    rows = [("flask-thing", snd row) | row <- affecting (Just 9.8) (Just 0.9)]
                evalRule (depsWith rows) ctx rule pd >>= (`shouldSatisfy` isDeny)

        it "matches a PyPI fix and keeps its display spelling in the decision message" $ do
            let pd = pypiPkg 0
                rows = [("flask-thing", snd row) | row <- fixRows]
            decision <- decideWith (depsWith rows) [atDefaultPrecedence shippedRemediation] pd
            admittedBy decision `shouldBe` Just "AllowIfRemediatesCve"
            renderDecision pd decision `shouldSatisfy` T.isInfixOf "Flask_Thing@1.0.0"

        it "does not fast-track a PyPI fix while a canonical-name advisory still affects it" $ do
            let pd = pypiPkg 0
                rows = [("flask-thing", snd row) | row <- fixRows <> affecting Nothing Nothing]
            evalRule (depsWith rows) ctx shippedRemediation pd >>= (`shouldSatisfy` isNoDecision)

        for_ [mkPackageName Npm Nothing "Flask_Thing", mkPackageName Npm (Just (mkScope "Acme")) "Flask_Thing"] $ \name ->
            it (toString ("preserves npm identity " <> renderPackageName name)) $ do
                let pd = (pkg Nothing 0){evName = name}
                    exactRows = [(renderPackageName name, snd row) | row <- affecting Nothing Nothing]
                    otherRows = [("flask-thing", snd row) | row <- affecting Nothing Nothing]
                    exactFixes = [(renderPackageName name, snd row) | row <- fixRows]
                evalRule (depsWith exactRows) ctx (denyCveAt 0) pd >>= (`shouldSatisfy` isDeny)
                evalRule (depsWith otherRows) ctx (denyCveAt 0) pd >>= (`shouldSatisfy` isNoDecision)
                evalRule (depsWith exactFixes) ctx shippedRemediation pd >>= (`shouldSatisfy` isAllow)

    describe "canonical remediation refusal controls" $
        for_ [(PyPI, "1.19.0", "1.19", "1.18"), (Npm, "3.0.0+build.7", "3.0.0+build.7", "2.9.0")] $ \(eco, advisoryFix, servedFix, oldVersion) ->
            it ("admits an exact " <> show eco <> " fix only while no advisory affects it past the limits") $ do
                let name = mkPackageName eco Nothing "pkg"
                    evidence raw = completeEvidence (sampleDetails name (mkVersion eco raw))
                    fixedRow = ("pkg", AdvisoryRange "FIX" (Just 9) Nothing (FixedBefore advisoryFix) (Just 0.9))
                    stillAffected = ("pkg", AdvisoryRange "OTHER" (Just 9) Nothing Unbounded (Just 0.9))
                    fixedDeps = depsWith [fixedRow]
                    affectedDeps = depsWith [fixedRow, stillAffected]
                for_ [remediationWith FailDeny AbstainOnRemaining, shippedRemediation] $ \remediation -> do
                    evalRule fixedDeps ctx remediation (evidence servedFix) >>= (`shouldSatisfy` isAllow)
                    evalRule fixedDeps ctx remediation (evidence oldVersion) >>= (`shouldSatisfy` isNoDecision)
                    evalRule affectedDeps ctx remediation (evidence servedFix) >>= (`shouldSatisfy` isNoDecision)
                for_ [denyCveAt 0, denyEpssAt 0] $ \rule -> do
                    evalRule fixedDeps ctx rule (evidence oldVersion) >>= (`shouldSatisfy` isDeny)
                    evalRule affectedDeps ctx rule (evidence servedFix) >>= (`shouldSatisfy` isDeny)

    describe "evalRule" $ do
        it "AllowScope allows a matching scope" $
            evalRule inertRuleDeps ctx (AllowScope (mkScope "myorg")) (pkg (Just "myorg") 0)
                >>= (`shouldSatisfy` isAllow)
        it "AllowScope yields no decision on a non-matching scope" $
            evalRule inertRuleDeps ctx (AllowScope (mkScope "myorg")) (pkg (Just "other") 0)
                >>= (`shouldSatisfy` isNoDecision)
        it "AllowScope yields no decision on an unscoped package" $
            evalRule inertRuleDeps ctx (AllowScope (mkScope "myorg")) (pkg Nothing 0)
                >>= (`shouldSatisfy` isNoDecision)
        it "AllowIfOlderThan allows a version older than the threshold" $
            evalRule inertRuleDeps ctx (AllowIfOlderThan (7 * nominalDay)) (pkg Nothing 30)
                >>= (`shouldSatisfy` isAllow)
        it "AllowIfOlderThan yields no decision on a too-young version" $
            evalRule inertRuleDeps ctx (AllowIfOlderThan (7 * nominalDay)) (pkg Nothing 1)
                >>= (`shouldSatisfy` isNoDecision)
        it "DenyInstallTimeExecution denies a package that runs install scripts" $
            evalRule inertRuleDeps ctx DenyInstallTimeExecution (withInstallScripts (pkg Nothing 99))
                >>= (`shouldSatisfy` isDeny)
        it "DenyInstallTimeExecution yields no decision when there are no install scripts" $
            evalRule inertRuleDeps ctx DenyInstallTimeExecution (pkg Nothing 99)
                >>= (`shouldSatisfy` isNoDecision)
        it "DenyByIdentity matches a package name exactly" $
            evalRule inertRuleDeps ctx (DenyByIdentity "thing") (pkg Nothing 0)
                >>= (`shouldSatisfy` isDeny)
        it "DenyByIdentity matches a package@version exactly" $
            evalRule inertRuleDeps ctx (DenyByIdentity "thing@1.0.0") (pkg Nothing 0)
                >>= (`shouldSatisfy` isDeny)
        it "DenyByIdentity matches a scoped package name exactly" $
            evalRule inertRuleDeps ctx (DenyByIdentity "@myorg/thing") (pkg (Just "myorg") 0)
                >>= (`shouldSatisfy` isDeny)
        it "DenyByIdentity yields no decision on a non-match" $
            evalRule inertRuleDeps ctx (DenyByIdentity "other") (pkg Nothing 0)
                >>= (`shouldSatisfy` isNoDecision)
        it "AllowByIdentity matches a package name exactly" $
            evalRule inertRuleDeps ctx (AllowByIdentity "thing") (pkg Nothing 0)
                >>= (`shouldSatisfy` isAllow)
        it "AllowByIdentity matches a package@version exactly" $
            evalRule inertRuleDeps ctx (AllowByIdentity "thing@1.0.0") (pkg Nothing 0)
                >>= (`shouldSatisfy` isAllow)
        it "AllowByIdentity yields no decision on a non-match" $
            evalRule inertRuleDeps ctx (AllowByIdentity "other") (pkg Nothing 0)
                >>= (`shouldSatisfy` isNoDecision)

    describe "evalRule (AllowIfRemediatesCve)" $ do
        it "allows a version an advisory names as its exact fix, crediting the advisory" $
            evalRule (depsWith fixRows) ctx shippedRemediation (pkg Nothing 0)
                `shouldDecide` (Allow (Remediates (mkAdvisoryIds ("GHSA-fixed-0001" :| []))), "remediates GHSA-fixed-0001")
        it "names every advisory the version fixes in the reason" $ do
            let rows =
                    [ ("thing", AdvisoryRange "GHSA-fixed-0001" Nothing (Just "0") (FixedBefore "1.0.0") Nothing)
                    , ("thing", AdvisoryRange "GHSA-fixed-0002" Nothing (Just "0.2.0") (FixedBefore "1.0.0") Nothing)
                    ]
            evalRule (depsWith rows) ctx shippedRemediation (pkg Nothing 0)
                `shouldDecide` (Allow (Remediates (mkAdvisoryIds ("GHSA-fixed-0001" :| ["GHSA-fixed-0002"]))), "remediates GHSA-fixed-0001, GHSA-fixed-0002")
        it "matches the OSV wire form of a scoped name" $ do
            let rows = [("@myorg/thing", AdvisoryRange "GHSA-fixed-0003" Nothing (Just "0") (FixedBefore "1.0.0") Nothing)]
            evalRule (depsWith rows) ctx shippedRemediation (pkg (Just "myorg") 0)
                `shouldDecide` (Allow (Remediates (mkAdvisoryIds ("GHSA-fixed-0003" :| []))), "remediates GHSA-fixed-0003")
        it "abstains when no advisory names the version as a fix (exact match only)" $ do
            -- 1.0.0 sits past this advisory's 0.9.0 fix, but the fast lane is a
            -- deliberate exact-fix probe: being merely unaffected earns nothing.
            let rows = [("thing", AdvisoryRange "GHSA-fixed-0001" Nothing (Just "0") (FixedBefore "0.9.0") Nothing)]
            evalRule (depsWith rows) ctx shippedRemediation (pkg Nothing 0)
                `shouldDecide` (NoDecision FixesNoAdvisory, "no advisory names this version as its fix")
        it "abstains in abstain mode when the version still sits inside another advisory's affected range" $ do
            let rows =
                    fixRows
                        <> [("thing", AdvisoryRange "GHSA-open-0002" Nothing (Just "0.5.0") Unbounded Nothing)]
            evalRule (depsWith rows) ctx (remediationWith FailDeny AbstainOnRemaining) (pkg Nothing 0)
                `shouldDecide` (NoDecision (FixesButStillAffected (mkAdvisoryIds ("GHSA-fixed-0001" :| [])) (mkAdvisoryIds ("GHSA-open-0002" :| []))), "fixes GHSA-fixed-0001 with GHSA-open-0002 remaining")
        it "abstains under the shipped limits when a remaining advisory stores no score" $ do
            let rows =
                    fixRows
                        <> [("thing", AdvisoryRange "GHSA-open-0002" Nothing (Just "0.5.0") Unbounded Nothing)]
                unscored = mkAdvisoryIds ("GHSA-open-0002" :| [])
            evalRule (depsWith rows) ctx shippedRemediation (pkg Nothing 0)
                >>= (`shouldBe` NoDecision (RemediationBeyondLimits (ScoreComparison (mkAdvisoryIds ("GHSA-fixed-0001" :| [])) unscored (ScoreSides Unscored Unscored) (ScoreSides Unscored Unscored) defaultDifferentialLimits) (mkLimitShortfalls (RemainingUnscored Cvss unscored :| [FixedUnscored Cvss, RemainingUnscored Epss unscored]))))
        for_ [FailDeny, FailNoDecision] $ \alignment ->
            it ("cannot vet under onUnavailable " <> show alignment <> " when no advisory database is loaded") $
                evalRule inertRuleDeps ctx (differentialRemediation alignment) (pkg Nothing 0)
                    `shouldDecide` (CannotVet alignment NoDatabaseLoaded, "no advisory database loaded")

    describe "evalRule (DenyIfCve)" $ do
        it "denies an affected version whose advisory meets the threshold, naming it" $
            evalRule (depsWith (affecting (Just 9.8) Nothing)) ctx (denyCveAt 8.0) (pkg Nothing 0)
                `shouldDecide` (Deny (Just (DbEtag "etag-1")) (AffectedBy Cvss 8.0 (mkAdvisoryIds ("GHSA-affect-0001" :| []))), "affected by GHSA-affect-0001 (CVSS >= 8.0)")
        it "abstains when the affecting advisory is below the threshold" $
            evalRule (depsWith (affecting (Just 5.0) Nothing)) ctx (denyCveAt 8.0) (pkg Nothing 0)
                >>= (`shouldSatisfy` isNoDecision)
        it "denies an unscored advisory (fail-closed: npm malware carries no score)" $
            evalRule (depsWith (affecting Nothing Nothing)) ctx (denyCveAt 8.0) (pkg Nothing 0)
                >>= (`shouldSatisfy` isDeny)
        it "abstains when the version sits outside the affected range" $ do
            -- 1.0.0 is past this advisory's exclusive 1.0.0 fix, so unaffected.
            let rows = [("thing", AdvisoryRange "GHSA-affect-0002" (Just 9.9) (Just "0") (FixedBefore "1.0.0") Nothing)]
            evalRule (depsWith rows) ctx (denyCveAt 8.0) (pkg Nothing 0)
                >>= (`shouldSatisfy` isNoDecision)
        it "fails closed (Undecidable) when no advisory database is loaded" $
            decideWith inertRuleDeps [atDefaultPrecedence (denyCveAt 8.0)] (pkg Nothing 0)
                >>= (`shouldSatisfy` isUndecidable)
        it "fails open (skips) when configured onUnavailable=skip and no database is loaded" $
            decideWith inertRuleDeps [atDefaultPrecedence (DenyIfCve (DenyIfCveParams 8.0 FailNoDecision))] (pkg Nothing 0)
                >>= (`shouldSatisfy` isBlockedByDefault)

    describe "evalRule (DenyIfEpss)" $ do
        it "denies an affected version whose advisory meets the threshold, naming it" $
            evalRule (depsWith (affecting Nothing (Just 0.75))) ctx (denyEpssAt 0.5) (pkg Nothing 0)
                `shouldDecide` (Deny (Just (DbEtag "etag-1")) (AffectedBy Epss 0.5 (mkAdvisoryIds ("GHSA-affect-0001" :| []))), "affected by GHSA-affect-0001 (EPSS >= 0.5)")
        it "denies at the threshold exactly, which is where an at-or-above gate closes" $
            evalRule (depsWith (affecting Nothing (Just 0.5))) ctx (denyEpssAt 0.5) (pkg Nothing 0)
                >>= (`shouldSatisfy` isDeny)
        it "abstains when the affecting advisory scores below the threshold" $
            evalRule (depsWith (affecting Nothing (Just 0.1))) ctx (denyEpssAt 0.5) (pkg Nothing 0)
                >>= (`shouldSatisfy` isNoDecision)
        forM_ unscoredEpssCases $ \(label, score) ->
            it ("abstains for " <> label) $ do
                score `shouldBe` Nothing
                evalRule (depsWith (affecting (Just 9.8) score)) ctx (denyEpssAt 0.5) (pkg Nothing 0)
                    >>= (`shouldSatisfy` isNoDecision)
        it "denies another affecting advisory whose EPSS score meets the threshold" $ do
            let rows =
                    affecting Nothing Nothing
                        <> [("thing", AdvisoryRange "CVE-2026-10002" Nothing (Just "0") Unbounded (Just 0.75))]
            evalRule (depsWith rows) ctx (denyEpssAt 0.5) (pkg Nothing 0)
                `shouldDecide` (Deny (Just (DbEtag "etag-1")) (AffectedBy Epss 0.5 (mkAdvisoryIds ("CVE-2026-10002" :| []))), "affected by CVE-2026-10002 (EPSS >= 0.5)")
        it "abstains when the version sits outside the affected range" $ do
            let rows = [("thing", AdvisoryRange "GHSA-affect-0002" Nothing (Just "0") (FixedBefore "1.0.0") (Just 0.99))]
            evalRule (depsWith rows) ctx (denyEpssAt 0.5) (pkg Nothing 0)
                >>= (`shouldSatisfy` isNoDecision)
        it "fails closed (Undecidable) when no advisory database is loaded" $
            decideWith inertRuleDeps [atDefaultPrecedence (denyEpssAt 0.5)] (pkg Nothing 0)
                >>= (`shouldSatisfy` isUndecidable)
        it "fails open (skips) when configured onUnavailable=skip and no database is loaded" $
            decideWith inertRuleDeps [atDefaultPrecedence (DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision))] (pkg Nothing 0)
                >>= (`shouldSatisfy` isBlockedByDefault)

    describe "individual EPSS gaps in public and mirror admission's shared evaluator" $ do
        forM_ unscoredEpssCases $ \(label, score) -> do
            it ("keeps deny-by-default for " <> label) $
                decideWith (depsWith (affecting Nothing score)) [atDefaultPrecedence (denyEpssAt 0.5)] (pkg Nothing 99)
                    >>= (`shouldSatisfy` isBlockedByDefault)
            it ("allows through another rule for " <> label) $
                decideWith
                    (depsWith (affecting Nothing score))
                    (map atDefaultPrecedence [denyEpssAt 0.5, AllowIfOlderThan (7 * nominalDay)])
                    (pkg Nothing 99)
                    >>= \d -> admittedBy d `shouldBe` Just "AllowIfOlderThan"
        it "restores an EPSS denial when a known high score returns" $ do
            let policy = map atDefaultPrecedence [denyEpssAt 0.5, AllowIfOlderThan (7 * nominalDay)]
            forM_ [Just 0.75, Nothing, Just 0.75] $ \score -> do
                decision <- decideWith (depsWith (affecting Nothing score)) policy (pkg Nothing 99)
                case score of
                    Nothing -> admittedBy decision `shouldBe` Just "AllowIfOlderThan"
                    Just _ -> blockedBy decision `shouldBe` Just "DenyIfEpss"
        it "retains DenyIfCve's unscored malware denial when EPSS abstains" $
            decideWith
                (depsWith (affecting Nothing Nothing))
                (map atDefaultPrecedence [denyEpssAt 0.5, denyCveAt 8.0])
                (pkg Nothing 99)
                >>= \d -> blockedBy d `shouldBe` Just "DenyIfCve"
        it "retains another decisive denial when EPSS abstains" $
            decideWith
                (depsWith (affecting Nothing Nothing))
                (map atDefaultPrecedence [denyEpssAt 0.5, DenyByIdentity "thing"])
                (pkg Nothing 99)
                >>= \d -> blockedBy d `shouldBe` Just "DenyByIdentity"

    describe "deny precedence (DenyExternalDependencies)" $ do
        it "overrides the quarantine and an identity pin at their default precedences" $
            decide (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), AllowByIdentity "thing", DenyExternalDependencies]) (fetchesOutside (pkg Nothing 99))
                >>= \d -> blockedBy d `shouldBe` Just "DenyExternalDependencies"
        it "yields to an identity pin above its precedence" $
            decide [atDefaultPrecedence DenyExternalDependencies, at 301 (AllowByIdentity "thing")] (fetchesOutside (pkg Nothing 0))
                >>= \d -> admittedBy d `shouldBe` Just "AllowByIdentity"
        it "shares the deny band with the install-script deny" $
            defaultPrecedence DenyExternalDependencies `shouldBe` defaultPrecedence DenyInstallTimeExecution
        it "leaves the Dredger's rules when limited to admission" $
            revocationRules [admissionOnly (atDefaultPrecedence DenyExternalDependencies), atDefaultPrecedence (AllowIfOlderThan 0)]
                `shouldBe` [atDefaultPrecedence (AllowIfOlderThan 0)]

    describe "deny precedence (DenyIfEpss)" $ do
        it "overrides the quarantine allow at default precedences, whatever the order" $ do
            let rs = [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay)), atDefaultPrecedence (denyEpssAt 0.5)]
                deps = depsWith (affecting Nothing (Just 0.75))
            decideWith deps rs (pkg Nothing 99) >>= \d -> blockedBy d `shouldBe` Just "DenyIfEpss"
            decideWith deps (reverse rs) (pkg Nothing 99) >>= \d -> blockedBy d `shouldBe` Just "DenyIfEpss"
        it "yields to an operator's identity pin, the documented escape hatch" $
            -- AllowByIdentity (250) outranks the advisory deny band (225), as it does for
            -- DenyIfCve: an operator who pins a version has decided it must ship.
            decideWith
                (depsWith (affecting Nothing (Just 0.99)))
                (map atDefaultPrecedence [AllowByIdentity "thing@1.0.0", denyEpssAt 0.5])
                (pkg Nothing 0)
                >>= \d -> admittedBy d `shouldBe` Just "AllowByIdentity"
        it "is outranked by an install-script deny, which keeps the last word" $
            decideWith
                (depsWith (affecting Nothing (Just 0.99)))
                (map atDefaultPrecedence [DenyInstallTimeExecution, denyEpssAt 0.5])
                (withInstallScripts (pkg Nothing 0))
                >>= \d -> blockedBy d `shouldBe` Just "DenyInstallTimeExecution"
        it "ties with DenyIfCve at their shared default, and the boot order breaks it by name" $
            -- Both fire on the same advisory. The earlier name takes the credit, so a
            -- decision never depends on the configured order.
            decideWith
                (depsWith (affecting (Just 9.8) (Just 0.99)))
                (map atDefaultPrecedence [denyEpssAt 0.5, denyCveAt 8.0])
                (pkg Nothing 0)
                >>= \d -> blockedBy d `shouldBe` Just "DenyIfCve"

    describe "PrecededRule" $ do
        it "exposes the precedence and rule it was built with" $ do
            -- The fields a config loader reads to patch a rule's precedence.
            let pr = PrecededRule 250 AdmissionOnly DenyInstallTimeExecution
            rulePrecedence pr `shouldBe` 250
            ruleReach pr `shouldBe` AdmissionOnly
            prRule pr `shouldBe` DenyInstallTimeExecution

    describe "defaultPrecedence" $ do
        it "ranks DenyInstallTimeExecution strictly above every allow default" $ do
            let allows = [AllowScope (mkScope "x"), AllowIfOlderThan 0, AllowByIdentity "x", shippedRemediation]
            defaultPrecedence DenyInstallTimeExecution
                `shouldSatisfy` (\d -> all ((d >) . defaultPrecedence) allows)
        it "orders the allow band by explicitness: quarantine < fast lane < scope < identity" $
            ( defaultAllowIfOlderThanPrecedence
            , defaultAllowIfRemediatesCvePrecedence
            , defaultAllowScopePrecedence
            , defaultAllowByIdentityPrecedence
            )
                `shouldSatisfy` (\(q, f, s, i) -> q < f && f < s && s < i)
        it "atDefaultPrecedence pairs a rule with its type default, at both phases" $
            atDefaultPrecedence DenyInstallTimeExecution
                `shouldBe` PrecededRule defaultDenyInstallTimeExecutionPrecedence AdmissionAndRevocation DenyInstallTimeExecution

    describe "prepare" $ do
        for_ [FailDeny, FailNoDecision] $ \alignment ->
            it ("attaches a resilience to AllowIfRemediatesCve and its " <> show alignment <> " to every unavailability") $
                -- An expired push follows the remediation choice too, unlike an advisory deny's.
                prepare unloadedDeps [atDefaultPrecedence (differentialRemediation alignment)] >>= \case
                    [r@PreparedRule{prepEval = PerPackage packageRead}] -> do
                        isJust (prepResilience r) `shouldBe` True
                        (onExpiredPush (prAlignment packageRead), onFaultedRead (prAlignment packageRead)) `shouldBe` (alignment, alignment)
                    other -> expectationFailure ("expected one prepared package read, got " <> show (length other))
        it "prepares every pure built-in to run directly, with no resilience, database or not" $
            for_ [inertRuleDeps, unloadedDeps] $ \deps -> do
                rules <-
                    prepare
                        deps
                        ( map
                            atDefaultPrecedence
                            [ AllowScope (mkScope "myorg")
                            , AllowIfOlderThan (7 * nominalDay)
                            , AllowByIdentity "thing"
                            , DenyInstallTimeExecution
                            , DenyByIdentity "thing"
                            ]
                        )
                map (isJust . prepResilience) rules `shouldBe` replicate 5 False

    describe "bootOrder" $ do
        it "orders highest precedence first, then rule name ascending" $ do
            -- A shuffled configured set arranges into one total order: precedence
            -- descending, then name as the deterministic tiebreak.
            rules <-
                prepare
                    inertRuleDeps
                    [ at 100 (AllowIfOlderThan (7 * nominalDay))
                    , at 300 DenyInstallTimeExecution
                    , at 200 (AllowScope (mkScope "myorg"))
                    ]
            map prepName (bootOrder rules)
                `shouldBe` ["DenyInstallTimeExecution", "AllowScope", "AllowIfOlderThan"]
        it "breaks an equal-precedence tie by name ascending" $ do
            rules <-
                prepare
                    inertRuleDeps
                    [ at 200 (AllowScope (mkScope "myorg"))
                    , at 200 (AllowIfOlderThan (7 * nominalDay))
                    ]
            map prepName (bootOrder rules)
                `shouldBe` ["AllowIfOlderThan", "AllowScope"]

    describe "renderBootOrder" $ do
        it "emits one line per rule, in boot order, with each precedence and the phases it applies at" $
            renderBootOrder
                [ at 100 (AllowIfOlderThan (7 * nominalDay))
                , admissionOnly (at 300 DenyInstallTimeExecution)
                ]
                `shouldBe` [ "rule 1: DenyInstallTimeExecution (precedence 300, applies at admission only)"
                           , "rule 2: AllowIfOlderThan (precedence 100, applies at admission and revocation)"
                           ]
        it "lists the rules in the order the engine evaluates the ones prepared from them" $ do
            -- Equal precedences resolve by name, and a repeated type keeps its given order.
            let policy =
                    [ at 200 (AllowScope (mkScope "myorg"))
                    , at 200 (AllowIfOlderThan (7 * nominalDay))
                    , admissionOnly (at 400 (DenyByIdentity "thing"))
                    , at 400 (DenyByIdentity "other")
                    , at 300 DenyInstallTimeExecution
                    ]
            evaluated <- bootOrder <$> prepare inertRuleDeps policy
            map prepName evaluated
                `shouldBe` ["DenyByIdentity", "DenyByIdentity", "DenyInstallTimeExecution", "AllowIfOlderThan", "AllowScope"]
            renderBootOrder policy
                `shouldBe` [ "rule 1: DenyByIdentity (precedence 400, applies at admission only)"
                           , "rule 2: DenyByIdentity (precedence 400, applies at admission and revocation)"
                           , "rule 3: DenyInstallTimeExecution (precedence 300, applies at admission and revocation)"
                           , "rule 4: AllowIfOlderThan (precedence 200, applies at admission and revocation)"
                           , "rule 5: AllowScope (precedence 200, applies at admission and revocation)"
                           ]
        it "is empty for an empty rule set" $
            renderBootOrder [] `shouldBe` []

    describe "evalRules" $ do
        it "denies by default with no rules" $
            decide [] (pkg (Just "myorg") 99) >>= (`shouldBe` BlockedByDefault [])
        it "admits via the single matching allow rule" $
            decide [atDefaultPrecedence (AllowScope (mkScope "myorg"))] (pkg (Just "myorg") 0)
                >>= \d -> admittedBy d `shouldBe` Just "AllowScope"
        it "the higher-precedence allow wins among allows" $
            -- The version is too young for the age rule, but the scope rule matches.
            -- At default precedences the scope allow outranks it anyway.
            decide
                (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), AllowScope (mkScope "myorg")])
                (pkg (Just "myorg") 0)
                >>= \d -> admittedBy d `shouldBe` Just "AllowScope"
        it "a matching deny rule overrides an allow at default precedence, whatever the order" $ do
            let rs = map atDefaultPrecedence [AllowScope (mkScope "myorg"), DenyInstallTimeExecution]
                p = withInstallScripts (pkg (Just "myorg") 99)
            decide rs p >>= \d -> blockedBy d `shouldBe` Just "DenyInstallTimeExecution"
            decide (reverse rs) p >>= \d -> blockedBy d `shouldBe` Just "DenyInstallTimeExecution"
        it "resolves an equal-precedence allow-vs-deny tie by name, not by deny-priority" $ do
            -- Equal explicit precedence uses the rule name as its tiebreak.
            let rs = [at 300 (AllowScope (mkScope "myorg")), at 300 DenyInstallTimeExecution]
                p = withInstallScripts (pkg (Just "myorg") 99)
            decide rs p >>= \d -> admittedBy d `shouldBe` Just "AllowScope"
            decide (reverse rs) p >>= \d -> admittedBy d `shouldBe` Just "AllowScope"
        it "breaks an equal-precedence allow-vs-allow tie by name, regardless of order" $ do
            -- The boot order breaks an equal-precedence tie by the smallest ruleName, not by list
            -- position. "AllowIfOlderThan" sorts before "AllowScope", so it takes the credit.
            let allows =
                    [ at 150 (AllowScope (mkScope "myorg"))
                    , at 150 (AllowIfOlderThan (7 * nominalDay))
                    ]
                p = pkg (Just "myorg") 30
            decide allows p >>= \d -> admittedBy d `shouldBe` Just "AllowIfOlderThan"
            decide (reverse allows) p >>= \d -> admittedBy d `shouldBe` Just "AllowIfOlderThan"
        it "an operator-elevated allow outranks a higher-default deny" $
            -- The operator lifts the scope allow above the deny's default precedence,
            -- so the engine admits a trusted internal scope despite its install scripts.
            decide
                [ at (defaultDenyInstallTimeExecutionPrecedence + 1) (AllowScope (mkScope "myorg"))
                , atDefaultPrecedence DenyInstallTimeExecution
                ]
                (withInstallScripts (pkg (Just "myorg") 99))
                >>= \d -> admittedBy d `shouldBe` Just "AllowScope"
        it "DenyByIdentity outranks an AllowScope for the same name" $ do
            -- Precedence test: DenyByIdentity (400) outranks AllowScope (200)
            let rs = map atDefaultPrecedence [AllowScope (mkScope "myorg"), DenyByIdentity "@myorg/thing"]
                p = pkg (Just "myorg") 0
            decide rs p >>= \d -> blockedBy d `shouldBe` Just "DenyByIdentity"
            decide (reverse rs) p >>= \d -> blockedBy d `shouldBe` Just "DenyByIdentity"
        it "the remediation fast lane admits a young fix ahead of the quarantine" $
            -- The whole point of the rule: the engine admits a security patch too young
            -- for min-age because an advisory names it as the exact fix.
            decideWith
                (depsWith fixRows)
                (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), shippedRemediation])
                (pkg Nothing 0)
                >>= \d -> admittedBy d `shouldBe` Just "AllowIfRemediatesCve"
        it "the shipped limits admit a young partial fix ahead of the quarantine, and abstain past them" $ do
            let policy = map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), shippedRemediation]
            decideWith (depsWith (partialFixRows 4.2)) policy (pkg Nothing 0)
                >>= \d -> admittedBy d `shouldBe` Just "AllowIfRemediatesCve"
            decideWith (depsWith (partialFixRows 7.0)) policy (pkg Nothing 0)
                >>= (`shouldSatisfy` isBlockedByDefault)
            decideWith (depsWith (partialFixRows 4.2)) (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), remediationWith FailDeny AbstainOnRemaining]) (pkg Nothing 0)
                >>= (`shouldSatisfy` isBlockedByDefault)
        it "a failing advisory lookup under skip leaves the quarantine to govern, and nothing turns Undecidable" $ do
            -- An unconfirmable remediation costs the fix its fast lane, never availability and never an admission.
            let broken = faultingDeps "advisory database exploded"
                policy = map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), differentialRemediation FailNoDecision]
            decideWith broken policy (pkg Nothing 30)
                >>= \d -> admittedBy d `shouldBe` Just "AllowIfOlderThan"
            decideWith broken policy (pkg Nothing 1) >>= \case
                BlockedByDefault _ -> pass
                other -> expectationFailure ("expected BlockedByDefault, got " <> show other)
        it "a failing advisory lookup under deny refuses even a version the quarantine would admit" $ do
            let policy = map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), shippedRemediation]
            for_ [pkg Nothing 1, pkg Nothing 30] $
                decideWith (faultingDeps "advisory database exploded") policy >=> (`shouldSatisfy` isUndecidable)
        it "a stricter advisory deny blocks a partial fix the shipped limits admit" $ do
            let rows = depsWith (partialFixRows 4.2)
                policy deny = map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), shippedRemediation, deny]
            admittedBy <$> decideWith rows (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), shippedRemediation]) (pkg Nothing 0)
                `shouldReturn` Just "AllowIfRemediatesCve"
            blockedBy <$> decideWith rows (policy (denyCveAt 4.0)) (pkg Nothing 0) `shouldReturn` Just "DenyIfCve"
            blockedBy <$> decideWith rows (policy (denyEpssAt 0.005)) (pkg Nothing 0) `shouldReturn` Just "DenyIfEpss"
        it "denies by default when every rule is non-decisive, collecting each reason in boot order" $
            -- The audit trail carries each non-decisive rule's reason in boot order, highest
            -- precedence first: AllowScope (200) then AllowIfOlderThan (100).
            decide
                (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), AllowScope (mkScope "myorg")])
                (pkg (Just "other") 1)
                >>= \decision -> do
                    decision `shouldBe` BlockedByDefault [ScopeNotAllowListed (mkScope "myorg"), PublishedTooRecently nominalDay (7 * nominalDay)]
                    sentences decision
                        `shouldBe` [ "scope is not the allow-listed @myorg"
                                   , "published only 1 day ago, minimum age is 7 days"
                                   ]
        it "abstains on an undetermined install signal and lets a lower age allow admit" $
            decide [at 300 DenyInstallTimeExecution, at 100 (AllowIfOlderThan 0)] (pkg Nothing 30){evInstallCode = Known CodeExecUnknown}
                >>= \d -> admittedBy d `shouldBe` Just "AllowIfOlderThan"

    describe "verdict exceptions" $ do
        let versions = [(pkg Nothing 30){evVersion = v} | v <- map (mkVersion Npm) ["1.0.0", "2.0.0", "3.0.0"]]
            poisoned = mkVersion Npm "2.0.0"
            refusal name = Undecidable (WillResolve Nothing) (RuleUnable name (RuleThrew (show thrownVerdict)))
            healthy = Admitted "AllowIfOlderThan" (PublishedLongEnough (30 * nominalDay) (7 * nominalDay)) []
            bombVerdict ev = if evVersion ev == poisoned then impureThrow thrownVerdict else NoDecision FixesNoAdvisory

        for_ poisonedVerdicts $ \(label, verdict) ->
            it ("refuses by name when a per-version rule throws in " <> label) $
                evalRules ctx [constRule "PureBomb" verdict] (pkg Nothing 0)
                    >>= (`shouldMatchDecision` refusal "PureBomb")

        it "refuses only the poisoned verdict across one listing" $ do
            let bomb = PreparedRule "PureBomb" 300 (PerVersion (\_ -> pure . bombVerdict))
            age <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
            decideVersion <- newEvaluator ctx (bomb : age)
            traverse decideVersion versions `shouldReturnDecisions` [healthy, refusal "PureBomb", healthy]

        it "refuses only the poisoned publish time across one listing" $ do
            rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
            decideVersion <- newEvaluator ctx rules
            let evidence = [if evVersion ev == poisoned then ev{evPublishedAt = Known (Just (impureThrow thrownVerdict))} else ev | ev <- versions]
            traverse decideVersion evidence `shouldReturnDecisions` [healthy, refusal "AllowIfOlderThan", healthy]
            evalRules ctx rules (pkg Nothing 30){evPublishedAt = Known (Just (impureThrow thrownVerdict))}
                >>= (`shouldMatchDecision` refusal "AllowIfOlderThan")

        it "refuses only the poisoned advisory verdict without poisoning the shared read" $ do
            lookupCount <- newIORef (0 :: Int)
            let bomb =
                    mapPackageRead
                        (\packageRead -> packageRead{prVerdict = const bombVerdict})
                        (packageRule "AdvisoryBomb" 300 FailNoDecision Nothing (modifyIORef' lookupCount (+ 1)) (NoDecision FixesNoAdvisory))
            age <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
            decideVersion <- newEvaluator ctx (bomb : age)
            traverse decideVersion versions `shouldReturnDecisions` [healthy, refusal "AdvisoryBomb", healthy]
            readIORef lookupCount `shouldReturn` 1

        it "keeps a first-version advisory fault local when the source observer evaluates it" $ do
            reports <- newIORef []
            let reporter = noSourceReporter{reportSource = evaluate >=> (\health -> modifyIORef' reports (health :))}
                bomb =
                    mapPackageRead
                        (\packageRead -> packageRead{prVerdict = const bombVerdict, prReporter = reporter})
                        (packageRule "AdvisoryBomb" 300 FailNoDecision Nothing pass (NoDecision FixesNoAdvisory))
                firstPoisoned = filter ((== poisoned) . evVersion) versions <> filter ((/= poisoned) . evVersion) versions
            age <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
            decideVersion <- newEvaluator ctx (bomb : age)
            traverse decideVersion firstPoisoned `shouldReturnDecisions` [refusal "AdvisoryBomb", healthy, healthy]
            readIORef reports >>= \case
                [SourceUnavailable name (RuleThrew detail)] -> do
                    name `shouldBe` "AdvisoryBomb"
                    detail `shouldHaveExceptionCause` show thrownVerdict
                other -> expectationFailure ("expected one thrown-rule source report, got " <> show other)

    describe "properties" $ do
        it "an empty rule set always denies by default" $
            hedgehog $ do
                mScope <- forAll (Gen.maybe genScope)
                ageDays <- forAll genAgeDays
                d <- liftIO (decide [] (pkg mScope ageDays))
                d === BlockedByDefault []

        it "every rule non-decisive yields deny-by-default" $
            hedgehog $ do
                -- A non-matching scope, a too-young age, and no install scripts leave every rule
                -- non-decisive, whatever the precedences.
                scopeTxt <- forAll genScope
                otherTxt <- forAll (Gen.filter (/= scopeTxt) genScope)
                precs <- forAll (Gen.list (Range.singleton 3) genPrecedence)
                let rules =
                        zipWith
                            at
                            precs
                            [AllowScope (mkScope scopeTxt), AllowIfOlderThan (7 * nominalDay), DenyInstallTimeExecution]
                liftIO (decide rules (pkg (Just otherTxt) 1)) >>= \case
                    BlockedByDefault _ -> H.success
                    other -> H.annotateShow other >> H.failure

        it "the highest-precedence deny wins over any lower allow" $
            hedgehog $ do
                scopeTxt <- forAll genScope
                ageDays <- forAll genAgeDays
                allowPrec <- forAll genPrecedence
                denyPrec <- forAll (Gen.int (Range.linear (allowPrec + 1) (allowPrec + 1000)))
                let rules = [at allowPrec (AllowScope (mkScope scopeTxt)), at denyPrec DenyInstallTimeExecution]
                    p = withInstallScripts (pkg (Just scopeTxt) ageDays)
                d <- liftIO (decide rules p)
                blockedBy d === Just "DenyInstallTimeExecution"

        it "an operator-elevated allow outranks a lower-precedence deny" $
            hedgehog $ do
                scopeTxt <- forAll genScope
                ageDays <- forAll genAgeDays
                denyPrec <- forAll genPrecedence
                allowPrec <- forAll (Gen.int (Range.linear (denyPrec + 1) (denyPrec + 1000)))
                let rules = [at allowPrec (AllowScope (mkScope scopeTxt)), at denyPrec DenyInstallTimeExecution]
                    p = withInstallScripts (pkg (Just scopeTxt) ageDays)
                d <- liftIO (decide rules p)
                admittedBy d === Just "AllowScope"

        it "the decision is invariant under shuffling the rule list" $
            hedgehog $ do
                -- Colliding precedences exercise deterministic rule-name tiebreaks.
                scopeTxt <- forAll genScope
                ageDays <- forAll genAgeDays
                n <- forAll (Gen.int (Range.linear 0 6))
                rules <- forAll (Gen.list (Range.singleton n) (genFiringRule scopeTxt))
                precs <- forAll (Gen.list (Range.singleton n) genPrecedence)
                let preceded = zipWith at precs rules
                    p = withInstallScripts (pkg (Just scopeTxt) ageDays)
                perm <- forAll (Gen.shuffle preceded)
                original <- liftIO (decide preceded p)
                shuffled <- liftIO (decide perm p)
                canonical original === canonical shuffled

        modifyMaxSuccess (const 1000) . it "a default advisory deny that blocks alone still blocks beside any remediation rule" $
            hedgehog $ do
                (rows, remediation) <- forAll (Gen.frequency [(3, (,) <$> genRows ["0.5.0", "1.0.0", "2.0.0"] <*> genRemediation), (1, genLooseFixOfBlocked)])
                deny <- forAll (Gen.element [denyCveAt 7.0, denyEpssAt 0.5, DenyIfKnownMalicious FailDeny])
                let deps = depsWith (map ("thing",) rows)
                alone <- liftIO (decideWith deps [atDefaultPrecedence deny] (pkg Nothing 1))
                remediationAlone <- liftIO (decideWith deps [atDefaultPrecedence remediation] (pkg Nothing 1))
                whole <- liftIO (decideWith deps (map atDefaultPrecedence [deny, remediation, AllowIfOlderThan (7 * nominalDay)]) (pkg Nothing 1))
                H.cover 5 "the deny alone blocks" (isJust (blockedBy alone))
                H.cover 10 "the remediation rule alone admits what the deny blocks" (isJust (blockedBy alone) && isJust (admittedBy remediationAlone))
                when (isJust (blockedBy alone)) (blockedBy whole === blockedBy alone)

        it "a matching scope or identity allow decides ahead of any remediation rule, whatever the source state" $
            hedgehog $ do
                allow <- forAll (Gen.element [AllowScope (mkScope "myorg"), AllowByIdentity "@myorg/thing"])
                remediation <- forAll genRemediation
                standing <- forAll Gen.enumBounded
                d <- liftIO (decideWith (sourceStateDeps standing (depsWith (partialFixRows 4.2))) (map atDefaultPrecedence [allow, remediation, AllowIfOlderThan (7 * nominalDay)]) (pkg (Just "myorg") 1))
                admittedBy d === Just (ruleName allow)

        it "the install-script deny always wins at default precedences" $
            hedgehog $ do
                scopeTxt <- forAll genScope
                ageDays <- forAll genAgeDays
                let rules = map atDefaultPrecedence [AllowScope (mkScope scopeTxt), DenyInstallTimeExecution]
                    p = withInstallScripts (pkg (Just scopeTxt) ageDays)
                d <- liftIO (decide rules p)
                blockedBy d === Just "DenyInstallTimeExecution"

    describe "identity-only evidence" $ do
        it "AllowIfOlderThan cannot decide, because nothing read the publish time" $
            evalRule inertRuleDeps ctx (AllowIfOlderThan (7 * nominalDay)) (listed Nothing)
                >>= (`shouldSatisfy` isCannotVet)
        it "DenyInstallTimeExecution cannot decide, because nothing read the install signal" $
            evalRule inertRuleDeps ctx DenyInstallTimeExecution (listed Nothing)
                >>= (`shouldSatisfy` isCannotVet)
        it "a read publish time that is absent still abstains rather than refusing" $
            evalRule inertRuleDeps ctx (AllowIfOlderThan (7 * nominalDay)) (completeEvidence (sampleDetails (mkPackageName Npm Nothing "thing") v1_0_0))
                >>= (`shouldSatisfy` isNoDecision)
        it "an identity deny decides, because identity is all it reads" $
            decide [atDefaultPrecedence (DenyByIdentity "thing@1.0.0")] (listed Nothing)
                >>= \d -> blockedBy d `shouldBe` Just "DenyByIdentity"
        it "a higher-precedence allow whose own facts are present still wins" $
            decide
                [at 500 (AllowScope (mkScope "myorg")), atDefaultPrecedence (DenyByIdentity "@myorg/thing")]
                (listed (Just "myorg"))
                >>= \d -> admittedBy d `shouldBe` Just "AllowScope"
        it "refuses rather than reaching a lower identity deny past an unresolved rule" $
            decide
                [at 500 (AllowIfOlderThan (7 * nominalDay)), atDefaultPrecedence (DenyByIdentity "thing@1.0.0")]
                (listed Nothing)
                >>= (`shouldSatisfy` isUndecidable)
        it "denies by default when no rule is decisive" $
            decide [atDefaultPrecedence (AllowScope (mkScope "myorg"))] (listed Nothing)
                >>= (`shouldSatisfy` isBlockedByDefault)
        it "an advisory deny decides, because identity and the database are all it reads" $
            decideWith (depsWith (affecting (Just 9.8) Nothing)) [atDefaultPrecedence (denyCveAt 8.0)] (listed Nothing)
                >>= \d -> blockedBy d `shouldBe` Just "DenyIfCve"
        it "an affecting advisory with no EPSS score still abstains" $
            decideWith (depsWith (affecting Nothing Nothing)) [atDefaultPrecedence (denyEpssAt 0.5)] (listed Nothing)
                >>= (`shouldSatisfy` isBlockedByDefault)

availabilitySpec :: Spec
availabilitySpec = describe "availability denies" $
    for_ [(DenyIfYanked, Yanked, VersionYanked, VersionNotYanked), (DenyIfDeprecated, Deprecated, VersionDeprecated, VersionNotDeprecated)] $ \(rule, flag, denied, absent) -> do
        for_ [Available, Deprecated, Yanked] $ \actual ->
            it (toString (ruleName rule) <> " reads only its own flag: " <> show actual) $
                evalRule inertRuleDeps ctx rule (pkg Nothing 30){evAvailability = Known actual}
                    `shouldReturn` if actual == flag then Deny Nothing denied else NoDecision absent
        it (toString (ruleName rule) <> " refuses unread availability") $
            evalRule inertRuleDeps ctx rule (listed Nothing) `shouldReturn` CannotVet FailDeny AvailabilityUnread
        it (toString (ruleName rule) <> " outranks a default identity allow and permits an explicit override") $ do
            let evidence = (pkg Nothing 30){evAvailability = Known flag}
            blockedBy <$> decide (map atDefaultPrecedence [rule, AllowByIdentity "thing"]) evidence `shouldReturn` Just (ruleName rule)
            admittedBy <$> decide [atDefaultPrecedence rule, atPrecedence 350 (AllowByIdentity "thing")] evidence `shouldReturn` Just "AllowByIdentity"

knownMaliciousSpec :: Spec
knownMaliciousSpec = describe "DenyIfKnownMalicious" $ do
    let mal = DenyIfKnownMalicious FailDeny
        row ident severity = AdvisoryRange ident severity (Just "0") (FixedBefore "2.0.0") Nothing
        evidence = pkg Nothing 99
        known = KnownMalicious (mkAdvisoryIds ("MAL-test" :| []))
    for_ [Nothing, Just 0, Just 9.8] $ \severity ->
        it ("denies primary MAL evidence with CVSS " <> show severity <> " and no EPSS") $
            evalRule (depsWith [("thing", row "MAL-test" severity)]) ctx mal evidence
                `shouldReturn` Deny (Just (DbEtag "etag-1")) known
    for_ ["GHSA-test", "mal-test", "MAL-", "MAL", "X-MAL-test"] $ \ident ->
        it ("does not classify the unscored primary identifier " <> toString ident) $
            evalRule (depsWith [("thing", row ident Nothing)]) ctx mal evidence
                `shouldReturn` NoDecision NoKnownMaliciousAdvisory
    it "deduplicates affecting MAL identifiers and excludes non-MAL evidence" $
        evalRule (depsWith [("thing", row ident Nothing) | ident <- ["MAL-test", "MAL-test", "GHSA-other", "MAL-second"]]) ctx mal evidence
            `shouldReturn` Deny (Just (DbEtag "etag-1")) (KnownMalicious (mkAdvisoryIds ("MAL-test" :| ["MAL-second"])))
    for_ [Npm, PyPI] $ \eco ->
        for_ [("1.0.0", True), ("1.5.0", True), ("2.0.0", False), ("3.0.0", True), ("3.1.0", False)] $ \(version, denied) ->
            it ("matches " <> show eco <> " disjoint range and point evidence at " <> toString version) $ do
                let name = mkPackageName eco Nothing "thing"
                    rows = [("thing", row "MAL-range" Nothing), ("thing", AdvisoryRange "MAL-point" Nothing (Just "3.0.0") (LastAffected "3.0.0") Nothing)]
                verdict <- evalRule (depsWith rows) ctx mal (identityEvidence name (mkVersion eco version))
                isDeny verdict `shouldBe` denied
    for_ [id, reverse] $ \order -> do
        it "outranks a default identity allow independently of input order" $
            decideWith (depsWith [("thing", row "MAL-test" Nothing)]) (order (map atDefaultPrecedence [mal, AllowByIdentity "thing"])) evidence
                `shouldReturn` Blocked "DenyIfKnownMalicious" (Just (DbEtag "etag-1")) known
        it "yields to an explicit identity allow above 300" $ do
            result <- decideWith (depsWith [("thing", row "MAL-test" Nothing)]) (order [atDefaultPrecedence mal, at 301 (AllowByIdentity "thing")]) evidence
            admittedBy result `shouldBe` Just "AllowByIdentity"
        it "yields to a default identity deny at 400" $
            blockedBy <$> decideWith (depsWith [("thing", row "MAL-test" Nothing)]) (order (map atDefaultPrecedence [mal, DenyByIdentity "thing"])) evidence
                `shouldReturn` Just "DenyByIdentity"
        for_ [(DenyIfDeprecated, Deprecated, "DenyIfDeprecated"), (DenyIfYanked, Yanked, "DenyIfKnownMalicious")] $ \(other, availability, winner) ->
            it ("orders an equal-precedence deny at 300 by rule name: " <> toString (ruleName other)) $
                blockedBy <$> decideWith (depsWith [("thing", row "MAL-test" Nothing)]) (order (map atDefaultPrecedence [mal, other])) evidence{evAvailability = Known availability}
                    `shouldReturn` Just winner
    for_ [FailDeny, FailNoDecision] $ \alignment -> do
        it ("uses " <> show alignment <> " before the first generation") $
            evalRule unloadedDeps ctx (DenyIfKnownMalicious alignment) evidence
                `shouldReturn` CannotVet alignment NoDatabaseLoaded
        it ("retains " <> show alignment <> " on a faulted read") $ do
            result <- decideWith (faultingDeps "MAL read failed") (map atDefaultPrecedence [DenyIfKnownMalicious alignment, AllowByIdentity "thing"]) evidence
            case alignment of
                FailDeny -> result `shouldSatisfy` isUndecidable
                FailNoDecision -> skippedChecks result `shouldSatisfy` (not . null)
    it "queries canonical PyPI identity without requiring metadata facts" $ do
        let rows = [("flask-thing", row "MAL-test" Nothing)]
        evalRule (depsWith rows) ctx mal (identityEvidence (unscopedPyPI "Flask_Thing") (mkVersion PyPI "1.0"))
            `shouldReturn` Deny (Just (DbEtag "etag-1")) known

    for_ [("active", Nothing, True), ("withdrawn", Just (String "2026-10-01T00:00:00Z"), False)] $ \(label, withdrawn, denied) ->
        it ("compiles " <> label <> " MAL points through an epoch-5 artifact") $ do
            archive <- maliciousZip Npm [("thing", ["1.0.0"]), ("chalk", ["5.6.1"])] withdrawn
            withOsvZipDb Npm archive $ \path -> withServedArtifact Npm path $ \deps _ -> do
                verdict <- evalRule deps ctx mal evidence
                isDeny verdict `shouldBe` denied
                evalRule deps ctx mal (identityEvidence (unscopedNpm "chalk") (mkVersion Npm "5.6.1"))
                    >>= (`shouldSatisfy` isDeny)

    it "shares one canonical lookup across MAL, CVSS, EPSS, remediation, and all versions" $ do
        lookedUpNames <- newIORef ([] :: [Text])
        let lookupRows name = modifyIORef' lookedUpNames (<> [name]) $> []
            deps = servingRuleDeps (DbEtag "shared") (CveLookup lookupRows (pure []))
        rules <- prepare deps (map atDefaultPrecedence [mal, denyCveAt 8, denyEpssAt 0.5, shippedRemediation, AllowIfOlderThan 0])
        decideVersion <- newEvaluator ctx rules
        void (mapConcurrently (\v -> decideVersion evidence{evVersion = mkVersion Npm v}) ["1.0.0", "2.0.0", "3.0.0"])
        readIORef lookedUpNames `shouldReturn` ["thing"]
        void (decideVersion evidence{evName = unscopedNpm "other"})
        void (decideVersion evidence)
        readIORef lookedUpNames `shouldReturn` ["thing", "other", "thing"]
    it "keeps the borrowed MAL generation after a slot changes between versions" $ do
        let rows = [("thing", AdvisoryRange "MAL-pinned" Nothing (Just "2.0.0") (LastAffected "2.0.0") Nothing)]
        held <- newIORef (Just (DbEtag "before", fakeCveLookup rows))
        let deps = inertRuleDeps{rdAdvisoryDatabase = staticAdvisoryDatabase (pure AdvisoryFresh) (\use -> readIORef held >>= use)}
        rules <- prepare deps (map atDefaultPrecedence [mal, denyCveAt 8, denyEpssAt 0.5, shippedRemediation, AllowIfOlderThan 0])
        decideVersion <- newEvaluator ctx rules
        void (decideVersion evidence)
        writeIORef held (Just (DbEtag "after", fakeCveLookup []))
        decideVersion evidence{evVersion = mkVersion Npm "2.0.0"}
            `shouldReturn` Blocked "DenyIfKnownMalicious" (Just (DbEtag "before")) (KnownMalicious (mkAdvisoryIds ("MAL-pinned" :| [])))
        fresh <- newEvaluator ctx rules
        fresh evidence{evVersion = mkVersion Npm "2.0.0"} >>= (\decision -> admittedBy decision `shouldBe` Just "AllowIfOlderThan")

    it "keeps one borrowed generation for the remediation differential after a slot changes" $ do
        held <- newIORef (Just (DbEtag "before", fakeCveLookup (partialFixRows 4.2)))
        let deps = inertRuleDeps{rdAdvisoryDatabase = staticAdvisoryDatabase (pure AdvisoryFresh) (\use -> readIORef held >>= use)}
            young = pkg Nothing 0
        rules <- prepare deps (map atDefaultPrecedence [mal, denyCveAt 8, denyEpssAt 0.5, shippedRemediation, AllowIfOlderThan (7 * nominalDay)])
        decideVersion <- newEvaluator ctx rules
        blockedBy <$> decideVersion young{evVersion = mkVersion Npm "0.6.0"} `shouldReturn` Just "DenyIfCve"
        writeIORef held (Just (DbEtag "after", fakeCveLookup (partialFixRows 7.0)))
        admittedBy <$> decideVersion young `shouldReturn` Just "AllowIfRemediatesCve"
        fresh <- newEvaluator ctx rules
        fresh young >>= (`shouldSatisfy` isBlockedByDefault)

    for_ [Available, Deprecated, Yanked] $ \availability ->
        it ("does not treat registry availability as MAL evidence: " <> show availability) $
            evalRule (depsWith []) ctx mal evidence{evAvailability = Known availability}
                `shouldReturn` NoDecision NoKnownMaliciousAdvisory
