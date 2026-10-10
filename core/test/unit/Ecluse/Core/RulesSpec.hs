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
import Data.Time (NominalDiffTime, addUTCTime, nominalDay)
import Data.Universe.Class (Universe (universe))
import Database.SQLite.Simple (Connection, Only, query, withConnection)
import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import UnliftIO.Exception (finally, throwIO)

import Ecluse.Core.Breaker (Breaker, initialBreaker, recordFailure)
import Ecluse.Core.Cve (AdvisoryRange (..), CveDb (..), CveLookup (..), MissingScorePolicy (..), openCveDb, scoreAtLeast)
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssOptional))
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore, LastAffected, Unbounded))
import Ecluse.Core.Package
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.Test.Cve (fakeCveLookup, referenceInside, unscoredEpssCases)
import Ecluse.Test.Osv (CorpusVersion (CorpusV2), RangeRow, mkValidDbWithRows)
import Ecluse.Test.Osv.Withdrawal (withdrawalZip)
import Ecluse.Test.OsvDb (withFixtureOsvDb, withOsvZipDb)
import Ecluse.Test.Package (sampleDetails, scopedNpm, unscopedNpm, unscopedPyPI, v1_0_0)
import Ecluse.Test.Rules (
    admittedBy,
    atDefaultPrecedence,
    blockedBy,
    evalRule,
    inertRuleDeps,
    isAllow,
    isBlockedByDefault,
    isCannotVet,
    isDeny,
    isNoDecision,
    isUndecidable,
    mapResilience,
    sentences,
    servingRuleDeps,
    verdictSentence,
    withInstallScripts,
 )
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

import Ecluse.Core.Rules
import Ecluse.Core.Rules.Effectful (EffectfulConfig (ecBreakerCooldown, ecBreakerThreshold))
import Ecluse.Core.Rules.Freshness
import Ecluse.Core.Rules.Render (renderDecision, renderInability, renderReason)
import Ecluse.Core.Rules.Types
import Ecluse.Rules.Support (ctx, now, pkg, sixDayLimit)

-- | Identity alone for the fixture package, the evidence an authenticated store listing carries.
listed :: Maybe Text -> RuleEvidence
listed mScope = identityEvidence (mkPackageName Npm (mkScope <$> mScope) "thing") v1_0_0

-- | Put a rule at an explicit precedence (the operator-override form).
at :: Int -> Rule -> PrecededRule
at = PrecededRule

{- | Decide a policy through the one engine ('prepare' then 'evalRules') under the
given capabilities.
-}
decideWith :: RuleDeps -> [PrecededRule] -> RuleEvidence -> IO Decision
decideWith deps prs ev = prepare deps prs >>= \prepared -> evalRules ctx prepared ev

-- | 'decideWith' for the pure built-ins, which consult no capability.
decide :: [PrecededRule] -> RuleEvidence -> IO Decision
decide = decideWith inertRuleDeps

-- | Expect the verdict, and the sentence a reader sees for its reason.
shouldDecide :: IO RuleVerdict -> (RuleVerdict, Text) -> Expectation
shouldDecide evaluated (verdict, sentence) = do
    actual <- evaluated
    actual `shouldBe` verdict
    verdictSentence actual `shouldBe` sentence

-- | Rule capabilities whose advisory database is the given fake's rows.
depsWith :: [(Text, AdvisoryRange)] -> RuleDeps
depsWith rows = servingRuleDeps (DbEtag "etag-1") (fakeCveLookup rows)

-- | Rule capabilities with a database configured and no generation loaded, as before the first sync.
unloadedDeps :: RuleDeps
unloadedDeps = inertRuleDeps{rdAdvisoryDatabase = AdvisoryDatabase (\use -> use Nothing)}

-- | Rule capabilities with a database configured whose every lookup throws.
faultingDeps :: Text -> RuleDeps
faultingDeps detail = inertRuleDeps{rdAdvisoryDatabase = AdvisoryDatabase (\_ -> throwIO (TestContractEscape detail))}

{- | One advisory naming @thing\@1.0.0@ (the version 'pkg' builds) as its exact
fixed bound, with no other advisory leaving the package affected.
-}
fixRows :: [(Text, AdvisoryRange)]
fixRows = [("thing", AdvisoryRange "GHSA-fixed-0001" Nothing (Just "0") (FixedBefore "1.0.0") Nothing)]

{- | One advisory covering @[0, 2.0.0)@, so it affects the version 'pkg' builds, carrying the
CVSS and EPSS scores under test.
-}
affecting :: Maybe Double -> Maybe Double -> [(Text, AdvisoryRange)]
affecting severity epss = [("thing", AdvisoryRange "GHSA-affect-0001" severity (Just "0") (FixedBefore "2.0.0") epss)]

denyCveAt :: Double -> Rule
denyCveAt threshold = DenyIfCve (DenyIfCveParams threshold FailDeny)

denyEpssAt :: Double -> Rule
denyEpssAt threshold = DenyIfEpss (DenyIfEpssParams threshold FailDeny)

genScope :: Gen Text
genScope = Gen.text (Range.linear 1 12) Gen.alpha

genAgeDays :: Gen Integer
genAgeDays = Gen.integral (Range.linear 0 3650)

genPrecedence :: Gen Int
genPrecedence = Gen.int (Range.linear 0 1000)

{- | A rule that fires on the package the order-independence property builds: scoped
@scopeTxt@, old, and running install scripts. None yields no decision, so every rule competes.
-}
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
pushedAgo age deps =
    deps{rdAdvisoryFreshness = pure (assessAdvisoryAge sixDayLimit now (PublishedAt (addUTCTime (negate age) now)))}

-- | Capabilities serving a generation the store gave no publication time for.
undated :: RuleDeps -> RuleDeps
undated deps = deps{rdAdvisoryFreshness = pure (assessAdvisoryAge sixDayLimit now UndatedGeneration)}

-- | Capabilities whose serving artifact is three days past the six-day maximum.
expired :: RuleDeps -> RuleDeps
expired = pushedAgo (9 * nominalDay)

{- | The same rule with its breaker already open at 'now'. An open breaker fast-fails the rule's
own IO, so it establishes that the push-age gate runs ahead of breaker admission.
-}
openBreakerOn :: PreparedRule -> IO PreparedRule
openBreakerOn rule = case prepResilience rule of
    Nothing -> pure rule
    Just res -> do
        tripped <- newTVarIO (trippedBreaker (resConfig res))
        pure (mapResilience (\held -> held{resBreaker = tripped, resClock = pure now}) rule)

trippedBreaker :: EffectfulConfig -> Breaker
trippedBreaker cfg = foldl' recordOne initialBreaker [1 .. ecBreakerThreshold cfg]
  where
    recordOne breaker _ = recordFailure (ecBreakerThreshold cfg) (ecBreakerCooldown cfg) now breaker

expirySpec :: Spec
expirySpec = describe "an expired advisory push" $ do
    for_ [DenyIfCve (DenyIfCveParams 8.0 FailNoDecision), DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision)] $ \rule ->
        it (toString (ruleName rule <> " refuses under onUnavailable: skip, past a lower age allow")) $
            decideWith
                (expired (depsWith (affecting (Just 9.8) (Just 0.9))))
                (map atDefaultPrecedence [rule, AllowIfOlderThan (7 * nominalDay)])
                (pkg Nothing 99)
                >>= (`shouldSatisfy` isUndecidable)

    for_ [DenyIfCve (DenyIfCveParams 8.0 FailNoDecision), DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision)] $ \rule ->
        it (toString (ruleName rule <> " refuses on a serving generation with no publication time")) $
            decideWith
                (undated (depsWith (affecting (Just 9.8) (Just 0.9))))
                (map atDefaultPrecedence [rule, AllowIfOlderThan (7 * nominalDay)])
                (pkg Nothing 99)
                >>= (`shouldSatisfy` isUndecidable)

    it "abstains on the remediation allow when the push carries no publication time" $
        decideWith (undated (depsWith fixRows)) [atDefaultPrecedence AllowIfRemediatesCve] (pkg Nothing 0)
            >>= (`shouldSatisfy` isBlockedByDefault)

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

    it "abstains on the remediation allow rather than admitting on expired evidence" $
        decideWith (expired (depsWith fixRows)) [atDefaultPrecedence AllowIfRemediatesCve] (pkg Nothing 0)
            >>= (`shouldSatisfy` isBlockedByDefault)

    it "abstains on a remediation allow ranked above the deny, so the refusal takes effect" $
        decideWith
            (expired (depsWith fixRows))
            [at 300 AllowIfRemediatesCve, atDefaultPrecedence (denyCveAt 8.0)]
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

    it "keeps the remediation allow an expired push abstained, since a deny refuses on expiry instead" $ do
        decision <- decideWith (expired (depsWith fixRows)) (map atDefaultPrecedence [AllowIfRemediatesCve, AllowIfOlderThan (7 * nominalDay)]) (pkg Nothing 30)
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
                (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision), AllowScope (mkScope "myorg"), DenyIfCve (DenyIfCveParams 8.0 FailNoDecision), AllowIfRemediatesCve])
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

    it "does not turn an unavailable check into an admission by itself" $
        -- Nothing else admits a young version, so the skipped deny leaves deny-by-default standing.
        decideWith inertRuleDeps skipPolicy (pkg Nothing 1) >>= (`shouldSatisfy` isBlockedByDefault)

sourceHealthSpec :: Spec
sourceHealthSpec = describe "advisory source health reporting" $ do
    it "reports an absent database as unavailable, and a loaded one as answered, once per advisory rule" $ do
        (absent, absentReports) <- observedDeps inertRuleDeps
        void (decideWith absent skipPolicy (pkg Nothing 30))
        reported absentReports `shouldReturn` [SourceUnavailable "DenyIfCve" NoDatabaseLoaded]
        (loaded, loadedReports) <- observedDeps (depsWith [])
        void (decideWith loaded skipPolicy (pkg Nothing 30))
        reported loadedReports `shouldReturn` [SourceAnswered "DenyIfCve"]

    it "reports under a fail-closed alignment too, so a refusing outage is observed" $ do
        (deps, reports) <- observedDeps inertRuleDeps
        void (decideWith deps [atDefaultPrecedence (denyCveAt 8.0)] (pkg Nothing 30))
        reported reports `shouldReturn` [SourceUnavailable "DenyIfCve" NoDatabaseLoaded]

    it "reports an expired push as unavailable with its cause" $ do
        (deps, reports) <- observedDeps (expired (depsWith []))
        void (decideWith deps [atDefaultPrecedence (denyCveAt 8.0)] (pkg Nothing 30))
        reported reports >>= \case
            [SourceUnavailable "DenyIfCve" cause] -> renderInability cause `shouldSatisfy` T.isInfixOf "past the maximum"
            other -> expectationFailure ("expected one unavailability, got " <> show other)

    it "reports a lookup fault once, with its detail, and nothing again for the decided verdict" $ do
        (deps, reports) <- observedDeps (faultingDeps "advisory database exploded")
        void (decideWith deps skipPolicy (pkg Nothing 30))
        reported reports >>= \case
            [SourceUnavailable "DenyIfCve" detail] -> renderInability detail `shouldSatisfy` T.isInfixOf "advisory database exploded"
            other -> expectationFailure ("expected one unavailability, got " <> show other)

    it "reports nothing for a pure rule, which reads no source" $ do
        (deps, reports) <- observedDeps inertRuleDeps
        void (decideWith deps (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), DenyInstallTimeExecution]) (pkg Nothing 30))
        reported reports `shouldReturn` []

    it "reports the remediation allow as answered without a database, because abstaining is its verdict" $ do
        (deps, reports) <- observedDeps inertRuleDeps
        void (decideWith deps [atDefaultPrecedence AllowIfRemediatesCve] (pkg Nothing 30))
        reported reports `shouldReturn` [SourceAnswered "AllowIfRemediatesCve"]

{- | Each advisory rule's verdict when no generation answers, with its decision's sentence, verbatim.
A deployment with no database configured and one awaiting its first sync must read the same.
-}
noDatabaseVerdicts :: [(Text, Rule, RuleVerdict, Text)]
noDatabaseVerdicts =
    [ ("AllowIfRemediatesCve", AllowIfRemediatesCve, NoDecision NoDatabaseToRemediate, "no advisory database is loaded")
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
        let shipped = map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), AllowIfRemediatesCve]
        for_ [pkg Nothing 1, pkg Nothing 30] $ \ev -> do
            unconfigured <- decideWith inertRuleDeps shipped ev
            decideWith unloadedDeps shipped ev >>= (`shouldBe` unconfigured)

{- | The reference: an SQL fix probe and a range read for every version, each range checked by parsing
the version and its bounds again. One read per request must reproduce it verdict for verdict.
-}
perVersionVerdict :: DbEtag -> (Text -> Text -> IO Bool) -> CveLookup -> Rule -> RuleEvidence -> IO RuleVerdict
perVersionVerdict etag probe cve rule ev = case rule of
    AllowIfRemediatesCve ->
        probe name version >>= \case
            False -> pure (NoDecision FixesNoAdvisory)
            True -> remediation <$> cveAdvisoriesFor cve name
    DenyIfCve params -> deny DenyMissingScore Cvss (dicMinCvss params) arSeverity <$> cveAdvisoriesFor cve name
    DenyIfEpss params -> deny AbstainMissingScore Epss (dieMinEpss params) arEpss <$> cveAdvisoriesFor cve name
    other -> fail ("not an advisory rule: " <> show other)
  where
    eco = pkgEcosystem (evName ev)
    name = TS.toText (pkgCanonical (evName ev))
    version = renderVersion (evVersion ev)
    remediation ranges =
        let remediated = ordNub [arCveId ar | ar <- ranges, arUpperBound ar == FixedBefore version]
            stillOpen = ordNub [arCveId ar | ar <- ranges, referenceInside eco version ar]
         in case (mkAdvisoryIds <$> nonEmpty remediated, mkAdvisoryIds <$> nonEmpty stillOpen) of
                (Nothing, _) -> NoDecision FixesNoAdvisory
                (Just fixed, Just open) -> NoDecision (FixesButStillAffected fixed open)
                (Just fixed, Nothing) -> Allow (Remediates fixed)
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
        [PreparedRule{prepEval = PerPackage packageRead}] -> (\rows -> map (prVerdict packageRead rows) versions) <$> prRows packageRead name
        _ -> fail "expected one prepared package read"

-- | Every advisory rule, at thresholds on, between, and past the fixtures' scores.
differentialRules :: [Rule]
differentialRules =
    AllowIfRemediatesCve
        : [DenyIfCve (DenyIfCveParams threshold alignment) | threshold <- [0, 5.0, 7.0, 9.9], alignment <- [FailDeny, FailNoDecision]]
            <> [DenyIfEpss (DenyIfEpssParams threshold FailDeny) | threshold <- [0, 0.25, 0.5, 0.95, 1]]

{- | Rows covering every bound shape the reader decodes: segment pairs, both bound columns on one
row, points, prereleases, build metadata, "v" prefixes, unparseable bounds, and missing scores.
-}
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

{- | Every rule decides each version from one read as it does from a read per version, and a mixed
policy's shared evaluator matches a fresh one per version. Returns the verdicts compared.
-}
agreesOn :: (Text -> Text -> IO Bool) -> CveLookup -> [PackageName] -> IO [RuleVerdict]
agreesOn probe cve names =
    fmap (concat . concat) . forM (names <> [unscopedNpm "no-such-package"]) $ \name -> do
        -- A version built under another ecosystem's grammar reaches the matcher's re-parse.
        versions <- (\vs -> [(v, builtUnder) | v <- vs, builtUnder <- universe]) <$> versionsFor cve name
        let evidence = [completeEvidence (sampleDetails name (mkVersion builtUnder v)) | (v, builtUnder) <- versions]
        compared <- forM differentialRules $ \rule -> do
            perVersion <- traverse (perVersionVerdict etag probe cve rule) evidence
            perRequest <- perRequestVerdicts deps rule name evidence
            zip versions perRequest `shouldBe` zip versions perVersion
            pure perVersion
        rules <- prepare deps (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), AllowIfRemediatesCve, denyCveAt 7.0, denyEpssAt 0.5])
        shared <- newEvaluator ctx rules >>= \decideShared -> traverse decideShared evidence
        fresh <- traverse (evalRules ctx rules) evidence
        zip versions shared `shouldBe` zip versions fresh
        pure compared
  where
    etag = DbEtag "differential"
    deps = servingRuleDeps etag cve

-- | Open an accepted artifact and lend the reference's fix probe and the lookup, then close both.
withOpened :: FilePath -> ((Text -> Text -> IO Bool) -> CveLookup -> IO a) -> IO a
withOpened path use =
    openCveDb Npm EpssOptional path >>= \case
        Left rejection -> fail ("differential artifact rejected: " <> show rejection)
        Right db -> withConnection path (\conn -> use (sqlFixProbe conn) (cveDbLookup db)) `finally` cveDbClose db

-- | The names an artifact holds rows for, as npm packages.
coveredPackages :: CveLookup -> IO [PackageName]
coveredPackages cve = map unscopedNpm <$> cveCoveredNames cve

differentialSpec :: Spec
differentialSpec = describe "one read per request against a read per version, over a real artifact" $ do
    it "agrees on hand-built rows covering every bound shape and fix spelling" $
        withSystemTempDirectory "ecluse-rules-differential" $ \dir -> do
            let path = dir </> "osv.db"
            mkValidDbWithRows path [] edgeCaseRows
            verdicts <- withOpened path $ \probe cve -> do
                names <- coveredPackages cve
                agreesOn probe cve (scopedNpm "scope" "pkg" : names)
            -- The rows reach an admission, a denial, a still-affected fix, and a version no advisory fixes.
            verdicts `shouldSatisfy` any isAllow
            verdicts `shouldSatisfy` any isDeny
            map verdictSentence (filter isNoDecision verdicts) `shouldSatisfy` any (T.isInfixOf "but is still affected by")
            verdicts `shouldSatisfy` elem (NoDecision FixesNoAdvisory)
            map verdictSentence verdicts `shouldSatisfy` elem "no advisory names this version as its fix"

    it "agrees on the compiled corpus" $
        withFixtureOsvDb CorpusV2 $
            \path -> withOpened path $ \probe cve -> void (coveredPackages cve >>= agreesOn probe cve)

    for_ [("active", Nothing), ("withdrawn", Just (String "2024-05-14T20:15:44Z"))] $ \(label, withdrawn) ->
        it ("agrees on a compiled archive whose advisory is " <> label) $ do
            archive <- withdrawalZip withdrawn
            withOsvZipDb Npm archive $ \path ->
                withOpened path (\probe cve -> void (agreesOn probe cve (map unscopedNpm ["withdrawal-only", "withdrawal-overlap", "corpus-vuln"])))

-- | The PyPI counterpart of 'pkg': @Flask_Thing\@1.0.0@, published the given days before 'now'.
pypiPkg :: Integer -> RuleEvidence
pypiPkg ageDays = (pkg Nothing ageDays){evName = unscopedPyPI "Flask_Thing"}

{- | Each verdict a built-in rule reaches and the sentence its reason reads as, for both
ecosystems. A rule that gives another rule's reason, or a reworded sentence, fails its row.
-}
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
    , ("AllowIfRemediatesCve on a PyPI project", depsWith [("flask-thing", snd row) | row <- fixRows], AllowIfRemediatesCve, pypiPkg 0, Allow (Remediates (mkAdvisoryIds ("GHSA-fixed-0001" :| []))), "remediates GHSA-fixed-0001")
    ]
  where
    myorg = mkScope "myorg"
    quarantineRule = AllowIfOlderThan (7 * nominalDay)
    unpublished ev = ev{evPublishedAt = Known Nothing}
    pypiAffecting = [("flask-thing", snd row) | row <- affecting (Just 9.8) (Just 0.9)]

ruleSentenceSpec :: Spec
ruleSentenceSpec = describe "the sentence each built-in rule's reason reads as" $ do
    for_ ruleSentences $ \(label, deps, rule, ev, verdict, sentence) ->
        it label $
            evalRule deps ctx rule ev `shouldDecide` (verdict, sentence)

    for_ [(AllowIfOlderThan (7 * nominalDay), "AllowIfOlderThan: the publish time is not available"), (DenyInstallTimeExecution, "DenyInstallTimeExecution: the install-time execution signal is not available")] $ \(rule, sentence) ->
        it (toString (ruleName rule <> " names itself in the refusal an unread fact decides")) $ do
            decision <- decide [atDefaultPrecedence rule] (listed Nothing)
            decision `shouldSatisfy` isUndecidable
            sentences decision `shouldBe` [sentence]

spec :: Spec
spec = do
    expirySpec
    evidenceSpec
    sourceHealthSpec
    noDatabaseSpec
    differentialSpec
    ruleSentenceSpec
    describe "advisory package identity" $ do
        for_ [denyCveAt 0, denyEpssAt 0] $ \rule ->
            it (toString (ruleName rule <> " queries the canonical PyPI name")) $ do
                let pd = pypiPkg 0
                    rows = [("flask-thing", snd row) | row <- affecting (Just 9.8) (Just 0.9)]
                evalRule (depsWith rows) ctx rule pd >>= (`shouldSatisfy` isDeny)

        it "matches a PyPI fix and keeps its display spelling in the decision message" $ do
            let pd = pypiPkg 0
                rows = [("flask-thing", snd row) | row <- fixRows]
            decision <- decideWith (depsWith rows) [atDefaultPrecedence AllowIfRemediatesCve] pd
            admittedBy decision `shouldBe` Just "AllowIfRemediatesCve"
            renderDecision pd decision `shouldSatisfy` T.isInfixOf "Flask_Thing@1.0.0"

        it "does not fast-track a PyPI fix while a canonical-name advisory still affects it" $ do
            let pd = pypiPkg 0
                rows = [("flask-thing", snd row) | row <- fixRows <> affecting Nothing Nothing]
            evalRule (depsWith rows) ctx AllowIfRemediatesCve pd >>= (`shouldSatisfy` isNoDecision)

        for_ [mkPackageName Npm Nothing "Flask_Thing", mkPackageName Npm (Just (mkScope "Acme")) "Flask_Thing"] $ \name ->
            it (toString ("preserves npm identity " <> renderPackageName name)) $ do
                let pd = (pkg Nothing 0){evName = name}
                    exactRows = [(renderPackageName name, snd row) | row <- affecting Nothing Nothing]
                    otherRows = [("flask-thing", snd row) | row <- affecting Nothing Nothing]
                    exactFixes = [(renderPackageName name, snd row) | row <- fixRows]
                evalRule (depsWith exactRows) ctx (denyCveAt 0) pd >>= (`shouldSatisfy` isDeny)
                evalRule (depsWith otherRows) ctx (denyCveAt 0) pd >>= (`shouldSatisfy` isNoDecision)
                evalRule (depsWith exactFixes) ctx AllowIfRemediatesCve pd >>= (`shouldSatisfy` isAllow)

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
            evalRule (depsWith fixRows) ctx AllowIfRemediatesCve (pkg Nothing 0)
                `shouldDecide` (Allow (Remediates (mkAdvisoryIds ("GHSA-fixed-0001" :| []))), "remediates GHSA-fixed-0001")
        it "names every advisory the version fixes in the reason" $ do
            let rows =
                    [ ("thing", AdvisoryRange "GHSA-fixed-0001" Nothing (Just "0") (FixedBefore "1.0.0") Nothing)
                    , ("thing", AdvisoryRange "GHSA-fixed-0002" Nothing (Just "0.2.0") (FixedBefore "1.0.0") Nothing)
                    ]
            evalRule (depsWith rows) ctx AllowIfRemediatesCve (pkg Nothing 0)
                `shouldDecide` (Allow (Remediates (mkAdvisoryIds ("GHSA-fixed-0001" :| ["GHSA-fixed-0002"]))), "remediates GHSA-fixed-0001, GHSA-fixed-0002")
        it "matches the OSV wire form of a scoped name" $ do
            let rows = [("@myorg/thing", AdvisoryRange "GHSA-fixed-0003" Nothing (Just "0") (FixedBefore "1.0.0") Nothing)]
            evalRule (depsWith rows) ctx AllowIfRemediatesCve (pkg (Just "myorg") 0)
                `shouldDecide` (Allow (Remediates (mkAdvisoryIds ("GHSA-fixed-0003" :| []))), "remediates GHSA-fixed-0003")
        it "abstains when no advisory names the version as a fix (exact match only)" $ do
            -- 1.0.0 sits past this advisory's 0.9.0 fix, but the fast lane is a
            -- deliberate exact-fix probe: being merely unaffected earns nothing.
            let rows = [("thing", AdvisoryRange "GHSA-fixed-0001" Nothing (Just "0") (FixedBefore "0.9.0") Nothing)]
            evalRule (depsWith rows) ctx AllowIfRemediatesCve (pkg Nothing 0)
                `shouldDecide` (NoDecision FixesNoAdvisory, "no advisory names this version as its fix")
        it "abstains when the version still sits inside another advisory's affected range" $ do
            let rows =
                    fixRows
                        <> [("thing", AdvisoryRange "GHSA-open-0002" Nothing (Just "0.5.0") Unbounded Nothing)]
            evalRule (depsWith rows) ctx AllowIfRemediatesCve (pkg Nothing 0)
                `shouldDecide` (NoDecision (FixesButStillAffected (mkAdvisoryIds ("GHSA-fixed-0001" :| [])) (mkAdvisoryIds ("GHSA-open-0002" :| []))), "fixes GHSA-fixed-0001 but is still affected by GHSA-open-0002")
        it "abstains when no advisory database is loaded" $
            evalRule inertRuleDeps ctx AllowIfRemediatesCve (pkg Nothing 0)
                `shouldDecide` (NoDecision NoDatabaseToRemediate, "no advisory database is loaded")

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
            let pr = PrecededRule 250 DenyInstallTimeExecution
            rulePrecedence pr `shouldBe` 250
            prRule pr `shouldBe` DenyInstallTimeExecution

    describe "defaultPrecedence" $ do
        it "ranks DenyInstallTimeExecution strictly above every allow default" $ do
            let allows = [AllowScope (mkScope "x"), AllowIfOlderThan 0, AllowByIdentity "x", AllowIfRemediatesCve]
            defaultPrecedence DenyInstallTimeExecution
                `shouldSatisfy` (\d -> all ((d >) . defaultPrecedence) allows)
        it "orders the allow band by explicitness: quarantine < fast lane < scope < identity" $
            ( defaultAllowIfOlderThanPrecedence
            , defaultAllowIfRemediatesCvePrecedence
            , defaultAllowScopePrecedence
            , defaultAllowByIdentityPrecedence
            )
                `shouldSatisfy` (\(q, f, s, i) -> q < f && f < s && s < i)
        it "atDefaultPrecedence pairs a rule with its type default" $
            atDefaultPrecedence DenyInstallTimeExecution
                `shouldBe` PrecededRule defaultDenyInstallTimeExecutionPrecedence DenyInstallTimeExecution

    describe "prepare" $ do
        it "attaches a resilience and fail-open alignments to AllowIfRemediatesCve" $
            -- The one thing a reviewer must check on the remediation lane: an
            -- uncomputable lookup abstains (FailNoDecision) and never admits or 503s.
            prepare unloadedDeps [atDefaultPrecedence AllowIfRemediatesCve] >>= \case
                [r@PreparedRule{prepEval = PerPackage packageRead}] -> do
                    isJust (prepResilience r) `shouldBe` True
                    (onExpiredPush (prAlignment packageRead), onFaultedRead (prAlignment packageRead)) `shouldBe` (FailNoDecision, FailNoDecision)
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
        it "emits one line per rule, in boot order, with each precedence" $ do
            rules <-
                prepare
                    inertRuleDeps
                    [ at 100 (AllowIfOlderThan (7 * nominalDay))
                    , at 300 DenyInstallTimeExecution
                    ]
            renderBootOrder rules
                `shouldBe` [ "rule 1: DenyInstallTimeExecution (precedence 300)"
                           , "rule 2: AllowIfOlderThan (precedence 100)"
                           ]
        it "is empty for an empty rule set" $
            prepare inertRuleDeps [] >>= \rules -> renderBootOrder rules `shouldBe` []

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
                (map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), AllowIfRemediatesCve])
                (pkg Nothing 0)
                >>= \d -> admittedBy d `shouldBe` Just "AllowIfRemediatesCve"
        it "a failing advisory lookup abstains: the quarantine still governs, and nothing turns Undecidable" $ do
            -- The deliberate failure asymmetry: an unconfirmable remediation costs the fix its fast
            -- lane, never availability and never an admission.
            let broken = faultingDeps "advisory database exploded"
                policy = map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), AllowIfRemediatesCve]
            -- An old enough version still rides the ordinary allow.
            decideWith broken policy (pkg Nothing 30)
                >>= \d -> admittedBy d `shouldBe` Just "AllowIfOlderThan"
            -- A young version is denied by default, not fail-closed 'Undecidable'.
            decideWith broken policy (pkg Nothing 1) >>= \case
                BlockedByDefault _ -> pass
                other -> expectationFailure ("expected BlockedByDefault, got " <> show other)
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
                            PrecededRule
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
                let preceded = zipWith PrecededRule precs rules
                    p = withInstallScripts (pkg (Just scopeTxt) ageDays)
                perm <- forAll (Gen.shuffle preceded)
                original <- liftIO (decide preceded p)
                shuffled <- liftIO (decide perm p)
                canonical original === canonical shuffled

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
