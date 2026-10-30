-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What the two evidence builders carry, what a reason's payload guarantees, what each rule's
advisory predicates answer, and which rules the Dredger evaluates. A determined absence and an
unread fact must stay distinguishable, and the rules that apply at revocation must never block a
version the whole policy admits.
-}
module Ecluse.Core.Rules.TypesSpec (spec) where

import Prelude hiding (universe)

import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, nominalDay)
import Data.Universe.Class (Universe (universe))
import Hedgehog (Gen, forAll)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)
import UnliftIO.Exception (evaluate, impureThrow)

import Ecluse.Core.Cve (AdvisoryRange (AdvisoryRange))
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Osv.Types (UpperBound (FixedBefore))
import Ecluse.Core.Package (
    Availability (Available, Deprecated, Yanked),
    CodeExecSignal (CodeExecUnknown, NoCodeOnInstall, RunsCodeOnInstall),
    DependencySources (DependencySourcesUnknown, FetchesOutsideRegistry, RegistryDependencies),
    PackageDetails (pkgAvailability, pkgDependencySources, pkgInstallCode, pkgPublishedAt),
    mkScope,
    pkgEcosystem,
 )
import Ecluse.Core.Rules (RuleDeps (rdAdvisoryDatabase), prepare)
import Ecluse.Core.Rules.Freshness (AdvisoryFreshness (AdvisoryFresh, AdvisoryUndated))
import Ecluse.Core.Rules.Types (
    AdvisoryScore (Cvss, Epss),
    Decision,
    DenyIfCveParams (DenyIfCveParams),
    DenyIfEpssParams (DenyIfEpssParams),
    Fact (Known, Unread),
    FailureAlignment (FailDeny, FailNoDecision),
    Inability (RuleThrew),
    LimitShortfall (GapBelowMinimum, RemainingUnscored),
    PrecededRule (PrecededRule, prRule, rulePrecedence, ruleReach),
    Reason (..),
    RemainingAdvisories (AbstainOnRemaining),
    Rule (..),
    RuleEvidence (evAvailability, evDependencySources, evInstallCode, evName, evPublishedAt, evVersion),
    RuleReach (AdmissionAndRevocation, AdmissionOnly),
    ScoreComparison (ScoreComparison),
    ScoreReading (Scored, Unscored),
    ScoreSides (ScoreSides),
    completeEvidence,
    defaultDifferentialLimits,
    defaultPrecedence,
    deniesOnAdvisories,
    identityEvidence,
    mkAdvisoryIds,
    mkLimitShortfalls,
    readsAdvisories,
    reasonAdvisories,
    requiresEpss,
    revocationRules,
    ruleDenies,
    unAdvisoryIds,
 )
import Ecluse.Core.Version (mkVersion)
import Ecluse.Rules.Remediation.Support (genRemediation)
import Ecluse.Rules.Support (ctx, now)
import Ecluse.Test.Cve (fakeCveLookup)
import Ecluse.Test.Package (sampleDetails, scopedNpm, unscopedNpm, unscopedPyPI, v1_0_0)
import Ecluse.Test.Rules (admissionOnly, atPrecedence, blockedBy, differentialRemediation, evalRule, evalRules, inertRuleDeps, isAllow, isApproved, isUndecidable, remediationWith, servingRuleDeps, shippedRemediation, staticAdvisoryDatabase, withAdvisoryFreshness)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

import Ecluse.Core.Rules.Types qualified as Types
import Ecluse.Core.Wire (WireVocab (wireTable), lookupWire, renderWire)

published :: UTCTime
published = UTCTime (fromGregorian 2026 3 1) 0

spec :: Spec
spec = do
    vocabularySpec
    completeSpec
    identitySpec
    distinctionSpec
    ruleDeniesSpec
    advisoryPredicateSpec
    revocationSpec
    advisoryIdsSpec
    reasonAdvisoriesSpec

vocabularySpec :: Spec
vocabularySpec = describe "RuleType" $ do
    let vocabulary =
            [ (Types.AllowScopeType, "AllowScope", AllowScope (mkScope "acme"))
            , (Types.AllowIfOlderThanType, "AllowIfOlderThan", AllowIfOlderThan nominalDay)
            , (Types.AllowByIdentityType, "AllowByIdentity", AllowByIdentity "thing")
            , (Types.AllowIfRemediatesCveType, "AllowIfRemediatesCve", shippedRemediation)
            , (Types.DenyIfCveType, "DenyIfCve", DenyIfCve (DenyIfCveParams 8 FailDeny))
            , (Types.DenyIfKnownMaliciousType, "DenyIfKnownMalicious", DenyIfKnownMalicious FailDeny)
            , (Types.DenyIfEpssType, "DenyIfEpss", DenyIfEpss (DenyIfEpssParams 0.5 FailDeny))
            , (Types.DenyIfYankedType, "DenyIfYanked", DenyIfYanked)
            , (Types.DenyIfDeprecatedType, "DenyIfDeprecated", DenyIfDeprecated)
            , (Types.DenyInstallTimeExecutionType, "DenyInstallTimeExecution", DenyInstallTimeExecution)
            , (Types.DenyExternalDependenciesType, "DenyExternalDependencies", DenyExternalDependencies)
            , (Types.DenyByIdentityType, "DenyByIdentity", DenyByIdentity "thing")
            ]
    it "covers the typed universe and the wire table exactly" $ do
        let types = [ty | (ty, _, _) <- vocabulary]
        (universe :: [Types.RuleType]) `shouldMatchList` types
        (toList wireTable :: [(Types.RuleType, Text)]) `shouldMatchList` [(ty, name) | (ty, name, _) <- vocabulary]
    for_ vocabulary $ \(ty, name, configured) ->
        it ("keeps the wire spelling and type identity for " <> toString name) $ do
            renderWire ty `shouldBe` name
            lookupWire name `shouldBe` Just ty
            Types.ruleType configured `shouldBe` ty
            Types.ruleName configured `shouldBe` name
    it "rejects an unknown wire spelling" $
        (lookupWire "DenyInstallTimeExecutio" :: Maybe Types.RuleType) `shouldBe` Nothing

completeSpec :: Spec
completeSpec = describe "completeEvidence" $ do
    it "carries every fact the rules read as Known" $ do
        let details =
                (sampleDetails (unscopedNpm "left-pad") v1_0_0)
                    { pkgPublishedAt = Just published
                    , pkgAvailability = Deprecated
                    , pkgInstallCode = RunsCodeOnInstall "postinstall hook"
                    , pkgDependencySources = FetchesOutsideRegistry "dependency 'left-pad' in dependencies names a git source"
                    }
            evidence = completeEvidence details
        evName evidence `shouldBe` unscopedNpm "left-pad"
        evVersion evidence `shouldBe` v1_0_0
        evPublishedAt evidence `shouldBe` Known (Just published)
        evAvailability evidence `shouldBe` Known Deprecated
        evInstallCode evidence `shouldBe` Known (RunsCodeOnInstall "postinstall hook")
        evDependencySources evidence `shouldBe` Known (FetchesOutsideRegistry "dependency 'left-pad' in dependencies names a git source")

    it "keeps an absent publish time as a reading, not as an absent reading" $
        evPublishedAt (completeEvidence (sampleDetails (unscopedNpm "left-pad") v1_0_0))
            `shouldBe` Known Nothing

identitySpec :: Spec
identitySpec = describe "identityEvidence" $ do
    it "carries the identity a store listing establishes" $ do
        let evidence = identityEvidence (unscopedNpm "left-pad") v1_0_0
        evName evidence `shouldBe` unscopedNpm "left-pad"
        evVersion evidence `shouldBe` v1_0_0

    it "carries no reading of any fact a manifest would supply" $ do
        let evidence = identityEvidence (unscopedNpm "left-pad") v1_0_0
        evPublishedAt evidence `shouldBe` Unread
        evAvailability evidence `shouldBe` Unread
        evInstallCode evidence `shouldBe` Unread
        evDependencySources evidence `shouldBe` Unread

-- A rule reads these two cases differently: it abstains on the first and refuses on the second,
-- so nothing may collapse them.
distinctionSpec :: Spec
distinctionSpec = describe "a determined absence against an unread fact" $
    it "gives the two builders different entries for the same package" $ do
        let read' = completeEvidence (sampleDetails (unscopedNpm "left-pad") v1_0_0)
            unread = identityEvidence (unscopedNpm "left-pad") v1_0_0
        evPublishedAt read' `shouldNotBe` evPublishedAt unread
        evInstallCode read' `shouldNotBe` evInstallCode unread
        evDependencySources read' `shouldNotBe` evDependencySources unread

ruleDeniesSpec :: Spec
ruleDeniesSpec = describe "ruleDenies" $
    modifyMaxSuccess (const 1000) $
        it "is true only of a rule that never admits, whatever it reads" $
            hedgehog $ do
                rule <- forAll genRule
                advisories <- forAll Gen.enumBounded
                evidence <- forAll genEvidence
                verdict <- liftIO (evalRule (depsIn advisories) ctx rule evidence)
                H.cover 5 "an allow admits" (isAllow verdict)
                H.cover 30 "a deny is evaluated" (ruleDenies rule)
                H.annotateShow verdict
                H.assert (not (ruleDenies rule && isAllow verdict))

-- | Whether each rule reads advisories, refuses without them, and reads EPSS, over every type.
advisoryPredicates :: [(Rule, (Bool, Bool, Bool))]
advisoryPredicates =
    [ (AllowScope (mkScope "acme"), (False, False, False))
    , (AllowIfOlderThan nominalDay, (False, False, False))
    , (AllowByIdentity "thing", (False, False, False))
    , (shippedRemediation, (True, True, True))
    , (remediationWith FailDeny AbstainOnRemaining, (True, True, False))
    , (differentialRemediation FailNoDecision, (True, False, True))
    , (remediationWith FailNoDecision AbstainOnRemaining, (True, False, False))
    , (DenyIfCve (DenyIfCveParams 8 FailDeny), (True, True, False))
    , (DenyIfCve (DenyIfCveParams 8 FailNoDecision), (True, True, False))
    , (DenyIfKnownMalicious FailNoDecision, (True, True, False))
    , (DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision), (True, True, True))
    , (DenyIfYanked, (False, False, False))
    , (DenyIfDeprecated, (False, False, False))
    , (DenyInstallTimeExecution, (False, False, False))
    , (DenyExternalDependencies, (False, False, False))
    , (DenyByIdentity "thing", (False, False, False))
    ]

advisoryPredicateSpec :: Spec
advisoryPredicateSpec = describe "advisory predicates" $ do
    it "covers every rule type" $
        ordNub (map (Types.ruleType . fst) advisoryPredicates) `shouldMatchList` (universe :: [Types.RuleType])
    for_ advisoryPredicates $ \(rule, expected) ->
        it ("answers for " <> show rule) $
            (readsAdvisories rule, deniesOnAdvisories rule, requiresEpss rule) `shouldBe` expected

revocationSpec :: Spec
revocationSpec = describe "revocationRules" $ do
    it "keeps the rules that apply at revocation, in their given order" $ do
        let age = atPrecedence 100 (AllowIfOlderThan (7 * nominalDay))
            scripts = admissionOnly (atPrecedence 300 DenyInstallTimeExecution)
            revoked = atPrecedence 400 (DenyByIdentity "thing")
        revocationRules [age, scripts, revoked] `shouldBe` [age, revoked]

    it "keeps every rule of a policy that limits none" $ do
        let policy = [atPrecedence 100 (AllowIfOlderThan (7 * nominalDay)), atPrecedence 400 (DenyByIdentity "thing")]
        revocationRules policy `shouldBe` policy

    -- The combination configuration refuses: leaving an allow out lets a lower deny block alone.
    it "would block a version the gate admits, were an allow limited to admission" $ do
        let policy = [admissionOnly (atPrecedence 500 (AllowByIdentity "thing")), atPrecedence 400 (DenyByIdentity "thing")]
            evidence = identityEvidence (unscopedNpm "thing") v1_0_0
        decideUnder NoDatabase policy evidence >>= (`shouldSatisfy` isApproved)
        decideUnder NoDatabase (revocationRules policy) evidence >>= \d -> blockedBy d `shouldBe` Just "DenyByIdentity"

    -- Fewer tests let "an allow admits at the gate" fall under its bar by chance.
    modifyMaxSuccess (const 10000) $
        it "never blocks a version the whole policy admits, on the same evidence" $
            hedgehog $ do
                policy <- forAll genPolicy
                advisories <- forAll Gen.enumBounded
                evidence <- forAll genEvidence
                gate <- liftIO (decideUnder advisories policy evidence)
                dredger <- liftIO (decideUnder advisories (revocationRules policy) evidence)
                let blocks = isJust . blockedBy
                    limited = filter ((== AdmissionOnly) . ruleReach) policy
                H.cover 30 "the policy holds a deny limited to admission" (not (null limited))
                H.cover 30 "the policy holds a deny at both phases" (any (ruleDenies . prRule) (revocationRules policy))
                H.cover 30 "the policy holds an allow" (not (all (ruleDenies . prRule) policy))
                H.cover 3 "a deny limited to admission blocks at the gate, and the Dredger keeps the version" (blocks gate && not (blocks dredger))
                H.cover 4 "a deny at both phases blocks for the Dredger" (blocks dredger)
                H.cover 10 "an allow admits at the gate" (isApproved gate)
                H.cover 15 "the evidence is identity alone, as an unread manifest leaves it" (evPublishedAt evidence == Unread && evInstallCode evidence == Unread && evAvailability evidence == Unread && evDependencySources evidence == Unread)
                H.cover 20 "the version is a PyPI release" (pkgEcosystem (evName evidence) == PyPI)
                H.cover 2 "the gate cannot decide and the Dredger blocks" (isUndecidable gate && blocks dredger)
                H.cover 30 "two rules tie on precedence" (length (ordNub (map rulePrecedence policy)) < length policy)
                H.annotateShow (gate, dredger)
                H.assert (not (blocks dredger && isApproved gate))

-- | The advisory database both evaluations of one case read, so their evidence is the same.
data AdvisoryState
    = NoDatabase
    | Unloaded
    | Serving
    | ServingUndated
    deriving stock (Bounded, Enum, Eq, Show)

depsIn :: AdvisoryState -> RuleDeps
depsIn = \case
    NoDatabase -> inertRuleDeps
    Unloaded -> inertRuleDeps{rdAdvisoryDatabase = staticAdvisoryDatabase (pure AdvisoryFresh) (\use -> use Nothing)}
    Serving -> serving
    ServingUndated -> withAdvisoryFreshness (pure AdvisoryUndated) serving
  where
    -- One advisory affects @thing@ below 2.0.0 and names 2.0.0 as its fix.
    serving =
        servingRuleDeps
            (DbEtag "etag-1")
            (fakeCveLookup [("thing", AdvisoryRange "GHSA-affect-0001" (Just 9.8) (Just "0") (FixedBefore "2.0.0") (Just 0.9))])

decideUnder :: AdvisoryState -> [PrecededRule] -> RuleEvidence -> IO Decision
decideUnder advisories policy evidence =
    prepare (depsIn advisories) policy >>= \prepared -> evalRules ctx prepared evidence

-- One in three policies puts an admission-only deny above a deny at both phases.
genPolicy :: Gen [PrecededRule]
genPolicy = Gen.frequency [(2, anyRules), (1, (<>) <$> shadowedDeny <*> anyRules)]
  where
    anyRules = Gen.list (Range.constant 0 6) genPreceded
    genDeny = Gen.filter ruleDenies genRule
    genNamingDeny = Gen.frequency [(2, pure (DenyByIdentity "thing")), (1, genDeny)]
    shadowedDeny = (\above below -> [PrecededRule 9 AdmissionOnly above, PrecededRule 8 AdmissionAndRevocation below]) <$> genDeny <*> genNamingDeny

-- | A configured rule with legal reach and precedence ties against other rules.
genPreceded :: Gen PrecededRule
genPreceded = do
    rule <- genRule
    reach <- if ruleDenies rule then Gen.element [AdmissionOnly, AdmissionAndRevocation] else pure AdmissionAndRevocation
    prec <- Gen.frequency [(1, pure (defaultPrecedence rule)), (2, Gen.int (Range.linear 0 4))]
    pure (PrecededRule prec reach rule)

-- | Every rule type, with parameters that fire on the versions 'genEvidence' builds.
genRule :: Gen Rule
genRule =
    Gen.choice . map (fmap generated) $
        [ pure (AllowScope (mkScope "acme"))
        , pure (AllowIfOlderThan (7 * nominalDay))
        , AllowByIdentity <$> genIdentity
        , genRemediation
        , pure DenyIfYanked
        , pure DenyIfDeprecated
        , pure DenyInstallTimeExecution
        , pure DenyExternalDependencies
        , DenyByIdentity <$> genIdentity
        , DenyIfCve <$> (DenyIfCveParams <$> Gen.element [0, 7, 10] <*> genAlignment)
        , DenyIfKnownMalicious <$> genAlignment
        , DenyIfEpss <$> (DenyIfEpssParams <$> Gen.element [0, 0.5, 1] <*> genAlignment)
        ]
  where
    genIdentity = Gen.element ["thing", "thing@1.0.0", "thing@2.0.0", "@acme/thing"]
    genAlignment = Gen.element [FailDeny, FailNoDecision]

-- Total over 'Rule', so a new rule type fails to compile here until 'genRule' generates it.
generated :: Rule -> Rule
generated rule = case rule of
    AllowScope{} -> rule
    AllowIfOlderThan{} -> rule
    AllowByIdentity{} -> rule
    AllowIfRemediatesCve{} -> rule
    DenyIfYanked -> rule
    DenyIfDeprecated -> rule
    DenyInstallTimeExecution -> rule
    DenyExternalDependencies -> rule
    DenyByIdentity{} -> rule
    DenyIfCve{} -> rule
    DenyIfKnownMalicious{} -> rule
    DenyIfEpss{} -> rule

-- | An npm or PyPI version with each fact read or unread, from complete evidence to identity alone.
genEvidence :: Gen RuleEvidence
genEvidence = do
    name <- Gen.element [unscopedNpm "thing", scopedNpm "acme" "thing", unscopedPyPI "thing"]
    version <- mkVersion (pkgEcosystem name) <$> Gen.element ["1.0.0", "2.0.0"]
    (publishedAt, installCode, availability, dependencies) <- Gen.frequency [(1, pure (Unread, Unread, Unread, Unread)), (3, (,,,) <$> genPublishedAt <*> genInstallCode <*> Gen.element [Unread, Known Available, Known Deprecated, Known Yanked] <*> genDependencies)]
    pure (identityEvidence name version){evPublishedAt = publishedAt, evInstallCode = installCode, evAvailability = availability, evDependencySources = dependencies}
  where
    genPublishedAt = Gen.element [Unread, Known Nothing, Known (Just (daysAgo 30)), Known (Just (daysAgo 1))]
    genInstallCode = Gen.element [Unread, Known NoCodeOnInstall, Known (RunsCodeOnInstall "postinstall hook"), Known CodeExecUnknown]
    genDependencies = Gen.element [Unread, Known RegistryDependencies, Known DependencySourcesUnknown, Known (FetchesOutsideRegistry "dependency 'left-pad' in dependencies names a git source")]
    daysAgo days = addUTCTime (negate (days * nominalDay)) now

laterAdvisory :: TestContractEscape
laterAdvisory = TestContractEscape "a later advisory was evaluated"

-- | Reasons whose advisory lists hold a later identifier, or a later stretch of list, that throws.
deferredReasons :: [(String, Reason)]
deferredReasons =
    [ ("a later identifier", Remediates (mkAdvisoryIds ("GHSA-a" :| [impureThrow laterAdvisory])))
    , ("the rest of the list", AffectedBy Cvss 7.0 (mkAdvisoryIds ("GHSA-a" :| "GHSA-b" : impureThrow laterAdvisory)))
    , ("the advisories still affecting a fix", FixesButStillAffected (mkAdvisoryIds ("GHSA-a" :| [])) (mkAdvisoryIds ("GHSA-b" :| [impureThrow laterAdvisory])))
    , ("the advisories still holding a named fix", FixStillApplies (mkAdvisoryIds ("GHSA-a" :| [impureThrow laterAdvisory])))
    , ("the remaining advisories a differential admission compared", RemediatesWithinLimits (compared (mkAdvisoryIds ("GHSA-b" :| [impureThrow laterAdvisory]))))
    , ("a later condition a partial fix failed", RemediationBeyondLimits (compared (mkAdvisoryIds ("GHSA-b" :| []))) (mkLimitShortfalls (GapBelowMinimum Cvss :| [impureThrow laterAdvisory])))
    , ("the unscored advisories a failed condition names", RemediationBeyondLimits (compared (mkAdvisoryIds ("GHSA-b" :| []))) (mkLimitShortfalls (RemainingUnscored Epss (mkAdvisoryIds ("GHSA-b" :| [impureThrow laterAdvisory])) :| [])))
    ]
  where
    compared remaining = ScoreComparison (mkAdvisoryIds ("GHSA-a" :| [])) remaining (ScoreSides (Scored 9.8) (Scored 4.2)) (ScoreSides Unscored (Scored 0.005)) defaultDifferentialLimits

-- One row per constructor. Each remediation row carries distinct fixed and remaining ids, so naming a fixed one fails.
reasonAdvisoriesSpec :: Spec
reasonAdvisoriesSpec = describe "reasonAdvisories" $
    for_ rows $ \(label, reason, expected) ->
        it ("names only the advisories still affecting the version: " <> label) $
            reasonAdvisories reason `shouldBe` expected
  where
    fixed = mkAdvisoryIds ("GHSA-fixed-1" :| ["GHSA-fixed-2"])
    open = mkAdvisoryIds ("GHSA-open-1" :| ["GHSA-open-2"])
    stillOpen = ["GHSA-open-1", "GHSA-open-2"]
    compared = ScoreComparison fixed open (ScoreSides (Scored 9.8) (Scored 4.2)) (ScoreSides Unscored (Scored 0.005)) defaultDifferentialLimits
    lookalike = "affected by GHSA-text-0001 (CVSS >= 7.0)"
    rows :: [(String, Reason, [Text])]
    rows =
        [ ("ScopeAllowListed", ScopeAllowListed (mkScope "acme"), [])
        , ("ScopeNotAllowListed", ScopeNotAllowListed (mkScope "acme"), [])
        , ("PublishedLongEnough", PublishedLongEnough 8 7, [])
        , ("PublishedTooRecently", PublishedTooRecently 1 7, [])
        , ("PublishTimeUnknown", PublishTimeUnknown, [])
        , ("RunsOnInstall", RunsOnInstall lookalike, [])
        , ("NothingRunsOnInstall", NothingRunsOnInstall, [])
        , ("InstallCodeUndetermined", InstallCodeUndetermined, [])
        , ("VersionYanked", VersionYanked, [])
        , ("VersionNotYanked", VersionNotYanked, [])
        , ("VersionDeprecated", VersionDeprecated, [])
        , ("VersionNotDeprecated", VersionNotDeprecated, [])
        , ("IdentityRevoked", IdentityRevoked "GHSA-identity", [])
        , ("IdentityNotRevoked", IdentityNotRevoked "GHSA-identity", [])
        , ("IdentityAllowListed", IdentityAllowListed "GHSA-identity", [])
        , ("IdentityNotAllowListed", IdentityNotAllowListed "GHSA-identity", [])
        , ("Remediates", Remediates fixed, [])
        , ("RemediatesWithinLimits", RemediatesWithinLimits compared, stillOpen)
        , ("FixesButStillAffected", FixesButStillAffected fixed open, stillOpen)
        , ("RemediationBeyondLimits", RemediationBeyondLimits compared (mkLimitShortfalls (RemainingUnscored Epss open :| [GapBelowMinimum Cvss])), stillOpen)
        , ("FixStillApplies", FixStillApplies open, stillOpen)
        , ("FixesNoAdvisory", FixesNoAdvisory, [])
        , ("AffectedBy", AffectedBy Cvss 7.0 open, stillOpen)
        , ("KnownMalicious", KnownMalicious (mkAdvisoryIds ("MAL-one" :| ["MAL-two"])), ["MAL-one", "MAL-two"])
        , ("NoKnownMaliciousAdvisory", NoKnownMaliciousAdvisory, [])
        , ("NotAffectedAtThreshold", NotAffectedAtThreshold Cvss, [])
        , ("RuleUnable", RuleUnable "DenyIfCve" (RuleThrew lookalike), [])
        ]

-- A rule finds the later advisories by matching ranges lazily. Left unevaluated, that matching
-- would run where the reason is rendered, outside the handler that evaluated the verdict.
advisoryIdsSpec :: Spec
advisoryIdsSpec = describe "mkAdvisoryIds" $ do
    it "keeps the identifiers in the order given" $
        unAdvisoryIds (mkAdvisoryIds ("GHSA-a" :| ["GHSA-b"])) `shouldBe` "GHSA-a" :| ["GHSA-b"]

    for_ deferredReasons $ \(label, reason) ->
        it ("evaluates every advisory with the reason that names it: " <> label) $
            evaluate reason `shouldThrow` (== laterAdvisory)
