-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What the two evidence builders carry, what a reason's advisory list guarantees, and which rules
the Dredger evaluates. A determined absence and an unread fact must stay distinguishable, and the
rules that apply at revocation must never block a version the whole policy admits.
-}
module Ecluse.Core.Rules.TypesSpec (spec) where

import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, nominalDay)
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
    CodeExecSignal (CodeExecUnknown, NoCodeOnInstall, RunsCodeOnInstall),
    PackageDetails (pkgInstallCode, pkgPublishedAt),
    mkScope,
    pkgEcosystem,
 )
import Ecluse.Core.Rules (AdvisoryDatabase (AdvisoryDatabase), RuleDeps (rdAdvisoryDatabase, rdAdvisoryFreshness), evalRules, prepare)
import Ecluse.Core.Rules.Freshness (AdvisoryFreshness (AdvisoryUndated))
import Ecluse.Core.Rules.Types (
    AdvisoryScore (Cvss),
    Decision,
    DenyIfCveParams (DenyIfCveParams),
    DenyIfEpssParams (DenyIfEpssParams),
    Fact (Known, Unread),
    FailureAlignment (FailDeny, FailNoDecision),
    PrecededRule (PrecededRule, prRule, rulePrecedence, ruleReach),
    Reason (AffectedBy, FixesButStillAffected, Remediates),
    Rule (..),
    RuleEvidence (evInstallCode, evName, evPublishedAt, evVersion),
    RuleReach (AdmissionAndRevocation, AdmissionOnly),
    completeEvidence,
    defaultPrecedence,
    identityEvidence,
    mkAdvisoryIds,
    revocationRules,
    ruleDenies,
    unAdvisoryIds,
 )
import Ecluse.Core.Version (mkVersion)
import Ecluse.Rules.Support (ctx, now)
import Ecluse.Test.Cve (fakeCveLookup)
import Ecluse.Test.Package (sampleDetails, scopedNpm, unscopedNpm, unscopedPyPI, v1_0_0)
import Ecluse.Test.Rules (admissionOnly, atPrecedence, blockedBy, evalRule, inertRuleDeps, isAllow, isApproved, isUndecidable, servingRuleDeps)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

published :: UTCTime
published = UTCTime (fromGregorian 2026 3 1) 0

spec :: Spec
spec = do
    completeSpec
    identitySpec
    distinctionSpec
    ruleDeniesSpec
    revocationSpec
    advisoryIdsSpec

completeSpec :: Spec
completeSpec = describe "completeEvidence" $ do
    it "carries every fact the rules read as Known" $ do
        let details =
                (sampleDetails (unscopedNpm "left-pad") v1_0_0)
                    { pkgPublishedAt = Just published
                    , pkgInstallCode = RunsCodeOnInstall "postinstall hook"
                    }
            evidence = completeEvidence details
        evName evidence `shouldBe` unscopedNpm "left-pad"
        evVersion evidence `shouldBe` v1_0_0
        evPublishedAt evidence `shouldBe` Known (Just published)
        evInstallCode evidence `shouldBe` Known (RunsCodeOnInstall "postinstall hook")

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
        evInstallCode evidence `shouldBe` Unread

{- A rule reads these two cases differently: it abstains on the first and refuses on the second,
so nothing may collapse them. -}
distinctionSpec :: Spec
distinctionSpec = describe "a determined absence against an unread fact" $
    it "gives the two builders different entries for the same package" $ do
        let read' = completeEvidence (sampleDetails (unscopedNpm "left-pad") v1_0_0)
            unread = identityEvidence (unscopedNpm "left-pad") v1_0_0
        evPublishedAt read' `shouldNotBe` evPublishedAt unread
        evInstallCode read' `shouldNotBe` evInstallCode unread

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

    modifyMaxSuccess (const 5000) $
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
                H.cover 15 "the evidence is identity alone, as an unread manifest leaves it" (evPublishedAt evidence == Unread && evInstallCode evidence == Unread)
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
    Unloaded -> inertRuleDeps{rdAdvisoryDatabase = AdvisoryDatabase (\use -> use Nothing)}
    Serving -> serving
    ServingUndated -> serving{rdAdvisoryFreshness = pure AdvisoryUndated}
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

{- | A rule as configuration resolves one: a deny at either reach, and an allow at both phases.
Colliding precedences put an allow above, below, and beside each deny.
-}
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
        , pure AllowIfRemediatesCve
        , pure DenyInstallTimeExecution
        , DenyByIdentity <$> genIdentity
        , DenyIfCve <$> (DenyIfCveParams <$> Gen.element [0, 7, 10] <*> genAlignment)
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
    AllowIfRemediatesCve -> rule
    DenyInstallTimeExecution -> rule
    DenyByIdentity{} -> rule
    DenyIfCve{} -> rule
    DenyIfEpss{} -> rule

-- | An npm or PyPI version with each fact read or unread, from complete evidence to identity alone.
genEvidence :: Gen RuleEvidence
genEvidence = do
    name <- Gen.element [unscopedNpm "thing", scopedNpm "acme" "thing", unscopedPyPI "thing"]
    version <- mkVersion (pkgEcosystem name) <$> Gen.element ["1.0.0", "2.0.0"]
    (publishedAt, installCode) <- Gen.frequency [(1, pure (Unread, Unread)), (3, (,) <$> genPublishedAt <*> genInstallCode)]
    pure (identityEvidence name version){evPublishedAt = publishedAt, evInstallCode = installCode}
  where
    genPublishedAt = Gen.element [Unread, Known Nothing, Known (Just (daysAgo 30)), Known (Just (daysAgo 1))]
    genInstallCode = Gen.element [Unread, Known NoCodeOnInstall, Known (RunsCodeOnInstall "postinstall hook"), Known CodeExecUnknown]
    daysAgo days = addUTCTime (negate (days * nominalDay)) now

laterAdvisory :: TestContractEscape
laterAdvisory = TestContractEscape "a later advisory was evaluated"

-- | Reasons whose advisory lists hold a later identifier, or a later stretch of list, that throws.
deferredReasons :: [(String, Reason)]
deferredReasons =
    [ ("a later identifier", Remediates (mkAdvisoryIds ("GHSA-a" :| [impureThrow laterAdvisory])))
    , ("the rest of the list", AffectedBy Cvss 7.0 (mkAdvisoryIds ("GHSA-a" :| "GHSA-b" : impureThrow laterAdvisory)))
    , ("the advisories still affecting a fix", FixesButStillAffected (mkAdvisoryIds ("GHSA-a" :| [])) (mkAdvisoryIds ("GHSA-b" :| [impureThrow laterAdvisory])))
    ]

{- A rule finds the later advisories by matching ranges lazily. Left unevaluated, that matching
would run where the reason is rendered, outside the handler that evaluated the verdict. -}
advisoryIdsSpec :: Spec
advisoryIdsSpec = describe "mkAdvisoryIds" $ do
    it "keeps the identifiers in the order given" $
        unAdvisoryIds (mkAdvisoryIds ("GHSA-a" :| ["GHSA-b"])) `shouldBe` "GHSA-a" :| ["GHSA-b"]

    for_ deferredReasons $ \(label, reason) ->
        it ("evaluates every advisory with the reason that names it: " <> label) $
            evaluate reason `shouldThrow` (== laterAdvisory)
