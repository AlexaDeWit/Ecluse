module Ecluse.RulesSpec (spec) where

import Data.List (nub)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, nominalDay)
import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Ecosystem (Ecosystem (..))
import Ecluse.Package
import Ecluse.Rules
import Ecluse.Rules.Types
import Ecluse.Version (mkVersion)

-- | A fixed "now" so age-based tests are deterministic.
now :: UTCTime
now = UTCTime (fromGregorian 2026 6 20) 0

ctx :: EvalContext
ctx = EvalContext now

-- | A single inert artifact; the rules under test do not inspect artifacts.
sampleArtifact :: Artifact
sampleArtifact =
    Artifact
        { artFilename = "thing-1.0.0.tgz"
        , artUrl = "https://example.test/thing-1.0.0.tgz"
        , artKind = Tarball
        , artHashes = []
        , artSize = Nothing
        , artInterpreter = Nothing
        , artYanked = False
        , artProvenance = Nothing
        }

{- | A package version under an optional npm scope, published @ageDays@ days
before 'now'. Other fields are fixed; the rules under test read only the scope,
the publish age, and the install-code signal.
-}
pkg :: Maybe Text -> Integer -> PackageDetails
pkg mScope ageDays =
    PackageDetails
        { pkgName = mkPackageName Npm (mkScope <$> mScope) "thing"
        , pkgVersion = mkVersion Npm "1.0.0"
        , pkgPublishedAt = Just (addUTCTime (negate (fromInteger ageDays * nominalDay)) now)
        , pkgInstallCode = NoCodeOnInstall
        , pkgTrust = Untrusted
        , pkgAvailability = Available
        , pkgArtifacts = sampleArtifact :| []
        , pkgLicenses = ["MIT"]
        , pkgPublisher = Nothing
        , pkgMaintainers = []
        , pkgDependencies = []
        }

isAllow :: RuleOutcome -> Bool
isAllow (Allow _) = True
isAllow _ = False

isAbstain :: RuleOutcome -> Bool
isAbstain (Abstain _) = True
isAbstain _ = False

isDeny :: RuleOutcome -> Bool
isDeny (Deny _) = True
isDeny _ = False

approvedBy :: Decision -> Maybe Rule
approvedBy (Approved r _) = Just r
approvedBy _ = Nothing

deniedBy :: Decision -> Maybe Rule
deniedBy (Denied r _) = Just r
deniedBy _ = Nothing

-- | Mark the version as running code on install, for the deny-rule tests.
withInstallScripts :: PackageDetails -> PackageDetails
withInstallScripts pd = pd{pkgInstallCode = RunsCodeOnInstall "postinstall hook"}

-- | Put a rule at an explicit precedence (the operator-override form).
at :: Int -> Rule -> PrecededRule
at = PrecededRule

genScope :: Gen Text
genScope = Gen.text (Range.linear 1 12) Gen.alpha

genAgeDays :: Gen Integer
genAgeDays = Gen.integral (Range.linear 0 3650)

genPrecedence :: Gen Int
genPrecedence = Gen.int (Range.linear 0 1000)

{- | A rule that takes a position against the package the order-independence
property builds (scoped @scopeTxt@, old, running install scripts): a matching
'AllowScope', an 'AllowIfPublishedBefore' it is old enough for, or
'DenyHasInstallScripts'. None abstains, so every generated rule competes.
-}
genFiringRule :: Text -> Gen Rule
genFiringRule scopeTxt =
    Gen.element
        [ AllowScope (mkScope scopeTxt)
        , AllowIfPublishedBefore (7 * nominalDay)
        , DenyHasInstallScripts
        ]

{- | Canonicalise a decision for order-independence comparison: the audit-reason
list of a 'DeniedByDefault' is sorted, since reasons are gathered in list order
and a permutation only reorders them. 'Approved' \/ 'Denied' are left as-is —
the generators that feed this give every rule a distinct precedence, so the
winner is unique regardless of order.
-}
canonical :: Decision -> Decision
canonical (DeniedByDefault reasons) = DeniedByDefault (sort reasons)
canonical d = d

spec :: Spec
spec = do
    describe "evalRule" $ do
        it "AllowScope allows a matching scope" $
            evalRule ctx (AllowScope (mkScope "myorg")) (pkg (Just "myorg") 0)
                `shouldSatisfy` isAllow
        it "AllowScope abstains on a non-matching scope" $
            evalRule ctx (AllowScope (mkScope "myorg")) (pkg (Just "other") 0)
                `shouldSatisfy` isAbstain
        it "AllowScope abstains on an unscoped package" $
            evalRule ctx (AllowScope (mkScope "myorg")) (pkg Nothing 0)
                `shouldSatisfy` isAbstain
        it "AllowIfPublishedBefore allows a version older than the threshold" $
            evalRule ctx (AllowIfPublishedBefore (7 * nominalDay)) (pkg Nothing 30)
                `shouldSatisfy` isAllow
        it "AllowIfPublishedBefore abstains on a too-young version" $
            evalRule ctx (AllowIfPublishedBefore (7 * nominalDay)) (pkg Nothing 1)
                `shouldSatisfy` isAbstain
        it "DenyHasInstallScripts denies a package that runs install scripts" $
            evalRule ctx DenyHasInstallScripts (withInstallScripts (pkg Nothing 99))
                `shouldSatisfy` isDeny
        it "DenyHasInstallScripts abstains when there are no install scripts" $
            evalRule ctx DenyHasInstallScripts (pkg Nothing 99)
                `shouldSatisfy` isAbstain

    describe "PrecededRule" $ do
        it "exposes the precedence and rule it was built with" $ do
            -- The fields a config loader reads to patch a rule's precedence.
            let pr = PrecededRule 250 DenyHasInstallScripts
            rulePrecedence pr `shouldBe` 250
            prRule pr `shouldBe` DenyHasInstallScripts
        it "shows both fields" $
            show (PrecededRule 250 DenyHasInstallScripts)
                `shouldBe` ("PrecededRule {rulePrecedence = 250, prRule = DenyHasInstallScripts}" :: String)

    describe "defaultPrecedence" $ do
        it "ranks every deny default strictly above every allow default" $
            -- The out-of-the-box invariant: a matching deny overrides any allow.
            defaultPrecedence DenyHasInstallScripts
                `shouldSatisfy` (\d -> d > defaultPrecedence (AllowScope (mkScope "x")) && d > defaultPrecedence (AllowIfPublishedBefore 0))
        it "atDefaultPrecedence pairs a rule with its type default" $
            atDefaultPrecedence DenyHasInstallScripts
                `shouldBe` PrecededRule defaultDenyHasInstallScriptsPrecedence DenyHasInstallScripts

    describe "evalRules" $ do
        it "denies by default with no rules" $
            evalRules ctx [] (pkg (Just "myorg") 99) `shouldBe` DeniedByDefault []
        it "approves via the single matching allow rule" $
            approvedBy (evalRules ctx [atDefaultPrecedence (AllowScope (mkScope "myorg"))] (pkg (Just "myorg") 0))
                `shouldBe` Just (AllowScope (mkScope "myorg"))
        it "the higher-precedence allow wins among allows" $
            -- The version is too young for the age rule, but the scope rule
            -- matches; at default precedences the scope allow outranks it anyway.
            approvedBy
                ( evalRules
                    ctx
                    (map atDefaultPrecedence [AllowIfPublishedBefore (7 * nominalDay), AllowScope (mkScope "myorg")])
                    (pkg (Just "myorg") 0)
                )
                `shouldBe` Just (AllowScope (mkScope "myorg"))
        it "a matching deny rule overrides an allow at default precedence, whatever the order" $ do
            let rs = map atDefaultPrecedence [AllowScope (mkScope "myorg"), DenyHasInstallScripts]
                p = withInstallScripts (pkg (Just "myorg") 99)
            deniedBy (evalRules ctx rs p) `shouldBe` Just DenyHasInstallScripts
            deniedBy (evalRules ctx (reverse rs) p) `shouldBe` Just DenyHasInstallScripts
        it "an operator-elevated allow outranks a higher-default deny" $
            -- The scope allow is lifted above the deny's default precedence, so a
            -- trusted internal scope is admitted despite running install scripts.
            approvedBy
                ( evalRules
                    ctx
                    [ at (defaultDenyHasInstallScriptsPrecedence + 1) (AllowScope (mkScope "myorg"))
                    , atDefaultPrecedence DenyHasInstallScripts
                    ]
                    (withInstallScripts (pkg (Just "myorg") 99))
                )
                `shouldBe` Just (AllowScope (mkScope "myorg"))
        it "denies by default when every rule abstains, collecting each reason in order" $
            -- The audit trail carries each abstaining rule's actual reason, in
            -- the order the rules were given.
            case evalRules
                ctx
                (map atDefaultPrecedence [AllowScope (mkScope "myorg"), AllowIfPublishedBefore (7 * nominalDay)])
                (pkg (Just "other") 1) of
                DeniedByDefault reasons ->
                    reasons
                        `shouldBe` [ "scope is not the allow-listed @myorg"
                                   , "published only 1 day ago, minimum age is 7 days"
                                   ]
                other -> expectationFailure ("expected DeniedByDefault, got " <> show other)

    describe "properties" $ do
        it "an empty rule set always denies by default" $
            hedgehog $ do
                mScope <- forAll (Gen.maybe genScope)
                ageDays <- forAll genAgeDays
                evalRules ctx [] (pkg mScope ageDays) === DeniedByDefault []

        it "every rule abstaining yields deny-by-default" $
            hedgehog $ do
                -- A non-matching scope and a too-young age make both allows
                -- abstain; the package runs no install scripts so the deny
                -- abstains too — so whatever the precedences, nothing fires.
                scopeTxt <- forAll genScope
                otherTxt <- forAll (Gen.filter (/= scopeTxt) genScope)
                precs <- forAll (Gen.list (Range.singleton 3) genPrecedence)
                let rules =
                        zipWith
                            PrecededRule
                            precs
                            [AllowScope (mkScope scopeTxt), AllowIfPublishedBefore (7 * nominalDay), DenyHasInstallScripts]
                case evalRules ctx rules (pkg (Just otherTxt) 1) of
                    DeniedByDefault _ -> H.success
                    other -> H.annotateShow other >> H.failure

        it "at equal precedence a deny beats an allow" $
            hedgehog $ do
                scopeTxt <- forAll genScope
                ageDays <- forAll genAgeDays
                prec <- forAll genPrecedence
                let rules = [at prec (AllowScope (mkScope scopeTxt)), at prec DenyHasInstallScripts]
                    p = withInstallScripts (pkg (Just scopeTxt) ageDays)
                deniedBy (evalRules ctx rules p) === Just DenyHasInstallScripts

        it "the highest-precedence deny wins over any lower allow" $
            hedgehog $ do
                scopeTxt <- forAll genScope
                ageDays <- forAll genAgeDays
                allowPrec <- forAll genPrecedence
                denyPrec <- forAll (Gen.int (Range.linear (allowPrec + 1) (allowPrec + 1000)))
                let rules = [at allowPrec (AllowScope (mkScope scopeTxt)), at denyPrec DenyHasInstallScripts]
                    p = withInstallScripts (pkg (Just scopeTxt) ageDays)
                deniedBy (evalRules ctx rules p) === Just DenyHasInstallScripts

        it "an operator-elevated allow outranks a lower-precedence deny" $
            hedgehog $ do
                scopeTxt <- forAll genScope
                ageDays <- forAll genAgeDays
                denyPrec <- forAll genPrecedence
                allowPrec <- forAll (Gen.int (Range.linear (denyPrec + 1) (denyPrec + 1000)))
                let rules = [at allowPrec (AllowScope (mkScope scopeTxt)), at denyPrec DenyHasInstallScripts]
                    p = withInstallScripts (pkg (Just scopeTxt) ageDays)
                approvedBy (evalRules ctx rules p) === Just (AllowScope (mkScope scopeTxt))

        it "the decision is independent of rule order" $
            hedgehog $ do
                -- Build a rule set whose firing rules all have distinct
                -- precedences, so a unique rule wins and the decision is fully
                -- determined; then any permutation must yield the same decision
                -- (modulo the order abstain reasons are gathered).
                scopeTxt <- forAll genScope
                ageDays <- forAll genAgeDays
                n <- forAll (Gen.int (Range.linear 0 6))
                rules <- forAll (Gen.list (Range.singleton n) (genFiringRule scopeTxt))
                distinctPrecs <- forAll (distinctPrecedences n)
                let preceded = zipWith PrecededRule distinctPrecs rules
                    p = withInstallScripts (pkg (Just scopeTxt) ageDays)
                perm <- forAll (Gen.shuffle preceded)
                canonical (evalRules ctx preceded p) === canonical (evalRules ctx perm p)

        it "the install-script deny always wins at default precedences" $
            hedgehog $ do
                scopeTxt <- forAll genScope
                ageDays <- forAll genAgeDays
                let rules = map atDefaultPrecedence [AllowScope (mkScope scopeTxt), DenyHasInstallScripts]
                    p = withInstallScripts (pkg (Just scopeTxt) ageDays)
                deniedBy (evalRules ctx rules p) === Just DenyHasInstallScripts
  where
    -- A list of @n@ distinct precedences, so each rule competes at its own rank.
    distinctPrecedences :: Int -> Gen [Int]
    distinctPrecedences n =
        Gen.filter
            (\xs -> length (nub xs) == n)
            (Gen.list (Range.singleton n) genPrecedence)
