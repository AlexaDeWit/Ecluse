module Ecluse.RulesSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, nominalDay)
import Hedgehog (Gen, forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package

import Ecluse.Core.Rules
import Ecluse.Core.Rules.Types
import Ecluse.Core.Version (mkVersion)

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

isAllow :: RuleResult -> Bool
isAllow (Allow _) = True
isAllow _ = False

isNoDecision :: RuleResult -> Bool
isNoDecision (NoDecision _) = True
isNoDecision _ = False

isDeny :: RuleResult -> Bool
isDeny (Deny _) = True
isDeny _ = False

-- | The name credited for an admission, if any (the engine credits by name).
admittedBy :: Decision -> Maybe Text
admittedBy (Admitted name _) = Just name
admittedBy _ = Nothing

-- | The name credited for a block, if any.
blockedBy :: Decision -> Maybe Text
blockedBy (Blocked name _) = Just name
blockedBy _ = Nothing

-- | Mark the version as running code on install, for the deny-rule tests.
withInstallScripts :: PackageDetails -> PackageDetails
withInstallScripts pd = pd{pkgInstallCode = RunsCodeOnInstall "postinstall hook"}

-- | Put a rule at an explicit precedence (the operator-override form).
at :: Int -> Rule -> PrecededRule
at = PrecededRule

{- | Decide a built-in policy through the one engine ('prepare' then 'evalRules'), in
'IO'. The pure vocabulary now has no pure entry point, so the tests run in 'IO'.
-}
decide :: [PrecededRule] -> PackageDetails -> IO Decision
decide prs pd = prepare prs >>= \prepared -> evalRules ctx prepared pd

genScope :: Gen Text
genScope = Gen.text (Range.linear 1 12) Gen.alpha

genAgeDays :: Gen Integer
genAgeDays = Gen.integral (Range.linear 0 3650)

genPrecedence :: Gen Int
genPrecedence = Gen.int (Range.linear 0 1000)

{- | A rule that takes a position against the package the order-independence
property builds (scoped @scopeTxt@, old, running install scripts): a matching
'AllowScope', an 'AllowIfPublishedBefore' it is old enough for, or
'DenyInstallTimeExecution'. None yields no decision, so every generated rule competes.
-}
genFiringRule :: Text -> Gen Rule
genFiringRule scopeTxt =
    Gen.element
        [ AllowScope (mkScope scopeTxt)
        , AllowIfPublishedBefore (7 * nominalDay)
        , DenyInstallTimeExecution
        ]

{- | Canonicalise a decision for order-independence comparison: the audit-reason
list of a 'BlockedByDefault' is sorted, since reasons are gathered in boot order and
a permutation of the configured set only reorders the non-decisive ones at equal
precedence. 'Admitted' \/ 'Blocked' are left as-is — a permutation cannot change the
credited rule, since the boot order resolves every equal-precedence tie by name.
-}
canonical :: Decision -> Decision
canonical (BlockedByDefault reasons) = BlockedByDefault (sort reasons)
canonical d = d

spec :: Spec
spec = do
    describe "evalRule" $ do
        it "AllowScope allows a matching scope" $
            evalRule ctx (AllowScope (mkScope "myorg")) (pkg (Just "myorg") 0)
                >>= (`shouldSatisfy` isAllow)
        it "AllowScope yields no decision on a non-matching scope" $
            evalRule ctx (AllowScope (mkScope "myorg")) (pkg (Just "other") 0)
                >>= (`shouldSatisfy` isNoDecision)
        it "AllowScope yields no decision on an unscoped package" $
            evalRule ctx (AllowScope (mkScope "myorg")) (pkg Nothing 0)
                >>= (`shouldSatisfy` isNoDecision)
        it "AllowIfPublishedBefore allows a version older than the threshold" $
            evalRule ctx (AllowIfPublishedBefore (7 * nominalDay)) (pkg Nothing 30)
                >>= (`shouldSatisfy` isAllow)
        it "AllowIfPublishedBefore yields no decision on a too-young version" $
            evalRule ctx (AllowIfPublishedBefore (7 * nominalDay)) (pkg Nothing 1)
                >>= (`shouldSatisfy` isNoDecision)
        it "DenyInstallTimeExecution denies a package that runs install scripts" $
            evalRule ctx DenyInstallTimeExecution (withInstallScripts (pkg Nothing 99))
                >>= (`shouldSatisfy` isDeny)
        it "DenyInstallTimeExecution yields no decision when there are no install scripts" $
            evalRule ctx DenyInstallTimeExecution (pkg Nothing 99)
                >>= (`shouldSatisfy` isNoDecision)

    describe "PrecededRule" $ do
        it "exposes the precedence and rule it was built with" $ do
            -- The fields a config loader reads to patch a rule's precedence.
            let pr = PrecededRule 250 DenyInstallTimeExecution
            rulePrecedence pr `shouldBe` 250
            prRule pr `shouldBe` DenyInstallTimeExecution
        it "shows both fields" $
            show (PrecededRule 250 DenyInstallTimeExecution)
                `shouldBe` ("PrecededRule {rulePrecedence = 250, prRule = DenyInstallTimeExecution}" :: String)

    describe "defaultPrecedence" $ do
        it "ranks every deny default strictly above every allow default" $
            -- The out-of-the-box invariant: a matching deny overrides any allow.
            defaultPrecedence DenyInstallTimeExecution
                `shouldSatisfy` (\d -> d > defaultPrecedence (AllowScope (mkScope "x")) && d > defaultPrecedence (AllowIfPublishedBefore 0))
        it "atDefaultPrecedence pairs a rule with its type default" $
            atDefaultPrecedence DenyInstallTimeExecution
                `shouldBe` PrecededRule defaultDenyInstallTimeExecutionPrecedence DenyInstallTimeExecution

    describe "bootOrder" $ do
        it "orders highest precedence first, then rule name ascending" $ do
            -- A shuffled configured set arranges into one total order: precedence
            -- descending, then name as the deterministic tiebreak.
            rules <-
                prepare
                    [ at 100 (AllowIfPublishedBefore (7 * nominalDay))
                    , at 300 DenyInstallTimeExecution
                    , at 200 (AllowScope (mkScope "myorg"))
                    ]
            map prepName (bootOrder rules)
                `shouldBe` ["DenyInstallTimeExecution", "AllowScope", "AllowIfPublishedBefore"]
        it "breaks an equal-precedence tie by name ascending" $ do
            rules <-
                prepare
                    [ at 200 (AllowScope (mkScope "myorg"))
                    , at 200 (AllowIfPublishedBefore (7 * nominalDay))
                    ]
            map prepName (bootOrder rules)
                `shouldBe` ["AllowIfPublishedBefore", "AllowScope"]

    describe "renderBootOrder" $ do
        it "emits one line per rule, in boot order, with each precedence" $ do
            rules <-
                prepare
                    [ at 100 (AllowIfPublishedBefore (7 * nominalDay))
                    , at 300 DenyInstallTimeExecution
                    ]
            renderBootOrder rules
                `shouldBe` [ "rule 1: DenyInstallTimeExecution (precedence 300)"
                           , "rule 2: AllowIfPublishedBefore (precedence 100)"
                           ]
        it "is empty for an empty rule set" $
            prepare [] >>= \rules -> renderBootOrder rules `shouldBe` []

    describe "evalRules" $ do
        it "denies by default with no rules" $
            decide [] (pkg (Just "myorg") 99) >>= (`shouldBe` BlockedByDefault [])
        it "admits via the single matching allow rule" $
            decide [atDefaultPrecedence (AllowScope (mkScope "myorg"))] (pkg (Just "myorg") 0)
                >>= \d -> admittedBy d `shouldBe` Just "AllowScope"
        it "the higher-precedence allow wins among allows" $
            -- The version is too young for the age rule, but the scope rule
            -- matches; at default precedences the scope allow outranks it anyway.
            decide
                (map atDefaultPrecedence [AllowIfPublishedBefore (7 * nominalDay), AllowScope (mkScope "myorg")])
                (pkg (Just "myorg") 0)
                >>= \d -> admittedBy d `shouldBe` Just "AllowScope"
        it "a matching deny rule overrides an allow at default precedence, whatever the order" $ do
            let rs = map atDefaultPrecedence [AllowScope (mkScope "myorg"), DenyInstallTimeExecution]
                p = withInstallScripts (pkg (Just "myorg") 99)
            decide rs p >>= \d -> blockedBy d `shouldBe` Just "DenyInstallTimeExecution"
            decide (reverse rs) p >>= \d -> blockedBy d `shouldBe` Just "DenyInstallTimeExecution"
        it "resolves an equal-precedence allow-vs-deny tie by name, not by deny-priority" $ do
            -- The deliberate change from the two-tier design: at *equal explicit*
            -- precedence there is no deny-over-allow runtime rule — the boot order
            -- resolves the tie by name. "AllowScope" sorts before
            -- "DenyInstallTimeExecution", so the allow is credited even though the
            -- version also trips the deny. (Deny-over-allow still holds out of the
            -- box, where the deny default sits strictly higher.)
            let rs = [at 300 (AllowScope (mkScope "myorg")), at 300 DenyInstallTimeExecution]
                p = withInstallScripts (pkg (Just "myorg") 99)
            decide rs p >>= \d -> admittedBy d `shouldBe` Just "AllowScope"
            decide (reverse rs) p >>= \d -> admittedBy d `shouldBe` Just "AllowScope"
        it "breaks an equal-precedence allow-vs-allow tie by name, regardless of order" $ do
            -- Two allows fire at the *same* precedence; the tie is resolved by name
            -- (the smallest ruleName), not list position, so the same rule is
            -- credited whichever order it is supplied in.
            -- "AllowIfPublishedBefore" sorts before "AllowScope", so it is credited.
            let allows =
                    [ at 150 (AllowScope (mkScope "myorg"))
                    , at 150 (AllowIfPublishedBefore (7 * nominalDay))
                    ]
                p = pkg (Just "myorg") 30
            decide allows p >>= \d -> admittedBy d `shouldBe` Just "AllowIfPublishedBefore"
            decide (reverse allows) p >>= \d -> admittedBy d `shouldBe` Just "AllowIfPublishedBefore"
        it "an operator-elevated allow outranks a higher-default deny" $
            -- The scope allow is lifted above the deny's default precedence, so a
            -- trusted internal scope is admitted despite running install scripts.
            decide
                [ at (defaultDenyInstallTimeExecutionPrecedence + 1) (AllowScope (mkScope "myorg"))
                , atDefaultPrecedence DenyInstallTimeExecution
                ]
                (withInstallScripts (pkg (Just "myorg") 99))
                >>= \d -> admittedBy d `shouldBe` Just "AllowScope"
        it "denies by default when every rule is non-decisive, collecting each reason in boot order" $
            -- The audit trail carries each non-decisive rule's actual reason, in
            -- boot order (highest precedence first): AllowScope (200) then
            -- AllowIfPublishedBefore (100).
            decide
                (map atDefaultPrecedence [AllowIfPublishedBefore (7 * nominalDay), AllowScope (mkScope "myorg")])
                (pkg (Just "other") 1)
                >>= \case
                    BlockedByDefault reasons ->
                        reasons
                            `shouldBe` [ "scope is not the allow-listed @myorg"
                                       , "published only 1 day ago, minimum age is 7 days"
                                       ]
                    other -> expectationFailure ("expected BlockedByDefault, got " <> show other)

    describe "properties" $ do
        it "an empty rule set always denies by default" $
            hedgehog $ do
                mScope <- forAll (Gen.maybe genScope)
                ageDays <- forAll genAgeDays
                d <- liftIO (decide [] (pkg mScope ageDays))
                d === BlockedByDefault []

        it "every rule non-decisive yields deny-by-default" $
            hedgehog $ do
                -- A non-matching scope and a too-young age make both allows
                -- yield no decision; the package runs no install scripts so the deny
                -- does too — so whatever the precedences, nothing fires.
                scopeTxt <- forAll genScope
                otherTxt <- forAll (Gen.filter (/= scopeTxt) genScope)
                precs <- forAll (Gen.list (Range.singleton 3) genPrecedence)
                let rules =
                        zipWith
                            PrecededRule
                            precs
                            [AllowScope (mkScope scopeTxt), AllowIfPublishedBefore (7 * nominalDay), DenyInstallTimeExecution]
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
                -- Precedences may collide, so equal-precedence ties — including an
                -- allow-vs-allow tie where two firing allows share a precedence —
                -- are exercised. The boot order resolves every tie by name, so the
                -- credited winner (and the whole 'Decision') is order-independent;
                -- only the gathered reason order tracks the input list within a tie,
                -- so 'canonical' sorts it before comparing.
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

    describe "renderDecision" $ do
        let pd = pkg (Just "myorg") 0
        it "renders an admission naming the rule and its reason" $
            renderDecision pd (Admitted "AllowScope" "scope @myorg is allow-listed")
                `shouldSatisfy` (\t -> T.isInfixOf "AllowScope" t && T.isInfixOf "approved" t)
        it "renders a block naming the rule and its reason" $
            renderDecision pd (Blocked "DenyAdvisory" "affected by an advisory")
                `shouldSatisfy` (\t -> T.isInfixOf "DenyAdvisory" t && T.isInfixOf "affected by an advisory" t)
        it "renders a deny-by-default explaining no rule allowed it" $
            renderDecision pd (BlockedByDefault ["scope is not the allow-listed @myorg"])
                `shouldSatisfy` (\t -> T.isInfixOf "no rule allowed it" t && T.isInfixOf "allow-listed" t)
        it "renders an undecidable outcome explaining it could not be evaluated" $
            renderDecision pd (Undecidable (WillResolve Nothing) "the advisory source is down")
                `shouldSatisfy` (\t -> T.isInfixOf "could not be evaluated" t && T.isInfixOf "advisory source is down" t)
