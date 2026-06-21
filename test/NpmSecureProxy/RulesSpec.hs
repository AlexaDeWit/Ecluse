module NpmSecureProxy.RulesSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, nominalDay)
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import NpmSecureProxy.Package
import NpmSecureProxy.Rules
import NpmSecureProxy.Rules.Types

-- | A fixed "now" so age-based tests are deterministic.
now :: UTCTime
now = UTCTime (fromGregorian 2026 6 20) 0

ctx :: EvalContext
ctx = EvalContext now

{- | A package version under an optional scope, published @ageDays@ days before
'now'. Other fields are fixed; rules under test only read scope and age.
-}
pkg :: Maybe Text -> Integer -> PackageDetails
pkg mScope ageDays =
    PackageDetails
        { pkgName =
            PackageName
                { packageScope = mkScope <$> mScope
                , packageBaseName = "thing"
                }
        , pkgVersion = mkVersion "1.0.0"
        , pkgPublishedAt = addUTCTime (negate (fromInteger ageDays * nominalDay)) now
        , pkgHasInstallScripts = False
        , pkgDeprecated = Nothing
        , pkgDist = Dist "https://example.test/thing-1.0.0.tgz" Nothing Nothing
        , pkgLicense = Just "MIT"
        , pkgMaintainers = []
        , pkgDependencies = Map.empty
        }

isAllow :: RuleOutcome -> Bool
isAllow (Allow _) = True
isAllow _ = False

isAbstain :: RuleOutcome -> Bool
isAbstain (Abstain _) = True
isAbstain _ = False

approvedBy :: Decision -> Maybe Rule
approvedBy (Approved r _) = Just r
approvedBy _ = Nothing

genScope :: Gen Text
genScope = Gen.text (Range.linear 1 12) Gen.alpha

genAgeDays :: Gen Integer
genAgeDays = Gen.integral (Range.linear 0 3650)

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

    describe "evalRules" $ do
        it "denies by default with no rules" $
            evalRules ctx [] (pkg (Just "myorg") 99) `shouldBe` DeniedByDefault []
        it "approves via the first rule that allows" $
            approvedBy (evalRules ctx [AllowScope (mkScope "myorg")] (pkg (Just "myorg") 0))
                `shouldBe` Just (AllowScope (mkScope "myorg"))
        it "the first decisive rule wins" $
            -- The version is too young for the age rule, but the (earlier) scope
            -- rule matches, so the scope rule decides.
            approvedBy
                ( evalRules
                    ctx
                    [AllowScope (mkScope "myorg"), AllowIfPublishedBefore (7 * nominalDay)]
                    (pkg (Just "myorg") 0)
                )
                `shouldBe` Just (AllowScope (mkScope "myorg"))
        it "denies by default when every rule abstains" $
            case evalRules
                ctx
                [AllowScope (mkScope "myorg"), AllowIfPublishedBefore (7 * nominalDay)]
                (pkg (Just "other") 1) of
                DeniedByDefault reasons -> length reasons `shouldBe` 2
                other -> expectationFailure ("expected DeniedByDefault, got " <> show other)

    describe "properties" $ do
        it "an empty rule set always denies by default" $
            hedgehog $ do
                mScope <- forAll (Gen.maybe genScope)
                ageDays <- forAll genAgeDays
                evalRules ctx [] (pkg mScope ageDays) === DeniedByDefault []

        it "a matching AllowScope always approves, whatever the age" $
            hedgehog $ do
                scopeTxt <- forAll genScope
                ageDays <- forAll genAgeDays
                let s = mkScope scopeTxt
                approvedBy (evalRules ctx [AllowScope s] (pkg (Just scopeTxt) ageDays))
                    === Just (AllowScope s)
