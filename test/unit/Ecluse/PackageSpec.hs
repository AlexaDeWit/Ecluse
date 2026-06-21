module Ecluse.PackageSpec (spec) where

import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Package

spec :: Spec
spec = do
    describe "Scope" $ do
        it "mkScope strips a leading '@'" $
            unScope (mkScope "@myorg") `shouldBe` "myorg"
        it "mkScope leaves an unprefixed scope unchanged" $
            unScope (mkScope "myorg") `shouldBe` "myorg"
        it "renderScope adds the leading '@'" $
            renderScope (mkScope "@myorg") `shouldBe` "@myorg"
        it "normalises scopes regardless of a leading '@'" $
            hedgehog $ do
                s <- forAll (Gen.text (Range.linear 1 16) Gen.alphaNum)
                mkScope ("@" <> s) === mkScope s

    describe "mkPackageName" $ do
        it "renders a scoped npm package as @scope/name" $
            renderPackageName (mkPackageName Npm (Just (mkScope "myorg")) "thing")
                `shouldBe` "@myorg/thing"
        it "renders an unscoped package as just the name" $
            renderPackageName (mkPackageName Npm Nothing "thing") `shouldBe` "thing"
        it "keeps npm canonical names verbatim (case-sensitive)" $
            pkgCanonical (mkPackageName Npm Nothing "Thing") `shouldBe` "Thing"
        it "normalises PyPI names per PEP 503" $
            pkgCanonical (mkPackageName PyPI Nothing "Flask_Thing.X")
                `shouldBe` "flask-thing-x"
        it "treats PyPI names equal up to normalisation" $
            mkPackageName PyPI Nothing "Flask" `shouldBe` mkPackageName PyPI Nothing "flask"

    describe "mkVersion / parseVersionKey" $ do
        it "round-trips the raw text through unVersion" $
            hedgehog $ do
                v <- forAll (Gen.text (Range.linear 1 12) Gen.ascii)
                unVersion (mkVersion Npm v) === v
        it "keeps the raw text even when unparseable (proxy fidelity)" $
            unVersion (mkVersion PyPI "totally bogus") `shouldBe` "totally bogus"
        it "has no key for unparseable input" $
            versionKey (mkVersion PyPI "totally bogus") `shouldBe` Nothing
        it "parses a valid version into a key" $
            versionKey (mkVersion Npm "1.2.3") `shouldSatisfy` isJust
        it "parseVersionKey reports an error for invalid input" $
            parseVersionKey Npm "nope" `shouldSatisfy` isLeft

    describe "compareVersions" $ do
        let cmp eco a b = compareVersions (mkVersion eco a) (mkVersion eco b)
        it "npm orders release numbers numerically (10 > 9)" $
            cmp Npm "1.10.0" "1.9.0" `shouldBe` Just GT
        it "npm ranks a prerelease below its release" $
            cmp Npm "1.0.0-rc.1" "1.0.0" `shouldBe` Just LT
        it "npm ranks a numeric prerelease id below an alphanumeric one" $
            cmp Npm "1.0.0-1" "1.0.0-alpha" `shouldBe` Just LT
        it "npm ranks more prerelease fields above fewer" $
            cmp Npm "1.0.0-alpha" "1.0.0-alpha.1" `shouldBe` Just LT
        it "PyPI treats trailing zeros as equal (1.0 == 1.0.0)" $
            cmp PyPI "1.0" "1.0.0" `shouldBe` Just EQ
        it "PyPI ranks a dev release below the final" $
            cmp PyPI "1.0.dev1" "1.0" `shouldBe` Just LT
        it "PyPI ranks a prerelease below the final" $
            cmp PyPI "1.0a1" "1.0" `shouldBe` Just LT
        it "PyPI ranks a post-release above the final" $
            cmp PyPI "1.0.post1" "1.0" `shouldBe` Just GT
        it "PyPI canonicalises a non-normalised spelling (1.0ALPHA1 == 1.0a1)" $
            cmp PyPI "1.0ALPHA1" "1.0a1" `shouldBe` Just EQ
        it "RubyGems ranks a letter (prerelease) segment below the release" $
            cmp RubyGems "1.0.0.beta1" "1.0.0" `shouldBe` Just LT
        it "RubyGems orders numeric segments numerically" $
            cmp RubyGems "1.10.0" "1.9.0" `shouldBe` Just GT
        it "is Nothing when a version cannot be parsed" $
            cmp Npm "not a version" "1.0.0" `shouldBe` Nothing
        it "is reflexive — EQ when parseable, Nothing otherwise" $
            hedgehog $ do
                eco <- forAll (Gen.element [Npm, PyPI, RubyGems])
                ver <-
                    forAll
                        ( Gen.text
                            (Range.linear 1 12)
                            (Gen.element ('.' : '-' : ['0' .. '9'] <> "abrcdevpost"))
                        )
                let x = mkVersion eco ver
                compareVersions x x === (EQ <$ versionKey x)
