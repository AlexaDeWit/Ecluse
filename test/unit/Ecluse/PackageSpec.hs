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

    describe "Version" $
        it "round-trips through mkVersion / unVersion" $
            hedgehog $ do
                v <- forAll (Gen.text (Range.linear 1 12) Gen.ascii)
                unVersion (mkVersion v) === v

    describe "compareVersion" $ do
        let v = mkVersion
        it "npm orders release numbers numerically (10 > 9)" $
            compareVersion Npm (v "1.10.0") (v "1.9.0") `shouldBe` GT
        it "npm ranks a prerelease below its release" $
            compareVersion Npm (v "1.0.0-rc.1") (v "1.0.0") `shouldBe` LT
        it "npm ranks a numeric prerelease id below an alphanumeric one" $
            compareVersion Npm (v "1.0.0-1") (v "1.0.0-alpha") `shouldBe` LT
        it "npm ranks more prerelease fields above fewer" $
            compareVersion Npm (v "1.0.0-alpha") (v "1.0.0-alpha.1") `shouldBe` LT
        it "PyPI treats trailing zeros as equal (1.0 == 1.0.0)" $
            compareVersion PyPI (v "1.0") (v "1.0.0") `shouldBe` EQ
        it "PyPI ranks a dev release below the final" $
            compareVersion PyPI (v "1.0.dev1") (v "1.0") `shouldBe` LT
        it "PyPI ranks a prerelease below the final" $
            compareVersion PyPI (v "1.0a1") (v "1.0") `shouldBe` LT
        it "PyPI ranks a post-release above the final" $
            compareVersion PyPI (v "1.0.post1") (v "1.0") `shouldBe` GT
        it "RubyGems ranks a letter (prerelease) segment below the release" $
            compareVersion RubyGems (v "1.0.0.beta1") (v "1.0.0") `shouldBe` LT
        it "RubyGems orders numeric segments numerically" $
            compareVersion RubyGems (v "1.10.0") (v "1.9.0") `shouldBe` GT
        it "is reflexive for every ecosystem" $
            hedgehog $ do
                eco <- forAll (Gen.element [Npm, PyPI, RubyGems])
                ver <-
                    forAll
                        ( Gen.text
                            (Range.linear 1 12)
                            (Gen.element ('.' : '-' : ['0' .. '9'] <> "abrcdevpost"))
                        )
                compareVersion eco (mkVersion ver) (mkVersion ver) === EQ
