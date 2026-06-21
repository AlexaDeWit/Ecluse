module Ecluse.PackageSpec (spec) where

import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Ecosystem (Ecosystem (..))
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
