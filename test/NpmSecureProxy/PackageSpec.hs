module NpmSecureProxy.PackageSpec (spec) where

import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import NpmSecureProxy.Package

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

    describe "renderPackageName" $ do
        it "renders a scoped package as @scope/name" $
            renderPackageName (PackageName (Just (mkScope "myorg")) "thing")
                `shouldBe` "@myorg/thing"
        it "renders an unscoped package as just the name" $
            renderPackageName (PackageName Nothing "thing") `shouldBe` "thing"

    describe "Version" $
        it "round-trips through mkVersion / unVersion" $
            hedgehog $ do
                v <- forAll (Gen.text (Range.linear 1 12) Gen.ascii)
                unVersion (mkVersion v) === v
