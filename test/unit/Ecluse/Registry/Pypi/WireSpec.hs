module Ecluse.Registry.Pypi.WireSpec (spec) where

import Data.Aeson (eitherDecode)
import Test.Hspec

import Ecluse.Core.Registry.Pypi.Wire (ProjectJson, projectVersions)

spec :: Spec
spec = describe "Ecluse.Core.Registry.Pypi.Wire" $ do
    it "reads the published versions from the releases map" $ do
        let body = "{\"info\":{},\"releases\":{\"1.0.0\":[],\"2.0.0a1\":[{\"url\":\"x\"}]}}"
        (projectVersions <$> eitherDecode body) `shouldBe` Right ["1.0.0", "2.0.0a1"]

    it "treats a document with no releases object as no versions (lenient)" $ do
        let body = "{\"info\":{}}"
        (projectVersions <$> eitherDecode body) `shouldBe` Right []

    it "fails to decode a non-object body" $
        (eitherDecode "[]" :: Either String ProjectJson) `shouldSatisfy` isLeft
