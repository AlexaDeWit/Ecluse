-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Test.Registry.Npm.StoredVersionsSpec (spec) where

import Test.Hspec

import Ecluse.Test.Registry.Npm.StoredVersions (storedVersionKeys)

spec :: Spec
spec = describe "strict store inventory observations" $ do
    it "recognises the exact present version" $
        storedVersionKeys 200 "{\"versions\":{\"1.0.0\":{}}}" `shouldBe` Right ["1.0.0"]
    it "does not confuse a version substring or a metadata value with membership" $ do
        let observed = storedVersionKeys 200 "{\"description\":\"1.0.0\",\"versions\":{\"11.0.0\":{\"note\":\"1.0.0\"}}}"
        observed `shouldBe` Right ["11.0.0"]
        fmap (elem "1.0.0") observed `shouldBe` Right False
    it "recognises a readable empty inventory" $
        storedVersionKeys 200 "{\"versions\":{}}" `shouldBe` Right []
    it "recognises only HTTP 404 as absence" $ do
        storedVersionKeys 404 "not found" `shouldBe` Right []
        for_ [401, 403, 429, 500, 503] $ \status ->
            storedVersionKeys status "{}" `shouldBe` Left ("packument returned HTTP " <> show status)
    it "rejects malformed JSON and missing or invalid versions" $
        for_ ["not JSON", "{}", "{\"versions\":null}", "{\"versions\":[]}"] $ \body ->
            storedVersionKeys 200 body `shouldSatisfy` isLeft
