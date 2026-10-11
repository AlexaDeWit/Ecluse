-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Registration checks inspect labels without executing fixture acquisition or assertions.
module Ecluse.Test.Contract.BindingSpec (spec) where

import Test.Hspec

import Ecluse.Test.Contract.Binding
import Ecluse.Test.Contract.Catalogue

spec :: Spec
spec = describe "labelledCaseIds" $ do
    it "does not run acquisition, examples, or cleanup" $ do
        effects <- newIORef (0 :: Int)
        let body =
                beforeAll_ (modifyIORef' effects (+ 1)) $
                    afterAll_ (modifyIORef' effects (+ 1)) $
                        it
                            (caseLabel "read/identity" "identity")
                            (modifyIORef' effects (+ 1) :: Expectation)
        labelledCaseIds body `shouldReturn` ["read/identity"]
        readIORef effects `shouldReturn` 0
    it "retains duplicate labels so the caller cannot lose duplicate evidence" $ do
        let body = replicateM_ 2 $ it (caseLabel "read/identity" "identity") (pure () :: Expectation)
        labelledCaseIds body `shouldReturn` ["read/identity", "read/identity"]
    it "keeps native unlabelled cases outside the migrated inventory" $
        labelledCaseIds (it "native parser oracle" (pure () :: Expectation)) `shouldReturn` []
    it "preserves malformed explicit labels for catalogue rejection" $
        for_ ["[INVALID] identity", "[read/identity]", "[read/identity identity", "[] identity"] $ \label ->
            labelledCaseIds (it label (pure () :: Expectation)) `shouldReturn` ["malformed:" <> toText label]
    it "preserves unknown valid labels for obligation rejection" $
        labelledCaseIds (it (caseLabel "unknown/identity" "identity") (pure () :: Expectation)) `shouldReturn` ["unknown/identity"]
    it "keeps every fixed-suite example in the registration check" $ do
        allCaseIds (it "native oracle" (pure () :: Expectation)) `shouldReturn` ["unregistered:native oracle"]
        allCaseIds (it (caseLabel "read/identity" "identity") (pure () :: Expectation)) `shouldReturn` ["read/identity"]
