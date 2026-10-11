-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Runner parsing excludes filters that would split a shared lifecycle.
module Ecluse.Test.Contract.OptionsSpec (spec) where

import Test.Hspec

import Ecluse.Test.Contract.Options
import Ecluse.Test.Contract.Types

spec :: Spec
spec = describe "parseCommand" $ do
    it "keeps discovery independent of execution" $ do
        parseCommand ["--catalogue-list"] `shouldBe` Right ListCatalogue
        parseCommand ["--catalogue-plan"] `shouldBe` Right PrintPlan
    it "retains an audit with no reports so completeness rejects missing artifacts" $
        parseCommand ["--audit", "plan.json"] `shouldBe` Right (Audit "plan.json" [])
    it "passes exact selection and replay information" $
        parseCommand ["--group", "example/read", "--seed", "42", "--timeout", "60", "--report", "result.json"]
            `shouldBe` Right (Execute $ RunOptions emptySelection{selectionGroup = Just "example/read"} (Just "result.json") 42 (Just 60) False)
    forM_ [["--match", "anything"], ["--dry-run"], ["--skip", "anything"], ["--group"], ["--timeout", "0"], ["--seed", "invalid"]] $ \arguments ->
        it ("rejects " <> show arguments) $ parseCommand arguments `shouldSatisfy` isLeft
