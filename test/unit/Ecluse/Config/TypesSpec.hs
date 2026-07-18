-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.TypesSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Config.Rule (PolicyError (..), renderPolicyError)
import Ecluse.Config.Types (mkUrl, unUrl)

spec :: Spec
spec = do
    urlSpec
    policyErrorRenderSpec

urlSpec :: Spec
urlSpec = describe "Url" $ do
    it "trims surrounding whitespace and round-trips" $
        (unUrl <$> mkUrl "  https://x  ") `shouldBe` Right "https://x"
    it "rejects an all-whitespace value" $
        mkUrl "   " `shouldBe` Left "expected a non-empty URL"

policyErrorRenderSpec :: Spec
policyErrorRenderSpec = describe "renderPolicyError" $
    -- Each constructor renders a distinct, operator-facing line.
    it "renders every policy-error kind" $ do
        renderPolicyError (MissingRuleType "x") `shouldSatisfy` ("missing" `T.isInfixOf`)
        renderPolicyError (UnknownRuleType "x" "Y") `shouldSatisfy` ("unknown type" `T.isInfixOf`)
        renderPolicyError (MalformedRule "x" "bad") `shouldSatisfy` ("bad" `T.isInfixOf`)
        renderPolicyError (SuppressUnknownRule "x") `shouldSatisfy` ("disables" `T.isInfixOf`)
