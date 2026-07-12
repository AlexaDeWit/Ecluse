-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Rules.PolicySpec (spec) where

import Test.Hspec

import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Rules.Policy
import Ecluse.Test.Rules (atDefaultPrecedence)

spec :: Spec
spec = do
    describe "PrecededRule" $ do
        it "exposes the precedence and rule it was built with" $ do
            -- The fields a config loader reads to patch a rule's precedence.
            let pr = PrecededRule 250 DenyInstallTimeExecution
            rulePrecedence pr `shouldBe` 250
            prRule pr `shouldBe` DenyInstallTimeExecution
        it "shows both fields" $
            show (PrecededRule 250 DenyInstallTimeExecution)
                `shouldBe` ("PrecededRule {rulePrecedence = 250, prRule = DenyInstallTimeExecution}" :: String)

    describe "defaultPrecedence" $ do
        it "ranks every deny default strictly above every allow default" $ do
            -- The out-of-the-box invariant: a matching deny overrides any allow.
            let allows = [AllowScope (mkScope "x"), AllowIfOlderThan 0, AllowByIdentity "x", AllowIfRemediatesCve]
            defaultPrecedence DenyInstallTimeExecution
                `shouldSatisfy` (\d -> all ((d >) . defaultPrecedence) allows)
        it "orders the allow band by explicitness: quarantine < fast lane < scope < identity" $
            ( defaultAllowIfOlderThanPrecedence
            , defaultAllowIfRemediatesCvePrecedence
            , defaultAllowScopePrecedence
            , defaultAllowByIdentityPrecedence
            )
                `shouldSatisfy` (\(q, f, s, i) -> q < f && f < s && s < i)
        it "atDefaultPrecedence pairs a rule with its type default" $
            atDefaultPrecedence DenyInstallTimeExecution
                `shouldBe` PrecededRule defaultDenyInstallTimeExecutionPrecedence DenyInstallTimeExecution
