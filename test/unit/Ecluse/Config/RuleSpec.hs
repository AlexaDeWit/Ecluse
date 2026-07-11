-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.RuleSpec (spec) where

import Data.Aeson (Value (Object), eitherDecodeStrict)
import Data.Aeson.Types (parseEither, (.!=), (.:?))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Config (defaultPolicy)
import Ecluse.Config.Rule
import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Rules.Types (
    DenyIfCveParams (..),
    FailureAlignment (..),
    PrecededRule (..),
    Rule (..),
    defaultAllowByIdentityPrecedence,
    defaultAllowIfOlderThanPrecedence,
    defaultAllowIfRemediatesCvePrecedence,
    defaultDenyIfCvePrecedence,
    defaultDenyInstallTimeExecutionPrecedence,
    ruleName,
 )

spec :: Spec
spec = describe "rulePolicySpec" $ do
    describe "resolveJson" $ do
        it "overrides a default rule's precedence" $
            resolveJson "{\"rules\":{\"min-age\":{\"precedence\":175}}}"
                `shouldBe` Right
                    [ PrecededRule defaultAllowIfRemediatesCvePrecedence AllowIfRemediatesCve
                    , PrecededRule 175 (AllowIfOlderThan (7 * 86400))
                    ]

        it "adds a new rule that carries a full type at its type's default precedence" $
            resolveJson "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\"}}}"
                `shouldBe` Right
                    [ PrecededRule defaultAllowIfOlderThanPrecedence (AllowIfOlderThan (7 * 86400))
                    , PrecededRule defaultAllowIfRemediatesCvePrecedence AllowIfRemediatesCve
                    , PrecededRule defaultDenyInstallTimeExecutionPrecedence DenyInstallTimeExecution
                    ]

        it "adds a new rule with an explicit precedence" $
            resolveJson "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\",\"precedence\":275}}}"
                `shouldBe` Right
                    [ PrecededRule defaultAllowIfOlderThanPrecedence (AllowIfOlderThan (7 * 86400))
                    , PrecededRule defaultAllowIfRemediatesCvePrecedence AllowIfRemediatesCve
                    , PrecededRule 275 DenyInstallTimeExecution
                    ]

        it "suppresses a default rule with enabled:false" $
            resolveJson "{\"rules\":{\"min-age\":{\"enabled\":false}}}"
                `shouldBe` Right [PrecededRule defaultAllowIfRemediatesCvePrecedence AllowIfRemediatesCve]

        it "suppresses the default remediation fast lane with enabled:false" $
            resolveJson "{\"rules\":{\"remediation-fast-track\":{\"enabled\":false}}}"
                `shouldBe` Right [PrecededRule defaultAllowIfOlderThanPrecedence (AllowIfOlderThan (7 * 86400))]

        it "adds an AllowScope rule from a scope field" $
            resolveJson "{\"rules\":{\"trusted\":{\"type\":\"AllowScope\",\"scope\":\"myorg\"}}}"
                `shouldSatisfy` containsAllowScope

        it "adds a new AllowIfOlderThan rule from a valid ageSeconds" $
            resolveJson "{\"rules\":{\"young\":{\"type\":\"AllowIfOlderThan\",\"ageSeconds\":100}}}"
                `shouldSatisfy` either
                    (const False)
                    (elem (PrecededRule defaultAllowIfOlderThanPrecedence (AllowIfOlderThan 100)))

        it "accepts a restated type on a patch that matches the default's kind" $
            resolveJson "{\"rules\":{\"min-age\":{\"type\":\"AllowIfOlderThan\",\"ageSeconds\":100}}}"
                `shouldBe` Right
                    [ PrecededRule defaultAllowIfOlderThanPrecedence (AllowIfOlderThan 100)
                    , PrecededRule defaultAllowIfRemediatesCvePrecedence AllowIfRemediatesCve
                    ]

        it "rejects a restated type on a patch that changes the default's kind" $
            resolveJson "{\"rules\":{\"min-age\":{\"type\":\"DenyInstallTimeExecution\"}}}"
                `shouldBe` Left [MalformedRule "min-age" "\"type\" \"DenyInstallTimeExecution\" does not match the default rule it patches"]

        it "rejects a restated unknown type on a patch" $
            resolveJson "{\"rules\":{\"min-age\":{\"type\":\"Bogus\"}}}"
                `shouldBe` Left [UnknownRuleType "min-age" "Bogus"]

        it "rejects a negative ageSeconds when adding a rule" $
            resolveJson "{\"rules\":{\"young\":{\"type\":\"AllowIfOlderThan\",\"ageSeconds\":-1}}}"
                `shouldBe` Left [MalformedRule "young" "\"ageSeconds\" must be non-negative"]

        it "rejects a negative ageSeconds when patching the default" $
            resolveJson "{\"rules\":{\"min-age\":{\"ageSeconds\":-1}}}"
                `shouldBe` Left [MalformedRule "min-age" "\"ageSeconds\" must be non-negative"]

        it "rejects adding an AllowIfOlderThan without ageSeconds" $
            resolveJson "{\"rules\":{\"young\":{\"type\":\"AllowIfOlderThan\"}}}"
                `shouldBe` Left [MalformedRule "young" "\"AllowIfOlderThan\" requires \"ageSeconds\""]

        it "adds an AllowIfRemediatesCve rule at its type's default precedence" $
            resolveJson "{\"rules\":{\"cve-fast-lane\":{\"type\":\"AllowIfRemediatesCve\"}}}"
                `shouldSatisfy` either
                    (const False)
                    (elem (PrecededRule defaultAllowIfRemediatesCvePrecedence AllowIfRemediatesCve))

        it "adds an AllowByIdentity rule from an identity field" $
            resolveJson "{\"rules\":{\"pinned-fix\":{\"type\":\"AllowByIdentity\",\"identity\":\"left-pad@1.3.0\"}}}"
                `shouldSatisfy` either
                    (const False)
                    (elem (PrecededRule defaultAllowByIdentityPrecedence (AllowByIdentity "left-pad@1.3.0")))

        it "rejects adding an AllowByIdentity without identity" $
            resolveJson "{\"rules\":{\"pinned-fix\":{\"type\":\"AllowByIdentity\"}}}"
                `shouldBe` Left [MalformedRule "pinned-fix" "\"AllowByIdentity\" requires \"identity\""]

    describe "DenyIfCve (add, patch, and validation)" $ do
        it "adds a DenyIfCve from a minSeverity, defaulting onUnavailable to fail-closed" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minSeverity\":8}}}"
                `shouldSatisfy` hasRuleAtPrec defaultDenyIfCvePrecedence (DenyIfCve (DenyIfCveParams 8 FailDeny))

        it "reads onUnavailable:skip as fail-open" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minSeverity\":5.5,\"onUnavailable\":\"skip\"}}}"
                `shouldSatisfy` hasRuleAtPrec defaultDenyIfCvePrecedence (DenyIfCve (DenyIfCveParams 5.5 FailNoDecision))

        it "reads onUnavailable:deny as fail-closed" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minSeverity\":8,\"onUnavailable\":\"deny\"}}}"
                `shouldSatisfy` hasRuleAtPrec defaultDenyIfCvePrecedence (DenyIfCve (DenyIfCveParams 8 FailDeny))

        it "rejects a DenyIfCve add missing its minSeverity" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\"}}}"
                `shouldBe` Left [MalformedRule "deny-cve" "\"DenyIfCve\" requires \"minSeverity\""]

        it "rejects a minSeverity above the CVSS range" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minSeverity\":11}}}"
                `shouldBe` Left [MalformedRule "deny-cve" "\"minSeverity\" must be a CVSS score between 0 and 10"]

        it "rejects a negative minSeverity" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minSeverity\":-1}}}"
                `shouldBe` Left [MalformedRule "deny-cve" "\"minSeverity\" must be a CVSS score between 0 and 10"]

        it "rejects an unknown onUnavailable value" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minSeverity\":8,\"onUnavailable\":\"maybe\"}}}"
                `shouldBe` Left [MalformedRule "deny-cve" "\"onUnavailable\" must be \"deny\" or \"skip\", not \"maybe\""]

        it "patches an existing DenyIfCve's minSeverity, keeping its alignment" $
            resolveJsonOver cveBase "{\"rules\":{\"deny-cve\":{\"minSeverity\":9}}}"
                `shouldSatisfy` hasRuleAtPrec defaultDenyIfCvePrecedence (DenyIfCve (DenyIfCveParams 9 FailDeny))

        it "patches an existing DenyIfCve's alignment, keeping its minSeverity" $
            resolveJsonOver cveBase "{\"rules\":{\"deny-cve\":{\"onUnavailable\":\"skip\"}}}"
                `shouldSatisfy` hasRuleAtPrec defaultDenyIfCvePrecedence (DenyIfCve (DenyIfCveParams 5 FailNoDecision))

        it "validates minSeverity on the patch path too, not only on add" $
            resolveJsonOver cveBase "{\"rules\":{\"deny-cve\":{\"minSeverity\":50}}}"
                `shouldBe` Left [MalformedRule "deny-cve" "\"minSeverity\" must be a CVSS score between 0 and 10"]

    describe "merging over a multi-rule shared policy" $ do
        it "overrides an AllowScope default's scope and precedence" $
            resolveJsonOver mixedBase "{\"rules\":{\"trusted\":{\"scope\":\"other\",\"precedence\":205}}}"
                `shouldSatisfy` hasRuleAtPrec 205 (AllowScope (mkScope "other"))

        it "keeps an AllowScope default's scope when only its precedence changes" $
            resolveJsonOver mixedBase "{\"rules\":{\"trusted\":{\"precedence\":210}}}"
                `shouldSatisfy` hasRuleAtPrec 210 (AllowScope (mkScope "myorg"))

        it "patches a DenyInstallTimeExecution default's precedence" $
            resolveJsonOver mixedBase "{\"rules\":{\"deny-scripts\":{\"precedence\":350}}}"
                `shouldSatisfy` hasRuleAtPrec 350 DenyInstallTimeExecution

        it "accepts a restated matching type on an AllowScope default" $
            resolveJsonOver mixedBase "{\"rules\":{\"trusted\":{\"type\":\"AllowScope\",\"scope\":\"acme\"}}}"
                `shouldSatisfy` hasRuleAtPrec 200 (AllowScope (mkScope "acme"))

        it "accepts a restated matching type on a DenyInstallTimeExecution default" $
            resolveJsonOver mixedBase "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\"}}}"
                `shouldSatisfy` hasRuleAtPrec 300 DenyInstallTimeExecution

        it "rejects a restated mismatching type on a DenyInstallTimeExecution default" $
            resolveJsonOver mixedBase "{\"rules\":{\"deny-scripts\":{\"type\":\"AllowScope\"}}}"
                `shouldBe` Left [MalformedRule "deny-scripts" "\"type\" \"AllowScope\" does not match the default rule it patches"]

        it "suppresses one rule from a multi-rule base, keeping the rest" $
            resolveJsonOver mixedBase "{\"rules\":{\"trusted\":{\"enabled\":false}}}"
                `shouldBe` Right
                    [ PrecededRule 100 (AllowIfOlderThan (7 * 86400))
                    , PrecededRule 300 DenyInstallTimeExecution
                    ]

    describe "fail-loud merge references" $ do
        let cases :: [(String, ByteString, [PolicyError])]
            cases =
                [
                    ( "an unknown rule type (a typo'd deny must not vanish)"
                    , "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecutio\"}}}"
                    , [UnknownRuleType "deny-scripts" "DenyInstallTimeExecutio"]
                    )
                ,
                    ( "a mis-cased rule type (DenyIfCVE is not the shipped DenyIfCve)"
                    , "{\"rules\":{\"cve\":{\"type\":\"DenyIfCVE\"}}}"
                    , [UnknownRuleType "cve" "DenyIfCVE"]
                    )
                ,
                    ( "a new name missing its type"
                    , "{\"rules\":{\"mystery\":{\"precedence\":120}}}"
                    , [MissingRuleType "mystery"]
                    )
                ,
                    ( "a suppression of a rule no default defines"
                    , "{\"rules\":{\"min-aeg\":{\"enabled\":false}}}"
                    , [SuppressUnknownRule "min-aeg"]
                    )
                ,
                    ( "an AllowScope add missing its scope value"
                    , "{\"rules\":{\"trusted\":{\"type\":\"AllowScope\"}}}"
                    , [MalformedRule "trusted" "\"AllowScope\" requires \"scope\""]
                    )
                ]
        for_ cases $ \(label, body, expected) ->
            it ("rejects " <> label) $
                resolveJson body `shouldBe` Left expected

    it "aggregates every merge error in one run (not fail-on-first)" $ do
        let body =
                "{\"rules\":{\"bad-type\":{\"type\":\"Nope\"},\"ghost\":{\"enabled\":false}}}"
        case resolveJson body of
            Left errs ->
                errs
                    `shouldMatchList` [UnknownRuleType "bad-type" "Nope", SuppressUnknownRule "ghost"]
            Right rs -> expectationFailure ("expected aggregated errors, got " <> show rs)

    describe "rule-type name contract" $ do
        it "covers exactly the diagnostic knownRuleTypes list (neither has drifted)" $
            map fst knownRuleAdds `shouldMatchList` knownRuleTypes

        it "accepts every known rule type and round-trips it through ruleName" $
            for_ knownRuleAdds $ \(ty, body) ->
                case resolveJsonOver emptyPolicy body of
                    Right [PrecededRule _ rule] -> ruleName rule `shouldBe` ty
                    other ->
                        expectationFailure
                            (T.unpack ty <> ": expected a single resolved rule, got " <> show other)

        it "rejects restating a default as another known type with MalformedRule, not UnknownRuleType" $
            for_ (filter (/= "AllowIfOlderThan") knownRuleTypes) $ \ty ->
                resolveJson ("{\"rules\":{\"min-age\":{\"type\":\"" <> encodeUtf8 ty <> "\"}}}")
                    `shouldBe` Left
                        [MalformedRule "min-age" ("\"type\" \"" <> ty <> "\" does not match the default rule it patches")]

        it "rejects restating a default as an unknown type with UnknownRuleType" $
            resolveJson "{\"rules\":{\"min-age\":{\"type\":\"AllowIfOlderThat\"}}}"
                `shouldBe` Left [UnknownRuleType "min-age" "AllowIfOlderThat"]

resolveJson :: ByteString -> Either [PolicyError] [PrecededRule]
resolveJson = resolveJsonOver defaultPolicy

resolveJsonOver :: RulePolicy -> ByteString -> Either [PolicyError] [PrecededRule]
resolveJsonOver base body = case eitherDecodeStrict body :: Either String Value of
    Left e -> Left [MalformedRule "<decode>" (T.pack e)]
    Right (Object o) -> case parseEither (\obj -> obj .:? "rules" .!= emptyPatch) o of
        Left err -> Left [MalformedRule "<parse>" (T.pack err)]
        Right patch -> sortOn rulePrecedence . Map.elems . policyRules <$> resolvePolicy base patch
    Right _ -> Left [MalformedRule "<parse>" "expected object"]

mixedBase :: RulePolicy
mixedBase =
    RulePolicy
        ( Map.fromList
            [ ("min-age", PrecededRule 100 (AllowIfOlderThan (7 * 86400)))
            , ("trusted", PrecededRule 200 (AllowScope (mkScope "myorg")))
            , ("deny-scripts", PrecededRule 300 DenyInstallTimeExecution)
            ]
        )

-- | A base policy carrying a DenyIfCve rule, for exercising the patch path.
cveBase :: RulePolicy
cveBase =
    RulePolicy
        (Map.fromList [("deny-cve", PrecededRule defaultDenyIfCvePrecedence (DenyIfCve (DenyIfCveParams 5 FailDeny)))])

containsAllowScope :: Either [PolicyError] [PrecededRule] -> Bool
containsAllowScope (Right rs) = any isAllowScope rs
  where
    isAllowScope (PrecededRule _ (AllowScope _)) = True
    isAllowScope _ = False
containsAllowScope _ = False

hasRuleAtPrec :: Int -> Rule -> Either [PolicyError] [PrecededRule] -> Bool
hasRuleAtPrec prec rule (Right rs) = PrecededRule prec rule `elem` rs
hasRuleAtPrec _ _ _ = False

{- | A minimal well-formed "add" patch for each rule type, keyed by its type name.
Tied to 'knownRuleTypes' by the "covers exactly" expectation, so a new 'Rule' type
cannot be added without extending this table (and through it the round-trip pin), and
the round-trip proves each type's 'buildRule' branch and 'ruleName' agree on the name.
-}
knownRuleAdds :: [(Text, ByteString)]
knownRuleAdds =
    [ ("AllowScope", "{\"rules\":{\"r\":{\"type\":\"AllowScope\",\"scope\":\"myorg\"}}}")
    , ("AllowIfOlderThan", "{\"rules\":{\"r\":{\"type\":\"AllowIfOlderThan\",\"ageSeconds\":100}}}")
    , ("AllowByIdentity", "{\"rules\":{\"r\":{\"type\":\"AllowByIdentity\",\"identity\":\"left-pad@1.3.0\"}}}")
    , ("AllowIfRemediatesCve", "{\"rules\":{\"r\":{\"type\":\"AllowIfRemediatesCve\"}}}")
    , ("DenyIfCve", "{\"rules\":{\"r\":{\"type\":\"DenyIfCve\",\"minSeverity\":8}}}")
    , ("DenyInstallTimeExecution", "{\"rules\":{\"r\":{\"type\":\"DenyInstallTimeExecution\"}}}")
    , ("DenyByIdentity", "{\"rules\":{\"r\":{\"type\":\"DenyByIdentity\",\"identity\":\"left-pad\"}}}")
    ]
