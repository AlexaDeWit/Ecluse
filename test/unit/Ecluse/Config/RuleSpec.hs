-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.RuleSpec (spec) where

import Prelude hiding (universe)

import Data.Aeson (Value (Object), eitherDecodeStrict)
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Universe.Class (Universe (universe))
import Test.Hspec

import Ecluse.Config (decodeDefaultConfig)
import Ecluse.Config.Aeson (parseRulesPatch)
import Ecluse.Config.DefaultConfig (defaultConfigBytes)
import Ecluse.Config.Rule
import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Rules.Types (
    AllowIfRemediatesCveParams (..),
    DenyIfCveParams (..),
    DenyIfEpssParams (..),
    DifferentialLimits (..),
    FailureAlignment (..),
    PrecededRule (..),
    RemainingAdvisories (..),
    Rule (..),
    RuleReach (..),
    RuleType,
    ScoreGap (..),
    defaultAllowByIdentityPrecedence,
    defaultAllowIfOlderThanPrecedence,
    defaultAllowIfRemediatesCvePrecedence,
    defaultAllowScopePrecedence,
    defaultDenyIfCvePrecedence,
    defaultDenyIfEpssPrecedence,
    defaultDenyInstallTimeExecutionPrecedence,
    defaultDifferentialLimits,
    ruleName,
 )
import Ecluse.Core.Wire (renderWire)
import Ecluse.Test.Rules (admissionOnly, atPrecedence, differentialRemediation, remediationWith, shippedRemediation)

spec :: Spec
spec = do
    rulePolicySpec
    remediationSpec
    appliesToSpec
    policyErrorRenderSpec

policyErrorRenderSpec :: Spec
policyErrorRenderSpec = describe "renderPolicyError" $
    -- Each constructor renders a distinct, operator-facing line.
    it "renders every policy-error kind" $ do
        renderPolicyError (MissingRuleType "x") `shouldSatisfy` ("missing" `T.isInfixOf`)
        renderPolicyError (UnknownRuleType "x" "Y") `shouldSatisfy` ("unknown type" `T.isInfixOf`)
        renderPolicyError (MalformedRule "x" "bad") `shouldSatisfy` ("bad" `T.isInfixOf`)
        renderPolicyError (SuppressUnknownRule "x") `shouldSatisfy` ("disables" `T.isInfixOf`)

rulePolicySpec :: Spec
rulePolicySpec = describe "rulePolicySpec" $ do
    describe "resolveJson" $ do
        it "overrides a default rule's precedence" $
            resolveJson "{\"rules\":{\"min-age\":{\"precedence\":175}}}"
                `shouldBe` Right
                    [ atPrecedence defaultAllowIfRemediatesCvePrecedence shippedRemediation
                    , atPrecedence 175 (AllowIfOlderThan (7 * 86400))
                    ]

        it "adds a new rule that carries a full type at its type's default precedence" $
            resolveJson "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\"}}}"
                `shouldBe` Right
                    [ atPrecedence defaultAllowIfOlderThanPrecedence (AllowIfOlderThan (7 * 86400))
                    , atPrecedence defaultAllowIfRemediatesCvePrecedence shippedRemediation
                    , atPrecedence defaultDenyInstallTimeExecutionPrecedence DenyInstallTimeExecution
                    ]

        it "adds a new rule with an explicit precedence" $
            resolveJson "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\",\"precedence\":275}}}"
                `shouldBe` Right
                    [ atPrecedence defaultAllowIfOlderThanPrecedence (AllowIfOlderThan (7 * 86400))
                    , atPrecedence defaultAllowIfRemediatesCvePrecedence shippedRemediation
                    , atPrecedence 275 DenyInstallTimeExecution
                    ]

        it "suppresses a default rule with enabled:false" $
            resolveJson "{\"rules\":{\"min-age\":{\"enabled\":false}}}"
                `shouldBe` Right [atPrecedence defaultAllowIfRemediatesCvePrecedence shippedRemediation]

        it "suppresses the default remediation fast lane with enabled:false" $
            resolveJson "{\"rules\":{\"remediation-fast-track\":{\"enabled\":false}}}"
                `shouldBe` Right [atPrecedence defaultAllowIfOlderThanPrecedence (AllowIfOlderThan (7 * 86400))]

        it "adds an AllowScope rule from a scope field" $
            resolveJson "{\"rules\":{\"trusted\":{\"type\":\"AllowScope\",\"scope\":\"myorg\"}}}"
                `shouldResolveTo` (atPrecedence defaultAllowScopePrecedence (AllowScope (mkScope "myorg")) : shippedRules)

        it "adds a new AllowIfOlderThan rule from a valid ageSeconds" $
            resolveJson "{\"rules\":{\"young\":{\"type\":\"AllowIfOlderThan\",\"ageSeconds\":100}}}"
                `shouldResolveTo` (atPrecedence defaultAllowIfOlderThanPrecedence (AllowIfOlderThan 100) : shippedRules)

        it "accepts a restated type on a patch that matches the default's kind" $
            resolveJson "{\"rules\":{\"min-age\":{\"type\":\"AllowIfOlderThan\",\"ageSeconds\":100}}}"
                `shouldBe` Right
                    [ atPrecedence defaultAllowIfOlderThanPrecedence (AllowIfOlderThan 100)
                    , atPrecedence defaultAllowIfRemediatesCvePrecedence shippedRemediation
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
            -- A second rule of the shipped fast lane's own type, so the resolved policy
            -- carries both rather than one overwriting the other.
            resolveJson "{\"rules\":{\"cve-fast-lane\":{\"type\":\"AllowIfRemediatesCve\",\"onUnavailable\":\"skip\",\"remainingAdvisories\":\"differential\"}}}"
                `shouldResolveTo` (atPrecedence defaultAllowIfRemediatesCvePrecedence (differentialRemediation FailNoDecision) : shippedRules)

        it "adds an AllowByIdentity rule from an identity field" $
            resolveJson "{\"rules\":{\"pinned-fix\":{\"type\":\"AllowByIdentity\",\"identity\":\"left-pad@1.3.0\"}}}"
                `shouldResolveTo` (atPrecedence defaultAllowByIdentityPrecedence (AllowByIdentity "left-pad@1.3.0") : shippedRules)

        it "rejects adding an AllowByIdentity without identity" $
            resolveJson "{\"rules\":{\"pinned-fix\":{\"type\":\"AllowByIdentity\"}}}"
                `shouldBe` Left [MalformedRule "pinned-fix" "\"AllowByIdentity\" requires \"identity\""]

    describe "DenyIfKnownMalicious validation" $
        it "rejects malformed unavailable handling on a manual rule declaration" $
            resolveJson "{\"rules\":{\"manual-mal\":{\"type\":\"DenyIfKnownMalicious\",\"onUnavailable\":\"maybe\"}}}"
                `shouldBe` Left [MalformedRule "manual-mal" "\"onUnavailable\" must be \"deny\" or \"skip\", not \"maybe\""]

    describe "DenyIfCve (add, patch, and validation)" $ do
        it "adds a DenyIfCve from a minCvss, defaulting onUnavailable to fail-closed" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minCvss\":8}}}"
                `shouldSatisfy` hasRuleAtPrec defaultDenyIfCvePrecedence (DenyIfCve (DenyIfCveParams 8 FailDeny))

        it "reads onUnavailable:skip as fail-open" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minCvss\":5.5,\"onUnavailable\":\"skip\"}}}"
                `shouldSatisfy` hasRuleAtPrec defaultDenyIfCvePrecedence (DenyIfCve (DenyIfCveParams 5.5 FailNoDecision))

        it "reads onUnavailable:deny as fail-closed" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minCvss\":8,\"onUnavailable\":\"deny\"}}}"
                `shouldSatisfy` hasRuleAtPrec defaultDenyIfCvePrecedence (DenyIfCve (DenyIfCveParams 8 FailDeny))

        it "rejects a DenyIfCve add missing its minCvss" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\"}}}"
                `shouldBe` Left [MalformedRule "deny-cve" "\"DenyIfCve\" requires \"minCvss\""]

        it "rejects a minCvss above the CVSS range" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minCvss\":11}}}"
                `shouldBe` Left [MalformedRule "deny-cve" "\"minCvss\" must be a CVSS score between 0 and 10"]

        it "rejects a negative minCvss" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minCvss\":-1}}}"
                `shouldBe` Left [MalformedRule "deny-cve" "\"minCvss\" must be a CVSS score between 0 and 10"]

        it "rejects an unknown onUnavailable value" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minCvss\":8,\"onUnavailable\":\"maybe\"}}}"
                `shouldBe` Left [MalformedRule "deny-cve" "\"onUnavailable\" must be \"deny\" or \"skip\", not \"maybe\""]

        it "patches an existing DenyIfCve's minCvss, keeping its alignment" $
            resolveJsonOver cveBase "{\"rules\":{\"deny-cve\":{\"minCvss\":9}}}"
                `shouldSatisfy` hasRuleAtPrec defaultDenyIfCvePrecedence (DenyIfCve (DenyIfCveParams 9 FailDeny))

        it "patches an existing DenyIfCve's alignment, keeping its minCvss" $
            resolveJsonOver cveBase "{\"rules\":{\"deny-cve\":{\"onUnavailable\":\"skip\"}}}"
                `shouldSatisfy` hasRuleAtPrec defaultDenyIfCvePrecedence (DenyIfCve (DenyIfCveParams 5 FailNoDecision))

        it "validates minCvss on the patch path too, not only on add" $
            resolveJsonOver cveBase "{\"rules\":{\"deny-cve\":{\"minCvss\":50}}}"
                `shouldBe` Left [MalformedRule "deny-cve" "\"minCvss\" must be a CVSS score between 0 and 10"]

        -- The rename carries no alias, so the old spelling must not reach the threshold as a
        -- silently absent one. The rule group refuses it as an unknown key before that.
        it "refuses the retired minSeverity spelling on the add and the patch path" $ do
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minSeverity\":8}}}"
                `shouldSatisfy` refusalMentions "minSeverity"
            resolveJsonOver cveBase "{\"rules\":{\"deny-cve\":{\"minSeverity\":9}}}"
                `shouldSatisfy` refusalMentions "minSeverity"

    describe "DenyIfEpss (add, patch, and validation)" $ do
        it "adds a DenyIfEpss from a minEpss, defaulting onUnavailable to fail-closed" $
            resolveJson "{\"rules\":{\"deny-epss\":{\"type\":\"DenyIfEpss\",\"minEpss\":0.5}}}"
                `shouldSatisfy` hasRuleAtPrec defaultDenyIfEpssPrecedence (DenyIfEpss (DenyIfEpssParams 0.5 FailDeny))

        it "reads onUnavailable:skip as fail-open" $
            resolveJson "{\"rules\":{\"deny-epss\":{\"type\":\"DenyIfEpss\",\"minEpss\":0.5,\"onUnavailable\":\"skip\"}}}"
                `shouldSatisfy` hasRuleAtPrec defaultDenyIfEpssPrecedence (DenyIfEpss (DenyIfEpssParams 0.5 FailNoDecision))

        it "rejects an add missing its minEpss" $
            resolveJson "{\"rules\":{\"deny-epss\":{\"type\":\"DenyIfEpss\"}}}"
                `shouldBe` Left [MalformedRule "deny-epss" "\"DenyIfEpss\" requires \"minEpss\""]

        it "rejects a minEpss outside the probability range" $ do
            resolveJson "{\"rules\":{\"deny-epss\":{\"type\":\"DenyIfEpss\",\"minEpss\":1.5}}}"
                `shouldBe` Left [MalformedRule "deny-epss" "\"minEpss\" must be an EPSS probability between 0 and 1"]
            resolveJson "{\"rules\":{\"deny-epss\":{\"type\":\"DenyIfEpss\",\"minEpss\":-0.1}}}"
                `shouldBe` Left [MalformedRule "deny-epss" "\"minEpss\" must be an EPSS probability between 0 and 1"]

        it "patches an existing rule's threshold, keeping its alignment" $
            resolveJsonOver epssBase "{\"rules\":{\"deny-epss\":{\"minEpss\":0.9}}}"
                `shouldSatisfy` hasRuleAtPrec defaultDenyIfEpssPrecedence (DenyIfEpss (DenyIfEpssParams 0.9 FailDeny))

        it "validates minEpss on the patch path too, not only on add" $
            resolveJsonOver epssBase "{\"rules\":{\"deny-epss\":{\"minEpss\":2}}}"
                `shouldBe` Left [MalformedRule "deny-epss" "\"minEpss\" must be an EPSS probability between 0 and 1"]

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
                    [ atPrecedence 100 (AllowIfOlderThan (7 * 86400))
                    , atPrecedence 300 DenyInstallTimeExecution
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

    describe "a rule reads only its own type's parameters" $ do
        let straySays name ty key =
                Left [MalformedRule name ("\"" <> ty <> "\" does not read \"" <> key <> "\"")]

        it "refuses a foreign parameter on an added rule" $
            resolveJson "{\"rules\":{\"young\":{\"type\":\"AllowIfOlderThan\",\"ageSeconds\":100,\"minCvss\":8}}}"
                `shouldBe` straySays "young" "AllowIfOlderThan" "minCvss"

        it "refuses a foreign parameter on a patched default" $
            resolveJson "{\"rules\":{\"min-age\":{\"ageSeconds\":100,\"onUnavailable\":\"skip\"}}}"
                `shouldBe` straySays "min-age" "AllowIfOlderThan" "onUnavailable"

        it "refuses the other deny's threshold on each advisory rule" $ do
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"minCvss\":8,\"minEpss\":0.5}}}"
                `shouldBe` straySays "deny-cve" "DenyIfCve" "minEpss"
            resolveJson "{\"rules\":{\"deny-epss\":{\"type\":\"DenyIfEpss\",\"minEpss\":0.5,\"minCvss\":8}}}"
                `shouldBe` straySays "deny-epss" "DenyIfEpss" "minCvss"

        it "refuses a foreign parameter on a patched DenyIfCve" $
            resolveJsonOver cveBase "{\"rules\":{\"deny-cve\":{\"minEpss\":0.5}}}"
                `shouldBe` straySays "deny-cve" "DenyIfCve" "minEpss"

        it "names every stray at once, in schema order" $
            resolveJson "{\"rules\":{\"r\":{\"type\":\"AllowIfRemediatesCve\",\"scope\":\"acme\",\"minEpss\":0.5}}}"
                `shouldBe` Left
                    [MalformedRule "r" "\"AllowIfRemediatesCve\" does not read \"scope\", \"minEpss\""]

        it "refuses a parameter on DenyExternalDependencies, which reads none" $
            resolveJson "{\"rules\":{\"deps\":{\"type\":\"DenyExternalDependencies\",\"identity\":\"left-pad\"}}}"
                `shouldBe` straySays "deps" "DenyExternalDependencies" "identity"

        it "reports the stray even when a required parameter is also missing" $
            resolveJson "{\"rules\":{\"deny-cve\":{\"type\":\"DenyIfCve\",\"ageSeconds\":1}}}"
                `shouldBe` straySays "deny-cve" "DenyIfCve" "ageSeconds"

        it "still reports an unknown type rather than blaming its parameters" $
            resolveJson "{\"rules\":{\"r\":{\"type\":\"Bogus\",\"minCvss\":8}}}"
                `shouldBe` Left [UnknownRuleType "r" "Bogus"]

        it "never counts type, precedence, or enabled as a rule's own parameter" $ do
            resolveJsonOver emptyPolicy "{\"rules\":{\"r\":{\"type\":\"DenyInstallTimeExecution\",\"precedence\":275}}}"
                `shouldBe` Right [atPrecedence 275 DenyInstallTimeExecution]
            resolveJson "{\"rules\":{\"min-age\":{\"enabled\":false}}}"
                `shouldBe` Right [atPrecedence defaultAllowIfRemediatesCvePrecedence shippedRemediation]

    describe "availability rule phases" $
        for_ [("DenyIfYanked" :: Text, DenyIfYanked), ("DenyIfDeprecated", DenyIfDeprecated)] $ \(ty, rule) ->
            it ("preserves, widens and disables " <> toString ty) $ do
                let added = "{\"rules\":{\"availability\":{\"type\":\"" <> encodeUtf8 ty <> "\"}}}"
                    limited = RulePolicy (Map.singleton "availability" (admissionOnly (atPrecedence 300 rule)))
                resolveJsonOver emptyPolicy added `shouldBe` Right [atPrecedence 300 rule]
                resolveJsonOver limited "{\"rules\":{\"availability\":{\"precedence\":350}}}" `shouldBe` Right [admissionOnly (atPrecedence 350 rule)]
                resolveJsonOver limited "{\"rules\":{\"availability\":{\"appliesTo\":[\"admission\",\"revocation\"]}}}" `shouldBe` Right [atPrecedence 300 rule]
                resolveJsonOver limited "{\"rules\":{\"availability\":{\"enabled\":false}}}" `shouldBe` Right []

    describe "rule-type name contract" $ do
        it "uses the canonical typed vocabulary for diagnostic names" $
            knownRuleTypes `shouldBe` map renderWire (universe :: [RuleType])

        it "covers exactly the diagnostic knownRuleTypes list (neither has drifted)" $
            map fst knownRuleAdds `shouldMatchList` knownRuleTypes

        it "accepts every known rule type and round-trips it through ruleName" $
            for_ knownRuleAdds $ \(ty, body) ->
                case resolveJsonOver emptyPolicy body of
                    Right [PrecededRule _ _ rule] -> ruleName rule `shouldBe` ty
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

appliesToSpec :: Spec
appliesToSpec = describe "appliesTo, the phases a rule applies at" $ do
    describe "a rule with no setting" $ do
        it "applies at both phases, shipped or added" $
            fmap (map ruleReach) (resolveJson "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\"}}}")
                `shouldBe` Right [AdmissionAndRevocation, AdmissionAndRevocation, AdmissionAndRevocation]

        it "keeps the phases of the rule it patches" $
            resolveJsonOver limitedBase "{\"rules\":{\"deny-scripts\":{\"precedence\":350}}}"
                `shouldBe` Right [admissionOnly (atPrecedence 350 DenyInstallTimeExecution)]

    describe "a stated list" $ do
        it "limits a deny to admission" $
            resolveJson (addedDeny "[\"admission\"]")
                `shouldResolveTo` (admissionOnly (atPrecedence defaultDenyInstallTimeExecutionPrecedence DenyInstallTimeExecution) : shippedRules)

        it "reads both words as both phases, in either order and with a word repeated" $
            for_ ["[\"admission\",\"revocation\"]", "[\"revocation\",\"admission\"]", "[\"admission\",\"revocation\",\"admission\"]"] $ \phases ->
                resolveJsonOver emptyPolicy ("{\"rules\":{\"r\":{\"type\":\"DenyByIdentity\",\"identity\":\"left-pad\",\"appliesTo\":" <> phases <> "}}}")
                    `shouldBe` Right [atPrecedence 400 (DenyByIdentity "left-pad")]

        it "reads a repeated admission as admission alone" $
            resolveJsonOver emptyPolicy "{\"rules\":{\"r\":{\"type\":\"DenyInstallTimeExecution\",\"appliesTo\":[\"admission\",\"admission\"]}}}"
                `shouldBe` Right [admissionOnly (atPrecedence 300 DenyInstallTimeExecution)]

        it "widens a rule the layer below limits to admission" $
            resolveJsonOver limitedBase "{\"rules\":{\"deny-scripts\":{\"appliesTo\":[\"admission\",\"revocation\"]}}}"
                `shouldBe` Right [atPrecedence 300 DenyInstallTimeExecution]

        it "narrows a rule the layer below applies at both phases" $
            resolveJsonOver mixedBase "{\"rules\":{\"deny-scripts\":{\"appliesTo\":[\"admission\"]}}}"
                `shouldSatisfy` either (const False) (elem (admissionOnly (atPrecedence 300 DenyInstallTimeExecution)))

        it "admits admission alone on each deny type, and refuses it on each allow type" $ do
            let limited ty = [resolveJsonOver emptyPolicy (limitedToAdmission body) | (known, body) <- knownRuleAdds, known == ty]
                denies = ["DenyIfKnownMalicious", "DenyIfYanked", "DenyIfDeprecated", "DenyByIdentity", "DenyInstallTimeExecution", "DenyExternalDependencies", "DenyIfCve", "DenyIfEpss"]
                allows = ["AllowByIdentity", "AllowScope", "AllowIfOlderThan", "AllowIfRemediatesCve"]
            (denies <> allows) `shouldMatchList` knownRuleTypes
            for_ denies $ \ty -> map (fmap (map ruleReach)) (limited ty) `shouldBe` [Right [AdmissionOnly]]
            for_ allows $ \ty -> limited ty `shouldBe` [Left [MalformedRule "r" (allowLimitedToAdmission ty)]]

    describe "an ambiguous setting" $ do
        let cases :: [(String, ByteString, [PolicyError])]
            cases =
                [ ("an empty list", addedDeny "[]", [MalformedRule "deny-scripts" namesNoPhase])
                , ("revocation alone", addedDeny "[\"revocation\"]", [MalformedRule "deny-scripts" revocationAlone])
                ,
                    ( "an added allow limited to admission"
                    , "{\"rules\":{\"pinned\":{\"type\":\"AllowByIdentity\",\"identity\":\"left-pad\",\"appliesTo\":[\"admission\"]}}}"
                    , [MalformedRule "pinned" (allowLimitedToAdmission "AllowByIdentity")]
                    )
                ,
                    ( "a shipped allow limited to admission by a patch"
                    , "{\"rules\":{\"min-age\":{\"appliesTo\":[\"admission\"]}}}"
                    , [MalformedRule "min-age" (allowLimitedToAdmission "AllowIfOlderThan")]
                    )
                , ("an unknown word", addedDeny "[\"admission\",\"serve\"]", [MalformedRule "deny-scripts" (unknownPhase "serve")])
                , ("a word for the second phase other than revocation", addedDeny "[\"admission\",\"pruning\"]", [MalformedRule "deny-scripts" (unknownPhase "pruning")])
                ,
                    ( "every unknown word, each once"
                    , addedDeny "[\"serve\",\"mirror\",\"serve\"]"
                    , [MalformedRule "deny-scripts" (unknownPhase "serve"), MalformedRule "deny-scripts" (unknownPhase "mirror")]
                    )
                ]
        for_ cases $ \(label, body, expected) ->
            it ("refuses " <> label) $
                resolveJson body `shouldBe` Left expected

        it "refuses a value that is not a list of words, naming the key" $
            for_ ["\"admission\"", "true", "[1]", "{\"admission\":true}"] $ \value ->
                resolveJson (addedDeny value) `shouldSatisfy` refusalMentions "appliesTo"

        it "refuses an empty string, in place of the list and inside it" $ do
            resolveJson (addedDeny "\"\"") `shouldSatisfy` refusalMentions "rule.appliesTo: parsing [] failed, expected Array, but encountered String"
            resolveJson (addedDeny "[\"\"]") `shouldBe` Left [MalformedRule "deny-scripts" (unknownPhase "")]
            resolveJson (addedDeny "[\"admission\",\"\"]") `shouldBe` Left [MalformedRule "deny-scripts" (unknownPhase "")]

        it "refuses a key written with no value, on an added rule and on a patch" $ do
            -- An empty value must not read as both phases, which is the reading that deletes.
            resolveJson (addedDeny "null") `shouldSatisfy` refusalMentions "rule.appliesTo is written with no value"
            resolveJsonOver limitedBase "{\"rules\":{\"deny-scripts\":{\"appliesTo\":null}}}"
                `shouldSatisfy` refusalMentions "rule.appliesTo is written with no value"

        it "reports every refused setting of one load together" $ do
            let body =
                    "{\"rules\":{\"empty\":{\"type\":\"DenyInstallTimeExecution\",\"appliesTo\":[]}"
                        <> ",\"alone\":{\"type\":\"DenyInstallTimeExecution\",\"appliesTo\":[\"revocation\"]}"
                        <> ",\"min-age\":{\"appliesTo\":[\"admission\"]}"
                        <> ",\"typo\":{\"type\":\"DenyInstallTimeExecution\",\"appliesTo\":[\"admision\"]}"
                        <> ",\"ghost\":{\"enabled\":false}}}"
            case resolveJson body of
                Left errs ->
                    errs
                        `shouldMatchList` [ MalformedRule "empty" namesNoPhase
                                          , MalformedRule "alone" revocationAlone
                                          , MalformedRule "min-age" (allowLimitedToAdmission "AllowIfOlderThan")
                                          , MalformedRule "typo" (unknownPhase "admision")
                                          , SuppressUnknownRule "ghost"
                                          ]
                Right rs -> expectationFailure ("expected every refusal, got " <> show rs)

    describe "beside enabled: false" $
        it "switches the rule off at both phases, and reads no other key" $ do
            -- The document may limit a rule that the environment then switches off, on one merged entry.
            resolveJsonOver limitedBase "{\"rules\":{\"deny-scripts\":{\"enabled\":false,\"appliesTo\":[\"admission\",\"revocation\"]}}}"
                `shouldBe` Right []
            resolveJson "{\"rules\":{\"min-age\":{\"enabled\":false,\"appliesTo\":[\"admission\"]}}}"
                `shouldBe` Right [atPrecedence defaultAllowIfRemediatesCvePrecedence shippedRemediation]

-- | Every row of the remediation rule's resolution table, each with the exact result or refusal.
remediationSpec :: Spec
remediationSpec = describe "AllowIfRemediatesCve (choices, limits, and validation)" $ do
    let shippedAt = atPrecedence defaultAllowIfRemediatesCvePrecedence
        minAge = atPrecedence defaultAllowIfOlderThanPrecedence (AllowIfOlderThan (7 * 86400))
        patched params = Right [minAge, shippedAt (AllowIfRemediatesCve params)]
        within base = AllowIfRemediatesCveParams FailDeny (Differential base)
        malformed name reason = Left [MalformedRule name reason]
        onlyDifferential key = "\"" <> key <> "\" applies only when \"remainingAdvisories\" is \"differential\""
        abstainingBase = RulePolicy (Map.singleton "fast" (shippedAt (remediationWith FailDeny AbstainOnRemaining)))
        tunedBase = RulePolicy (Map.singleton "fast" (shippedAt (remediationWith FailNoDecision (Differential tuned))))
        tuned = DifferentialLimits 5.0 0.05 (GapOfAtLeast 2.0) (GapOfAtLeast 0.01)
        added body = "{\"rules\":{\"r\":{\"type\":\"AllowIfRemediatesCve\"" <> body <> "}}}"

    it "resolves the shipped rule to deny and the shipped differential limits" $
        resolveJson "{\"rules\":{}}" `shouldBe` patched (within defaultDifferentialLimits)

    it "keeps the inherited mode and limits under an unavailability patch" $
        resolveJson "{\"rules\":{\"remediation-fast-track\":{\"onUnavailable\":\"skip\"}}}"
            `shouldBe` patched (AllowIfRemediatesCveParams FailNoDecision (Differential defaultDifferentialLimits))

    it "replaces one patched limit and inherits the others" $
        resolveJson "{\"rules\":{\"remediation-fast-track\":{\"maxRemainingCvss\":5.0}}}"
            `shouldBe` patched (within defaultDifferentialLimits{dlMaxRemainingCvss = 5.0})

    it "reads a zero gap as no gap condition" $
        resolveJson "{\"rules\":{\"remediation-fast-track\":{\"minCvssGap\":0,\"minEpssGap\":0.0}}}"
            `shouldBe` patched (within defaultDifferentialLimits{dlCvssGap = NoGapRequired})

    it "drops the inherited limits when a patch switches to abstain" $
        resolveJson "{\"rules\":{\"remediation-fast-track\":{\"remainingAdvisories\":\"abstain\"}}}"
            `shouldBe` patched (AllowIfRemediatesCveParams FailDeny AbstainOnRemaining)

    it "refuses each limit a switch to abstain supplies" $
        resolveJson "{\"rules\":{\"remediation-fast-track\":{\"remainingAdvisories\":\"abstain\",\"maxRemainingCvss\":5,\"minEpssGap\":0.1}}}"
            `shouldBe` Left [MalformedRule "remediation-fast-track" (onlyDifferential "maxRemainingCvss"), MalformedRule "remediation-fast-track" (onlyDifferential "minEpssGap")]

    it "refuses a limit patched onto a rule that already abstains" $
        resolveJsonOver abstainingBase "{\"rules\":{\"fast\":{\"maxRemainingEpss\":0.5}}}"
            `shouldBe` malformed "fast" (onlyDifferential "maxRemainingEpss")

    it "starts from the shipped limits when a patch switches an abstaining rule to differential" $
        resolveJsonOver abstainingBase "{\"rules\":{\"fast\":{\"remainingAdvisories\":\"differential\",\"minCvssGap\":2.5}}}"
            `shouldBe` Right [shippedAt (AllowIfRemediatesCve (within defaultDifferentialLimits{dlCvssGap = GapOfAtLeast 2.5}))]

    it "inherits the rule's own limits when a patch restates differential" $
        resolveJsonOver tunedBase "{\"rules\":{\"fast\":{\"remainingAdvisories\":\"differential\",\"maxRemainingEpss\":0.02}}}"
            `shouldBe` Right [shippedAt (remediationWith FailNoDecision (Differential tuned{dlMaxRemainingEpss = 0.02}))]

    it "requires both choices of a newly named rule, naming each missing key" $ do
        resolveJson (added ",\"remainingAdvisories\":\"abstain\"") `shouldBe` malformed "r" "\"AllowIfRemediatesCve\" requires \"onUnavailable\""
        resolveJson (added ",\"onUnavailable\":\"deny\"") `shouldBe` malformed "r" "\"AllowIfRemediatesCve\" requires \"remainingAdvisories\""
        resolveJson (added "")
            `shouldBe` Left [MalformedRule "r" "\"AllowIfRemediatesCve\" requires \"onUnavailable\"", MalformedRule "r" "\"AllowIfRemediatesCve\" requires \"remainingAdvisories\""]

    it "refuses every limit beside abstain on a newly named rule" $
        resolveJson (added ",\"onUnavailable\":\"deny\",\"remainingAdvisories\":\"abstain\",\"minCvssGap\":1,\"maxRemainingEpss\":0.5")
            `shouldBe` Left [MalformedRule "r" (onlyDifferential "maxRemainingEpss"), MalformedRule "r" (onlyDifferential "minCvssGap")]

    it "fills a newly named differential rule's omitted limits from the shipped ones" $
        resolveJson (added ",\"onUnavailable\":\"skip\",\"remainingAdvisories\":\"differential\",\"maxRemainingEpss\":0.05")
            `shouldResolveTo` (shippedAt (remediationWith FailNoDecision (Differential defaultDifferentialLimits{dlMaxRemainingEpss = 0.05})) : shippedRules)

    it "refuses an unknown value for either choice, naming the key and the value" $ do
        resolveJson "{\"rules\":{\"remediation-fast-track\":{\"onUnavailable\":\"maybe\"}}}"
            `shouldBe` malformed "remediation-fast-track" "\"onUnavailable\" must be \"deny\" or \"skip\", not \"maybe\""
        resolveJson "{\"rules\":{\"remediation-fast-track\":{\"remainingAdvisories\":\"partial\"}}}"
            `shouldBe` malformed "remediation-fast-track" "\"remainingAdvisories\" must be \"abstain\" or \"differential\", not \"partial\""

    it "refuses a limit outside its score's range, naming the key and the range" $ do
        let cvss key = MalformedRule "remediation-fast-track" ("\"" <> key <> "\" must be a CVSS score between 0 and 10")
            epss key = MalformedRule "remediation-fast-track" ("\"" <> key <> "\" must be an EPSS probability between 0 and 1")
        resolveJson "{\"rules\":{\"remediation-fast-track\":{\"maxRemainingCvss\":10.1,\"minCvssGap\":-1,\"maxRemainingEpss\":1.5,\"minEpssGap\":-0.1}}}"
            `shouldBe` Left [cvss "maxRemainingCvss", epss "maxRemainingEpss", cvss "minCvssGap", epss "minEpssGap"]
        resolveJson "{\"rules\":{\"remediation-fast-track\":{\"maxRemainingCvss\":1e400}}}"
            `shouldBe` Left [cvss "maxRemainingCvss"]

    it "refuses a non-finite limit" $
        for_ [0 / 0, 1 / 0, negate (1 / 0)] $ \nonFinite ->
            resolvePolicy
                (RulePolicy (Map.singleton "fast" (shippedAt shippedRemediation)))
                (RulePatch (Map.singleton "fast" (blankEntry{entryMinEpssGap = Just nonFinite})))
                `shouldBe` Left [MalformedRule "fast" "\"minEpssGap\" must be an EPSS probability between 0 and 1"]

    it "requires no choice to disable the shipped rule" $
        resolveJson "{\"rules\":{\"remediation-fast-track\":{\"enabled\":false}}}" `shouldBe` Right [minAge]

    it "refuses a limit on another rule type" $
        resolveJson "{\"rules\":{\"min-age\":{\"maxRemainingCvss\":5}}}"
            `shouldBe` malformed "min-age" "\"AllowIfOlderThan\" does not read \"maxRemainingCvss\""

-- | An entry that sets no key, for a case that sets one value JSON cannot carry.
blankEntry :: RuleEntry
blankEntry =
    RuleEntry
        { entryType = Nothing
        , entryPrecedence = Nothing
        , entryEnabled = Nothing
        , entryAgeSeconds = Nothing
        , entryScope = Nothing
        , entryIdentity = Nothing
        , entryMinCvss = Nothing
        , entryMinEpss = Nothing
        , entryOnUnavailable = Nothing
        , entryRemainingAdvisories = Nothing
        , entryMaxRemainingCvss = Nothing
        , entryMaxRemainingEpss = Nothing
        , entryMinCvssGap = Nothing
        , entryMinEpssGap = Nothing
        , entryAppliesTo = Nothing
        }

-- | A base policy whose install-code deny is limited to admission, as a shipped mount default may be.
limitedBase :: RulePolicy
limitedBase = RulePolicy (Map.fromList [("deny-scripts", admissionOnly (atPrecedence 300 DenyInstallTimeExecution))])

-- | A 'knownRuleAdds' body with its rule limited to admission.
limitedToAdmission :: ByteString -> ByteString
limitedToAdmission = encodeUtf8 . T.replace "{\"type\":" "{\"appliesTo\":[\"admission\"],\"type\":" . decodeUtf8

-- | An added install-code deny named @deny-scripts@, with the given JSON as its @appliesTo@.
addedDeny :: ByteString -> ByteString
addedDeny appliesTo = "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\",\"appliesTo\":" <> appliesTo <> "}}}"

-- | The four refusals of an ambiguous @appliesTo@, as an operator reads them after the rule's name.
namesNoPhase, revocationAlone :: Text
namesNoPhase = "\"appliesTo\" names no phase. Write [admission] or [admission, revocation], or switch the rule off with \"enabled\": false"
revocationAlone = "\"appliesTo\" must include \"admission\". A rule that applied at revocation alone would delete copies of versions the gate still admits"

unknownPhase, allowLimitedToAdmission :: Text -> Text
unknownPhase word = "\"appliesTo\" names unknown phase \"" <> word <> "\". The phases are \"admission\" and \"revocation\""
allowLimitedToAdmission ty =
    "\"" <> ty <> "\" is an allow, and an allow cannot be limited to admission. If the Dredger ignored it, a lower deny could delete a version this rule admits. Limit the deny instead"

-- | Resolve over the shipped catalogue without its MAL default, so the remediation rule stays patchable.
resolveJson :: ByteString -> Either [PolicyError] [PrecededRule]
resolveJson body = do
    (_, catalogue) <- first (pure . MalformedRule "<defaults>" . show) (decodeDefaultConfig defaultConfigBytes)
    resolveJsonOver (RulePolicy (Map.delete "deny-known-malicious" (policyRules catalogue))) body

resolveJsonOver :: RulePolicy -> ByteString -> Either [PolicyError] [PrecededRule]
resolveJsonOver base body = case eitherDecodeStrict body :: Either String Value of
    Left e -> Left [MalformedRule "<decode>" (T.pack e)]
    Right (Object o) -> case parseEither parseRulesPatch (Object o) of
        Left err -> Left [MalformedRule "<parse>" (T.pack err)]
        Right patch -> sortOn rulePrecedence . Map.elems . policyRules <$> resolvePolicy base patch
    Right _ -> Left [MalformedRule "<parse>" "expected object"]

mixedBase :: RulePolicy
mixedBase =
    RulePolicy
        ( Map.fromList
            [ ("min-age", atPrecedence 100 (AllowIfOlderThan (7 * 86400)))
            , ("trusted", atPrecedence 200 (AllowScope (mkScope "myorg")))
            , ("deny-scripts", atPrecedence 300 DenyInstallTimeExecution)
            ]
        )

-- | A base policy carrying a DenyIfCve rule, for exercising the patch path.
cveBase :: RulePolicy
cveBase =
    RulePolicy
        (Map.fromList [("deny-cve", atPrecedence defaultDenyIfCvePrecedence (DenyIfCve (DenyIfCveParams 5 FailDeny)))])

-- | The same, for the EPSS twin.
epssBase :: RulePolicy
epssBase =
    RulePolicy
        (Map.fromList [("deny-epss", atPrecedence defaultDenyIfEpssPrecedence (DenyIfEpss (DenyIfEpssParams 0.5 FailDeny)))])

-- | The whole resolved policy as a multiset, so an add cannot pass on a policy that also lost or re-graded a rule.
shouldResolveTo :: Either [PolicyError] [PrecededRule] -> [PrecededRule] -> Expectation
shouldResolveTo resolved expected = case resolved of
    Right rules -> rules `shouldMatchList` expected
    Left errs -> expectationFailure ("expected a resolved policy, got " <> show errs)

-- | The two rules the shipped policy carries, which every add above resolves beside.
shippedRules :: [PrecededRule]
shippedRules =
    [ atPrecedence defaultAllowIfOlderThanPrecedence (AllowIfOlderThan (7 * 86400))
    , atPrecedence defaultAllowIfRemediatesCvePrecedence shippedRemediation
    ]

hasRuleAtPrec :: Int -> Rule -> Either [PolicyError] [PrecededRule] -> Bool
hasRuleAtPrec prec rule (Right rs) = atPrecedence prec rule `elem` rs
hasRuleAtPrec _ _ _ = False

-- A refusal whose rendered text names the key, whichever layer raised it.
refusalMentions :: Text -> Either [PolicyError] [PrecededRule] -> Bool
refusalMentions needle = either (any (T.isInfixOf needle . renderPolicyError)) (const False)

-- | A minimal add for each rule type. A case ties it to 'knownRuleTypes', so a new type cannot join without one.
knownRuleAdds :: [(Text, ByteString)]
knownRuleAdds =
    [ ("AllowScope", "{\"rules\":{\"r\":{\"type\":\"AllowScope\",\"scope\":\"myorg\"}}}")
    , ("AllowIfOlderThan", "{\"rules\":{\"r\":{\"type\":\"AllowIfOlderThan\",\"ageSeconds\":100}}}")
    , ("AllowByIdentity", "{\"rules\":{\"r\":{\"type\":\"AllowByIdentity\",\"identity\":\"left-pad@1.3.0\"}}}")
    , ("AllowIfRemediatesCve", "{\"rules\":{\"r\":{\"type\":\"AllowIfRemediatesCve\",\"onUnavailable\":\"deny\",\"remainingAdvisories\":\"abstain\"}}}")
    , ("DenyIfCve", "{\"rules\":{\"r\":{\"type\":\"DenyIfCve\",\"minCvss\":8}}}")
    , ("DenyIfKnownMalicious", "{\"rules\":{\"r\":{\"type\":\"DenyIfKnownMalicious\"}}}")
    , ("DenyIfEpss", "{\"rules\":{\"r\":{\"type\":\"DenyIfEpss\",\"minEpss\":0.5}}}")
    , ("DenyIfYanked", "{\"rules\":{\"r\":{\"type\":\"DenyIfYanked\"}}}")
    , ("DenyIfDeprecated", "{\"rules\":{\"r\":{\"type\":\"DenyIfDeprecated\"}}}")
    , ("DenyInstallTimeExecution", "{\"rules\":{\"r\":{\"type\":\"DenyInstallTimeExecution\"}}}")
    , ("DenyExternalDependencies", "{\"rules\":{\"r\":{\"type\":\"DenyExternalDependencies\"}}}")
    , ("DenyByIdentity", "{\"rules\":{\"r\":{\"type\":\"DenyByIdentity\",\"identity\":\"left-pad\"}}}")
    ]
