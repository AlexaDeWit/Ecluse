-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Ecluse.Config.Rule (
    RulePolicy (..),
    emptyPolicy,
    PolicyError (..),
    renderPolicyError,
    knownRuleTypes,
    resolvePolicy,
    RulePatch (..),
    emptyPatch,
    RuleEntry (..),
) where

import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)
import Validation (eitherToValidation, validationToEither)

import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Rules.Types (
    DenyIfCveParams (..),
    FailureAlignment (..),
    PrecededRule (..),
    Rule (..),
    defaultPrecedence,
    ruleName,
 )

newtype RulePolicy = RulePolicy
    { policyRules :: Map Text PrecededRule
    }
    deriving stock (Eq, Show)

emptyPolicy :: RulePolicy
emptyPolicy = RulePolicy Map.empty

data PolicyError
    = MissingRuleType Text
    | UnknownRuleType Text Text
    | MalformedRule Text Text
    | SuppressUnknownRule Text
    deriving stock (Eq, Show)

renderPolicyError :: PolicyError -> Text
renderPolicyError = \case
    MissingRuleType name ->
        "rule " <> quote name <> " is not a default and is missing its \"type\""
    UnknownRuleType name ty ->
        "rule " <> quote name <> " names unknown type " <> quote ty
    MalformedRule name reason ->
        "rule " <> quote name <> ": " <> reason
    SuppressUnknownRule name ->
        "rule " <> quote name <> " disables a rule that no default defines"

quote :: Text -> Text
quote t = "\"" <> t <> "\""

newtype RulePatch = RulePatch (Map Text RuleEntry)
    deriving stock (Eq, Show)

-- | An empty patch that does not override any rules.
emptyPatch :: RulePatch
emptyPatch = RulePatch Map.empty

data RuleEntry = RuleEntry
    { entryType :: Maybe Text
    , entryPrecedence :: Maybe Int
    , entryEnabled :: Maybe Bool
    , entryAgeSeconds :: Maybe Integer
    , entryScope :: Maybe Text
    , entryIdentity :: Maybe Text
    , entryMinSeverity :: Maybe Double
    , entryOnUnavailable :: Maybe Text
    }
    deriving stock (Eq, Show)

resolvePolicy :: RulePolicy -> RulePatch -> Either [PolicyError] RulePolicy
resolvePolicy (RulePolicy base) (RulePatch patch) =
    validationToEither $
        RulePolicy . foldl' applyResolvedEntry base
            <$> traverse (eitherToValidation . resolveEntry base) (Map.toList patch)

applyResolvedEntry :: Map Text PrecededRule -> (Text, Maybe PrecededRule) -> Map Text PrecededRule
applyResolvedEntry acc (name, Nothing) = Map.delete name acc
applyResolvedEntry acc (name, Just pr) = Map.insert name pr acc

resolveEntry :: Map Text PrecededRule -> (Text, RuleEntry) -> Either [PolicyError] (Text, Maybe PrecededRule)
resolveEntry base (name, entry)
    | entryEnabled entry == Just False =
        if Map.member name base
            then Right (name, Nothing)
            else Left [SuppressUnknownRule name]
    | otherwise =
        case Map.lookup name base of
            Just existing -> (name,) . Just <$> patchExistingRule name entry existing
            Nothing -> (name,) . Just <$> addNewRule name entry

patchExistingRule :: Text -> RuleEntry -> PrecededRule -> Either [PolicyError] PrecededRule
patchExistingRule name entry (PrecededRule prec rule) = do
    rule' <- patchRuleValue name entry rule
    pure (PrecededRule (fromMaybe prec (entryPrecedence entry)) rule')

addNewRule :: Text -> RuleEntry -> Either [PolicyError] PrecededRule
addNewRule name entry = case entryType entry of
    Nothing -> Left [MissingRuleType name]
    Just ty -> do
        rule <- buildRule name ty entry
        pure (PrecededRule (fromMaybe (defaultPrecedence rule) (entryPrecedence entry)) rule)

buildRule :: Text -> Text -> RuleEntry -> Either [PolicyError] Rule
buildRule name ty entry = case ty of
    "AllowIfOlderThan" ->
        AllowIfOlderThan
            <$> requireField name "AllowIfOlderThan" "ageSeconds" (validateAgeSeconds name) (entryAgeSeconds entry)
    "AllowScope" ->
        AllowScope . mkScope <$> requireField name "AllowScope" "scope" Right (entryScope entry)
    "DenyByIdentity" ->
        DenyByIdentity <$> requireField name "DenyByIdentity" "identity" Right (entryIdentity entry)
    "AllowByIdentity" ->
        AllowByIdentity <$> requireField name "AllowByIdentity" "identity" Right (entryIdentity entry)
    "AllowIfRemediatesCve" -> Right AllowIfRemediatesCve
    "DenyIfCve" -> DenyIfCve <$> buildDenyIfCveParams name entry
    "DenyInstallTimeExecution" -> Right DenyInstallTimeExecution
    _ -> Left [UnknownRuleType name ty]

{- | Extract a rule type's required field, running @validate@ on it, or report the
type is missing it (@"<ruleType>" requires "<field>"@). Unifies the required-field
decode across every builder that has one.
-}
requireField :: Text -> Text -> Text -> (a -> Either [PolicyError] b) -> Maybe a -> Either [PolicyError] b
requireField name ruleType field =
    maybe (Left [MalformedRule name (quote ruleType <> " requires " <> quote field)])

-- Validate a publish-age threshold: a non-negative number of seconds.
validateAgeSeconds :: Text -> Integer -> Either [PolicyError] NominalDiffTime
validateAgeSeconds name secs
    | secs >= 0 = Right (fromInteger secs)
    | otherwise = Left [MalformedRule name "\"ageSeconds\" must be non-negative"]

-- Validate a CVSS severity threshold: a base score in the range [0, 10].
validateMinSeverity :: Text -> Double -> Either [PolicyError] Double
validateMinSeverity name s
    | s >= 0 && s <= 10 = Right s
    | otherwise = Left [MalformedRule name "\"minSeverity\" must be a CVSS score between 0 and 10"]

{- | Decode 'DenyIfCve''s parameters. @minSeverity@ (a CVSS base score, 0 to 10)
is required, so an operator states the threshold consciously. @onUnavailable@ is
optional and defaults to @deny@ (fail-closed): a package the advisory database
cannot vet is refused rather than admitted.
-}
buildDenyIfCveParams :: Text -> RuleEntry -> Either [PolicyError] DenyIfCveParams
buildDenyIfCveParams name entry =
    DenyIfCveParams
        <$> requireField name "DenyIfCve" "minSeverity" (validateMinSeverity name) (entryMinSeverity entry)
        <*> parseOnUnavailable name (entryOnUnavailable entry)

-- Decode the @onUnavailable@ policy: how the rule resolves when the advisory
-- database cannot answer. Absent defaults to fail-closed.
parseOnUnavailable :: Text -> Maybe Text -> Either [PolicyError] FailureAlignment
parseOnUnavailable name = \case
    Nothing -> Right FailDeny
    Just "deny" -> Right FailDeny
    Just "skip" -> Right FailNoDecision
    Just other -> Left [MalformedRule name ("\"onUnavailable\" must be \"deny\" or \"skip\", not " <> quote other)]

patchRuleValue :: Text -> RuleEntry -> Rule -> Either [PolicyError] Rule
patchRuleValue name entry rule = do
    () <- checkRestatedType name entry rule
    case rule of
        AllowIfOlderThan d ->
            AllowIfOlderThan <$> maybe (Right d) (validateAgeSeconds name) (entryAgeSeconds entry)
        AllowScope s -> Right (AllowScope (maybe s mkScope (entryScope entry)))
        DenyByIdentity i -> Right (DenyByIdentity (fromMaybe i (entryIdentity entry)))
        AllowByIdentity i -> Right (AllowByIdentity (fromMaybe i (entryIdentity entry)))
        AllowIfRemediatesCve -> Right AllowIfRemediatesCve
        DenyIfCve params ->
            fmap DenyIfCve $
                DenyIfCveParams
                    <$> maybe (Right (dicMinSeverity params)) (validateMinSeverity name) (entryMinSeverity entry)
                    <*> maybe (Right (dicOnUnavailable params)) (parseOnUnavailable name . Just) (entryOnUnavailable entry)
        DenyInstallTimeExecution -> Right DenyInstallTimeExecution

checkRestatedType :: Text -> RuleEntry -> Rule -> Either [PolicyError] ()
checkRestatedType name entry rule = case entryType entry of
    Nothing -> Right ()
    Just ty
        | ty == ruleName rule -> Right ()
        | ty `elem` knownRuleTypes -> Left [MalformedRule name ("\"type\" " <> quote ty <> " does not match the default rule it patches")]
        | otherwise -> Left [UnknownRuleType name ty]

{- | The rule type names the diagnostics recognise: the vocabulary
'checkRestatedType' treats as a real-but-mismatched type (a 'MalformedRule')
rather than an unknown one (an 'UnknownRuleType'). Exported so a test can pin it
against drift from the 'Ecluse.Core.Rules.Types.Rule' constructors, their
'buildRule' branches, and 'Ecluse.Core.Rules.Types.ruleName'.
-}
knownRuleTypes :: [Text]
knownRuleTypes =
    [ "AllowScope"
    , "AllowIfOlderThan"
    , "AllowByIdentity"
    , "AllowIfRemediatesCve"
    , "DenyIfCve"
    , "DenyInstallTimeExecution"
    , "DenyByIdentity"
    ]
