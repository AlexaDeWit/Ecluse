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
    RuleEntry (..),
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Validation (eitherToValidation, validationToEither)

import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Rules.Types (
    DenyIfCveParams (..),
    DenyIfEpssParams (..),
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

data RuleEntry = RuleEntry
    { entryType :: Maybe Text
    , entryPrecedence :: Maybe Int
    , entryEnabled :: Maybe Bool
    , entryAgeSeconds :: Maybe Integer
    , entryScope :: Maybe Text
    , entryIdentity :: Maybe Text
    , entryMinCvss :: Maybe Double
    , entryMinEpss :: Maybe Double
    , entryOnUnavailable :: Maybe Text
    }
    deriving stock (Eq, Show)

{- | The parameter keys one rule type reads. 'refuseStrayParameters' refuses every other parameter
an entry sets, so the table and 'buildRule' name the same keys for each type.
-}
ruleTypeParameters :: Text -> [Text]
ruleTypeParameters = \case
    "AllowIfOlderThan" -> ["ageSeconds"]
    "AllowScope" -> ["scope"]
    "AllowByIdentity" -> ["identity"]
    "DenyByIdentity" -> ["identity"]
    "AllowIfRemediatesCve" -> []
    "DenyIfCve" -> ["minCvss", "onUnavailable"]
    "DenyIfEpss" -> ["minEpss", "onUnavailable"]
    "DenyInstallTimeExecution" -> []
    _ -> []

-- The parameter keys an entry actually sets, in a fixed order so a refusal reads the same twice.
setParameters :: RuleEntry -> [Text]
setParameters entry =
    [ key
    | (key, isSet) <-
        [ ("ageSeconds", isJust (entryAgeSeconds entry))
        , ("scope", isJust (entryScope entry))
        , ("identity", isJust (entryIdentity entry))
        , ("minCvss", isJust (entryMinCvss entry))
        , ("minEpss", isJust (entryMinEpss entry))
        , ("onUnavailable", isJust (entryOnUnavailable entry))
        ]
    , isSet
    ]

{- | Refuse a parameter the rule type does not read. Ignored, a threshold or an @onUnavailable@
under the wrong type reads on a deny gate as a setting the operator made and Écluse did not apply.
-}
refuseStrayParameters :: Text -> Text -> RuleEntry -> Either [PolicyError] ()
refuseStrayParameters name ty entry =
    case filter (`notElem` ruleTypeParameters ty) (setParameters entry) of
        [] -> Right ()
        strays ->
            Left
                [ MalformedRule
                    name
                    (quote ty <> " does not read " <> T.intercalate ", " (map quote strays))
                ]

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
    checkRestatedType name entry rule
    refuseStrayParameters name (ruleName rule) entry
    rule' <- patchRuleValue name entry rule
    pure (PrecededRule (fromMaybe prec (entryPrecedence entry)) rule')

-- The type is gated first, so a stray parameter is never reported against a type that does
-- not exist, and the stray before 'buildRule', so one run reports it beside a missing required key.
addNewRule :: Text -> RuleEntry -> Either [PolicyError] PrecededRule
addNewRule name entry = case entryType entry of
    Nothing -> Left [MissingRuleType name]
    Just ty
        | ty `notElem` knownRuleTypes -> Left [UnknownRuleType name ty]
        | otherwise -> do
            refuseStrayParameters name ty entry
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
    "DenyIfEpss" -> DenyIfEpss <$> buildDenyIfEpssParams name entry
    "DenyInstallTimeExecution" -> Right DenyInstallTimeExecution
    _ -> Left [UnknownRuleType name ty]

{- | Extract a rule type's required field and validate it, or report the type is missing it
(@"<ruleType>" requires "<field>"@).
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
validateMinCvss :: Text -> Double -> Either [PolicyError] Double
validateMinCvss name s
    | s >= 0 && s <= 10 = Right s
    | otherwise = Left [MalformedRule name "\"minCvss\" must be a CVSS score between 0 and 10"]

-- Validate an EPSS threshold: a probability in the range [0, 1].
validateMinEpss :: Text -> Double -> Either [PolicyError] Double
validateMinEpss name s
    | s >= 0 && s <= 1 = Right s
    | otherwise = Left [MalformedRule name "\"minEpss\" must be an EPSS probability between 0 and 1"]

{- | Decode 'DenyIfCve''s parameters. @minCvss@ is required, so an operator states the CVSS
threshold consciously. @onUnavailable@ defaults to @deny@, which fails closed.
-}
buildDenyIfCveParams :: Text -> RuleEntry -> Either [PolicyError] DenyIfCveParams
buildDenyIfCveParams name entry =
    DenyIfCveParams
        <$> requireField name "DenyIfCve" "minCvss" (validateMinCvss name) (entryMinCvss entry)
        <*> parseOnUnavailable name (entryOnUnavailable entry)

-- | Decode 'DenyIfEpss''s parameters, on the same terms as 'buildDenyIfCveParams'.
buildDenyIfEpssParams :: Text -> RuleEntry -> Either [PolicyError] DenyIfEpssParams
buildDenyIfEpssParams name entry =
    DenyIfEpssParams
        <$> requireField name "DenyIfEpss" "minEpss" (validateMinEpss name) (entryMinEpss entry)
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
patchRuleValue name entry rule =
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
                    <$> maybe (Right (dicMinCvss params)) (validateMinCvss name) (entryMinCvss entry)
                    <*> maybe (Right (dicOnUnavailable params)) (parseOnUnavailable name . Just) (entryOnUnavailable entry)
        DenyIfEpss params ->
            fmap DenyIfEpss $
                DenyIfEpssParams
                    <$> maybe (Right (dieMinEpss params)) (validateMinEpss name) (entryMinEpss entry)
                    <*> maybe (Right (dieOnUnavailable params)) (parseOnUnavailable name . Just) (entryOnUnavailable entry)
        DenyInstallTimeExecution -> Right DenyInstallTimeExecution

checkRestatedType :: Text -> RuleEntry -> Rule -> Either [PolicyError] ()
checkRestatedType name entry rule = case entryType entry of
    Nothing -> Right ()
    Just ty
        | ty == ruleName rule -> Right ()
        | ty `elem` knownRuleTypes -> Left [MalformedRule name ("\"type\" " <> quote ty <> " does not match the default rule it patches")]
        | otherwise -> Left [UnknownRuleType name ty]

{- | The rule type names the diagnostics recognise. 'checkRestatedType' reports one of these as a
mismatched 'MalformedRule', and anything else as an 'UnknownRuleType'.
-}
knownRuleTypes :: [Text]
knownRuleTypes =
    [ "AllowScope"
    , "AllowIfOlderThan"
    , "AllowByIdentity"
    , "AllowIfRemediatesCve"
    , "DenyIfCve"
    , "DenyIfEpss"
    , "DenyInstallTimeExecution"
    , "DenyByIdentity"
    ]
