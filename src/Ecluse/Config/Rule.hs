{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Ecluse.Config.Rule (
    RulePolicy (..),
    emptyPolicy,
    PolicyError (..),
    renderPolicyError,
    resolvePolicy,
    RulePatch (..),
    emptyPatch,
    RuleEntry (..),
) where

import Data.Map.Strict qualified as Map
import Validation (eitherToValidation, validationToEither)

import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Rules.Types (
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
    "AllowIfOlderThan" -> case entryAgeSeconds entry of
        Just secs
            | secs >= 0 -> Right (AllowIfOlderThan (fromInteger secs))
            | otherwise -> Left [MalformedRule name "\"ageSeconds\" must be non-negative"]
        Nothing -> Left [MalformedRule name "\"AllowIfOlderThan\" requires \"ageSeconds\""]
    "AllowScope" -> case entryScope entry of
        Just scope -> Right (AllowScope (mkScope scope))
        Nothing -> Left [MalformedRule name "\"AllowScope\" requires \"scope\""]
    "DenyByIdentity" -> case entryIdentity entry of
        Just ident -> Right (DenyByIdentity ident)
        Nothing -> Left [MalformedRule name "\"DenyByIdentity\" requires \"identity\""]
    "AllowByIdentity" -> case entryIdentity entry of
        Just ident -> Right (AllowByIdentity ident)
        Nothing -> Left [MalformedRule name "\"AllowByIdentity\" requires \"identity\""]
    "AllowIfRemediatesCve" -> Right AllowIfRemediatesCve
    "DenyInstallTimeExecution" -> Right DenyInstallTimeExecution
    _ -> Left [UnknownRuleType name ty]

patchRuleValue :: Text -> RuleEntry -> Rule -> Either [PolicyError] Rule
patchRuleValue name entry rule = do
    () <- checkRestatedType name entry rule
    case rule of
        AllowIfOlderThan d -> case entryAgeSeconds entry of
            Just secs
                | secs >= 0 -> Right (AllowIfOlderThan (fromInteger secs))
                | otherwise -> Left [MalformedRule name "\"ageSeconds\" must be non-negative"]
            Nothing -> Right (AllowIfOlderThan d)
        AllowScope s -> Right (AllowScope (maybe s mkScope (entryScope entry)))
        DenyByIdentity i -> Right (DenyByIdentity (fromMaybe i (entryIdentity entry)))
        AllowByIdentity i -> Right (AllowByIdentity (fromMaybe i (entryIdentity entry)))
        AllowIfRemediatesCve -> Right AllowIfRemediatesCve
        DenyInstallTimeExecution -> Right DenyInstallTimeExecution

checkRestatedType :: Text -> RuleEntry -> Rule -> Either [PolicyError] ()
checkRestatedType name entry rule = case entryType entry of
    Nothing -> Right ()
    Just ty
        | ty == ruleName rule -> Right ()
        | ty `elem` knownRuleTypes -> Left [MalformedRule name ("\"type\" " <> quote ty <> " does not match the default rule it patches")]
        | otherwise -> Left [UnknownRuleType name ty]

knownRuleTypes :: [Text]
knownRuleTypes =
    [ "AllowScope"
    , "AllowIfOlderThan"
    , "AllowByIdentity"
    , "AllowIfRemediatesCve"
    , "DenyInstallTimeExecution"
    , "DenyByIdentity"
    ]
