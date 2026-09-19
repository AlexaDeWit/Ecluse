-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

{- | Resolve a declared @rules@ patch against a base policy.

A patch entry names a rule: it adds one, refines an existing one key by key, or disables one with
@enabled: false@. Refusals accumulate across entries, so one load reports every malformed rule
rather than the first. A patch that names no default and gives no @type@ is refused, as is a
parameter the named type does not read, because an ignored parameter reads on a deny gate as a
setting the operator made and Écluse did not apply.
-}
module Ecluse.Config.Rule (
    -- * Policies
    RulePolicy (..),
    emptyPolicy,
    resolvePolicy,

    -- * Declared patches
    RulePatch (..),
    RuleEntry (..),
    knownRuleTypes,

    -- * Refusals
    PolicyError (..),
    renderPolicyError,
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

-- | A resolved rule set, keyed by the rule name an operator patches it under.
newtype RulePolicy = RulePolicy
    { policyRules :: Map Text PrecededRule
    }
    deriving stock (Eq, Show)

-- | The policy a load starts from before the shipped defaults are applied.
emptyPolicy :: RulePolicy
emptyPolicy = RulePolicy Map.empty

-- | A declared @rules@ object: one 'RuleEntry' per rule name it names.
newtype RulePatch = RulePatch (Map Text RuleEntry)
    deriving stock (Eq, Show)

{- | One rule's declared keys, every one optional. Which of them the entry may set depends on the
rule type, and 'refuseStrayParameters' refuses the rest.
-}
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

-- | Why one declared rule was refused. A load reports every one it accumulated.
data PolicyError
    = MissingRuleType Text
    | UnknownRuleType Text Text
    | MalformedRule Text Text
    | SuppressUnknownRule Text
    deriving stock (Eq, Show)

-- | One refusal as the boot reports it, naming the rule it was declared under.
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

{- | Apply a patch to a base policy. Every entry is resolved before any is applied, so one load
reports every refusal rather than stopping at the first.
-}
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

-- Refuse a parameter the rule type does not read. Ignored, a threshold or an @onUnavailable@
-- under the wrong type reads on a deny gate as a setting the operator made and Écluse did not apply.
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

-- The parameter keys one rule type reads, which 'buildRule' names the same keys for.
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

requireField :: Text -> Text -> Text -> (a -> Either [PolicyError] b) -> Maybe a -> Either [PolicyError] b
requireField name ruleType field =
    maybe (Left [MalformedRule name (quote ruleType <> " requires " <> quote field)])

-- @minCvss@ is required, so an operator states the CVSS threshold consciously. @onUnavailable@
-- defaults to @deny@, which fails closed.
buildDenyIfCveParams :: Text -> RuleEntry -> Either [PolicyError] DenyIfCveParams
buildDenyIfCveParams name entry =
    DenyIfCveParams
        <$> requireField name "DenyIfCve" "minCvss" (validateMinCvss name) (entryMinCvss entry)
        <*> parseOnUnavailable name (entryOnUnavailable entry)

-- On the same terms as 'buildDenyIfCveParams'.
buildDenyIfEpssParams :: Text -> RuleEntry -> Either [PolicyError] DenyIfEpssParams
buildDenyIfEpssParams name entry =
    DenyIfEpssParams
        <$> requireField name "DenyIfEpss" "minEpss" (validateMinEpss name) (entryMinEpss entry)
        <*> parseOnUnavailable name (entryOnUnavailable entry)

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

-- A publish-age threshold: a non-negative number of seconds.
validateAgeSeconds :: Text -> Integer -> Either [PolicyError] NominalDiffTime
validateAgeSeconds name secs
    | secs >= 0 = Right (fromInteger secs)
    | otherwise = Left [MalformedRule name "\"ageSeconds\" must be non-negative"]

-- A CVSS severity threshold: a base score in the range [0, 10].
validateMinCvss :: Text -> Double -> Either [PolicyError] Double
validateMinCvss name s
    | s >= 0 && s <= 10 = Right s
    | otherwise = Left [MalformedRule name "\"minCvss\" must be a CVSS score between 0 and 10"]

-- An EPSS threshold: a probability in the range [0, 1].
validateMinEpss :: Text -> Double -> Either [PolicyError] Double
validateMinEpss name s
    | s >= 0 && s <= 1 = Right s
    | otherwise = Left [MalformedRule name "\"minEpss\" must be an EPSS probability between 0 and 1"]

-- How the rule resolves when the advisory database cannot answer. Absent fails closed.
parseOnUnavailable :: Text -> Maybe Text -> Either [PolicyError] FailureAlignment
parseOnUnavailable name = \case
    Nothing -> Right FailDeny
    Just "deny" -> Right FailDeny
    Just "skip" -> Right FailNoDecision
    Just other -> Left [MalformedRule name ("\"onUnavailable\" must be \"deny\" or \"skip\", not " <> quote other)]
