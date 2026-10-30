-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

{- | Resolve named rule patches against a base policy, accumulating malformed entries.
Mount patches keep unspecified parameters and phases, or remove an entry with @enabled: false@.
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

import Prelude hiding (universe)

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Data.Universe.Class (Universe (universe))
import Validation (eitherToValidation, validationToEither)

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
    defaultDifferentialLimits,
    defaultPrecedence,
    ruleDenies,
    ruleName,
 )
import Ecluse.Core.Wire (renderWire)

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

-- | One rule's declared keys, every one optional. 'refuseStrayParameters' refuses those its type does not read.
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
    , entryRemainingAdvisories :: Maybe Text
    , entryMaxRemainingCvss :: Maybe Double
    , entryMaxRemainingEpss :: Maybe Double
    , entryMinCvssGap :: Maybe Double
    , entryMinEpssGap :: Maybe Double
    , entryAppliesTo :: Maybe [Text]
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

-- | Apply a patch to a base policy. Every entry resolves before any applies, so one load reports every refusal.
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
patchExistingRule name entry (PrecededRule prec reach rule) = do
    checkRestatedType name entry rule
    refuseStrayParameters name (ruleName rule) entry
    rule' <- patchRuleValue name entry rule
    reach' <- resolveReach name entry reach rule'
    pure (PrecededRule (fromMaybe prec (entryPrecedence entry)) reach' rule')

-- The type is gated first, so a stray parameter is never reported against a type that does
-- not exist, and the stray before 'buildRule', so it is reported ahead of a missing required key.
addNewRule :: Text -> RuleEntry -> Either [PolicyError] PrecededRule
addNewRule name entry = case entryType entry of
    Nothing -> Left [MissingRuleType name]
    Just ty
        | ty `notElem` knownRuleTypes -> Left [UnknownRuleType name ty]
        | otherwise -> do
            refuseStrayParameters name ty entry
            rule <- buildRule name ty entry
            reach <- resolveReach name entry AdmissionAndRevocation rule
            pure (PrecededRule (fromMaybe (defaultPrecedence rule) (entryPrecedence entry)) reach rule)

checkRestatedType :: Text -> RuleEntry -> Rule -> Either [PolicyError] ()
checkRestatedType name entry rule = case entryType entry of
    Nothing -> Right ()
    Just ty
        | ty == ruleName rule -> Right ()
        | ty `elem` knownRuleTypes -> Left [MalformedRule name ("\"type\" " <> quote ty <> " does not match the default rule it patches")]
        | otherwise -> Left [UnknownRuleType name ty]

-- | The rule type names the diagnostics recognise. Any other name is an 'UnknownRuleType'.
knownRuleTypes :: [Text]
knownRuleTypes = map renderWire (universe :: [RuleType])

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
    "AllowIfRemediatesCve" -> "onUnavailable" : "remainingAdvisories" : limitKeys
    "DenyIfKnownMalicious" -> ["onUnavailable"]
    "DenyIfCve" -> ["minCvss", "onUnavailable"]
    "DenyIfEpss" -> ["minEpss", "onUnavailable"]
    "DenyIfYanked" -> []
    "DenyIfDeprecated" -> []
    "DenyInstallTimeExecution" -> []
    "DenyExternalDependencies" -> []
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
        , ("remainingAdvisories", isJust (entryRemainingAdvisories entry))
        ]
            <> map (second isJust) (limitEntries entry)
    , isSet
    ]

-- The differential's limit keys, which only @remainingAdvisories: differential@ reads.
limitKeys :: [Text]
limitKeys = ["maxRemainingCvss", "maxRemainingEpss", "minCvssGap", "minEpssGap"]

limitEntries :: RuleEntry -> [(Text, Maybe Double)]
limitEntries entry = zip limitKeys [entryMaxRemainingCvss entry, entryMaxRemainingEpss entry, entryMinCvssGap entry, entryMinEpssGap entry]

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
    "AllowIfRemediatesCve" -> AllowIfRemediatesCve <$> remediationParams name entry Nothing
    "DenyIfKnownMalicious" -> DenyIfKnownMalicious <$> parseOnUnavailable name (entryOnUnavailable entry)
    "DenyIfCve" -> DenyIfCve <$> buildDenyIfCveParams name entry
    "DenyIfEpss" -> DenyIfEpss <$> buildDenyIfEpssParams name entry
    "DenyIfYanked" -> Right DenyIfYanked
    "DenyIfDeprecated" -> Right DenyIfDeprecated
    "DenyInstallTimeExecution" -> Right DenyInstallTimeExecution
    "DenyExternalDependencies" -> Right DenyExternalDependencies
    _ -> Left [UnknownRuleType name ty]

requireField :: Text -> Text -> Text -> (a -> Either [PolicyError] b) -> Maybe a -> Either [PolicyError] b
requireField name ruleType field = maybe (missingField name ruleType field)

missingField :: Text -> Text -> Text -> Either [PolicyError] a
missingField name ruleType field = Left [MalformedRule name (quote ruleType <> " requires " <> quote field)]

-- @minCvss@ is required, so an operator states the CVSS threshold consciously. @onUnavailable@
-- defaults to @deny@, which fails closed.
buildDenyIfCveParams :: Text -> RuleEntry -> Either [PolicyError] DenyIfCveParams
buildDenyIfCveParams name entry =
    DenyIfCveParams
        <$> requireField name "DenyIfCve" "minCvss" (validateCvss name "minCvss") (entryMinCvss entry)
        <*> parseOnUnavailable name (entryOnUnavailable entry)

-- On the same terms as 'buildDenyIfCveParams'.
buildDenyIfEpssParams :: Text -> RuleEntry -> Either [PolicyError] DenyIfEpssParams
buildDenyIfEpssParams name entry =
    DenyIfEpssParams
        <$> requireField name "DenyIfEpss" "minEpss" (validateEpss name "minEpss") (entryMinEpss entry)
        <*> parseOnUnavailable name (entryOnUnavailable entry)

patchRuleValue :: Text -> RuleEntry -> Rule -> Either [PolicyError] Rule
patchRuleValue name entry rule =
    case rule of
        AllowIfOlderThan d ->
            AllowIfOlderThan <$> maybe (Right d) (validateAgeSeconds name) (entryAgeSeconds entry)
        AllowScope s -> Right (AllowScope (maybe s mkScope (entryScope entry)))
        DenyByIdentity i -> Right (DenyByIdentity (fromMaybe i (entryIdentity entry)))
        AllowByIdentity i -> Right (AllowByIdentity (fromMaybe i (entryIdentity entry)))
        AllowIfRemediatesCve params -> AllowIfRemediatesCve <$> remediationParams name entry (Just params)
        DenyIfKnownMalicious alignment ->
            DenyIfKnownMalicious <$> maybe (Right alignment) (parseAlignment name) (entryOnUnavailable entry)
        DenyIfCve params ->
            fmap DenyIfCve $
                DenyIfCveParams
                    <$> maybe (Right (dicMinCvss params)) (validateCvss name "minCvss") (entryMinCvss entry)
                    <*> maybe (Right (dicOnUnavailable params)) (parseAlignment name) (entryOnUnavailable entry)
        DenyIfEpss params ->
            fmap DenyIfEpss $
                DenyIfEpssParams
                    <$> maybe (Right (dieMinEpss params)) (validateEpss name "minEpss") (entryMinEpss entry)
                    <*> maybe (Right (dieOnUnavailable params)) (parseAlignment name) (entryOnUnavailable entry)
        DenyIfYanked -> Right DenyIfYanked
        DenyIfDeprecated -> Right DenyIfDeprecated
        DenyInstallTimeExecution -> Right DenyInstallTimeExecution
        DenyExternalDependencies -> Right DenyExternalDependencies

-- A publish-age threshold: a non-negative number of seconds.
validateAgeSeconds :: Text -> Integer -> Either [PolicyError] NominalDiffTime
validateAgeSeconds name secs
    | secs >= 0 = Right (fromInteger secs)
    | otherwise = Left [MalformedRule name "\"ageSeconds\" must be non-negative"]

-- A CVSS score in the range [0, 10]. Every comparison with NaN is false, so the range refuses it.
validateCvss :: Text -> Text -> Double -> Either [PolicyError] Double
validateCvss name key s
    | s >= 0 && s <= 10 = Right s
    | otherwise = Left [MalformedRule name (quote key <> " must be a CVSS score between 0 and 10")]

-- An EPSS probability in the range [0, 1], refusing NaN on the same terms.
validateEpss :: Text -> Text -> Double -> Either [PolicyError] Double
validateEpss name key s
    | s >= 0 && s <= 1 = Right s
    | otherwise = Left [MalformedRule name (quote key <> " must be an EPSS probability between 0 and 1")]

-- How an advisory deny resolves when the database cannot answer. Absent fails closed.
parseOnUnavailable :: Text -> Maybe Text -> Either [PolicyError] FailureAlignment
parseOnUnavailable name = maybe (Right FailDeny) (parseAlignment name)

parseAlignment :: Text -> Text -> Either [PolicyError] FailureAlignment
parseAlignment name = \case
    "deny" -> Right FailDeny
    "skip" -> Right FailNoDecision
    other -> Left [MalformedRule name ("\"onUnavailable\" must be \"deny\" or \"skip\", not " <> quote other)]

-- A new rule states both choices, and a patch inherits the ones it leaves out. Nothing defaults them.
remediationParams :: Text -> RuleEntry -> Maybe AllowIfRemediatesCveParams -> Either [PolicyError] AllowIfRemediatesCveParams
remediationParams name entry inherited =
    validationToEither $
        AllowIfRemediatesCveParams
            <$> eitherToValidation (stated "onUnavailable" (parseAlignment name) (entryOnUnavailable entry) (aircOnUnavailable <$> inherited))
            <*> eitherToValidation (stated "remainingAdvisories" (parseMode name) (entryRemainingAdvisories entry) (modeOf . aircRemaining <$> inherited) >>= remainingIn name entry (aircRemaining <$> inherited))
  where
    stated key parse declared current = case (declared, current) of
        (Just word, _) -> parse word
        (Nothing, Just value) -> Right value
        (Nothing, Nothing) -> missingField name "AllowIfRemediatesCve" key

data RemainingMode = AbstainMode | DifferentialMode

modeOf :: RemainingAdvisories -> RemainingMode
modeOf = \case
    AbstainOnRemaining -> AbstainMode
    Differential{} -> DifferentialMode

parseMode :: Text -> Text -> Either [PolicyError] RemainingMode
parseMode name = \case
    "abstain" -> Right AbstainMode
    "differential" -> Right DifferentialMode
    other -> Left [MalformedRule name ("\"remainingAdvisories\" must be \"abstain\" or \"differential\", not " <> quote other)]

-- Abstaining drops inherited limits and refuses supplied ones. Differential starts from the inherited
-- limits, or the shipped ones when it switches from abstaining.
remainingIn :: Text -> RuleEntry -> Maybe RemainingAdvisories -> RemainingMode -> Either [PolicyError] RemainingAdvisories
remainingIn name entry inherited = \case
    AbstainMode -> case [key | (key, Just _) <- limitEntries entry] of
        [] -> Right AbstainOnRemaining
        supplied -> Left [MalformedRule name (quote key <> " applies only when \"remainingAdvisories\" is \"differential\"") | key <- supplied]
    DifferentialMode -> Differential <$> overlayLimits name entry (fromMaybe defaultDifferentialLimits (inheritedLimits =<< inherited))
  where
    inheritedLimits = \case
        Differential limits -> Just limits
        AbstainOnRemaining -> Nothing

-- Each supplied limit replaces its base value. A zero gap is no gap condition.
overlayLimits :: Text -> RuleEntry -> DifferentialLimits -> Either [PolicyError] DifferentialLimits
overlayLimits name entry base =
    validationToEither $
        DifferentialLimits
            <$> overlay (validateCvss name "maxRemainingCvss") (entryMaxRemainingCvss entry) (dlMaxRemainingCvss base)
            <*> overlay (validateEpss name "maxRemainingEpss") (entryMaxRemainingEpss entry) (dlMaxRemainingEpss base)
            <*> overlay (fmap scoreGap . validateCvss name "minCvssGap") (entryMinCvssGap entry) (dlCvssGap base)
            <*> overlay (fmap scoreGap . validateEpss name "minEpssGap") (entryMinEpssGap entry) (dlEpssGap base)
  where
    overlay validate supplied current = eitherToValidation (maybe (Right current) validate supplied)
    scoreGap least
        | least == 0 = NoGapRequired
        | otherwise = GapOfAtLeast least

-- The phases the entry states, or @inherited@ where it states none. An allow the Dredger did not
-- read would leave a lower deny free to delete a version the gate admits, so only a deny narrows.
resolveReach :: Text -> RuleEntry -> RuleReach -> Rule -> Either [PolicyError] RuleReach
resolveReach name entry inherited rule = case entryAppliesTo entry of
    Nothing -> Right inherited
    Just phases ->
        parseAppliesTo name phases >>= \case
            AdmissionOnly | not (ruleDenies rule) -> Left [MalformedRule name (admissionOnlyAllow (ruleName rule))]
            reach -> Right reach
  where
    admissionOnlyAllow ty =
        quote ty
            <> " is an allow, and an allow cannot be limited to admission. If the Dredger ignored it, a lower deny could delete a version this rule admits. Limit the deny instead"

-- A repeated phase counts once, and every unknown word is reported.
parseAppliesTo :: Text -> [Text] -> Either [PolicyError] RuleReach
parseAppliesTo name phases = case (unknown, "admission" `elem` phases, "revocation" `elem` phases) of
    (_ : _, _, _) -> Left (map (MalformedRule name . unknownPhase) unknown)
    ([], True, True) -> Right AdmissionAndRevocation
    ([], True, False) -> Right AdmissionOnly
    ([], False, True) ->
        Left [MalformedRule name "\"appliesTo\" must include \"admission\". A rule that applied at revocation alone would delete copies of versions the gate still admits"]
    ([], False, False) ->
        Left [MalformedRule name "\"appliesTo\" names no phase. Write [admission] or [admission, revocation], or switch the rule off with \"enabled\": false"]
  where
    unknown = ordNub (filter (`notElem` ["admission", "revocation"]) phases)
    unknownPhase phase = "\"appliesTo\" names unknown phase " <> quote phase <> ". The phases are \"admission\" and \"revocation\""
