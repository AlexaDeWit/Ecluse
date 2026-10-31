-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Discovery checks requirements before any provider can acquire resources.
module Ecluse.Test.Contract.Catalogue (
    validateCatalogue,
    combineCatalogues,
    caseLabel,
    labelCaseId,
    duplicates,
) where

import Data.Char (isAsciiLower, isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Test.Contract.Types

-- | Combine tier declarations without hiding duplicate provider, group or case identities.
combineCatalogues :: [Catalogue] -> Catalogue
combineCatalogues catalogues =
    Catalogue
        (ordNub $ concatMap catalogueProductionEcosystems catalogues)
        (concatMap catalogueProfiles catalogues)
        (concatMap catalogueObligations catalogues)
        (concatMap catalogueGroups catalogues)

-- | Embed an immutable case ID without making the human description a selector.
caseLabel :: Text -> Text -> String
caseLabel identifier description = toString $ "[" <> identifier <> "] " <> description

-- | Extract only the explicit ID prefix, never infer an ID from a description.
labelCaseId :: String -> Maybe Text
labelCaseId label = do
    rest <- T.stripPrefix "[" (toText label)
    let (identifier, suffix) = T.breakOn "] " rest
    guard (not (T.null suffix) && validId identifier)
    pure identifier

-- | All repeated values, once each, for exact completeness diagnostics.
duplicates :: (Ord a) => [a] -> [a]
duplicates = Map.keys . Map.filter (> (1 :: Int)) . Map.fromListWith (+) . map (,1)

-- | Reject incompatible providers, missing obligations, duplicate IDs, and ghost registrations.
validateCatalogue :: Catalogue -> Either [Text] Catalogue
validateCatalogue catalogue = if null problems then Right catalogue else Left problems
  where
    groups = catalogueGroups catalogue
    profiles = catalogueProfiles catalogue
    obligations = catalogueObligations catalogue
    cases = concatMap groupCases groups
    profileMap = Map.fromList [(profileId profile, profile) | profile <- profiles]
    obligationMap = Map.fromList [(obligationId obligation, obligation) | obligation <- obligations]
    ids = map groupId groups <> map groupContractId groups <> map groupProfileId groups <> map profileId profiles <> map obligationId obligations <> map caseId cases
    problems =
        ["invalid ID: " <> identifier | identifier <- ids, not (validId identifier)]
            <> repeated "group" (map groupId groups)
            <> repeated "profile" (map profileId profiles)
            <> repeated "obligation" (map obligationId obligations)
            <> repeated "case" (map caseId cases)
            <> ["catalogue has no groups" | null groups]
            <> [ "production ecosystem lacks a profile: " <> ecosystem
               | ecosystem <- catalogueProductionEcosystems catalogue
               , all (notElem ecosystem . profileEcosystems) profiles
               ]
            <> [ "profile names an unknown production ecosystem: " <> ecosystem
               | profile <- profiles
               , ecosystem <- profileEcosystems profile
               , ecosystem `notElem` catalogueProductionEcosystems catalogue
               ]
            <> [ "profile lacks a supported obligation: " <> profileId profile
               | profile <- profiles
               , all (\obligation -> obligationProfile obligation /= profileId profile || obligationSupport obligation /= Supported) obligations
               ]
            <> concatMap (groupProblems profileMap obligationMap) groups
            <> concatMap (obligationProblems profileMap groups) obligations

repeated :: Text -> [Text] -> [Text]
repeated kind identifiers = map (("duplicate " <> kind <> " ID: ") <>) (duplicates identifiers)

validId :: Text -> Bool
validId value = not (T.null value) && T.all allowed value
  where
    allowed char = isAsciiLower char || isDigit char || char `elem` ("/-_." :: String)

groupProblems :: Map Text Profile -> Map Text Obligation -> Group -> [Text]
groupProblems profiles obligations scenarioGroup =
    ["empty group: " <> groupId scenarioGroup | null (groupCases scenarioGroup)]
        <> ["group has no partition: " <> groupId scenarioGroup | T.null (groupPartition scenarioGroup)]
        <> case Map.lookup (groupProfileId scenarioGroup) profiles of
            Nothing -> ["unknown group profile: " <> groupProfileId scenarioGroup]
            Just profile ->
                [ "group lacks capability: " <> groupId scenarioGroup <> ": " <> capability
                | capability <- groupCapabilities scenarioGroup
                , capability `notElem` profileCapabilities profile
                ]
                    <> concatMap checkCase (groupCases scenarioGroup)
  where
    checkCase item =
        ["case has no requirement: " <> caseId item | null (caseRequirements item)]
            <> concatMap (checkRequirement item) (caseRequirements item)
    checkRequirement item requirement = case Map.lookup requirement obligations of
        Nothing -> ["unknown case requirement: " <> requirement]
        Just obligation ->
            [ "case does not satisfy its declared obligation: " <> caseId item <> ": " <> requirement
            | obligationSupport obligation /= Supported
                || obligationProfile obligation /= groupProfileId scenarioGroup
                || obligationContract obligation /= groupContractId scenarioGroup
                || caseId item `notElem` obligationCases obligation
                || any (`notElem` groupCapabilities scenarioGroup) (obligationCapabilities obligation)
            ]

obligationProblems :: Map Text Profile -> [Group] -> Obligation -> [Text]
obligationProblems profiles groups obligation = case obligationSupport obligation of
    Unsupported reason -> ["unsupported obligation has no reason: " <> obligationId obligation | T.null reason]
    Planned reference -> ["planned obligation has no reference: " <> obligationId obligation | T.null reference]
    Supported ->
        ["required obligation has no cases: " <> obligationId obligation | null (obligationCases obligation)]
            <> repeated "obligation case" (obligationCases obligation)
            <> case Map.lookup (obligationProfile obligation) profiles of
                Nothing -> ["required profile missing: " <> obligationProfile obligation]
                Just profile ->
                    [ "required capability missing: " <> obligationId obligation <> ": " <> capability
                    | capability <- obligationCapabilities obligation
                    , capability `notElem` profileCapabilities profile
                    ]
                        <> [ "required case missing: " <> obligationId obligation <> ": " <> identifier
                           | identifier <- obligationCases obligation
                           , identifier `notElem` registered
                           ]
  where
    registered =
        [ caseId item
        | scenarioGroup <- groups
        , groupProfileId scenarioGroup == obligationProfile obligation
        , groupContractId scenarioGroup == obligationContract obligation
        , item <- groupCases scenarioGroup
        , obligationId obligation `elem` caseRequirements item
        ]
