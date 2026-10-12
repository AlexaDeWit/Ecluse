-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

-- | Full completion requires fixed business laws and audited evidence across isolated tiers.
module Ecluse.Test.Contract.Completion (
    CompletionScope (..),
    LawEvidence (..),
    Completion (..),
    requiredFamilyLaws,
    validateCompletion,
    auditCompletion,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T

import Ecluse.Test.Contract.Catalogue (duplicates, validateCatalogue)
import Ecluse.Test.Contract.Result
import Ecluse.Test.Contract.Types
import Ecluse.Test.Contract.UnitCatalogue (metadataFetchLaws, metadataProjectionLaws)

-- | The read-only scope never certifies full ecosystem completion.
data CompletionScope = FullEcosystem | ReadOnlyEcosystemFoundation | FullPackageStore
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A law names its actual case, group and tier, independently of provider capability flags.
data LawEvidence = LawEvidence
    { evidenceFamily :: Text
    , evidenceLaw :: Text
    , evidenceTier :: Tier
    , evidenceGroup :: Text
    , evidenceCase :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Registrations supply evidence but cannot redefine the scope's required families or laws.
data Completion = Completion
    { completionSubject :: Text
    , completionScope :: CompletionScope
    , completionEvidence :: [LawEvidence]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | This normative list is independent of concrete drivers, registration tables and scheduling.
requiredFamilyLaws :: CompletionScope -> [(Text, [Text])]
requiredFamilyLaws FullPackageStore =
    [ ("reads", ["present", "absent"])
    , ("writes", ["complete", "idempotent"])
    , ("inventory", ["exact"])
    , ("deletion", ["selected"])
    , ("integrity", ["bytes-and-digests"])
    , ("recovery", ["publish-retry", "reopen", "delete-retry"])
    ]
requiredFamilyLaws scope =
    [ ("proxying", ["identity-precedence", "authorised-union", "private-divergence", "artifact-integrity", "artifact-head", "conditional-coherence"] <> map ("metadata-fetch/" <>) metadataFetchLaws <> map ("metadata-projection/" <>) metadataProjectionLaws)
    , ("installation", ["accepted-install", "default-denial", "artifact-integrity", "artifact-head"])
    , ("policy", ["default-denial", "quarantine-private-authority", "public-integrity", "trusted-integrity"])
    , ("advisories", ["exact-fix", "deny-cve", "deny-epss", "last-good-generation"])
    , ("telemetry", ["signals", "disabled", "export-failure", "datadog"] <> [law | scope == FullEcosystem, law <- ["domain-spans", "private-fetch"]])
    ]
        <> [ family
           | scope == FullEcosystem
           , family <-
                [ ("mirroring", ["dependency-graph", "module-exports", "linked-executable", "origin-evidence", "installed-tree", "empty-store-control"])
                , ("publication", ["publish-install", "authorisation-refusal"])
                , ("dredger", ["inventory", "admission-only", "denied-versions", "dry-run", "consent-refusal", "first-party", "two-stores", "advisory-revocation", "denied-install", "delete-recovery", "refill", "missing-source", "late-copy", "old-worker", "old-dredger-refill", "old-dredger-loss"])
                ]
           ]

-- | Missing laws fail even when both their registration and their evidence mapping disappear.
validateCompletion :: Completion -> [Catalogue] -> Either [Text] Completion
validateCompletion completion catalogues = if null faults then Right completion else Left faults
  where
    evidence = completionEvidence completion
    requirements = [(family, law) | (family, laws) <- requiredFamilyLaws (completionScope completion), law <- laws]
    declared = [(evidenceFamily item, evidenceLaw item) | item <- evidence]
    groups = concatMap catalogueGroups catalogues
    faults =
        ["completion has no subject" | T.null (completionSubject completion)]
            <> concatMap (fromLeft [] . validateCatalogue) catalogues
            <> ["missing required law: " <> family <> "/" <> law | (family, law) <- requirements, (family, law) `notElem` declared]
            <> ["unknown completion law: " <> family <> "/" <> law | (family, law) <- declared, (family, law) `notElem` requirements]
            <> ["duplicate completion law: " <> family <> "/" <> law | (family, law) <- duplicates declared]
            <> ["duplicate completion case mapping: " <> identifier | (_, _, identifier) <- duplicates [(evidenceTier item, evidenceGroup item, evidenceCase item) | item <- evidence]]
            <> concatMap (evidenceProblems groups) evidence

evidenceProblems :: [Group] -> LawEvidence -> [Text]
evidenceProblems groups evidence = case matching of
    [scenarioGroup] ->
        [ "completion case missing: " <> evidenceCase evidence
        | evidenceCase evidence `notElem` map caseId (groupCases scenarioGroup)
        ]
    [] -> ["completion group missing: " <> evidenceGroup evidence <> ": " <> show (evidenceTier evidence)]
    _ -> ["ambiguous completion group: " <> evidenceGroup evidence <> ": " <> show (evidenceTier evidence)]
  where
    matching = filter (\scenarioGroup -> groupId scenarioGroup == evidenceGroup evidence && groupTier scenarioGroup == evidenceTier evidence) groups

-- | Each tier keeps its plan identity and audit, while all evidence belongs to one source and run.
auditCompletion :: Completion -> [(ExecutionIdentity, Plan, [Report])] -> [Text]
auditCompletion completion executions =
    fromLeft [] (validateCompletion completion [planCatalogue plan | (_, plan, _) <- executions])
        <> ["completion has no execution evidence" | null executions]
        <> ["completion combines different sources, architectures or runs" | length (ordNub runIdentities) > 1]
        <> concatMap (\(runIdentity, plan, reports) -> concatMap (auditPartitionReport runIdentity plan) reports) executions
        <> concatMap observed (completionEvidence completion)
  where
    runIdentities = [(identitySource runIdentity, identityArchitecture runIdentity, identityRunId runIdentity) | (runIdentity, _, _) <- executions]
    primary = [report | (_, _, reports) <- executions, report <- reports, not (reportRepeat report)]
    observed evidence = case matching evidence of
        [result] -> ["completion law did not pass: " <> evidenceFamily evidence <> "/" <> evidenceLaw evidence | crStatus result /= Passed]
        [] -> ["completion case did not execute: " <> evidenceCase evidence]
        _ -> ["completion case executed more than once: " <> evidenceCase evidence]
    matching evidence =
        [ result
        | report <- primary
        , scenarioGroup <- reportSelectedGroups report
        , groupTier scenarioGroup == evidenceTier evidence
        , groupId scenarioGroup == evidenceGroup evidence
        , result <- reportCases report
        , crGroupId result == evidenceGroup evidence
        , crCaseId result == evidenceCase evidence
        ]
