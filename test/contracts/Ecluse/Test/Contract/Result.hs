-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Execution evidence must cover the planned groups and cases exactly once.
module Ecluse.Test.Contract.Result (
    Plan (..),
    ExecutionIdentity (..),
    Status (..),
    GroupResult (..),
    CaseResult (..),
    Report (..),
    makePlan,
    auditReports,
    auditPartitionReport,
    auditSelection,
    auditExactIds,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericParseJSON, genericToJSON, withText)
import Data.Aeson.Types (Options (..))
import Data.Char (toLower)
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as T

import Ecluse.Test.Contract.Catalogue
import Ecluse.Test.Contract.Phase (PhaseTimings)
import Ecluse.Test.Contract.Types

-- | The plan contains only declarations, so its bytes can be hashed before image preparation.
data Plan = Plan
    { planSchemaVersion :: Int
    , planPartitions :: [Text]
    , planCatalogue :: Catalogue
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Plan where toJSON = genericToJSON (wire "plan")
instance FromJSON Plan where parseJSON = genericParseJSON (wire "plan")

-- | Reports from a different checkout, image, architecture, plan, or run cannot be combined.
data ExecutionIdentity = ExecutionIdentity
    { identitySource :: Text
    , identityImage :: Text
    , identityArchitecture :: Text
    , identityPlanDigest :: Text
    , identityRunId :: Text
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON ExecutionIdentity where toJSON = genericToJSON (wire "identity")
instance FromJSON ExecutionIdentity where parseJSON = genericParseJSON (wire "identity")

-- | Only Passed contributes successful required coverage.
data Status = Passed | Failed | Pending | SetupFailed | Cancelled | TimedOut
    deriving stock (Eq, Show)

instance ToJSON Status where toJSON = toJSON . statusText
instance FromJSON Status where
    parseJSON = withText "case status" $ \case
        "passed" -> pure Passed
        "failed" -> pure Failed
        "pending" -> pure Pending
        "setup-failed" -> pure SetupFailed
        "cancelled" -> pure Cancelled
        "timed-out" -> pure TimedOut
        _ -> fail "unknown case status"

statusText :: Status -> Text
statusText = \case
    Passed -> "passed"
    Failed -> "failed"
    Pending -> "pending"
    SetupFailed -> "setup-failed"
    Cancelled -> "cancelled"
    TimedOut -> "timed-out"

-- | Group failure includes hooks and cleanup that need not belong to an example.
data GroupResult = GroupResult
    { grGroupId :: Text
    , grStatus :: Status
    , grDurationSeconds :: Double
    , grDiagnostics :: [Text]
    , grPhases :: PhaseTimings
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON GroupResult where toJSON = genericToJSON (wire "gr")
instance FromJSON GroupResult where parseJSON = genericParseJSON (wire "gr")

-- | A formatter observation preserves the Hspec result and its diagnostics.
data CaseResult = CaseResult
    { crCaseId :: Text
    , crGroupId :: Text
    , crStatus :: Status
    , crDurationSeconds :: Double
    , crDiagnostics :: [Text]
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON CaseResult where toJSON = genericToJSON (wire "cr")
instance FromJSON CaseResult where parseJSON = genericParseJSON (wire "cr")

-- | Repeat attempts have separate reports and never fill omissions in the primary partition.
data Report = Report
    { reportSchemaVersion :: Int
    , reportIdentity :: ExecutionIdentity
    , reportShard :: Text
    , reportRepeat :: Bool
    , reportGroups :: [GroupResult]
    , reportCases :: [CaseResult]
    , reportSeed :: Integer
    , reportSelectedGroups :: [Group]
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Report where toJSON = genericToJSON (wire "report")
instance FromJSON Report where parseJSON = genericParseJSON (wire "report")

wire :: String -> Options
wire prefix = defaultOptions{fieldLabelModifier = lowerFirst . drop (length prefix)}
  where
    lowerFirst [] = []
    lowerFirst (char : rest) = toLower char : rest

-- | A validated plan has deterministic ordering and a nonempty partition list.
makePlan :: Catalogue -> Either [Text] Plan
makePlan catalogue = do
    valid <- validateCatalogue catalogue
    let canonical =
            valid
                { catalogueGroups = List.sortOn groupId (catalogueGroups valid)
                , catalogueProfiles = List.sortOn profileId (catalogueProfiles valid)
                , catalogueObligations = List.sortOn obligationId (catalogueObligations valid)
                }
    pure $ Plan 1 (Set.toAscList $ Set.fromList $ map groupPartition $ catalogueGroups canonical) canonical

-- | Check both group and case evidence, including each case's group ownership.
auditSelection :: [Group] -> [GroupResult] -> [CaseResult] -> [Text]
auditSelection groups actualGroups actualCases =
    auditExactIds "group" (map groupId groups) (map grGroupId actualGroups)
        <> auditExactIds "case" expectedCases (map crCaseId actualCases)
        <> [ "group did not pass: " <> grGroupId result <> ": " <> statusText (grStatus result)
           | result <- actualGroups
           , grStatus result /= Passed
           ]
        <> [ "case did not pass: " <> crCaseId result <> ": " <> statusText (crStatus result)
           | result <- actualCases
           , crStatus result /= Passed
           ]
        <> [ "case has wrong group: " <> crCaseId result
           | result <- actualCases
           , (crCaseId result, crGroupId result) `notElem` ownership
           ]
  where
    ownership = [(caseId item, groupId scenarioGroup) | scenarioGroup <- groups, item <- groupCases scenarioGroup]
    expectedCases = map fst ownership

-- | Missing shard files, repeats, pending cases, and mismatched identities fail the primary audit.
auditReports :: ExecutionIdentity -> Plan -> [Report] -> [Text]
auditReports expected plan reports =
    auditPlan expected plan
        <> auditExactIds "shard" (planPartitions plan) (map reportShard primary)
        <> concatMap reportProblems reports
        <> auditSelection groups (concatMap reportGroups primary) (concatMap reportCases primary)
  where
    groups = catalogueGroups $ planCatalogue plan
    primary = filter (not . reportRepeat) reports
    reportProblems report =
        ["unknown shard: " <> reportShard report | reportShard report `notElem` planPartitions plan]
            <> auditReportGroups expected (filter ((== reportShard report) . groupPartition) groups) report

-- | A cross-tier audit verifies a submitted partition against its original complete plan.
auditPartitionReport :: ExecutionIdentity -> Plan -> Report -> [Text]
auditPartitionReport expected plan report =
    auditPlan expected plan
        <> ["unknown shard: " <> reportShard report | reportShard report `notElem` planPartitions plan]
        <> ["completion report cannot be a repeat" | reportRepeat report]
        <> auditReportGroups expected (filter ((== reportShard report) . groupPartition) (catalogueGroups $ planCatalogue plan)) report

auditPlan :: ExecutionIdentity -> Plan -> [Text]
auditPlan expected plan =
    ["unsupported plan schema" | planSchemaVersion plan /= 1]
        <> fromLeft [] (validateCatalogue $ planCatalogue plan)
        <> auditExactIds "plan partition" cataloguePartitions (planPartitions plan)
        <> ["empty expected identity: " <> name | (name, value) <- identityFields expected, T.null value]
  where
    cataloguePartitions = Set.toAscList $ Set.fromList $ map groupPartition $ catalogueGroups $ planCatalogue plan

auditReportGroups :: ExecutionIdentity -> [Group] -> Report -> [Text]
auditReportGroups expected groups report =
    ["unsupported report schema: " <> reportShard report | reportSchemaVersion report /= 1]
        <> ["execution identity mismatch: " <> reportShard report | reportIdentity report /= expected]
        <> [ "selected manifest mismatch: " <> reportShard report
           | List.sortOn groupId (reportSelectedGroups report) /= List.sortOn groupId groups
           ]
        <> auditSelection groups (reportGroups report) (reportCases report)

identityFields :: ExecutionIdentity -> [(Text, Text)]
identityFields executionIdentity =
    [ ("source", identitySource executionIdentity)
    , ("image", identityImage executionIdentity)
    , ("architecture", identityArchitecture executionIdentity)
    , ("planDigest", identityPlanDigest executionIdentity)
    , ("runId", identityRunId executionIdentity)
    ]

-- | Report missing, unknown and duplicate identifiers without accepting duplicate evidence.
auditExactIds :: Text -> [Text] -> [Text] -> [Text]
auditExactIds kind expected actual =
    ["missing " <> kind <> ": " <> identifier | identifier <- Set.toAscList (expectedSet Set.\\ actualSet)]
        <> ["unknown " <> kind <> ": " <> identifier | identifier <- Set.toAscList (actualSet Set.\\ expectedSet)]
        <> ["duplicate " <> kind <> ": " <> identifier | identifier <- duplicates actual]
  where
    expectedSet = Set.fromList expected
    actualSet = Set.fromList actual
