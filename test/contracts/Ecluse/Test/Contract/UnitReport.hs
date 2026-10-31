-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Observe canonical unit examples without changing Hspec discovery, options or execution.
module Ecluse.Test.Contract.UnitReport (unitReportHook, unitCatalogueFor) where

import Data.List qualified as List
import Data.Text qualified as T
import Test.Hspec (Spec)
import Test.Hspec.Core.Format qualified as Format
import Test.Hspec.Core.Formatters.V2 qualified as Formatters
import Test.Hspec.Core.Runner qualified as Hspec
import Test.Hspec.Core.Spec (modifyConfig, runIO)

import Ecluse.Test.Contract.Catalogue (labelCaseId)
import Ecluse.Test.Contract.Manifest
import Ecluse.Test.Contract.Phase (PhaseTimings (..))
import Ecluse.Test.Contract.Result
import Ecluse.Test.Contract.Runner (captureResult)
import Ecluse.Test.Contract.Types

-- | Reporting is opt-in. Its complete canonical outcome check makes filtered CI runs fail closed.
unitReportHook :: Text -> IO Catalogue -> Spec -> Spec
unitReportHook suite inspect spec = do
    destination <- runIO $ lookupEnv "ECLTEST_UNIT_REPORT_PATH"
    case destination of
        Nothing -> spec
        Just path -> do
            configure <- runIO $ prepare path
            modifyConfig configure
            spec
  where
    prepare path = do
        when (null path) $ failRunner "ECLTEST_UNIT_REPORT_PATH must not be empty"
        planPath <- lookupEnv "ECLTEST_UNIT_PLAN_PATH" >>= maybe (failRunner "ECLTEST_UNIT_PLAN_PATH is required") pure
        when (null planPath) $ failRunner "ECLTEST_UNIT_PLAN_PATH must not be empty"
        catalogue <- inspect
        let scoped = unitCatalogueFor suite catalogue
        plan <- either (failRunner . T.unlines) pure $ makePlan scoped
        runIdentity <- readPlanIdentity False (digest $ jsonLine plan)
        writeJsonFile planPath plan
        writeJsonFile path (unitReport runIdentity suite 0 (catalogueGroups scoped) [])
        arguments <- getArgs
        pure $ \configuration ->
            let wrap formatter formatConfig = do
                    resolved <- Hspec.readConfig configuration arguments
                    when (Hspec.configDryRun resolved) $ failRunner "unit reporting requires execution, not --dry-run"
                    wrapUnitFormatter path runIdentity suite (catalogueGroups scoped) formatter formatConfig
             in configuration
                    { Hspec.configFormat = Just $ wrap $ fromMaybe (Formatters.formatterToFormat Formatters.checks) (Hspec.configFormat configuration)
                    , Hspec.configAvailableFormatters = [(name, wrap formatter) | (name, formatter) <- Hspec.configAvailableFormatters configuration]
                    }

-- | A unit executable emits one partition while retaining its independently declared groups.
unitCatalogueFor :: Text -> Catalogue -> Catalogue
unitCatalogueFor suite catalogue = catalogue{catalogueGroups = map (\scenarioGroup -> scenarioGroup{groupPartition = suite, groupCases = List.sortOn caseId (groupCases scenarioGroup)}) (catalogueGroups catalogue)}

-- Wrap the selected formatter and persist only actual labelled outcomes, including pending and failures.
wrapUnitFormatter :: FilePath -> ExecutionIdentity -> Text -> [Group] -> (Format.FormatConfig -> IO Format.Format) -> Format.FormatConfig -> IO Format.Format
wrapUnitFormatter path runIdentity suite groups formatter formatConfig = do
    display <- formatter formatConfig
    outcomes <- newIORef []
    failedOutside <- newIORef False
    pure $ \event -> do
        display event
        case event of
            Format.ItemDone (_, label) item ->
                case labelCaseId label >>= owner of
                    Just scenarioGroup -> do
                        captureResult scenarioGroup outcomes event
                        void $ persist outcomes failedOutside
                    Nothing -> case Format.itemResult item of
                        Format.Failure _ _ -> do
                            alreadyFailed <- readIORef failedOutside
                            unless alreadyFailed $ do
                                writeIORef failedOutside True
                                void $ persist outcomes failedOutside
                        _ -> pure ()
            Format.Done _ -> do
                report <- persist outcomes failedOutside
                let faults = auditSelection groups (reportGroups report) (reportCases report)
                unless (null faults) $ failRunner $ T.unlines faults
            _ -> pure ()
  where
    owner identifier = find (any ((== identifier) . caseId) . groupCases) groups
    persist outcomes failedOutside = do
        observed <- reverse <$> readIORef outcomes
        failed <- readIORef failedOutside
        let recorded = unitReport runIdentity suite (Format.formatConfigUsedSeed formatConfig) groups observed
            report = if failed then recorded{reportGroups = map (\scenarioGroup -> scenarioGroup{grStatus = Failed, grDiagnostics = ["unit suite reported a failure outside the canonical cases"]}) (reportGroups recorded)} else recorded
        writeJsonFile path report
        pure report

-- Group success requires every declared case to execute and pass. Missing cases are never synthesised.
unitReport :: ExecutionIdentity -> Text -> Integer -> [Group] -> [CaseResult] -> Report
unitReport runIdentity suite seed groups outcomes = Report 1 runIdentity suite False (map groupResult groups) outcomes seed groups
  where
    groupResult scenarioGroup =
        let actual = filter ((== groupId scenarioGroup) . crGroupId) outcomes
            complete = List.sort (map crCaseId actual) == List.sort (map caseId $ groupCases scenarioGroup)
            passed = complete && all ((== Passed) . crStatus) actual
         in GroupResult (groupId scenarioGroup) (if passed then Passed else Failed) (sum $ map crDurationSeconds actual) [] (PhaseTimings 0 0 0)
