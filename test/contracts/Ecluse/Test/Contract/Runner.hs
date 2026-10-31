-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Execute the concrete contract calls declared by a fixed suite entrypoint.
module Ecluse.Test.Contract.Runner (
    GroupAction,
    DeclaredGroup (..),
    RunSettings (..),
    contractMain,
    validateDeclaredGroups,
    captureResult,
) where

import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Text qualified as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getProgName)
import System.Timeout (timeout)
import Test.Hspec (Spec)
import Test.Hspec.Core.Format qualified as Format
import Test.Hspec.Core.Formatters.V2 qualified as Formatters
import Test.Hspec.Core.Runner qualified as Hspec

import Ecluse.Test.Contract.Binding (allCaseIds)
import Ecluse.Test.Contract.Catalogue
import Ecluse.Test.Contract.Manifest
import Ecluse.Test.Contract.Phase
import Ecluse.Test.Contract.Result
import Ecluse.Test.Contract.Types

-- | A group receives its own lifecycle clock before Hspec constructs its hooks.
type GroupAction = PhaseRecorder -> Spec

-- | A concrete contract call carries its metadata beside the actual Hspec action.
data DeclaredGroup = DeclaredGroup
    { declaredGroup :: Group
    , declaredAction :: GroupAction
    }

-- | Operational controls never choose contracts or providers.
data RunSettings = RunSettings
    { runReportPath :: Maybe FilePath
    , runSeed :: Integer
    , runTimeoutSeconds :: Maybe Int
    }
    deriving stock (Eq, Show)

-- | Validate the fixed list before checking prerequisites or acquiring resources.
contractMain :: Catalogue -> Text -> [DeclaredGroup] -> (Group -> IO (Maybe Text)) -> IO ()
contractMain catalogue suite declarations prerequisites = do
    arguments <- getArgs
    unless (null arguments) $ failRunner "fixed contract suites accept no arguments"
    validateDeclaredGroups catalogue suite declarations
    plan <- either (failRunner . T.unlines) pure $ makePlan catalogue
    settings <- readSettings
    executionIdentity <- readIdentity False (usesProductImage plan) $ digest (jsonLine plan)
    report <- executeGroups executionIdentity settings suite declarations prerequisites
    let faults = auditSelection (map declaredGroup declarations) (reportGroups report) (reportCases report)
    unless (null faults) $ do
        forM_ (reportGroups report) $ \result ->
            when (grStatus result /= Passed) $ mapM_ putTextLn (grDiagnostics result)
        when (isNothing $ runReportPath settings) $ BS.putStr $ jsonLine report
        failRunner $ T.unlines faults

-- | Inspect every registered example without running acquisition hooks or assertions.
validateDeclaredGroups :: Catalogue -> Text -> [DeclaredGroup] -> IO ()
validateDeclaredGroups catalogue suite declarations = do
    valid <- either (failRunner . T.unlines) pure $ validateCatalogue catalogue
    let expected = filter ((== suite) . groupPartition) $ catalogueGroups valid
        actual = map declaredGroup declarations
    unless (not (null expected) && List.sortOn groupId actual == List.sortOn groupId expected) $
        failRunner $
            "fixed suite declaration mismatch: " <> suite
    forM_ declarations $ \declaration -> do
        let metadata = declaredGroup declaration
        recorder <- newPhaseRecorder
        registered <- allCaseIds (declaredAction declaration recorder)
        unless (List.sort registered == List.sort (map caseId $ groupCases metadata)) $
            failRunner $
                "Hspec registration mismatch: " <> groupId metadata <> ": " <> show registered

readSettings :: IO RunSettings
readSettings = do
    path <- lookupEnv "ECLTEST_REPORT_PATH"
    when (path == Just "") $ failRunner "ECLTEST_REPORT_PATH must not be empty"
    seed <- lookupEnv "ECLTEST_SEED" >>= maybe (pure 1) (parseNumber "ECLTEST_SEED")
    seconds <- lookupEnv "ECLTEST_TIMEOUT" >>= traverse (parseNumber "ECLTEST_TIMEOUT")
    forM_ seconds $ \value ->
        unless (value > 0 && value <= maxBound `div` 1000000) $
            failRunner "ECLTEST_TIMEOUT requires positive whole seconds within the timer range"
    pure $ RunSettings path seed seconds
  where
    parseNumber name value = maybe (failRunner $ name <> " requires an integer") pure $ readMaybe value

-- Each completed group is persisted before the next acquisition begins.
executeGroups :: ExecutionIdentity -> RunSettings -> Text -> [DeclaredGroup] -> (Group -> IO (Maybe Text)) -> IO Report
executeGroups executionIdentity options suite declarations prerequisites = Exception.mask $ \restore -> do
    reportRef <- newIORef $ Report 1 executionIdentity suite False [] [] (runSeed options) (map declaredGroup declarations)
    persist options =<< readIORef reportRef
    forM_ declarations $ \declaration -> do
        (groupResult, caseResults, interrupted) <- restore $ executeGroup options prerequisites declaration
        modifyIORef' reportRef $ \report ->
            report
                { reportGroups = reportGroups report <> [groupResult]
                , reportCases = reportCases report <> caseResults
                }
        persist options =<< readIORef reportRef
        forM_ interrupted Exception.throwIO
    readIORef reportRef

executeGroup :: RunSettings -> (Group -> IO (Maybe Text)) -> DeclaredGroup -> IO (GroupResult, [CaseResult], Maybe SomeException)
executeGroup options prerequisites (DeclaredGroup scenarioGroup action) = Exception.mask $ \restore -> do
    start <- getCurrentTime
    results <- newIORef []
    recorder <- newPhaseRecorder
    execution <- Exception.try @SomeException $ restore $ bounded options $ do
        unavailable <- prerequisites scenarioGroup
        case unavailable of
            Just reason -> pure $ Left reason
            Nothing -> runSpec options scenarioGroup results (action recorder)
    end <- getCurrentTime
    observed <- reverse <$> readIORef results
    timings <- readPhaseTimings recorder
    executable <- toText <$> getProgName
    let (status, diagnostics, interrupted) = executionStatus execution
        missing =
            [ CaseResult (caseId item) (groupId scenarioGroup) (missingStatus status) 0 diagnostics
            | item <- groupCases scenarioGroup
            , caseId item `notElem` map crCaseId observed
            ]
        rerun = "ECLTEST_SEED=" <> show (runSeed options) <> " " <> executable
        groupResult = GroupResult (groupId scenarioGroup) status (realToFrac $ diffUTCTime end start) (diagnostics <> ["Rerun: " <> rerun]) timings
    pure (groupResult, observed <> missing, interrupted)

bounded :: RunSettings -> IO a -> IO (Maybe a)
bounded options action = case runTimeoutSeconds options of
    Nothing -> Just <$> action
    Just seconds -> timeout (seconds * 1000000) action

executionStatus :: Either SomeException (Maybe (Either Text Bool)) -> (Status, [Text], Maybe SomeException)
executionStatus = \case
    Left exception -> case Exception.fromException exception :: Maybe Exception.SomeAsyncException of
        Just _ -> (Cancelled, [toText $ displayException exception], Just exception)
        Nothing -> (SetupFailed, [toText $ displayException exception], Nothing)
    Right Nothing -> (TimedOut, ["group exceeded its timeout"], Nothing)
    Right (Just (Left reason)) -> (SetupFailed, [reason], Nothing)
    Right (Just (Right success)) -> (if success then Passed else Failed, [], Nothing)

missingStatus :: Status -> Status
missingStatus Passed = Failed
missingStatus status = status

runSpec :: RunSettings -> Group -> IORef [CaseResult] -> Spec -> IO (Either Text Bool)
runSpec options scenarioGroup results spec = do
    let configuration =
            Hspec.defaultConfig
                { Hspec.configFailOnEmpty = True
                , Hspec.configFailOnPending = False
                , Hspec.configFailOnFocused = True
                , Hspec.configFailOnEmptyDescription = True
                , Hspec.configConcurrentJobs = Just 1
                , Hspec.configSeed = Just $ runSeed options
                , Hspec.configFormat = Just $ \formatConfig -> do
                    display <- Formatters.formatterToFormat Formatters.specdoc formatConfig
                    pure $ \event -> do
                        display event
                        captureResult scenarioGroup results event
                }
    (configured, forest) <- Hspec.evalSpec configuration spec
    if all null forest
        then pure $ Left "group specification contains no examples"
        else do
            result <- Hspec.runSpecForest forest configured
            observed <- readIORef results
            pure $ Right $ Hspec.specResultSuccess result && all ((== Passed) . crStatus) observed

-- | Record one actual Hspec outcome with its owning group and original diagnostics.
captureResult :: Group -> IORef [CaseResult] -> Format.Event -> IO ()
captureResult scenarioGroup results = \case
    Format.ItemDone (_, label) item -> do
        let identifier = fromMaybe ("unregistered:" <> toText label) $ labelCaseId label
            Format.Seconds duration = Format.itemDuration item
            (status, diagnostics) = case Format.itemResult item of
                Format.Success -> (Passed, [])
                Format.Pending _ reason -> (Pending, maybe [] (pure . toText) reason)
                Format.Failure _ reason -> (Failed, [show reason])
        let information = [toText $ Format.itemInfo item | not $ null $ Format.itemInfo item]
        modifyIORef' results (CaseResult identifier (groupId scenarioGroup) status duration (information <> diagnostics) :)
    _ -> pure ()

persist :: RunSettings -> Report -> IO ()
persist options report = forM_ (runReportPath options) $ \path -> writeJsonFile path report
