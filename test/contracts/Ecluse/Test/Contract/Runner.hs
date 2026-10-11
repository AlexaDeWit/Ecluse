-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Hspec execution with pure discovery and complete machine-readable evidence.
module Ecluse.Test.Contract.Runner (GroupAction, contractMain, executeGroups) where

import Control.Exception qualified as Exception
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.Timeout (timeout)
import Test.Hspec (Spec)
import Test.Hspec.Core.Format qualified as Format
import Test.Hspec.Core.Formatters.V2 qualified as Formatters
import Test.Hspec.Core.Runner qualified as Hspec

import Ecluse.Test.Contract.Catalogue
import Ecluse.Test.Contract.Options
import Ecluse.Test.Contract.Phase
import Ecluse.Test.Contract.Result
import Ecluse.Test.Contract.Types

-- | A group receives its own lifecycle clock before Hspec constructs its hooks.
type GroupAction = PhaseRecorder -> Spec

-- | The prerequisite callback runs only after pure validation and whole-group selection.
contractMain :: Catalogue -> Map Text GroupAction -> (Group -> IO (Maybe Text)) -> IO ()
contractMain catalogue actions prerequisites = do
    command <- getArgs >>= either die pure . parseCommand
    plan <- either (die . T.unlines) pure $ makePlan catalogue
    let expectedActions = map groupId $ catalogueGroups catalogue
        missingActions = filter (`Map.notMember` actions) expectedActions
        unknownActions = filter (`notElem` expectedActions) (Map.keys actions)
    unless (null missingActions && null unknownActions) $ die $ "spec registration mismatch: " <> show (missingActions, unknownActions)
    case command of
        ListCatalogue -> BS.putStr $ jsonLine catalogue
        PrintPlan -> BS.putStr $ jsonLine plan
        SelectCatalogue selection -> either (die . T.unlines) (BS.putStr . jsonLine) $ selectGroups selection catalogue
        Audit path reportPaths -> auditFiles plan path reportPaths
        Execute options -> do
            groups <- either (die . T.unlines) pure $ selectGroups (runSelection options) catalogue
            identity <- readIdentity False (usesProductImage plan) $ digest (jsonLine plan)
            report <- executeGroups identity options groups actions prerequisites
            let faults = auditSelection groups (reportGroups report) (reportCases report)
            unless (null faults) $ do
                forM_ (reportGroups report) $ \result ->
                    when (grStatus result /= Passed) $ mapM_ putTextLn (grDiagnostics result)
                when (isNothing $ runReportPath options) $ BS.putStr $ jsonLine report
                die $ T.unlines faults

-- | Each completed group is persisted before the next acquisition begins.
executeGroups :: Identity -> RunOptions -> [Group] -> Map Text GroupAction -> (Group -> IO (Maybe Text)) -> IO Report
executeGroups identity options groups actions prerequisites = Exception.mask $ \restore -> do
    reportRef <- newIORef $ Report 1 identity shard (runRepeat options) [] [] (runSeed options) groups
    persist options =<< readIORef reportRef
    forM_ groups $ \group -> do
        (groupResult, caseResults, interrupted) <- restore $ executeGroup options actions prerequisites group
        modifyIORef' reportRef $ \report ->
            report
                { reportGroups = reportGroups report <> [groupResult]
                , reportCases = reportCases report <> caseResults
                }
        persist options =<< readIORef reportRef
        forM_ interrupted Exception.throwIO
    readIORef reportRef
  where
    shard = fromMaybe "all" $ selectionPartition $ runSelection options

executeGroup :: RunOptions -> Map Text GroupAction -> (Group -> IO (Maybe Text)) -> Group -> IO (GroupResult, [CaseResult], Maybe SomeException)
executeGroup options actions prerequisites group = Exception.mask $ \restore -> do
    start <- getCurrentTime
    results <- newIORef []
    recorder <- newPhaseRecorder
    execution <- Exception.try @SomeException $ restore $ bounded options $ do
        unavailable <- prerequisites group
        case unavailable of
            Just reason -> pure $ Left reason
            Nothing -> case Map.lookup (groupId group) actions of
                Nothing -> pure $ Left "group has no registered specification"
                Just spec -> Right <$> runSpec options group results (spec recorder)
    end <- getCurrentTime
    observed <- reverse <$> readIORef results
    timings <- readPhaseTimings recorder
    executable <- toText <$> getProgName
    let (status, diagnostics, interrupted) = executionStatus execution
        missing =
            [ CaseResult (caseId item) (groupId group) (missingStatus status) 0 diagnostics
            | item <- groupCases group
            , caseId item `notElem` map crCaseId observed
            ]
        rerun = executable <> " --group " <> groupId group <> " --seed " <> show (runSeed options)
        groupResult = GroupResult (groupId group) status (realToFrac $ diffUTCTime end start) (diagnostics <> ["Rerun: " <> rerun]) timings
    pure (groupResult, observed <> missing, interrupted)

bounded :: RunOptions -> IO a -> IO (Maybe a)
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

runSpec :: RunOptions -> Group -> IORef [CaseResult] -> Spec -> IO Bool
runSpec options group results spec = do
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
                        captureResult group results event
                }
    (configured, forest) <- Hspec.evalSpec configuration spec
    result <- Hspec.runSpecForest forest configured
    observed <- readIORef results
    pure $ Hspec.specResultSuccess result && all ((== Passed) . crStatus) observed

captureResult :: Group -> IORef [CaseResult] -> Format.Event -> IO ()
captureResult group results = \case
    Format.ItemDone (_, label) item -> do
        let identifier = fromMaybe ("unregistered:" <> toText label) $ labelCaseId label
            Format.Seconds duration = Format.itemDuration item
            (status, diagnostics) = case Format.itemResult item of
                Format.Success -> (Passed, [])
                Format.Pending _ reason -> (Pending, maybe [] (pure . toText) reason)
                Format.Failure _ reason -> (Failed, [show reason])
        let information = [toText $ Format.itemInfo item | not $ null $ Format.itemInfo item]
        modifyIORef' results (CaseResult identifier (groupId group) status duration (information <> diagnostics) :)
    _ -> pure ()

persist :: RunOptions -> Report -> IO ()
persist options report = forM_ (runReportPath options) $ \path -> do
    createDirectoryIfMissing True $ takeDirectory path
    BS.writeFile (path <> ".tmp") (jsonLine report)
    renameFile (path <> ".tmp") path

auditFiles :: Plan -> FilePath -> [FilePath] -> IO ()
auditFiles compiled path reportPaths = do
    bytes <- BS.readFile path
    plan <- decodeFileBytes bytes
    unless (plan == compiled) $ die "downloaded plan differs from the compiled catalogue"
    expected <- readIdentity True (usesProductImage plan) $ digest bytes
    reports <- traverse (BS.readFile >=> decodeFileBytes) reportPaths
    let faults = auditReports expected plan reports
    unless (null faults) $ die $ T.unlines faults
    putTextLn "Every required group and case passed with matching execution identities."

readIdentity :: Bool -> Bool -> Text -> IO Identity
readIdentity forceRequired imageRequired planDigest = do
    explicit <- (== Just "1") <$> lookupEnv "ECLTEST_REQUIRED"
    ci <- (== Just "true") <$> lookupEnv "CI"
    let required = forceRequired || explicit || ci
    let field name fallback =
            lookupEnv name >>= \case
                Just value | not (null value) -> pure $ toText value
                _ | required -> die $ "missing required execution identity: " <> toText name
                _ -> pure fallback
    source <- field "ECLTEST_SOURCE" "local"
    image <- if imageRequired then field "ECLTEST_IMAGE_ID" "local" else pure "not-applicable"
    architecture <- field "ECLTEST_ARCHITECTURE" "local"
    runId <- field "ECLTEST_RUN_ID" "local"
    declaredDigest <- field "ECLTEST_PLAN_DIGEST" planDigest
    unless (declaredDigest == planDigest) $ die "plan digest differs from the selected catalogue"
    pure $ Identity source image architecture planDigest runId

jsonLine :: (ToJSON a) => a -> ByteString
jsonLine value = BL.toStrict (encode value) <> "\n"

digest :: ByteString -> Text
digest bytes = show (hash bytes :: Digest SHA256)

decodeFileBytes :: (FromJSON a) => ByteString -> IO a
decodeFileBytes = either (die . toText) pure . eitherDecodeStrict'

die :: Text -> IO a
die message = putTextLn message >> exitFailure

usesProductImage :: Plan -> Bool
usesProductImage = any ((== E2E) . groupTier) . catalogueGroups . planCatalogue
