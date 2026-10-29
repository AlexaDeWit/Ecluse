-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Receipt assembly runs after measurement and keeps diagnostic runs out of full completion.
module Ecluse.BenchReport.PerformanceReceipt (
    writeConditions,
    conditionArguments,
    writePerformanceReceipts,
    receiptDiagnostic,
    clearPerformanceReceipts,
) where

import Data.Aeson (ToJSON (..), object, withObject, (.:), (.=))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Version (showVersion)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import System.Info (compilerName, compilerVersion)
import UnliftIO.Exception (tryAny)

import Ecluse.BenchReport.Performance
import Ecluse.BenchReport.PerformanceCommand
import Ecluse.Test.Contract.Manifest
import Ecluse.Test.Contract.Performance
import Ecluse.Test.Contract.Result (ExecutionIdentity (..))
import Ecluse.Test.Corpus (readCorpusPins)

-- | Task supplies these values from the same variables passed to the native command.
writeConditions :: FilePath -> [Text] -> IO ()
writeConditions path arguments = do
    conditions <- either failRunner pure (conditionArguments arguments)
    writeJsonFile path (Map.insert "compiler" (toText compilerName <> "-" <> toText (showVersion compilerVersion)) conditions)

-- | Reject malformed and duplicate declarations before measurement starts.
conditionArguments :: [Text] -> Either Text (Map Text Text)
conditionArguments arguments = do
    pairs <- traverse entry arguments
    let conditions = Map.fromList pairs
    unless (Map.size conditions == length pairs) (Left "duplicate measurement condition")
    when (Map.member "compiler" conditions) (Left "compiler identity comes from the compiled reporter")
    pure conditions
  where
    entry argument = case T.breakOn "=" argument of
        (key, value) | not (T.null key) && not (T.null value) -> Right (key, T.drop 1 value)
        _ -> Left ("expected condition key=value: " <> argument)

-- | Remove prior publication before reading arguments, identities or native evidence.
clearPerformanceReceipts :: FilePath -> IO ()
clearPerformanceReceipts root = do
    createDirectoryIfMissing True root
    forM_ paths $ \path -> do
        exists <- doesFileExist path
        when exists (removeFile path)
  where
    paths =
        (root </> "diagnostic.json")
            : [root </> subject </> name | subject <- ["ecosystem-npm", "ecosystem-pypi", "ecosystem-cargo", "ecosystem-rubygems"], name <- ["obligation.json", "request.json", "receipt.json"]]

-- | Fixed callers choose the evidence kind. All ecosystems retain receipts before any audit failure.
writePerformanceReceipts :: Text -> FilePath -> FilePath -> IO ()
writePerformanceReceipts kind root conditionsPath = do
    clearPerformanceReceipts root
    conditions <- readJsonFile conditionsPath
    outcome <- processOutcome
    attempt <- lookupEnv "ECLTEST_ATTEMPT" >>= maybe (failRunner "missing ECLTEST_ATTEMPT") (pure . toText)
    when (T.null attempt) $ failRunner "empty ECLTEST_ATTEMPT"
    native <- nativeEvidence kind (nativePath kind)
    case receiptDiagnostic kind conditions native of
        Just reason -> publishDiagnostic root kind conditions outcome attempt reason native
        Nothing -> do
            results <- traverse (publishSubject root kind conditions outcome attempt native) ["npm", "pypi", "cargo", "rubygems"]
            unless (and results) exitFailure

publishSubject :: FilePath -> Text -> Map Text Text -> NativeOutcome -> Text -> Maybe NativePerformance -> Text -> IO Bool
publishSubject root kind conditions outcome attempt native ecosystem = do
    result <- tryAny $ do
        let recorded = maybe conditions (\evidence -> Map.union (npConditions evidence) conditions) native
        obligation <- either failRunner pure (compileObligation kind ("ecosystem-" <> ecosystem) recorded)
        execution <- readRequiredPlanIdentity False (digest (jsonLine obligation))
        captures <- capturePaths ecosystem
        let directory = root </> toString (poSubject obligation)
            request = PerformanceRequest execution attempt obligation (artifactPaths kind <> captures) outcome recorded
        writeJsonFile (directory </> "obligation.json") obligation
        writeJsonFile (directory </> "request.json") request
        verifyRequest request
        receipt <- prepareReceipt request (nativePath kind)
        writeJsonFile (directory </> "receipt.json") receipt
        let faults = auditPerformanceReceipts execution attempt [obligation] [receipt]
        traverse_ putTextLn faults
        pure (null faults)
    case result of
        Left problem -> putTextLn ("performance receipt: " <> ecosystem <> ": " <> show problem) >> pure False
        Right passed -> pure passed

-- | Diagnostic reasons and floor eligibility share the native audit's policy.
receiptDiagnostic :: Text -> Map Text Text -> Maybe NativePerformance -> Maybe Text
receiptDiagnostic kind conditions native
    | kind `elem` ["load", "load-foundation"] && nonempty "thrashLimitsMiB" = Just "GC-thrash probe"
    | kind `elem` ["load", "load-foundation"] && nonempty "selectedScenarios" = Just "selected scenarios"
    | kind `elem` ["load", "load-foundation"] && maybe False (/= "full") (Map.lookup "loadProfile" conditions) = Just "selected load profile"
    | kind `elem` ["load", "load-foundation"] = native >>= loadFloorProblem . npConditions
    | kind == "live-allocation" && any ((== NativeUnavailable) . npOutcome) native = Just "live upstream unavailable"
    | otherwise = Nothing
  where
    nonempty key = maybe False (not . T.null . T.filter (not . (`elem` [' ', '\t', '\n', '\r']))) (Map.lookup key conditions)

processOutcome :: IO NativeOutcome
processOutcome =
    lookupEnv "PERF_NATIVE_OUTCOME" >>= \case
        Just "success" -> pure NativePassed
        Just "failure" -> pure NativeFailed
        Just "cancelled" -> pure NativeUnavailable
        Just "skipped" -> pure NativeUnavailable
        _ -> failRunner "performance receipt: set PERF_NATIVE_OUTCOME from the native step outcome"

nativeEvidence :: Text -> FilePath -> IO (Maybe NativePerformance)
nativeEvidence "benchmark" _ = pure Nothing
nativeEvidence _ path = do
    exists <- doesFileExist path
    if exists
        then do
            bytes <- BS.readFile path
            if BS.null bytes then pure Nothing else Just <$> readJsonFile path
        else pure Nothing

capturePaths :: Text -> IO [FilePath]
capturePaths ecosystem = do
    paths <-
        either (failRunner . toText) pure
            =<< readCorpusPins
                ( \pins -> do
                    captures <- pins .: "captures"
                    entries <- captures .: fromString (toString ecosystem)
                    traverse (withObject "capture" (.: "path")) entries
                )
    when (Map.null paths) $ failRunner ("no pinned captures for " <> ecosystem)
    pure ["bench/corpus" </> path | path <- Map.elems (paths :: Map Text FilePath)]

nativePath :: Text -> FilePath
nativePath = \case
    "allocation" -> "perf-allocation-observations.json"
    "live-allocation" -> "perf-acceptance-observations.json"
    "benchmark" -> "bench-results.csv"
    _ -> "load-observations.json"

reportPath :: Text -> FilePath
reportPath = \case
    "allocation" -> "perf-allocation-report.md"
    "live-allocation" -> "perf-acceptance-report.md"
    "benchmark" -> "bench-output.txt"
    _ -> "bench-load-results.md"

artifactPaths :: Text -> [FilePath]
artifactPaths kind =
    [nativePath kind, reportPath kind, "bench/corpus/pins.json"] <> case kind of
        "allocation" -> ["acceptance/criteria.json"]
        "live-allocation" -> ["acceptance/criteria.json"]
        "benchmark" -> []
        _ -> ["bench/load/floors.json"]

data Diagnostic = Diagnostic
    { diagnosticIdentity :: ExecutionIdentity
    , diagnosticAttempt :: Text
    , diagnosticReason :: Text
    , diagnosticOutcome :: NativeOutcome
    , diagnosticConditions :: Map Text Text
    , diagnosticReportDigest :: Text
    , diagnosticNativeDigest :: Text
    }

instance ToJSON Diagnostic where
    toJSON diagnostic =
        object
            [ "scope" .= ("diagnostic-only" :: Text)
            , "reason" .= diagnosticReason diagnostic
            , "source" .= identitySource execution
            , "runId" .= identityRunId execution
            , "architecture" .= identityArchitecture execution
            , "attempt" .= diagnosticAttempt diagnostic
            , "nativeOutcome" .= diagnosticOutcome diagnostic
            , "podShape" .= condition "podShape"
            , "selectedScenarios" .= condition "selectedScenarios"
            , "floorEnforcement" .= condition "floorEnforcement"
            , "floorEnforcementReasons" .= condition "floorEnforcementReasons"
            , "reportDigest" .= diagnosticReportDigest diagnostic
            , "nativeDigest" .= diagnosticNativeDigest diagnostic
            ]
      where
        execution = diagnosticIdentity diagnostic
        condition key = Map.findWithDefault "" key (diagnosticConditions diagnostic)

publishDiagnostic :: FilePath -> Text -> Map Text Text -> NativeOutcome -> Text -> Text -> Maybe NativePerformance -> IO ()
publishDiagnostic root kind conditions outcome attempt reason native = do
    execution <- readRequiredPlanIdentity False "not-applicable"
    reportDigest <- optionalDigest (reportPath kind)
    when (outcome == NativePassed && T.null reportDigest) $ failRunner "missing diagnostic report"
    nativeDigest <- optionalDigest (nativePath kind)
    let recorded = maybe conditions (\evidence -> Map.union (npConditions evidence) conditions) native
    writeJsonFile (root </> "diagnostic.json") (Diagnostic execution attempt reason outcome recorded reportDigest nativeDigest)
    putTextLn ("performance receipt: " <> reason <> " is diagnostic only")

optionalDigest :: FilePath -> IO Text
optionalDigest path = do
    exists <- doesFileExist path
    if exists
        then do
            bytes <- BS.readFile path
            pure (if BS.null bytes then "" else digest bytes)
        else pure ""
