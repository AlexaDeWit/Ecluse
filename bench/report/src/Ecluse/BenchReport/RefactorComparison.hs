-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Temporary issue 1565 controls run only in the comparison controller.
module Ecluse.BenchReport.RefactorComparison (audit, controls) where

import Data.Aeson (eitherDecodeStrict')
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.FilePath ((</>))
import UnliftIO.Exception (throwIO, tryAny)

import Ecluse.BenchReport (BenchRow (..), parseCsv)
import Ecluse.Test.Contract.Manifest (readJsonFile)
import Ecluse.Test.Contract.Performance
import Ecluse.Test.Contract.Result (ExecutionIdentity (..), auditExactIds)

-- | Require the historical full row list and all twenty pass-specific receipts.
audit :: FilePath -> Text -> Text -> Text -> Text -> FilePath -> IO ()
audit root runId attempt baseline candidate manifest = do
    expected <- manifestRows manifest
    actualPaths <- T.lines <$> readComparisonText (root </> "receipt-paths.txt")
    problems <- forM schedule $ \(kind, passName) -> do
        let source = if "A" `T.isPrefixOf` passName then baseline else candidate
            passRunId = runId <> "-" <> attempt <> "-" <> kind <> "-" <> passName
            directory = root </> toString (kind <> "-" <> passName)
        csvProblems <- if kind == "benchmark" then capture $ rowProblems expected <$> readComparisonText (directory </> "bench-results.csv") else pure []
        receiptProblems <- forM subjects $ \subject -> capture $ do
            receipt <- readJsonFile (directory </> "receipts" </> toString subject </> "receipt.json")
            pure (receiptFaults (source, passRunId, attempt, kind, subject) receipt)
        pure (map ((kind <> "-" <> passName <> ": ") <>) (csvProblems <> concat receiptProblems))
    let expectedPaths = [toText (root </> toString (kind <> "-" <> passName) </> "receipts" </> toString subject </> "receipt.json") | (kind, passName) <- schedule, subject <- subjects]
    finish (auditExactIds "receipt path" expectedPaths actualPaths <> concat problems)

schedule :: [(Text, Text)]
schedule = [("benchmark", passName) | passName <- ["A1", "A2", "B1", "A3", "B2", "B3"]] <> [("allocation", passName) | passName <- ["A1", "B1", "A2", "B2"]]

subjects :: [Text]
subjects = ["ecosystem-npm", "ecosystem-pypi"]

manifestRows :: FilePath -> IO [Text]
manifestRows path = do
    names <- T.lines <$> readComparisonText path
    finish (["expected 343 nonempty unique manifest rows" | length names /= 343 || any T.null names] <> auditExactIds "manifest row" names names)
    pure names

rowProblems :: [Text] -> Text -> [Text]
rowProblems expected csv = case parseCsv csv of
    Left problem -> [problem]
    Right rows ->
        let names = map (\row -> rowGroup row <> "." <> rowBench row) rows
         in auditExactIds "CSV row" expected names
                <> ["CSV row order or count differs" | expected /= names]
                <> ["missing or negative numeric measurement: " <> rowGroup row <> "." <> rowBench row | row <- rows, not (complete row)]
  where
    complete row = rowMeanPs row >= 0 && rowStdev2Ps row >= 0 && all (maybe False (>= 0)) [rowAllocatedBytes row, rowCopiedBytes row, rowPeakBytes row]

receiptFaults :: (Text, Text, Text, Text, Text) -> PerformanceReceipt -> [Text]
receiptFaults (source, runId, attempt, kind, subject) receipt =
    [ "receipt identity or native coverage differs"
    | prSchemaVersion receipt /= 1
        || identitySource (prIdentity receipt) /= source
        || identityRunId (prIdentity receipt) /= runId
        || prAttempt receipt /= attempt
        || prKind receipt /= kind
        || prSubject receipt /= subject
        || npOutcome native /= NativePassed
        || null observations
        || any ((/= NativePassed) . observationOutcome) observations
        || any (T.null . observationId) observations
        || Map.null (prArtifacts receipt)
    ]
        <> auditExactIds "receipt observation" (map observationId observations) (map observationId observations)
  where
    native = prNative receipt
    observations = npObservations native

readComparisonText :: FilePath -> IO Text
readComparisonText path = readFileBS path >>= either throwIO pure . TE.decodeUtf8'

capture :: IO [Text] -> IO [Text]
capture action = either (pure . show) id <$> tryAny action

finish :: [Text] -> IO ()
finish problems = unless (null problems) $ traverse_ putTextLn problems >> exitFailure

-- | Exercise rejected rows and receipt identities before any native measurements.
controls :: FilePath -> IO ()
controls manifest = do
    _ <- manifestRows manifest
    let header = "Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory\n"
        row = "\"All.group.quoted, \"\"name\"\"\",1,0,2,0,3\n"
        expected = ["group.quoted, \"name\""]
        receipt = PerformanceReceipt 1 (ExecutionIdentity "source" "image" "aarch64" "plan" "run") "1" "benchmark" "ecosystem-npm" (NativePerformance NativePassed [PerformanceObservation "row" NativePassed] Map.empty) (Map.singleton "artifact" "digest")
        expectedReceiptIdentity = ("source", "run", "1", "benchmark", "ecosystem-npm")
        invalidReceipts =
            [ receipt{prIdentity = (prIdentity receipt){identityRunId = "other"}}
            , receipt{prIdentity = (prIdentity receipt){identityRunId = ""}}
            , receipt{prIdentity = (prIdentity receipt){identitySource = "other"}}
            , receipt{prAttempt = "2"}
            , receipt{prSubject = "ecosystem-pypi"}
            , receipt{prKind = "allocation"}
            , receipt{prNative = (prNative receipt){npOutcome = NativeFailed}}
            , receipt{prNative = (prNative receipt){npOutcome = NativeUnavailable}}
            , receipt{prNative = (prNative receipt){npObservations = []}}
            , receipt{prNative = (prNative receipt){npObservations = [PerformanceObservation "row" NativePassed, PerformanceObservation "row" NativePassed]}}
            ]
        invalidCsv =
            [ ""
            , header
            , header <> row <> row
            , header <> "\"unterminated"
            , header <> "All.group.other,1,0,2,0,3\n"
            , header <> T.replace ",1,0,2,0,3" ",1,0,,0,3" row
            , header <> T.replace ",1,0,2,0,3" ",1,0,-1,0,3" row
            , header <> T.replace ",1,0,2,0,3" ",1,0,invalid,0,3" row
            , "Name,Mean (ps),2*Stdev (ps)\n" <> T.replace ",1,0,2,0,3" ",1,0" row
            ]
    finish $
        ["valid quoted CSV rejected" | not (null (rowProblems expected (header <> row)))]
            <> ["invalid CSV accepted" | csv <- invalidCsv, null (rowProblems expected csv)]
            <> ["valid receipt rejected" | not (null (receiptFaults expectedReceiptIdentity receipt))]
            <> ["invalid receipt accepted" | bad <- invalidReceipts, null (receiptFaults expectedReceiptIdentity bad)]
            <> ["invalid receipt set accepted" | paths <- [[], ["a", "a"], ["a", "other"]], null (auditExactIds "receipt" ["a", "b"] paths)]
            <> ["missing receipt identity accepted" | isRight (eitherDecodeStrict' "{}" :: Either String PerformanceReceipt)]
    putTextLn "comparison controls passed"
