-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Temporary issue 1565 controls run only in the comparison controller.
module Ecluse.BenchReport.RefactorComparison (audit, auditListing, checkInputs, controls) where

import Data.Aeson (eitherDecodeStrict')
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.FilePath ((</>))
import UnliftIO.Exception (tryAny)

import Ecluse.BenchReport (BenchRow (..), parseCsv)
import Ecluse.BenchReport.PerformanceCommand (compileObligation)
import Ecluse.BenchReport.RefactorEvidence (ComparisonScope (..), receiptArtifactFaults, scopeName, scopeSchedule, verifyEvidence)
import Ecluse.BenchReport.RefactorEvidenceControls (evidenceControls)
import Ecluse.BenchReport.RefactorFiles (captureProblems, readComparisonText, writeComparisonText)
import Ecluse.BenchReport.RefactorInputAudit (auditInputs)
import Ecluse.BenchReport.RefactorInputControls (inputControls)
import Ecluse.BenchReport.RefactorMetrics (auditListingMetrics, auditMetrics, metricControls)
import Ecluse.Test.Contract.Manifest (digest, jsonLine, readJsonFile)
import Ecluse.Test.Contract.Performance
import Ecluse.Test.Contract.Result (ExecutionIdentity (..), auditExactIds)

-- | Require the historical full row list and all twenty pass-specific receipts.
audit :: FilePath -> Text -> Text -> Text -> Text -> Text -> Text -> FilePath -> IO ()
audit = auditScope FullComparison

-- | Require all four listing passes from one explicitly scoped recovery run.
auditListing :: FilePath -> Text -> Text -> Text -> Text -> Text -> Text -> FilePath -> IO ()
auditListing = auditScope ListingComparison

-- | Reject changed prepared inputs before a recovery starts its native measurements.
checkInputs :: FilePath -> IO ()
checkInputs root = auditInputs root >>= finish

auditScope :: ComparisonScope -> FilePath -> Text -> Text -> Text -> Text -> Text -> Text -> FilePath -> IO ()
auditScope scope root runId attempt baseline baselineTree candidate candidateTree manifest = do
    expected <- manifestRows manifest
    pathsResult <- tryAny (T.lines <$> readComparisonText (root </> "receipt-paths.txt"))
    let actualPaths = fromRight [] pathsResult
        pathFaults = either (one . show) (const []) pathsResult
        schedule = filter ((`elem` ["benchmark", "allocation"]) . fst) (scopeSchedule scope)
    problems <- forM schedule $ \(kind, passName) -> do
        let source = if "A" `T.isPrefixOf` passName then baseline else candidate
            passRunId = runId <> "-" <> attempt <> "-" <> kind <> "-" <> passName
            directory = root </> toString (kind <> "-" <> passName)
        csvProblems <- if kind == "benchmark" then captureProblems $ rowProblems expected <$> readComparisonText (directory </> "bench-results.csv") else pure []
        receiptProblems <- forM subjects $ \subject -> captureProblems $ do
            receipt <- readJsonFile (directory </> "receipts" </> toString subject </> "receipt.json")
            reference <- readJsonFile (root </> toString (kind <> "-A1") </> "receipts" </> toString subject </> "receipt.json")
            let conditions = npConditions (prNative receipt)
            obligation <- either (fail . toString) pure (compileObligation kind subject conditions)
            let expectedIdentity = ExecutionIdentity source "not-applicable" "arm64" (digest (jsonLine obligation)) passRunId
            artifactFaults <- receiptArtifactFaults root kind passName receipt
            pure (artifactFaults <> receiptFaults (source, passRunId, attempt, kind, subject) receipt <> auditPerformanceReceipts expectedIdentity attempt [obligation] [receipt] <> conditionFaults (npConditions (prNative reference)) conditions)
        pure (map ((kind <> "-" <> passName <> ": ") <>) (csvProblems <> concat receiptProblems))
    let expectedPaths = [toText (toString (kind <> "-" <> passName) </> "receipts" </> toString subject </> "receipt.json") | (kind, passName) <- schedule, subject <- subjects]
    identityFaults <- verifyEvidence scope root runId attempt baseline baselineTree candidate candidateTree
    let evidenceFaults = pathFaults <> auditExactIds "receipt path" expectedPaths actualPaths <> concat problems <> identityFaults
        measure = case scope of
            FullComparison -> auditMetrics
            ListingComparison -> auditListingMetrics
    (metricFaults, breaches) <- if null evidenceFaults then measure root else pure ([], [])
    let faults = evidenceFaults <> metricFaults
    writeComparisonText (root </> "audit-faults.txt") (T.unlines faults)
    writeComparisonText (root </> "tolerance-breaches.tsv") (T.unlines breaches)
    putTextLn ("Scope: " <> scopeName scope <> ". Audit faults: " <> show (length faults) <> ". Measured tolerance breaches: " <> show (length breaches) <> ".")
    finish (faults <> ["measured tolerance breaches require review" | not (null breaches)])

conditionFaults :: Map Text Text -> Map Text Text -> [Text]
conditionFaults expected actual = ["probe condition differs: " <> key | (key, value) <- Map.toList expected, Map.lookup key actual /= Just value]

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
        || identityArchitecture (prIdentity receipt) /= "arm64"
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

finish :: [Text] -> IO ()
finish problems = unless (null problems) $ traverse_ putTextLn problems >> exitFailure

-- | Exercise rejected rows and receipt identities before any native measurements.
controls :: FilePath -> IO ()
controls manifest = do
    _ <- manifestRows manifest
    metricControls
    nativeControls
    evidenceControls manifest
    inputControls
    let header = "Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory\n"
        row = "\"All.group.quoted, \"\"name\"\"\",1,0,2,0,3\n"
        expected = ["group.quoted, \"name\""]
        receipt = PerformanceReceipt 1 (ExecutionIdentity "source" "image" "arm64" "plan" "run") "1" "benchmark" "ecosystem-npm" (NativePerformance NativePassed [PerformanceObservation "row" NativePassed] Map.empty) (Map.singleton "artifact" "digest")
        expectedReceiptIdentity = ("source", "run", "1", "benchmark", "ecosystem-npm")
        invalidReceipts =
            [ receipt{prIdentity = (prIdentity receipt){identityRunId = "other"}}
            , receipt{prIdentity = (prIdentity receipt){identityRunId = ""}}
            , receipt{prIdentity = (prIdentity receipt){identitySource = "other"}}
            , receipt{prIdentity = (prIdentity receipt){identityArchitecture = "x86_64"}}
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
            <> ["probe identity mismatch accepted" | key <- ["source", "run", "attempt", "rts", "inputs"], let expectedConditions = Map.singleton key "expected", actual <- [Map.empty, Map.singleton key "different"], null (conditionFaults expectedConditions actual)]
    putTextLn "comparison controls passed"

nativeControls :: IO ()
nativeControls = forM_ ["benchmark", "allocation"] $ \kind -> do
    let declarations = Map.fromList [("compiler", "declared"), ("optimisation", "-O1"), ("profiling", "off"), ("rts", "-T"), ("stdev", "10"), ("mode", "captures"), ("passes", "5")]
    obligation <- either (fail . toString) pure (compileObligation kind "ecosystem-npm" declarations)
    let execution = ExecutionIdentity "source" "not-applicable" "arm64" (digest (jsonLine obligation)) "run"
        native = NativePerformance NativePassed [PerformanceObservation row NativePassed | row <- poRows obligation] (poConditions obligation)
        receipt = PerformanceReceipt 1 execution "1" kind "ecosystem-npm" native (Map.singleton "artifact" (T.replicate 64 "0"))
        faults value = auditPerformanceReceipts execution "1" [obligation] [value]
        missing = receipt{prNative = native{npObservations = drop 1 (npObservations native)}}
        altered = receipt{prNative = native{npConditions = Map.insert (if kind == "benchmark" then "rts" else "passes") "different" (npConditions native)}}
    finish (["complete native control rejected" | not (null (faults receipt))] <> ["missing native observation accepted" | null (faults missing)] <> ["changed native conditions accepted" | null (faults altered)])
