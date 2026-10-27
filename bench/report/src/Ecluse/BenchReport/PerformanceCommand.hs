-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Artifact and identity binding runs after the native measurement process exits.
module Ecluse.BenchReport.PerformanceCommand (
    writeObligation,
    writeReceipt,
    auditReceipt,
    compileObligation,
    prepareReceipt,
    verifyRequest,
) where

import Data.Aeson (FromJSON, eitherDecodeStrict')
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Version (showVersion)
import System.Info (compilerName, compilerVersion)

import Ecluse.BenchLoad.Contract (LoadScope (FullLoad, ReadLoadFoundation), requiredLoadPasses)
import Ecluse.BenchLoad.Floors (passKey)
import Ecluse.BenchLoad.Pod (parsePodShape)
import Ecluse.BenchReport.Performance
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Test.Contract.Manifest (digest, digestFile, failRunner, jsonLine, readJsonFile, readPlanIdentity, writeJsonFile)
import Ecluse.Test.Contract.Performance
import Ecluse.Test.Contract.Result (ExecutionIdentity (identityPlanDigest))
import Ecluse.Test.Corpus (cargoCorpusPackages, corpusLabel, corpusPackages, cpName, pypiCorpusPackages, rubyGemsCorpusPackages)

-- | Compile requirements from canonical laws and declared corpus identities, without executing tests.
writeObligation :: Text -> Text -> FilePath -> IO ()
writeObligation kind subject conditionsPath = do
    conditions <- readJsonFile conditionsPath
    obligation <- either failRunner pure (compileObligation kind subject conditions)
    BS.putStr (jsonLine obligation)

-- | Required settings and rows come from reviewed harness requirements.
compileObligation :: Text -> Text -> Map Text Text -> Either Text PerformanceObligation
compileObligation kind subject conditions = do
    let keys =
            ["compiler", "optimisation", "profiling"] <> case kind of
                "allocation" -> ["mode", "passes"]
                "live-allocation" -> ["mode", "passes"]
                "benchmark" -> ["rts", "stdev"]
                _ -> ["podShape", "durationSeconds", "concurrency", "serveMaxInFlight", "affinity"]
        absent = [key | key <- keys, maybe True T.null (Map.lookup key conditions)]
    unless (null absent) (Left ("missing required performance conditions: " <> T.intercalate ", " absent))
    (ecosystem, packages) <- case subject of
        "ecosystem-npm" -> Right (Npm, corpusPackages)
        "ecosystem-pypi" -> Right (PyPI, pypiCorpusPackages)
        "ecosystem-cargo" -> Right (Cargo, cargoCorpusPackages)
        "ecosystem-rubygems" -> Right (RubyGems, rubyGemsCorpusPackages)
        _ -> Left ("unknown performance subject: " <> subject)
    rows <- case kind of
        "allocation" -> Right (allocationRequiredRows (ecosystemName ecosystem) (map cpName packages))
        "live-allocation" -> Right (allocationRequiredRows (ecosystemName ecosystem) (map cpName packages))
        "benchmark" -> Right (benchmarkRequiredRows (ecosystemName ecosystem) (map corpusLabel packages))
        "load-foundation" -> loadRows ReadLoadFoundation ecosystem
        "load" -> loadRows FullLoad ecosystem
        _ -> Left ("unknown performance kind: " <> kind)
    pure (PerformanceObligation kind subject rows (requiredConditions kind conditions))
  where
    loadRows scope ecosystem = do
        shape <- maybeToRight "load obligations require podShape" (Map.lookup "podShape" conditions) >>= parsePodShape
        passes <- first show (requiredLoadPasses scope ecosystem shape)
        pure [scenario <> "/" <> passKey whichPass | (_, (scenario, whichPass)) <- passes]

requiredConditions :: Text -> Map Text Text -> Map Text Text
requiredConditions kind conditions =
    Map.fromList $
        [("compiler", toText compilerName <> "-" <> toText (showVersion compilerVersion)), ("optimisation", "-O1"), ("profiling", "off")]
            <> case kind of
                "allocation" -> [("mode", "captures"), ("passes", "5")]
                "live-allocation" -> [("mode", "live"), ("passes", "5")]
                "benchmark" -> [("rts", "-T"), ("stdev", "10")]
                _ -> [("podShape", Map.findWithDefault "" "podShape" conditions), ("durationSeconds", "30"), ("concurrency", "100"), ("serveMaxInFlight", "automatic"), ("affinity", "taskset -c 0 oha")]

-- | Always write the receipt before reporting a failed or incomplete native measurement.
writeReceipt :: FilePath -> FilePath -> FilePath -> IO ()
writeReceipt requestPath nativePath receiptPath = do
    request <- readJsonFile requestPath
    verifyRequest request
    receipt <- prepareReceipt request nativePath
    writeJsonFile receiptPath receipt
    finishAudit request receipt

-- | Keep native conditions ahead of caller declarations and hash every supporting artifact.
prepareReceipt :: PerformanceRequest -> FilePath -> IO PerformanceReceipt
prepareReceipt request nativePath = do
    unless (nativePath `elem` requestArtifacts request) (failRunner "native artifact must belong to the requested artifact set")
    bytes <- BS.readFile nativePath
    native <-
        if poKind (requestObligation request) == "benchmark"
            then do
                measured <- either failRunner pure (benchmarkObservations (requestOutcome request) Map.empty (TE.decodeUtf8 bytes))
                consolePath <- maybe (failRunner "benchmark receipt requires a consoleLog recorded condition") (pure . toString) (Map.lookup "consoleLog" (requestRecordedConditions request))
                console <- TE.decodeUtf8 <$> BS.readFile consolePath
                pure measured{npObservations = npObservations measured <> benchmarkConsoleObservations console}
            else decodeJson bytes
    let consoleArtifacts = maybeToList (toString <$> Map.lookup "consoleLog" (requestRecordedConditions request))
        recorded = native{npConditions = Map.insert "nativeArtifact" (toText nativePath) (npConditions native)}
    artifacts <- hashArtifacts (ordNub (nativePath : consoleArtifacts <> requestArtifacts request))
    pure (makePerformanceReceipt request artifacts recorded)

-- | Rebuild observations and artifact hashes before accepting a stored receipt.
auditReceipt :: FilePath -> FilePath -> IO ()
auditReceipt requestPath receiptPath = do
    request <- readJsonFile requestPath
    verifyRequest request
    receipt <- readJsonFile receiptPath
    nativePath <- maybe (failRunner "receipt has no native artifact") (pure . toString) (Map.lookup "nativeArtifact" (npConditions (prNative receipt)))
    actual <- prepareReceipt request nativePath
    unless (actual == receipt) (failRunner "performance receipt differs from its native artifacts or execution request")
    finishAudit request receipt

-- | Rebuild requirements and compare the request with the current execution identity.
verifyRequest :: PerformanceRequest -> IO ()
verifyRequest request = do
    let obligation = requestObligation request
    compiled <- either failRunner pure (compileObligation (poKind obligation) (poSubject obligation) (poConditions obligation))
    unless (compiled == obligation) (failRunner "performance obligation differs from compiled requirements")
    unless (identityPlanDigest (requestIdentity request) == digest (jsonLine obligation)) (failRunner "performance plan digest mismatch")
    expected <- readPlanIdentity False (digest (jsonLine obligation))
    unless (requestIdentity request == expected) (failRunner "performance request identity differs from the current execution")
    attempt <- lookupEnv "ECLTEST_ATTEMPT" >>= maybe (failRunner "missing required execution identity: ECLTEST_ATTEMPT") (pure . toText)
    unless (requestAttempt request == attempt && not (T.null attempt)) (failRunner "performance request attempt differs from the current execution")

finishAudit :: PerformanceRequest -> PerformanceReceipt -> IO ()
finishAudit request receipt =
    unless (null problems) (failRunner (T.unlines problems))
  where
    problems = auditPerformanceReceipts (requestIdentity request) (requestAttempt request) [requestObligation request] [receipt]

hashArtifacts :: [FilePath] -> IO (Map FilePath Text)
hashArtifacts paths = Map.fromList <$> traverse (\path -> (path,) <$> digestFile path) paths

decodeJson :: (FromJSON a) => ByteString -> IO a
decodeJson = either (failRunner . toText) pure . eitherDecodeStrict'
