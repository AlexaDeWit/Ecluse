-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Oha reports statuses and transferred bytes. Its database supplies success-only latencies.
module Ecluse.BenchLoad.Oha (
    OhaRun (..),
    RunLength (..),
    OhaReport (..),
    runOha,
    ohaPinned,
) where

import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Map.Strict qualified as Map
import Database.SQLite.Simple (Only (fromOnly), query_, withConnection)
import System.FilePath ((</>))
import System.Process.Typed (nullStream, proc, runProcess_, setStdout)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.CpuUse (ohaCpu)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Latency (Percentiles (..), noPercentiles)

-- | How long a run lasts: a fixed duration, or a fixed number of requests.
data RunLength
    = ForSeconds Int
    | ForRequests Int
    deriving stock (Eq, Show)

-- | One @oha@ invocation. Repeating a URL weights it in the mix.
data OhaRun = OhaRun
    { orConnections :: Int
    , orLength :: RunLength
    , orHeaders :: [(Text, Text)]
    , orUrls :: [Text]
    , orSuccessLatencies :: Bool
    -- ^ Record every request so the report carries successful-response latencies. A warm-up skips it.
    }
    deriving stock (Eq, Show)

-- | What the harness reads from one run.
data OhaReport = OhaReport
    { ohaElapsedSeconds :: Double
    , ohaStatusCounts :: Map Text Int
    -- ^ Response counts keyed by HTTP status code.
    , ohaErrorCounts :: Map Text Int
    -- ^ Transport-error counts keyed by oha's error text, including its deadline aborts.
    , ohaFirstBytePercentiles :: Maybe Percentiles
    -- ^ First body bytes over all completed responses, including refusals.
    , ohaSuccessLatencies :: [Double]
    -- ^ Seconds per 2xx or 3xx response, empty when the run did not record requests.
    , ohaTransferredBodyBytes :: Maybe Word64
    -- ^ Oha's actual @summary.totalData@, including response bodies for non-success statuses.
    }
    deriving stock (Show)

instance FromJSON OhaReport where
    parseJSON = withObject "oha report" $ \o -> do
        summary <- o .: "summary"
        elapsed <- summary .: "total"
        transferred <- summary .:? "totalData"
        statusCounts <- o .:? "statusCodeDistribution" .!= mempty
        errorCounts <- o .:? "errorDistribution" .!= mempty
        firstByte <- o .:? "firstBytePercentiles"
        observed <- traverse firstByteObservations firstByte
        pure (OhaReport elapsed statusCounts errorCounts (join observed) [] transferred)

firstByteObservations :: Map Text (Maybe Double) -> Parser (Maybe Percentiles)
firstByteObservations values = do
    when (any (\value -> value < 0 || isNaN value || isInfinite value) (catMaybes (Map.elems values))) (fail "invalid Oha first-byte percentile")
    let at key = (* 1000) <$> join (Map.lookup key values)
        observed = Percentiles (at "p50") (at "p90") (at "p95") (at "p99") (at "p99.9")
    pure (observed <$ guard (observed /= noPercentiles))

-- | Run @oha@, pinned to the CPU 'ohaPinned' names.
runOha :: OhaRun -> IO OhaReport
runOha run = withSystemTempDirectory "ecluse-bench-oha" $ \dir -> do
    let urlsFile = dir </> "urls.txt"
        reportFile = dir </> "report.json"
        requestsFile = dir </> "requests.db"
    writeFileText urlsFile (unlines (orUrls run))
    pinned <- ohaPinned
    let args = ohaArgs run reportFile requestsFile urlsFile
        (command, finalArgs) = maybe ("oha", args) (\cpu -> ("taskset", ["-c", show cpu, "oha"] <> args)) pinned
    -- With a database, oha writes a progress line to stdout, so the report goes to a file.
    runProcess_ (setStdout nullStream (proc command finalArgs))
    raw <- readFileLBS reportFile
    report <- either (\err -> benchFail ("oha report did not parse: " <> toText err)) pure (eitherDecode raw)
    latencies <-
        if orSuccessLatencies run && sum (ohaStatusCounts report) > 0
            then successLatencies requestsFile
            else pure []
    pure report{ohaSuccessLatencies = latencies}

-- | The CPU @oha@ is pinned to: 'ohaCpu' under @BENCH_LOAD_ISOLATE_OHA=1@, and none otherwise.
ohaPinned :: IO (Maybe Int)
ohaPinned = (ohaCpu <$) . guard . (== Just "1") <$> lookupEnv "BENCH_LOAD_ISOLATE_OHA"

ohaArgs :: OhaRun -> FilePath -> FilePath -> FilePath -> [String]
ohaArgs run reportFile requestsFile urlsFile =
    ["--no-tui", "--output-format", "json", "-o", reportFile, "-c", show (orConnections run)]
        <> lengthArgs
        <> concatMap (\(name, value) -> ["-H", toString (name <> ": " <> value)]) (orHeaders run)
        <> (if orSuccessLatencies run then ["--db-url", requestsFile] else [])
        <> ["--urls-from-file", urlsFile]
  where
    lengthArgs = case orLength run of
        ForSeconds seconds -> ["-z", show seconds <> "s"]
        ForRequests count -> ["-n", show count]

successLatencies :: FilePath -> IO [Double]
successLatencies path =
    withConnection path $ \conn ->
        map fromOnly <$> query_ conn "SELECT duration FROM oha WHERE status >= 200 AND status < 400"
