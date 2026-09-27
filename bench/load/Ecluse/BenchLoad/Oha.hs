-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @oha@ load generator, run as a subprocess against a list of URLs. Its JSON report gives
the status and transport-error counts. Its per-request database gives the latency of successful
responses alone, because the report's own percentiles mix sheds in with served requests.
A run that cannot start, or whose report does not parse, fails the harness.
-}
module Ecluse.BenchLoad.Oha (
    OhaRun (..),
    RunLength (..),
    OhaReport (..),
    runOha,
) where

import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.!=), (.:), (.:?))
import Database.SQLite.Simple (Only (fromOnly), query_, withConnection)
import System.FilePath ((</>))
import System.Process.Typed (nullStream, proc, runProcess_, setStdout)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.Error (benchFail)

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
    , ohaSuccessLatencies :: [Double]
    -- ^ Seconds per 2xx or 3xx response, empty when the run did not record requests.
    }
    deriving stock (Show)

instance FromJSON OhaReport where
    parseJSON = withObject "oha report" $ \o -> do
        summary <- o .: "summary"
        elapsed <- summary .: "total"
        statusCounts <- o .:? "statusCodeDistribution" .!= mempty
        errorCounts <- o .:? "errorDistribution" .!= mempty
        pure (OhaReport elapsed statusCounts errorCounts [])

-- | Run @oha@, pinned to core 0 when @BENCH_LOAD_ISOLATE_OHA=1@ so it does not share the proxy's cores.
runOha :: OhaRun -> IO OhaReport
runOha run = withSystemTempDirectory "ecluse-bench-oha" $ \dir -> do
    let urlsFile = dir </> "urls.txt"
        reportFile = dir </> "report.json"
        requestsFile = dir </> "requests.db"
    writeFileText urlsFile (unlines (orUrls run))
    isolate <- (== Just "1") <$> lookupEnv "BENCH_LOAD_ISOLATE_OHA"
    let args = ohaArgs run reportFile requestsFile urlsFile
        (command, finalArgs) = if isolate then ("taskset", ["-c", "0", "oha"] <> args) else ("oha", args)
    -- With a database, oha writes a progress line to stdout, so the report goes to a file.
    runProcess_ (setStdout nullStream (proc command finalArgs))
    raw <- readFileLBS reportFile
    report <- either (\err -> benchFail ("oha report did not parse: " <> toText err)) pure (eitherDecode raw)
    latencies <-
        if orSuccessLatencies run && sum (ohaStatusCounts report) > 0
            then successLatencies requestsFile
            else pure []
    pure report{ohaSuccessLatencies = latencies}

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
