-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @oha@ load-generator driver. It spawns @oha@ as a subprocess against a URL. It
parses the JSON report into the throughput and latency-distribution figures the load
harness records.

The driver runs @oha@ (a single static binary, in the pin) with @--output-format json@.
That report carries a request-rate summary, a latency-percentile table, and the
status\/error distributions. This module knows that schema and nothing about any
ecosystem. It is part of the reusable harness core, shared unchanged across every upstream
a scenario might target.

The driver tolerates a degraded run on purpose: it reports a low success rate or a non-2xx
response and never throws on one. The load benchmarks tier is inform-only and characterises
behaviour rather than asserting a pass\/fail.
A genuinely broken run does throw: the subprocess cannot start, or its output does not
parse. That is a literal harness failure, the one red state the layer recognises.
-}
module Ecluse.BenchLoad.Oha (
    OhaReport (..),
    runOha,
    runOhaUrls,
    runOhaUrlsWith,
) where

import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.Map.Strict qualified as Map
import GHC.IO.Handle (hClose)
import System.Process.Typed (proc, readProcessStdout_)
import UnliftIO.Temporary (withSystemTempFile)

import Ecluse.BenchLoad.Error (benchFail)

-- | The fields of an @oha@ JSON report the load harness records.
data OhaReport = OhaReport
    { ohaRequestsPerSec :: Double
    -- ^ Achieved throughput over the run, requests per second.
    , ohaSuccessRate :: Double
    -- ^ Fraction of requests that succeeded, in @[0, 1]@.
    , ohaElapsedSeconds :: Double
    -- ^ The run's wall-clock duration, in seconds.
    , ohaP50, ohaP90, ohaP99, ohaP999 :: Maybe Double
    -- ^ Latency percentiles in seconds. 'Nothing' when no request succeeded.
    , ohaStatusCounts :: Map Text Int
    -- ^ Response counts keyed by HTTP status code (e.g. @"200"@).
    , ohaErrorCounts :: Map Text Int
    -- ^ Transport-error counts keyed by the error string (e.g. a refused connection).
    }
    deriving stock (Show)

instance FromJSON OhaReport where
    parseJSON = withObject "oha report" $ \o -> do
        summary <- o .: "summary"
        requestsPerSec <- summary .: "requestsPerSec"
        successRate <- summary .: "successRate"
        elapsed <- summary .: "total"
        percentiles <- o .: "latencyPercentiles"
        p50 <- percentiles .:? "p50"
        p90 <- percentiles .:? "p90"
        p99 <- percentiles .:? "p99"
        p999 <- percentiles .:? "p99.9"
        statusCounts <- o .:? "statusCodeDistribution" .!= Map.empty
        errorCounts <- o .:? "errorDistribution" .!= Map.empty
        pure
            OhaReport
                { ohaRequestsPerSec = requestsPerSec
                , ohaSuccessRate = successRate
                , ohaElapsedSeconds = elapsed
                , ohaP50 = p50
                , ohaP90 = p90
                , ohaP99 = p99
                , ohaP999 = p999
                , ohaStatusCounts = statusCounts
                , ohaErrorCounts = errorCounts
                }

{- | Drive @oha@ against a URL and parse its report, keeping the subprocess output off the
harness's stdout, which carries only the machine-readable report. Throws when @oha@ cannot
start or its JSON does not parse. A degraded run (errors, non-2xx) parses and comes back.
-}
runOha :: Int -> Int -> Text -> IO OhaReport
runOha concurrency durationSeconds url =
    runOhaArgs concurrency durationSeconds [toString url]

{- | Drive @oha@ against a weighted URL list. Repeating a URL @w@ times gives it weight @w@
in the served mix, which is how the harness drives a heavy-headed package mix. Same
failure contract as 'runOha'.
-}
runOhaUrls :: Int -> Int -> [Text] -> IO OhaReport
runOhaUrls = runOhaUrlsWith []

{- | 'runOhaUrls' with fixed extra request headers on every request: the revalidation
scenario's @If-None-Match@. Each pair becomes an @-H "name: value"@ argument.
-}
runOhaUrlsWith :: [(Text, Text)] -> Int -> Int -> [Text] -> IO OhaReport
runOhaUrlsWith headers concurrency durationSeconds urls =
    withSystemTempFile "ecluse-bench-urls.txt" $ \path handle -> do
        hClose handle
        writeFileText path (unlines urls)
        runOhaArgs concurrency durationSeconds (headerArgs <> ["--urls-from-file", path])
  where
    headerArgs :: [String]
    headerArgs = concatMap (\(name, value) -> ["-H", toString (name <> ": " <> value)]) headers

-- Run oha with the common reporting flags plus the given target arguments (a single
-- URL, or @--urls-from-file <path>@), and parse its JSON report.
runOhaArgs :: Int -> Int -> [String] -> IO OhaReport
runOhaArgs concurrency durationSeconds target = do
    isolate <- (== Just "1") <$> lookupEnv "BENCH_LOAD_ISOLATE_OHA"
    let (cmd, finalArgs) =
            if isolate
                then ("taskset", ["-c", "0", "oha"] <> args)
                else ("oha", args)
    raw <- readProcessStdout_ (proc cmd finalArgs)
    either (\err -> benchFail ("oha report did not parse: " <> toText err)) pure (eitherDecode raw)
  where
    args :: [String]
    args =
        [ "--no-tui"
        , "--output-format"
        , "json"
        , "-c"
        , show concurrency
        , "-z"
        , show durationSeconds <> "s"
        ]
            <> target
