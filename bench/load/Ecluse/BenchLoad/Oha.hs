{- | The @oha@ load-generator driver: spawn @oha@ as a subprocess against a URL and
parse its JSON report into the throughput and latency-distribution figures the load
harness records.

@oha@ (single static binary, in the pin) is driven with @--output-format json@, whose
report carries a request-rate summary, a latency-percentile table, and the status\/error
distributions. This module knows that schema and nothing about any ecosystem: it is
part of the reusable harness core, shared unchanged across every upstream a scenario
might target.

The driver is deliberately tolerant of a degraded run -- a low success rate or non-2xx
responses are reported, not thrown -- because the load benchmarks tier is inform-only and characterises
behaviour rather than asserting a pass\/fail (see @docs\/architecture\/performance.md@).
A genuinely broken run (the subprocess cannot start, or its output does not parse) does
throw, since that is a literal harness failure, the one red state the layer recognises.
-}
module Ecluse.BenchLoad.Oha (
    OhaReport (..),
    runOha,
    runOhaUrls,
) where

import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.Map.Strict qualified as Map
import GHC.IO.Handle (hClose)
import System.Environment (lookupEnv)
import System.Process.Typed (proc, readProcessStdout_)
import UnliftIO.Temporary (withSystemTempFile)

import Ecluse.BenchLoad.Error (benchFail)

{- | The fields of an @oha@ JSON report the harness records: the achieved request rate,
the fraction of requests that succeeded, the run's wall-clock duration, the latency
percentiles (in seconds, absent when no request succeeded), and the status-code and
error distributions.
-}
data OhaReport = OhaReport
    { ohaRequestsPerSec :: Double
    -- ^ Achieved throughput over the run, requests per second.
    , ohaSuccessRate :: Double
    -- ^ Fraction of requests that succeeded, in @[0, 1]@.
    , ohaElapsedSeconds :: Double
    -- ^ The run's wall-clock duration, in seconds.
    , ohaP50, ohaP90, ohaP99, ohaP999 :: Maybe Double
    -- ^ Latency percentiles in seconds; 'Nothing' when no request succeeded.
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

{- | Drive @oha@ against a URL at the given concurrency for the given number of
seconds, returning its parsed report. The subprocess output is captured (never the
harness's stdout, which carries only the machine-readable per-scenario report), so a
caller renders the figures itself.

Throws if @oha@ cannot be started or its JSON does not parse -- a literal harness
failure. A merely degraded run (errors, non-2xx) parses cleanly and is returned for the
caller to report.
-}
runOha :: Int -> Int -> Text -> IO OhaReport
runOha concurrency durationSeconds url =
    runOhaArgs concurrency durationSeconds [toString url]

{- | Drive @oha@ against a __weighted list of URLs__ at the given concurrency for the
given number of seconds, returning its parsed report. The list is written to a
temporary file and passed via @--urls-from-file@; @oha@ spreads requests across the
file in proportion to each URL's multiplicity, so repeating a URL @w@ times gives it
weight @w@ in the served mix -- the mechanism the load harness uses to drive a realistic
heavy-headed (Zipfian) package mix (a few hot packages, a long one-shot tail).

The same literal-failure contract as 'runOha': throws if @oha@ cannot be started or
its JSON does not parse, returns a degraded run for the caller to report.
-}
runOhaUrls :: Int -> Int -> [Text] -> IO OhaReport
runOhaUrls concurrency durationSeconds urls =
    withSystemTempFile "ecluse-bench-urls.txt" $ \path handle -> do
        hClose handle
        writeFileText path (unlines urls)
        runOhaArgs concurrency durationSeconds ["--urls-from-file", path]

-- Run oha with the common reporting flags plus the given target arguments (a single
-- URL, or @--urls-from-file <path>@), and parse its JSON report.
runOhaArgs :: Int -> Int -> [String] -> IO OhaReport
runOhaArgs concurrency durationSeconds target = do
    isolate <- (== Just "1") <$> lookupEnv "BENCH_LOAD_ISOLATE_OHA"
    let (cmd, finalArgs) = if isolate
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
