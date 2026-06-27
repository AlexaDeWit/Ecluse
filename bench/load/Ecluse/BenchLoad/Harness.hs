{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

{- | The ecosystem-agnostic core of the Layer B load harness: the load knobs, the
per-ecosystem fixture interface, the runtime-statistics capture, and the report
rendering — everything that is the same whatever upstream ecosystem a scenario drives.

== The extension point

Today only npm is served, but the proxy is built to front several upstream ecosystems
(PyPI, RubyGems, …). So the load harness is split into one reusable __structure__ and a
small per-ecosystem __interface__:

  * the structure — the @oha@ driver, the runtime-statistics capture, the scenario
    runner, and the report rendering — lives here and in "Ecluse.BenchLoad.Oha", and is
    reused unchanged across ecosystems;

  * the interface — an 'UpstreamFixture' (the Handle pattern: a record carrying an
    ecosystem and its 'Scenario's) — is implemented once per ecosystem. A 'Scenario'
    holds only the ecosystem-specific __setup and teardown__ ('scenarioBoot'): it boots
    that ecosystem's stub upstream(s) with the injected latency and payload size, wires
    the proxy, and yields a 'Driver' telling the harness what to drive. npm is the first
    and only instance ("Ecluse.BenchLoad.Npm"); adding PyPI is "write @pypiFixture@",
    not "rewrite the harness".

== Per-scenario process isolation

A 'Scenario' is run in its __own process__ (the driver re-execs the binary once per
scenario; see "Main"). Peak residency is read from the RTS as a process-wide high-water
mark, so a fresh process per scenario is what keeps each scenario's residency its own
rather than the running maximum of every scenario before it.

== Inform-only

Layer B never asserts a throughput pass\/fail (decision D1): the figures are reported
for a human to read and trend, never compared to a threshold. The one red state is a
__literal failure__ — the harness cannot boot, @oha@ cannot run, or a scenario served
nothing — surfaced as a thrown exception (a non-zero exit). See
@docs\/architecture\/performance.md@.
-}
module Ecluse.BenchLoad.Harness (
    -- * Load knobs
    LoadKnobs (..),
    defaultLoadKnobs,
    loadKnobsFromEnv,

    -- * The per-ecosystem fixture interface (the Handle pattern)
    UpstreamFixture (..),
    Scenario (..),
    Driver (..),

    -- * Running a scenario
    ScenarioReport (..),
    runScenario,

    -- * Rendering
    renderReports,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import GHC.Stats (
    GCDetails (gcdetails_live_bytes),
    RTSStats (allocated_bytes, gc, gc_elapsed_ns, gcs, major_gcs, max_live_bytes),
    getRTSStats,
    getRTSStatsEnabled,
 )
import Numeric (showFFloat)
import System.Mem (performMajorGC)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Oha (OhaReport (..), runOha)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)

-- ── load knobs ─────────────────────────────────────────────────────────────────

{- | The tunables every scenario shares: the load the generator applies (concurrency,
duration) and the shape of the upstream it applies it to (injected per-upstream latency
and the canned payload size). The latency and payload are consumed by a scenario's
ecosystem-specific setup ('scenarioBoot'); the concurrency and duration are consumed by
the harness when it drives the load.

The defaults model a realistic cache-miss on a chunky package; override them through the
environment ('loadKnobsFromEnv') to probe a different operating point. Absolutes are
runner-dependent and noisy (decision D2) — the work-normalized counters (allocations per
request) are the cross-runner-stable signal.
-}
data LoadKnobs = LoadKnobs
    { lkConcurrency :: Int
    -- ^ Concurrent connections the generator holds open (@oha -c@).
    , lkDurationSeconds :: Int
    -- ^ How long each scenario applies load, in seconds (@oha -z@; the in-process loop's run length).
    , lkUpstreamLatencyMicros :: Int
    -- ^ Latency a stub upstream injects before responding, modelling a real network hop.
    , lkPayloadBytes :: Int
    -- ^ Approximate size of the canned upstream payload (a packument body, or an artifact).
    }
    deriving stock (Eq, Show)

{- | The default operating point: 50 concurrent clients for 30 seconds against an
upstream with a 5 ms injected latency serving a ~256 KiB payload — a realistic chunky
cache-miss. Sane for a shared runner: enough load to saturate the proxy without a load
the generator itself cannot sustain.
-}
defaultLoadKnobs :: LoadKnobs
defaultLoadKnobs =
    LoadKnobs
        { lkConcurrency = 50
        , lkDurationSeconds = 30
        , lkUpstreamLatencyMicros = 5_000
        , lkPayloadBytes = 256 * 1024
        }

{- | Read the load knobs from the environment, each falling back to its
'defaultLoadKnobs' value: @BENCH_LOAD_CONCURRENCY@, @BENCH_LOAD_DURATION_SECONDS@,
@BENCH_LOAD_UPSTREAM_LATENCY_MS@ (milliseconds, converted to the microseconds the stub
delays by), and @BENCH_LOAD_PAYLOAD_BYTES@. A malformed value falls back to the default
rather than failing, since the knobs only shape an inform-only measurement.
-}
loadKnobsFromEnv :: IO LoadKnobs
loadKnobsFromEnv = do
    concurrency <- readEnvInt "BENCH_LOAD_CONCURRENCY" (lkConcurrency defaultLoadKnobs)
    duration <- readEnvInt "BENCH_LOAD_DURATION_SECONDS" (lkDurationSeconds defaultLoadKnobs)
    latencyMs <- readEnvInt "BENCH_LOAD_UPSTREAM_LATENCY_MS" (lkUpstreamLatencyMicros defaultLoadKnobs `div` 1_000)
    payload <- readEnvInt "BENCH_LOAD_PAYLOAD_BYTES" (lkPayloadBytes defaultLoadKnobs)
    pure
        LoadKnobs
            { lkConcurrency = max 1 concurrency
            , lkDurationSeconds = max 1 duration
            , lkUpstreamLatencyMicros = max 0 latencyMs * 1_000
            , lkPayloadBytes = max 1 payload
            }
  where
    readEnvInt :: String -> Int -> IO Int
    readEnvInt name fallback = maybe fallback (fromMaybe fallback . readMaybe) <$> lookupEnv name

-- ── the per-ecosystem fixture interface ──────────────────────────────────────────

{- | A per-ecosystem load-test fixture (the Handle pattern): the ecosystem it serves
and its load scenarios. One instance exists per upstream ecosystem; the harness
consumes it without knowing which ecosystem it is. npm is the first and only instance
("Ecluse.BenchLoad.Npm").
-}
data UpstreamFixture = UpstreamFixture
    { fixtureEcosystem :: Ecosystem
    -- ^ The upstream ecosystem this fixture exercises.
    , fixtureScenarios :: [Scenario]
    -- ^ The fixture's load scenarios (npm's three mandatory traffic shapes).
    }

{- | One load scenario: its identity and the ecosystem-specific __setup and teardown__
that boots its stub upstream(s), wires the proxy, and yields a 'Driver' to the harness.

'scenarioBoot' is the whole per-ecosystem surface. It takes the 'LoadKnobs' (for the
injected latency and payload its stubs honour) and a continuation, brackets the
setup\/teardown around it, and hands it the 'Driver' that says what to drive. The
continuation is higher-rank so the harness can run any measurement inside the bracket
while the fixture stays up.
-}
data Scenario = Scenario
    { scenarioName :: Text
    -- ^ A stable, argument-safe identifier (the driver passes it to the child process).
    , scenarioDescription :: Text
    -- ^ A one-line description of the traffic shape, for the rendered report.
    , scenarioBoot :: forall a. LoadKnobs -> (Driver -> IO a) -> IO a
    -- ^ Bracket the ecosystem-specific setup\/teardown and yield the 'Driver'.
    }

{- | What the harness drives once a scenario's fixture is booted. An HTTP scenario hands
back the proxy URL to load with @oha@; an in-process scenario (the worker mirror loop,
which has no HTTP surface) hands back an action that performs the load for the configured
duration and returns each unit's latency in seconds.
-}
data Driver
    = {- | Drive this URL with @oha@ (the proxy is up). The harness owns the concurrency
      and duration.
      -}
      DriveHttp Text
    | {- | Run the in-process load for the configured duration, returning each completed
      unit's latency in seconds. The harness wraps the RTS capture around the call and
      computes the throughput and percentiles from the timings.
      -}
      DriveInProcess (IO [Double])

-- ── the scenario report ──────────────────────────────────────────────────────────

{- | The figures one scenario yields. Serialised across the per-scenario process
boundary (each scenario runs in its own process; the driver collects the reports), so it
carries JSON instances.

Latencies are milliseconds; @Nothing@ when the run recorded no successful request.
Allocations per request is the work-normalized, cross-runner-stable signal; throughput
and the percentiles are runner-dependent and read coarsely.
-}
data ScenarioReport = ScenarioReport
    { srName :: Text
    , srDescription :: Text
    , srRequests :: Int
    -- ^ Requests (or jobs) the proxy actually processed over the measured window.
    , srThroughput :: Double
    -- ^ Requests (or jobs) per second.
    , srSuccessRate :: Double
    -- ^ Fraction of requests that succeeded, in @[0, 1]@.
    , srP50Ms, srP90Ms, srP99Ms, srP999Ms :: Maybe Double
    -- ^ Latency percentiles, in milliseconds.
    , srAllocPerReqBytes :: Double
    -- ^ Bytes allocated per request — the machine-independent signal.
    , srPeakResidencyBytes :: Word64
    -- ^ Peak live heap over this scenario's process (RTS @max_live_bytes@).
    , srRetainedBytes :: Word64
    -- ^ Live heap retained after a major GC at the scenario's end.
    , srGcs :: Word32
    -- ^ Total GCs over the measured window.
    , srMajorGcs :: Word32
    -- ^ Major (whole-heap) GCs over the measured window — the long-pause kind.
    , srGcWallMs :: Double
    -- ^ Wall-clock time spent in GC over the window, in milliseconds.
    , srMeanPauseMs :: Maybe Double
    -- ^ Mean GC pause over the window, in milliseconds; @Nothing@ when no GC ran.
    , srNote :: Text
    -- ^ A short note: the status-code distribution, and any transport errors.
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- ── running a scenario ──────────────────────────────────────────────────────────

{- | Boot a scenario's fixture, apply the load, capture the runtime statistics around
it, and return the figures. The fixture's bracket owns setup and teardown; this owns the
RTS capture and the measurement, which is the same whatever the ecosystem.

Throws on a literal failure — the RTS counters are unavailable (the binary was built
without @-T@), or the scenario served nothing — never on a slow or degraded result.
-}
runScenario :: LoadKnobs -> Scenario -> IO ScenarioReport
runScenario knobs scenario = do
    rtsOn <- getRTSStatsEnabled
    unless rtsOn $
        benchFail "bench-load needs the RTS stats (build with -with-rtsopts=-T); getRTSStatsEnabled is False"
    scenarioBoot scenario knobs (measure scenario)

-- Apply the load over a booted fixture and assemble the report. A short warm-up runs
-- first (JIT, connection pool, and the metadata cache settle, so the measured window is
-- steady-state); then a major GC zeroes the residual heap, the before-snapshot is taken,
-- the measured load runs, and the after-snapshot closes the window.
measure :: Scenario -> Driver -> IO ScenarioReport
measure scenario driver = do
    warmUp driver
    performMajorGC
    before <- getRTSStats
    (requests, throughput, successRate, percentilesMs, note) <- drive driver
    after <- getRTSStats
    when (requests <= 0) $
        benchFail ("scenario " <> scenarioName scenario <> " served no requests — a harness failure, not a result")
    performMajorGC
    retained <- gcdetails_live_bytes . gc <$> getRTSStats
    let (p50, p90, p99, p999) = percentilesMs
        allocated = fromIntegral (allocated_bytes after - allocated_bytes before)
        gcCount = gcs after - gcs before
        gcWallNs = fromIntegral (gc_elapsed_ns after - gc_elapsed_ns before)
    pure
        ScenarioReport
            { srName = scenarioName scenario
            , srDescription = scenarioDescription scenario
            , srRequests = requests
            , srThroughput = throughput
            , srSuccessRate = successRate
            , srP50Ms = p50
            , srP90Ms = p90
            , srP99Ms = p99
            , srP999Ms = p999
            , srAllocPerReqBytes = allocated / fromIntegral requests
            , srPeakResidencyBytes = max_live_bytes after
            , srRetainedBytes = retained
            , srGcs = gcCount
            , srMajorGcs = major_gcs after - major_gcs before
            , srGcWallMs = gcWallNs / 1_000_000
            , srMeanPauseMs = if gcCount == 0 then Nothing else Just (gcWallNs / 1_000_000 / fromIntegral gcCount)
            , srNote = note
            }

-- A brief warm-up before the measured window, so the steady state is what is measured.
-- The HTTP path runs a short @oha@ pass (which also primes the metadata cache for the
-- cache-hit scenario); the in-process path needs none worth a separate run.
warmUp :: Driver -> IO ()
warmUp = \case
    DriveHttp url -> void (runOha 8 warmupSeconds url)
    DriveInProcess _ -> pass
  where
    warmupSeconds :: Int
    warmupSeconds = 3

-- Apply the measured load and return the figures the RTS capture is paired with: the
-- request count, throughput, success rate, the four percentiles in milliseconds, and a
-- distribution note.
drive :: Driver -> IO (Int, Double, Double, (Maybe Double, Maybe Double, Maybe Double, Maybe Double), Text)
drive = \case
    DriveHttp url -> do
        report <- runOhaForKnobs url
        let requests = sum (Map.elems (ohaStatusCounts report))
            percentilesMs = (toMs (ohaP50 report), toMs (ohaP90 report), toMs (ohaP99 report), toMs (ohaP999 report))
        pure (requests, ohaRequestsPerSec report, ohaSuccessRate report, percentilesMs, distributionNote report)
    DriveInProcess act -> do
        start <- getMonotonicTime
        latencies <- act
        end <- getMonotonicTime
        let requests = length latencies
            elapsed = max 1e-9 (end - start)
            sorted = sort latencies
            pctl q = toMs (percentile q sorted)
        pure
            ( requests
            , fromIntegral requests / elapsed
            , 1.0
            , (pctl 0.50, pctl 0.90, pctl 0.99, pctl 0.999)
            , "in-process worker loop (no HTTP surface)"
            )
  where
    toMs :: Maybe Double -> Maybe Double
    toMs = fmap (* 1_000)

-- The oha invocation is closed over the harness's knobs; re-read them so the driver
-- needs no extra plumbing. The driver and the warm-up share the same concurrency/duration.
runOhaForKnobs :: Text -> IO OhaReport
runOhaForKnobs url = do
    knobs <- loadKnobsFromEnv
    runOha (lkConcurrency knobs) (lkDurationSeconds knobs) url

-- A nearest-rank percentile of a sorted, non-empty list; 'Nothing' for an empty one.
percentile :: Double -> [Double] -> Maybe Double
percentile _ [] = Nothing
percentile q xs =
    let n = length xs
        rank = ceiling (q * fromIntegral n) :: Int
        idx = min (n - 1) (max 0 (rank - 1))
     in xs !!? idx

-- A one-line note on the status-code distribution and any transport errors.
distributionNote :: OhaReport -> Text
distributionNote report =
    T.intercalate "; " (statusPart <> errorPart)
  where
    statusPart
        | Map.null (ohaStatusCounts report) = ["no responses"]
        | otherwise = ["status " <> renderCounts (ohaStatusCounts report)]
    errorPart
        | Map.null (ohaErrorCounts report) = []
        | otherwise = ["errors " <> renderCounts (ohaErrorCounts report)]
    renderCounts m = T.intercalate ", " [k <> "×" <> show v | (k, v) <- Map.toList m]

-- ── rendering ────────────────────────────────────────────────────────────────────

{- | Render the per-scenario reports to a Markdown section: a header naming the
ecosystem and the operating point, then one block per scenario with its throughput,
latency percentiles, allocations per request, residency, and GC stats. The same text
goes to stdout and to the GitHub run summary.
-}
renderReports :: LoadKnobs -> Ecosystem -> [ScenarioReport] -> Text
renderReports knobs ecosystem reports =
    T.unlines $
        [ "## Load test — throughput & latency (Layer B) over " <> ecosystemName ecosystem
        , ""
        , "Inform-only: the figures are reported for a human to read and trend, never compared to a threshold (decision D1). Throughput and latency are runner-dependent and read coarsely; allocations per request is the machine-independent signal. The in-process residency and GC stats are per scenario (each runs in its own process)."
        , ""
        , "Operating point: "
            <> show (lkConcurrency knobs)
            <> " concurrent · "
            <> show (lkDurationSeconds knobs)
            <> "s · "
            <> fmt1 (fromIntegral (lkUpstreamLatencyMicros knobs) / 1_000)
            <> " ms injected upstream latency · ~"
            <> fmtKiB (lkPayloadBytes knobs)
            <> " payload."
        , ""
        ]
            <> concatMap renderScenario reports

renderScenario :: ScenarioReport -> [Text]
renderScenario r =
    [ "### " <> srName r
    , ""
    , srDescription r
    , ""
    , "| metric | value |"
    , "| --- | --- |"
    , row "throughput" (fmt1 (srThroughput r) <> " req/s")
    , row "requests" (show (srRequests r) <> " (" <> fmt1 (srSuccessRate r * 100) <> "% success)")
    , row "latency p50 / p90 / p99 / p99.9" (msCell (srP50Ms r) <> " / " <> msCell (srP90Ms r) <> " / " <> msCell (srP99Ms r) <> " / " <> msCell (srP999Ms r))
    , row "allocations / request" (fmtKiB (round (srAllocPerReqBytes r)))
    , row "peak residency" (fmtMiB (srPeakResidencyBytes r))
    , row "retained heap" (fmtMiB (srRetainedBytes r))
    , row "GCs (total / major)" (show (srGcs r) <> " / " <> show (srMajorGcs r))
    , row "GC wall / mean pause" (fmt1 (srGcWallMs r) <> " ms / " <> maybe "n/a" (\p -> fmt2 p <> " ms") (srMeanPauseMs r))
    , row "distribution" (srNote r)
    , ""
    ]
  where
    row :: Text -> Text -> Text
    row k v = "| " <> k <> " | " <> v <> " |"

    msCell :: Maybe Double -> Text
    msCell = maybe "n/a" (\v -> fmt2 v <> " ms")

-- ── numeric formatting ───────────────────────────────────────────────────────────

fmt1, fmt2 :: Double -> Text
fmt1 x = toText (showFFloat (Just 1) x "")
fmt2 x = toText (showFFloat (Just 2) x "")

-- An integer byte count rendered in KiB (one decimal).
fmtKiB :: Int -> Text
fmtKiB bytes = fmt1 (fromIntegral bytes / 1024) <> " KiB"

-- A Word64 byte count rendered in MiB (one decimal).
fmtMiB :: Word64 -> Text
fmtMiB bytes = fmt1 (fromIntegral bytes / (1024 * 1024)) <> " MiB"
