-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

{- | The ecosystem-agnostic core of the load benchmarks harness: the load knobs, the
per-ecosystem fixture interface, the runtime-statistics capture, and the report rendering.
It holds everything that is the same whatever upstream ecosystem a scenario drives.

== The extension point

Today the proxy serves only npm, but it is built to front several upstream ecosystems
(PyPI, RubyGems, …). The load harness is therefore split into one reusable __structure__
and a small per-ecosystem __interface__:

  * the structure lives here and in "Ecluse.BenchLoad.Oha", and every ecosystem reuses it
    unchanged: the @oha@ driver, the runtime-statistics capture, the scenario runner, and
    the report rendering.

  * the interface is an 'UpstreamFixture' (the Handle pattern: a record carrying an
    ecosystem and its 'Scenario's), written once per ecosystem. A 'Scenario' holds only
    the ecosystem-specific __setup and teardown__ ('scenarioBoot'). It boots that
    ecosystem's stub upstream(s) with the injected latency and payload size, wires the
    proxy, and yields a 'Driver' telling the harness what to drive. npm is the first and
    only instance ("Ecluse.BenchLoad.Npm"). Adding PyPI is "write @pypiFixture@", not
    "rewrite the harness".

== Per-scenario process isolation

Each 'Scenario' runs in its __own process__: the driver re-execs the binary once per
scenario (see "Main"). Peak residency comes from the RTS as a process-wide high-water
mark. A fresh process per scenario is what keeps each scenario's residency its own, rather
than the running maximum of every scenario before it.

== Inform-only

The load benchmarks tier never asserts a throughput pass\/fail. A human reads and trends
the figures, and nothing compares them to a threshold. The one red state is a __literal
failure__: the harness cannot boot, @oha@ cannot run, or a scenario served nothing. The
harness surfaces that as a thrown exception (a non-zero exit).
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
    renderServiceTime,
    renderLoadSaturation,
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
import Ecluse.BenchLoad.Normalise (
    BaselineSource,
    NormalisedRow (NormalisedRow),
    SaturationInput (SaturationInput),
    deriveSaturation,
    queuingDominanceThreshold,
    renderNormalised,
    renderSaturation,
 )
import Ecluse.BenchLoad.Oha (OhaReport (..), runOha, runOhaUrls, runOhaUrlsWith)
import Ecluse.Composition.Sizing (resolveServeAdmission)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)

{- | Tunables every scenario shares: the load the generator applies and the shape of the
upstream it applies it to. Override them through the environment ('loadKnobsFromEnv').
-}
data LoadKnobs = LoadKnobs
    { lkConcurrency :: Int
    -- ^ Concurrent connections the generator holds open (@oha -c@).
    , lkDurationSeconds :: Int
    -- ^ How long each scenario applies load, in seconds (@oha -z@, the in-process loop's run length).
    , lkUpstreamLatencyMicros :: Int
    -- ^ Latency a stub upstream injects before responding, modelling a real network hop.
    , lkPayloadBytes :: Int
    {- ^ Approximate size of the worker and tarball scenarios' synthetic artifacts. The
    packument scenarios serve the corpus captures, so this knob does not size them.
    -}
    , lkCacheMaxEntries :: Int
    {- ^ Metadata-cache entry bound for the cache-eviction scenario. Set it below
    'lkWorkingSet', or the cache holds the whole set and never evicts.
    -}
    , lkWorkingSet :: Int
    {- ^ Number of distinct large packages in the cache-eviction working set, taken from the
    head of the corpus, heaviest first. The default exceeds the corpus, so the corpus bounds it.
    -}
    , lkServeMaxInFlight :: Maybe Int
    {- ^ Process-wide metadata admission capacity the proxy fixture exercises. 'Nothing'
    resolves the shipped computed default via 'resolveServeAdmission', as the composition root
    does.
    -}
    , lkPublicConnectionsPerHost :: Maybe Int
    {- ^ Public-upstream per-host connection-pool capacity. 'Nothing' resolves the shipped
    computed default from the file-descriptor limit via
    'resolvePublicConnections'\/'openFileSoftLimit', as the composition root does.
    -}
    , lkPrivateConnectionsPerHost :: Maybe Int
    {- ^ Private-upstream per-host connection-pool capacity. 'Nothing' resolves the shipped
    computed default from the file-descriptor limit, as the composition root does. The private
    pool does not follow the admission capacity, because a trusted tarball hit streams outside
    admission.
    -}
    }
    deriving stock (Eq, Show)

{- | The default operating point. The payload is about the median tarball of a
popular-package mix, and the concurrency loads the tarball paths under a realistic round
trip while staying inside what a shared runner's generator sustains.
-}
defaultLoadKnobs :: LoadKnobs
defaultLoadKnobs =
    LoadKnobs
        { lkConcurrency = 100
        , lkDurationSeconds = 30
        , lkUpstreamLatencyMicros = 5_000
        , lkPayloadBytes = 355 * 1024
        , lkCacheMaxEntries = 3
        , lkWorkingSet = 64
        , lkServeMaxInFlight = Nothing
        , lkPublicConnectionsPerHost = Nothing
        , lkPrivateConnectionsPerHost = Nothing
        }

{- | Read the load knobs from the environment: @BENCH_LOAD_CONCURRENCY@,
@BENCH_LOAD_DURATION_SECONDS@, @BENCH_LOAD_UPSTREAM_LATENCY_MS@ (milliseconds),
@BENCH_LOAD_PAYLOAD_BYTES@, @BENCH_LOAD_CACHE_MAX_ENTRIES@, @BENCH_LOAD_WORKING_SET@,
@BENCH_LOAD_SERVE_MAX_IN_FLIGHT@, @BENCH_LOAD_PUBLIC_CONNECTIONS_PER_HOST@, and
@BENCH_LOAD_PRIVATE_CONNECTIONS_PER_HOST@. A malformed value falls back to the default
rather than failing, and the three pool knobs stay 'Nothing' so the fixture resolves the
shipped computed default at use.
-}
loadKnobsFromEnv :: IO LoadKnobs
loadKnobsFromEnv = do
    concurrency <- readEnvInt "BENCH_LOAD_CONCURRENCY" (lkConcurrency defaultLoadKnobs)
    duration <- readEnvInt "BENCH_LOAD_DURATION_SECONDS" (lkDurationSeconds defaultLoadKnobs)
    latencyMs <- readEnvInt "BENCH_LOAD_UPSTREAM_LATENCY_MS" (lkUpstreamLatencyMicros defaultLoadKnobs `div` 1_000)
    payload <- readEnvInt "BENCH_LOAD_PAYLOAD_BYTES" (lkPayloadBytes defaultLoadKnobs)
    cacheMax <- readEnvInt "BENCH_LOAD_CACHE_MAX_ENTRIES" (lkCacheMaxEntries defaultLoadKnobs)
    workingSetSize <- readEnvInt "BENCH_LOAD_WORKING_SET" (lkWorkingSet defaultLoadKnobs)
    serveMaxInFlight <- (>>= readMaybe) <$> lookupEnv "BENCH_LOAD_SERVE_MAX_IN_FLIGHT"
    publicConnections <- (>>= readMaybe) <$> lookupEnv "BENCH_LOAD_PUBLIC_CONNECTIONS_PER_HOST"
    privateConnections <- (>>= readMaybe) <$> lookupEnv "BENCH_LOAD_PRIVATE_CONNECTIONS_PER_HOST"
    pure
        LoadKnobs
            { lkConcurrency = max 1 concurrency
            , lkDurationSeconds = max 1 duration
            , lkUpstreamLatencyMicros = max 0 latencyMs * 1_000
            , lkPayloadBytes = max 1 payload
            , lkCacheMaxEntries = max 1 cacheMax
            , lkWorkingSet = max 1 workingSetSize
            , lkServeMaxInFlight = max 1 <$> serveMaxInFlight
            , lkPublicConnectionsPerHost = max 1 <$> publicConnections
            , lkPrivateConnectionsPerHost = max 1 <$> privateConnections
            }
  where
    readEnvInt :: String -> Int -> IO Int
    readEnvInt name fallback = maybe fallback (fromMaybe fallback . readMaybe) <$> lookupEnv name

{- | A per-ecosystem load-test fixture: the ecosystem it serves and its load scenarios.
The harness consumes it without knowing which ecosystem it is.
-}
data UpstreamFixture = UpstreamFixture
    { fixtureEcosystem :: Ecosystem
    -- ^ The upstream ecosystem this fixture exercises.
    , fixtureScenarios :: [Scenario]
    -- ^ The fixture's load scenarios (npm's three mandatory traffic shapes).
    }

{- | One load scenario: its identity and the ecosystem-specific setup and teardown that
boots its stub upstreams, wires the proxy, and yields a 'Driver'. The 'scenarioBoot'
continuation is higher-rank so the harness can run any measurement while the fixture stays
up.
-}
data Scenario = Scenario
    { scenarioName :: Text
    -- ^ A stable, argument-safe identifier (the driver passes it to the child process).
    , scenarioDescription :: Text
    -- ^ A one-line description of the traffic shape, for the rendered report.
    , scenarioConcurrencyScale :: Int
    {- ^ Multiplier applied to 'lkConcurrency' for this scenario alone, @1@ for an ordinary
    scenario. A ceiling probe raises it so the load generator stops being the binding
    constraint. The scenario's description must state the factor, because the operating-point
    line prints the shared base.
    -}
    , scenarioBoot :: forall a. LoadKnobs -> (Driver -> IO a) -> IO a
    -- ^ Bracket the ecosystem-specific setup\/teardown and yield the 'Driver'.
    }

{- | What the harness drives once a scenario's fixture is up. An HTTP scenario hands back
a URL for @oha@, and an in-process scenario hands back an action returning each unit's
latency in seconds.
-}
data Driver
    = {- | Drive this URL with @oha@ (the proxy is up). The harness owns the concurrency
      and duration.
      -}
      DriveHttp Text
    | {- | Drive a __weighted list of URLs__ with @oha@ (the proxy is up). @oha@ spreads
      requests across the list in proportion to each URL's multiplicity. A hot package
      repeated many times and a heavy one listed once therefore realise a heavy-headed
      (Zipfian) serve mix. The harness owns the concurrency and duration.
      -}
      DriveHttpUrls [Text]
    | {- | Drive a weighted list of URLs with @oha@, every request carrying the given
      fixed headers: the revalidation scenario's conditional @If-None-Match@. The measured
      path is then the @304@ answer rather than the full body.
      -}
      DriveHttpHeaders [(Text, Text)] [Text]
    | {- | Run the in-process load for the configured duration, returning each completed
      unit's latency in seconds. The harness wraps the RTS capture around the call and
      computes the throughput and percentiles from the timings.
      -}
      DriveInProcess (IO [Double])

{- | The figures one scenario yields. The report crosses the per-scenario process boundary
as JSON, so it carries JSON instances. Latencies are milliseconds, and @Nothing@ when the
run recorded no successful request.
-}
data ScenarioReport = ScenarioReport
    { srName :: Text
    , srDescription :: Text
    , srConcurrency :: Int
    {- ^ The connections the generator held open for this scenario: the shared base times
    the scenario's 'scenarioConcurrencyScale'. Recorded so no reader misreads a scaled
    scenario (the ceiling probe) against the base operating point.
    -}
    , srRequests :: Int
    -- ^ Requests (or jobs) the proxy actually processed over the measured window.
    , srThroughput :: Double
    -- ^ Requests (or jobs) per second.
    , srSuccessRate :: Double
    -- ^ Fraction of requests that succeeded, in @[0, 1]@.
    , srDeadlineAborts :: Int
    {- ^ Requests the load generator abandoned at the run's deadline, the load-saturation
    signal. Zero for the in-process scenario, which has no deadline-bounded generator.
    -}
    , srP50Ms, srP90Ms, srP99Ms, srP999Ms :: Maybe Double
    -- ^ Latency percentiles, in milliseconds.
    , srAllocPerReqBytes :: Double
    {- ^ Bytes allocated per request, the machine-independent signal. The delta spans the
    whole bench process, so it folds in the stub upstreams and is a consistent over-count, not
    a pure proxy per-request cost.
    -}
    , srPeakResidencyBytes :: Word64
    {- ^ Peak live heap over this scenario's process (RTS @max_live_bytes@). A process
    high-water mark, so it spans the warm-up too, a wider window than the deltas.
    -}
    , srRetainedBytes :: Word64
    -- ^ Live heap retained after a major GC at the scenario's end.
    , srGcs :: Word32
    -- ^ Total GCs over the measured window.
    , srMajorGcs :: Word32
    -- ^ Major (whole-heap) GCs over the measured window, the long-pause kind.
    , srGcWallMs :: Double
    -- ^ Wall-clock time spent in GC over the window, in milliseconds.
    , srMeanPauseMs :: Maybe Double
    -- ^ Mean GC pause over the window, in milliseconds. @Nothing@ when no GC ran.
    , srNote :: Text
    -- ^ A short note: the status-code distribution, and any transport errors.
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

{- | Boot a scenario's fixture, apply the load, and return the figures. Throws only on a
literal failure: an unavailable RTS counter (a binary built without @-T@) or a scenario
that served nothing.
-}
runScenario :: LoadKnobs -> Scenario -> IO ScenarioReport
runScenario knobs scenario = do
    rtsOn <- getRTSStatsEnabled
    unless rtsOn $
        benchFail "bench-load needs the RTS stats (build with -with-rtsopts=-T); getRTSStatsEnabled is False"
    -- Apply the scenario's concurrency scale to the shared base here, once. The boot, the
    -- warm-up, and the measured drive then all see the scenario's own level.
    let scaled = knobs{lkConcurrency = lkConcurrency knobs * max 1 (scenarioConcurrencyScale scenario)}
    scenarioBoot scenario scaled (measure scaled scenario)

-- The measured window opens after a warm-up and a major GC, so the allocation and GC
-- figures are deltas over the steady state alone. Peak residency is a process high-water
-- mark, so it also spans the warm-up.
measure :: LoadKnobs -> Scenario -> Driver -> IO ScenarioReport
measure knobs scenario driver = do
    warmUp driver
    performMajorGC
    before <- getRTSStats
    (requests, throughput, successRate, percentilesMs, deadlineAborts, note) <- drive knobs driver
    after <- getRTSStats
    when (requests <= 0) $
        benchFail ("scenario " <> scenarioName scenario <> " served no requests -- a harness failure, not a result")
    performMajorGC
    retained <- gcdetails_live_bytes . gc <$> getRTSStats
    let (p50, p90, p99, p999) = percentilesMs
        allocated = fromIntegral (allocated_bytes after - allocated_bytes before)
        gcCount = gcs after - gcs before
        gcWallNs = fromIntegral (gc_elapsed_ns after - gc_elapsed_ns before)
    pure
        ScenarioReport
            { srName = scenarioName scenario
            , srConcurrency = lkConcurrency knobs
            , srDescription = scenarioDescription scenario
            , srRequests = requests
            , srThroughput = throughput
            , srSuccessRate = successRate
            , srDeadlineAborts = deadlineAborts
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

-- The HTTP warm-up also primes the metadata cache for the cache-hit scenario. The
-- in-process path needs none worth a separate run.
warmUp :: Driver -> IO ()
warmUp = \case
    DriveHttp url -> void (runOha 8 warmupSeconds url)
    DriveHttpUrls urls -> void (runOhaUrls 8 warmupSeconds urls)
    DriveHttpHeaders headers urls -> void (runOhaUrlsWith headers 8 warmupSeconds urls)
    DriveInProcess _ -> pass
  where
    warmupSeconds :: Int
    warmupSeconds = 3

-- Apply the measured load and return the request count, throughput, success rate, the
-- four percentiles in milliseconds, the deadline-abort count, and a distribution note.
drive :: LoadKnobs -> Driver -> IO (Int, Double, Double, (Maybe Double, Maybe Double, Maybe Double, Maybe Double), Int, Text)
drive knobs = \case
    DriveHttp url -> fromOha <$> runOha (lkConcurrency knobs) (lkDurationSeconds knobs) url
    DriveHttpUrls urls -> fromOha <$> runOhaUrls (lkConcurrency knobs) (lkDurationSeconds knobs) urls
    DriveHttpHeaders headers urls -> fromOha <$> runOhaUrlsWith headers (lkConcurrency knobs) (lkDurationSeconds knobs) urls
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
            , 0 -- no deadline-bounded generator here, so the deadline-abort count is explicitly zero
            , "in-process worker loop (no HTTP surface)"
            )
  where
    -- Project an oha report into the figures the RTS capture pairs with. The single-URL
    -- and weighted-URL-list HTTP drivers share it.
    fromOha :: OhaReport -> (Int, Double, Double, (Maybe Double, Maybe Double, Maybe Double, Maybe Double), Int, Text)
    fromOha report =
        let statusCounts = ohaStatusCounts report
            errorCounts = ohaErrorCounts report
            totalResponses = sum (Map.elems statusCounts)
            totalErrors = sum (Map.elems errorCounts)
            totalRequests = totalResponses + totalErrors

            isSuccess status = "2" `T.isPrefixOf` status || "3" `T.isPrefixOf` status
            successCount = sum [count | (status, count) <- Map.toList statusCounts, isSuccess status]

            elapsed = ohaElapsedSeconds report
            successReqsPerSec = if elapsed > 0 then fromIntegral successCount / elapsed else 0
            successRate = if totalRequests > 0 then fromIntegral successCount / fromIntegral totalRequests else 0
         in ( totalResponses
            , successReqsPerSec
            , successRate
            , (toMs (ohaP50 report), toMs (ohaP90 report), toMs (ohaP99 report), toMs (ohaP999 report))
            , deadlineAbortsOf report
            , distributionNote report
            )

    toMs :: Maybe Double -> Maybe Double
    toMs = fmap (* 1_000)

-- Best-effort saturation signal, never a gate. oha labels a deadline abandonment as the
-- transport error "aborted due to deadline", so this sums the error entries naming the
-- deadline. No matching label yields zero, whether none occurred or a future oha renamed it.
deadlineAbortsOf :: OhaReport -> Int
deadlineAbortsOf report =
    sum [n | (label, n) <- Map.toList (ohaErrorCounts report), "deadline" `T.isInfixOf` T.toLower label]

-- A nearest-rank percentile of a sorted, non-empty list. 'Nothing' for an empty one.
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

{- | Render the per-scenario reports to a Markdown section. The same text goes to stdout
and to the GitHub run summary.
-}
renderReports :: LoadKnobs -> Int -> Ecosystem -> [ScenarioReport] -> Text
renderReports knobs capabilities ecosystem reports =
    T.unlines $
        [ "## Load test -- throughput & latency over " <> ecosystemName ecosystem
        , ""
        , "_Inform-only: figures are read and trended by a human, never compared to a threshold. Allocations per request is the machine-independent signal. Reading notes are at the end of the report._"
        , ""
        , "**Operating point**"
        , ""
        , "| knob | value |"
        , "| --- | --- |"
        , opRow "load" (show (lkConcurrency knobs) <> " connections x " <> show (lkDurationSeconds knobs) <> " s (a scenario may scale its own connections; see the at-a-glance table)")
        , opRow "injected upstream latency" (fmt1 (fromIntegral (lkUpstreamLatencyMicros knobs) / 1_000) <> " ms")
        , opRow "admission" (show admissionCapacity <> " (" <> admissionOrigin <> ")")
        , opRow "private pool" privatePoolNote
        , opRow "public pool" publicPoolNote
        , opRow "GHC capabilities" (show capabilities <> " (scenario children pinned to the driver's count)")
        , opRow "packument corpus" "real-world captures (the packument scenarios serve the corpus)"
        , opRow "cache-eviction bound" (show (lkCacheMaxEntries knobs) <> " entries over a working set of up to " <> show (lkWorkingSet knobs))
        , opRow "worker artifact" ("~" <> fmtKiB (lkPayloadBytes knobs))
        , ""
        , "### At a glance"
        , ""
        , "| scenario | connections | req/s | success | p50 | p99 | alloc/req | peak residency |"
        , "| --- | --: | --: | --: | --: | --: | --: | --: |"
        ]
            <> map glanceRow reports
            <> [""]
            <> concatMap renderScenario reports
            <> readingNotes
  where
    -- Resolved through the same function as the composition root, so the reported
    -- admission is the admission the fixture ran with.
    admissionCapacity = fst (resolveServeAdmission (lkServeMaxInFlight knobs) capabilities)
    admissionOrigin = case lkServeMaxInFlight knobs of
        Just _ -> "explicit"
        Nothing -> "computed from " <> show capabilities <> " capabilities, as in production"

    -- The private pool is fd-derived rather than admission-derived. Name its origin so
    -- the line cannot mislead.
    privatePoolNote = case lkPrivateConnectionsPerHost knobs of
        Just n -> show n <> " (explicit)"
        Nothing -> "computed from the fd limit, as in production"

    -- The public pool is fd-derived too (half the private share). Name its origin the
    -- same way.
    publicPoolNote = case lkPublicConnectionsPerHost knobs of
        Just n -> show n <> " (explicit)"
        Nothing -> "computed from the fd limit, as in production"

    opRow :: Text -> Text -> Text
    opRow k v = "| " <> k <> " | " <> v <> " |"

    -- One at-a-glance row per scenario, linked to its section. The header anchor is the
    -- scenario name, and every name is already a kebab-case slug.
    glanceRow :: ScenarioReport -> Text
    glanceRow r =
        "| ["
            <> srName r
            <> "](#"
            <> srName r
            <> ") | "
            <> show (srConcurrency r)
            <> " | "
            <> fmt1 (srThroughput r)
            <> " | "
            <> fmt1 (srSuccessRate r * 100)
            <> "% | "
            <> maybe "n/a" (\v -> fmt2 v <> " ms") (srP50Ms r)
            <> " | "
            <> maybe "n/a" (\v -> fmt2 v <> " ms") (srP99Ms r)
            <> " | "
            <> fmtKiB (round (srAllocPerReqBytes r))
            <> " | "
            <> fmtMiB (srPeakResidencyBytes r)
            <> " |"

    -- A short closing section, so the numbers lead.
    readingNotes :: [Text]
    readingNotes =
        [ "### Reading the numbers"
        , ""
        , "- **Inform-only.** Throughput and latency are runner-dependent and read coarsely; nothing here gates."
        , "- **Allocations / request is the machine-independent signal**, measured over the whole bench process: the HTTP scenarios also run their in-process stub upstreams (only oha, a subprocess, is excluded), so it is a consistent over-count -- right for trending, not a pure proxy per-request cost, and not comparable to the work-per-request micro-benches."
        , "- **Peak residency is a process high-water mark** spanning the warm-up as well as the measured window; the allocation and GC figures are before/after deltas over the measured window only."
        , "- **Each scenario runs in its own process**, so residency and GC figures are per scenario."
        , "- **A low success rate is deliberate load shedding, not a broken run.** Success counts 2xx and 3xx only, so a shed `503` carrying `Retry-After` reads as a failure. A saturating scenario answers mostly `503`, so its allocations per request average over shed requests and are not a served request's cost."
        ]

renderScenario :: ScenarioReport -> [Text]
renderScenario r =
    [ "### " <> srName r
    , ""
    , "| metric | value |"
    , "| --- | --- |"
    , row "connections held open" (show (srConcurrency r))
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
    , "> " <> srDescription r
    , ""
    ]
  where
    row :: Text -> Text -> Text
    row k v = "| " <> k <> " | " <> v <> " |"

    msCell :: Maybe Double -> Text
    msCell = maybe "n/a" (\v -> fmt2 v <> " ms")

{- | Render the service-time attribution section (upstream against Écluse overhead) from
the concurrency-1 pass's reports, against the given baseline. The pure split and its layout
live in "Ecluse.BenchLoad.Normalise".
-}
renderServiceTime :: BaselineSource -> [ScenarioReport] -> Text
renderServiceTime source reports =
    renderNormalised source (map toRow reports)
  where
    toRow r = NormalisedRow (srName r) (srP50Ms r) (srP99Ms r)

{- | Render the load-saturation section by pairing each loaded report with its
concurrency-1 counterpart by name. The queuing derivation and its flag live in
"Ecluse.BenchLoad.Normalise".
-}
renderLoadSaturation :: [ScenarioReport] -> [ScenarioReport] -> Text
renderLoadSaturation c1Reports loadedReports =
    renderSaturation queuingDominanceThreshold (map (deriveSaturation queuingDominanceThreshold . toInput) loadedReports)
  where
    c1ByName :: Map Text ScenarioReport
    c1ByName = Map.fromList [(srName r, r) | r <- c1Reports]

    toInput loaded =
        SaturationInput
            (srName loaded)
            (srThroughput loaded)
            (srDeadlineAborts loaded)
            (srP50Ms =<< Map.lookup (srName loaded) c1ByName)
            (srP50Ms loaded)

fmt1, fmt2 :: Double -> Text
fmt1 x = toText (showFFloat (Just 1) x "")
fmt2 x = toText (showFFloat (Just 2) x "")

-- An integer byte count rendered in KiB (one decimal).
fmtKiB :: Int -> Text
fmtKiB bytes = fmt1 (fromIntegral bytes / 1024) <> " KiB"

-- A Word64 byte count rendered in MiB (one decimal).
fmtMiB :: Word64 -> Text
fmtMiB bytes = fmt1 (fromIntegral bytes / (1024 * 1024)) <> " MiB"
