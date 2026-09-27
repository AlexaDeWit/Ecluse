-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

{- | Load settings, the fixture interface, and the measurement of one scenario. Each scenario runs
in a child process against its own proxy process, so every figure belongs to that scenario alone.
Throughput and latency are informational. "Ecluse.BenchLoad.Verdict" names what fails a run.
-}
module Ecluse.BenchLoad.Harness (
    -- * Load knobs
    LoadKnobs (..),
    defaultLoadKnobs,
    loadKnobsFromEnv,

    -- * The per-ecosystem fixture interface (the Handle pattern)
    UpstreamFixture (..),
    Scenario (..),
    scenario,
    Target (..),
    proxied,
    Driver (..),
    Load (..),
    urlLoad,

    -- * Running a scenario
    ScenarioReport (..),
    LoadSummary (..),
    ProxyFigures (..),
    runScenario,
    warmUp,
    reportEvidence,
) where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import GHC.Stats (getRTSStatsEnabled)
import System.Mem (performMajorGC, performMinorGC)
import UnliftIO.Async (concurrently, withAsync)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Exposition (GaugeSummary, Sample (sampleName), commonLabels, renderSample, seriesTotal, summariseGauge)
import Ecluse.BenchLoad.Latency (Percentiles, isSuccessStatus, percentiles)
import Ecluse.BenchLoad.Oha (OhaReport (..), OhaRun (..), RunLength (ForRequests, ForSeconds), runOha)
import Ecluse.BenchLoad.PatternReport (ReplayTotals (..))
import Ecluse.BenchLoad.Patterns (RequestTrace (rtClients))
import Ecluse.BenchLoad.Pod (CgroupReading (..), counter, renderPodShape)
import Ecluse.BenchLoad.ProxyProcess (
    ProxyEnd (..),
    ProxyProcess,
    podShapeFromEnv,
    proxyBootLines,
    proxyCgroupNow,
    proxyIdleCgroupBytes,
    proxyIdleRts,
    proxyScrape,
    proxySnapshot,
    stopProxy,
 )
import Ecluse.BenchLoad.Replay (Replay (..), ReplayReport (..), runReplay)
import Ecluse.BenchLoad.RtsProbe (readRtsSnapshot)
import Ecluse.BenchLoad.RtsWindow (Collection (MajorCollection, MinorCollection), RtsSnapshot (rsLiveBytes), RtsWindow, rtsWindow)
import Ecluse.BenchLoad.Verdict (ProxyEnding (CleanShutdown), RunEvidence (..))
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Test.Poll (pollUntil)

-- | What every scenario shares: the load applied and the upstream it is applied to.
data LoadKnobs = LoadKnobs
    { lkConcurrency :: Int
    -- ^ Connections the generator holds open (@oha -c@).
    , lkDurationSeconds :: Int
    -- ^ How long each scenario, or each ramp step, applies load.
    , lkUpstreamLatencyMicros :: Int
    -- ^ Latency a stub upstream injects before responding, modelling a network hop.
    , lkPayloadBytes :: Int
    -- ^ Size of the synthetic artifacts. The metadata scenarios serve the corpus captures.
    , lkCacheMaxEntries :: Int
    -- ^ Entry bound for the cache-eviction scenario. Keep it below 'lkWorkingSet'.
    , lkWorkingSet :: Int
    -- ^ Distinct packages in the cache working set, heaviest first. The corpus bounds it.
    , lkServeMaxInFlight :: Maybe Int
    -- ^ Metadata admission capacity. 'Nothing' leaves it to the proxy's computed default.
    , lkPublicConnectionsPerHost :: Maybe Int
    -- ^ Public pool capacity. 'Nothing' leaves it to the proxy's computed default.
    , lkPrivateConnectionsPerHost :: Maybe Int
    -- ^ Private pool capacity. 'Nothing' leaves it to the proxy's computed default.
    }
    deriving stock (Eq, Show)

-- | The default concurrency, duration, upstream latency, and artifact size.
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

-- | Read @BENCH_LOAD_*@ overrides. A malformed value keeps the default.
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

-- | A per-ecosystem fixture: the ecosystem it serves and its scenarios.
data UpstreamFixture = UpstreamFixture
    { fixtureEcosystem :: Ecosystem
    , fixtureScenarios :: [Scenario]
    }

-- | A named scenario whose boot bracket keeps its fixture alive throughout measurement.
data Scenario = Scenario
    { scenarioName :: Text
    -- ^ A stable, argument-safe identifier (the driver passes it to the child process).
    , scenarioDescription :: Text
    , scenarioConcurrencyScale :: Int
    {- ^ Multiplier on 'lkConcurrency' for this scenario alone. The description must state it,
    because the operating point prints the shared base.
    -}
    , scenarioServiceTime :: Bool
    -- ^ Whether the concurrency-one pass runs it. Replays, bursts, ramps, and paired loads do not.
    , scenarioInProcess :: Bool
    -- ^ Work in the harness process, which no pod shape bounds, so only an unlimited run measures it.
    , scenarioBoot :: forall a. LoadKnobs -> (Target -> IO a) -> IO a
    }

-- | A proxied, duration-driven scenario at the base concurrency that joins the concurrency-one pass.
scenario :: Text -> Text -> (forall a. LoadKnobs -> (Target -> IO a) -> IO a) -> Scenario
scenario name description boot =
    Scenario
        { scenarioName = name
        , scenarioDescription = description
        , scenarioConcurrencyScale = 1
        , scenarioServiceTime = True
        , scenarioInProcess = False
        , scenarioBoot = boot
        }

-- | What a booted fixture hands the harness: the proxy it measures, if any, and how to load it.
data Target = Target
    { targetProxy :: Maybe ProxyProcess
    , targetDriver :: Driver
    }

-- | A target served by a proxy process.
proxied :: ProxyProcess -> Driver -> Target
proxied proxy = Target (Just proxy)

-- | Headers sent with every request, and a weighted URL list: a repeated URL carries more weight.
data Load = Load
    { loadHeaders :: [(Text, Text)]
    , loadUrls :: [Text]
    }

-- | A load with no extra headers.
urlLoad :: [Text] -> Load
urlLoad = Load []

-- | How a scenario applies its traffic.
data Driver
    = -- | Hold the base connections for the configured duration.
      DriveHttp Load
    | -- | Send this many requests at once to an idle proxy, once, with no warm-up.
      DriveBurst Int Text
    | -- | Run the load once per connection count, each for the configured duration.
      DriveRamp [Int] Load
    | -- | Measure the first load while the second runs beside it on its own connections.
      DriveUnder Load Load
    | -- | Consume a finite trace once from the fresh fixture, without priming any cache.
      DriveReplay Replay
    | -- | Run the in-process load for the configured duration, returning each unit's latency in seconds.
      DriveInProcess (IO [Double])

-- | One load's outcome over its window. Latencies cover successful responses only.
data LoadSummary = LoadSummary
    { lsLabel :: Text
    , lsConnections :: Int
    , lsElapsedSeconds :: Double
    , lsCompleted :: Int
    -- ^ HTTP responses of any status, or completed in-process units.
    , lsSuccesses :: Int
    -- ^ 2xx and 3xx responses: the primary figure.
    , lsRefusals :: Int
    -- ^ @429@ and @503@ responses, which carry the admission's back-pressure.
    , lsOtherStatuses :: Int
    , lsTransportFailures :: Int
    , lsDeadlineAborts :: Int
    -- ^ Requests still in flight when the window closed, or unfinished replay work.
    , lsLatency :: Percentiles
    , lsNote :: Text
    -- ^ The status and transport-error distribution.
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | What the proxy process showed beyond the traffic: its limits, its memory, and its ending.
data ProxyFigures = ProxyFigures
    { pfIdleRts :: Maybe RtsSnapshot
    , pfIdleCgroupBytes :: Maybe Int
    , pfCgroup :: Maybe CgroupReading
    -- ^ Read after the proxy exited, so an OOM kill is counted.
    , pfWindowThrottledUsec :: Maybe Int
    -- ^ CPU time the quota withheld during the window.
    , pfEnding :: ProxyEnding
    , pfExitedEarly :: Bool
    , pfStderrTail :: Text
    -- ^ Kept only for an ending other than a clean shutdown.
    , pfBootLines :: [Text]
    , pfInFlight :: GaugeSummary
    -- ^ @ecluse.serve.admission.in_flight@ sampled each second of the window.
    , pfAdmissionSeries :: [Text]
    -- ^ Every admission series at the end of the window.
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | A child process's report, which the driver renders and checks.
data ScenarioReport = ScenarioReport
    { srName :: Text
    , srDescription :: Text
    , srShape :: Text
    , srLoad :: LoadSummary
    , srCompanion :: Maybe LoadSummary
    -- ^ The concurrent load a paired scenario ran beside the measured one.
    , srSteps :: [LoadSummary]
    -- ^ One summary per ramp step.
    , srReplayTotals :: Maybe ReplayTotals
    , srRtsSource :: Text
    -- ^ Which process the RTS figures describe: the proxy, or the harness for in-process work.
    , srRtsWindow :: Maybe RtsWindow
    , srRtsEnd :: Maybe RtsSnapshot
    -- ^ The snapshot closing the window, whose maxima cover the process's life so far.
    , srRetainedBytes :: Maybe Word64
    -- ^ Live data after a major collection at the end of the scenario.
    , srProxy :: Maybe ProxyFigures
    , srEvidence :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Measure one fixture. Missing RTS counters fail the harness.
runScenario :: LoadKnobs -> Scenario -> IO ScenarioReport
runScenario knobs s = do
    rtsOn <- getRTSStatsEnabled
    unless rtsOn $
        benchFail "bench-load needs the RTS stats (build with -with-rtsopts=-T); getRTSStatsEnabled is False"
    shape <- podShapeFromEnv
    let scaled = knobs{lkConcurrency = lkConcurrency knobs * max 1 (scenarioConcurrencyScale s)}
    scenarioBoot s scaled (measure scaled s (renderPodShape shape))

measure :: LoadKnobs -> Scenario -> Text -> Target -> IO ScenarioReport
measure knobs s shape (Target proxy driver) = do
    warmUp driver
    settle proxy
    before <- snapshotOf proxy MajorCollection
    cgroupBefore <- cgroupOf proxy
    (outcome, inFlight) <- sampling proxy (drive knobs driver)
    after <- snapshotOf proxy MinorCollection
    cgroupAfter <- cgroupOf proxy
    admission <- maybe (pure []) admissionSeries proxy
    evidence <- case driver of
        DriveReplay replay -> replayEvidence replay
        _ -> pure ""
    retained <- snapshotOf proxy MajorCollection
    ends <- traverse (\p -> (p,) <$> stopProxy p) proxy
    pure
        ScenarioReport
            { srName = scenarioName s
            , srDescription = scenarioDescription s
            , srShape = shape
            , srLoad = doLoad outcome
            , srCompanion = doCompanion outcome
            , srSteps = doSteps outcome
            , srReplayTotals = doReplay outcome
            , srRtsSource = if isJust proxy then "proxy process" else "harness process"
            , srRtsWindow = rtsWindow <$> before <*> after
            , srRtsEnd = after
            , srRetainedBytes = rsLiveBytes <$> retained
            , srProxy = (\(p, end) -> proxyFigures p end (throttled cgroupBefore cgroupAfter) inFlight admission) <$> ends
            , srEvidence = evidence
            }
  where
    throttled a b = do
        start <- a
        stop <- b
        pure (counter "throttled_usec" (crCpuStat stop) - counter "throttled_usec" (crCpuStat start))

proxyFigures :: ProxyProcess -> ProxyEnd -> Maybe Int -> [Maybe Double] -> [Text] -> ProxyFigures
proxyFigures proxy end throttledUsec inFlight admission =
    ProxyFigures
        { pfIdleRts = proxyIdleRts proxy
        , pfIdleCgroupBytes = proxyIdleCgroupBytes proxy
        , pfCgroup = peCgroup end
        , pfWindowThrottledUsec = throttledUsec
        , pfEnding = peEnding end
        , pfExitedEarly = peExitedEarly end
        , pfStderrTail = if peEnding end == CleanShutdown then "" else peStderrTail end
        , pfBootLines = proxyBootLines proxy
        , pfInFlight = summariseGauge inFlight
        , pfAdmissionSeries = admission
        }

-- The proxy's counters over HTTP, or this process's own for in-process work.
snapshotOf :: Maybe ProxyProcess -> Collection -> IO (Maybe RtsSnapshot)
snapshotOf proxy collection = case proxy of
    Just p -> proxySnapshot p collection
    Nothing -> do
        case collection of
            MajorCollection -> performMajorGC
            MinorCollection -> performMinorGC
        Just <$> readRtsSnapshot

-- Wait up to a minute for the requests the warm-up abandoned at its deadline to leave admission,
-- so they do not spend the window's capacity. A request that never leaves shows in the gauge.
settle :: Maybe ProxyProcess -> IO ()
settle = traverse_ $ \p -> void (pollUntil 300 200_000 (== Just 0) (inFlightNow p))
  where
    inFlightNow p = fmap (fromMaybe 0 . seriesTotal inFlightSeries []) <$> proxyScrape p

cgroupOf :: Maybe ProxyProcess -> IO (Maybe CgroupReading)
cgroupOf = fmap join . traverse proxyCgroupNow

-- Sample the in-flight gauge once a second while the load runs. A failed scrape is a miss, and a
-- scrape with no series yet reads zero, because the gauge appears on its first admission.
sampling :: Maybe ProxyProcess -> IO a -> IO (a, [Maybe Double])
sampling proxy load = case proxy of
    Nothing -> (,[]) <$> load
    Just p -> do
        readings <- newIORef []
        let sampleOnce = do
                scraped <- proxyScrape p
                modifyIORef' readings ((fromMaybe 0 . seriesTotal inFlightSeries [] <$> scraped) :)
        result <- withAsync (forever (sampleOnce >> threadDelay 1_000_000)) (const load)
        (result,) . reverse <$> readIORef readings

inFlightSeries :: Text
inFlightSeries = "ecluse_serve_admission_in_flight"

admissionSeries :: ProxyProcess -> IO [Text]
admissionSeries proxy =
    proxyScrape proxy <&> \case
        Nothing -> ["(the final scrape failed)"]
        Just samples -> map (renderSample (commonLabels samples)) (filter (T.isInfixOf "admission" . sampleName) samples)

-- | Prime duration-driven HTTP loads. Bursts and finite replays meet a cold proxy.
warmUp :: Driver -> IO ()
warmUp = \case
    DriveHttp load -> warm load
    DriveRamp _ load -> warm load
    DriveUnder measured beside -> warm measured >> warm beside
    DriveBurst _ _ -> pass
    DriveReplay _ -> pass
    DriveInProcess _ -> pass
  where
    warm load = void (runOha (OhaRun 8 (ForSeconds 3) (loadHeaders load) (loadUrls load) False))

data DriveOutcome = DriveOutcome
    { doLoad :: LoadSummary
    , doCompanion :: Maybe LoadSummary
    , doSteps :: [LoadSummary]
    , doReplay :: Maybe ReplayTotals
    }

drive :: LoadKnobs -> Driver -> IO DriveOutcome
drive knobs = \case
    DriveHttp load -> alone . summariseOha "" connections <$> runOha (timed connections load)
    DriveBurst count url -> alone . summariseOha "" count <$> runOha (OhaRun count (ForRequests count) [] [url] True)
    DriveRamp steps load -> do
        reports <- traverse (\c -> (c,) <$> runOha (timed c load)) steps
        let merged = foldl' mergeReports (OhaReport 0 mempty mempty []) (map snd reports)
            stepSummaries = [summariseOha (show c <> " connections") c r | (c, r) <- reports]
        pure (DriveOutcome (summariseOha "" (foldl' max 0 steps) merged) Nothing stepSummaries Nothing)
    DriveUnder measured beside -> do
        (m, b) <- concurrently (runOha (timed connections measured)) (runOha (timed connections beside))
        pure (DriveOutcome (summariseOha "measured" connections m) (Just (summariseOha "concurrent load" connections b)) [] Nothing)
    DriveReplay replay -> do
        result <- runReplay replay
        let totals = replayTotals result
            clients = length (rtClients (replayTrace replay))
        pure (DriveOutcome (summariseOha "" clients (replayHttp result)){lsDeadlineAborts = rtotalUnfinished totals} Nothing [] (Just totals))
    DriveInProcess act -> do
        start <- getMonotonicTime
        latencies <- act
        end <- getMonotonicTime
        let completed = length latencies
        pure . alone $
            LoadSummary "" connections (max 1e-9 (end - start)) completed completed 0 0 0 0 (percentiles latencies) "in-process worker loop (no HTTP surface)"
  where
    connections = lkConcurrency knobs
    timed c load = OhaRun c (ForSeconds (lkDurationSeconds knobs)) (loadHeaders load) (loadUrls load) True
    alone summary = DriveOutcome summary Nothing [] Nothing

mergeReports :: OhaReport -> OhaReport -> OhaReport
mergeReports a b =
    OhaReport
        { ohaElapsedSeconds = ohaElapsedSeconds a + ohaElapsedSeconds b
        , ohaStatusCounts = Map.unionWith (+) (ohaStatusCounts a) (ohaStatusCounts b)
        , ohaErrorCounts = Map.unionWith (+) (ohaErrorCounts a) (ohaErrorCounts b)
        , ohaSuccessLatencies = ohaSuccessLatencies a <> ohaSuccessLatencies b
        }

summariseOha :: Text -> Int -> OhaReport -> LoadSummary
summariseOha label connections report =
    LoadSummary
        { lsLabel = label
        , lsConnections = connections
        , lsElapsedSeconds = ohaElapsedSeconds report
        , lsCompleted = completed
        , lsSuccesses = successes
        , lsRefusals = refusals
        , lsOtherStatuses = completed - successes - refusals
        , lsTransportFailures = sum (Map.elems (ohaErrorCounts report)) - aborts
        , lsDeadlineAborts = aborts
        , lsLatency = percentiles (ohaSuccessLatencies report)
        , lsNote = distributionNote report
        }
  where
    statuses = [(readMaybe (toString status) :: Maybe Int, n) | (status, n) <- Map.toList (ohaStatusCounts report)]
    completed = sum (map snd statuses)
    successes = sum [n | (Just status, n) <- statuses, isSuccessStatus status]
    refusals = sum [n | (Just status, n) <- statuses, status == 429 || status == 503]
    aborts = deadlineAbortsOf report

-- oha labels a request abandoned at the run's deadline "aborted due to deadline".
deadlineAbortsOf :: OhaReport -> Int
deadlineAbortsOf report =
    sum [n | (label, n) <- Map.toList (ohaErrorCounts report), "deadline" `T.isInfixOf` T.toLower label]

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

-- | The invariant evidence one report carries: its successes, its OOM kills, and its ending.
reportEvidence :: ScenarioReport -> RunEvidence
reportEvidence r =
    RunEvidence
        { reScenario = srName r <> " (" <> srShape r <> ", " <> show (lsConnections (srLoad r)) <> " connections)"
        , reSuccesses = [(lsLabel l, lsSuccesses l) | l <- srLoad r : maybeToList (srCompanion r)]
        , reOomKills = maybe 0 (counter "oom_kill" . crMemoryEvents) (pfCgroup =<< srProxy r)
        , reEnding = pfEnding <$> srProxy r
        }
