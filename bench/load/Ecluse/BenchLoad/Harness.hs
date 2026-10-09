-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}

{- | Load settings, the fixture interface, and the measurement of one scenario. Each scenario runs
in a child process against its own proxy process, so every figure belongs to that scenario alone.
Latency and memory are informational. "Ecluse.BenchLoad.Verdict" names what fails a run.
-}
module Ecluse.BenchLoad.Harness (
    -- * Load knobs
    LoadKnobs (..),
    defaultLoadKnobs,
    loadKnobsFromEnv,
    operatingPoint,

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
    windowSuccesses,
    windowAttempts,
    windowRefusals,
    reportEvidence,
) where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import UnliftIO.Async (concurrently, withAsync)

import Ecluse.BenchLoad.Exposition (
    CacheOutcomes,
    GaugeSummary,
    Sample (sampleName),
    cacheWindow,
    commonLabels,
    renderSample,
    ruleFailuresWindow,
    seriesTotal,
    summariseGauge,
 )
import Ecluse.BenchLoad.Floors (OperatingPoint (..))
import Ecluse.BenchLoad.Latency (Percentiles, isSuccessStatus, percentiles)
import Ecluse.BenchLoad.Oha (OhaReport (..), OhaRun (..), RunLength (ForRequests, ForSeconds), runOha)
import Ecluse.BenchLoad.PatternReport (ReplayTotals (..))
import Ecluse.BenchLoad.Patterns (RequestTrace (rtClients))
import Ecluse.BenchLoad.Pod (CgroupReading (..), counter, renderPodShape)
import Ecluse.BenchLoad.ProxyProcess (
    AdvisoryFeed,
    ProxyEnd (..),
    ProxyProcess,
    podShapeFromEnv,
    proxyBootLines,
    proxyBootRetries,
    proxyCgroupNow,
    proxyIdleCgroupBytes,
    proxyIdleRts,
    proxyRuleLines,
    proxyScrape,
    proxySnapshot,
    proxyTasksNow,
    stopProxy,
 )
import Ecluse.BenchLoad.Replay (Replay (..), ReplayReport (..), runReplay)
import Ecluse.BenchLoad.RtsProbe (requireRtsStats, snapshotAfter)
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
    , lkAdvisories :: Maybe AdvisoryFeed
    -- ^ The advisory store the proxy syncs from. 'Nothing' configures none, as the shipped default does.
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
        , lkAdvisories = Nothing
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
            , lkAdvisories = Nothing
            }
  where
    readEnvInt :: String -> Int -> IO Int
    readEnvInt name fallback = maybe fallback (fromMaybe fallback . readMaybe) <$> lookupEnv name

-- | The operating point of a run with these knobs, scenarios ('Nothing' is all of them), and pattern overrides.
operatingPoint :: LoadKnobs -> Maybe [Text] -> [Text] -> OperatingPoint
operatingPoint knobs selected patternOverrides =
    OperatingPoint
        { opDurationSeconds = lkDurationSeconds knobs
        , opConcurrency = lkConcurrency knobs
        , opPayloadBytes = lkPayloadBytes knobs
        , opUpstreamLatencyMs = lkUpstreamLatencyMicros knobs `div` 1_000
        , opCacheMaxEntries = lkCacheMaxEntries knobs
        , opWorkingSet = lkWorkingSet knobs
        , opServeMaxInFlight = lkServeMaxInFlight knobs
        , opPublicConnectionsPerHost = lkPublicConnectionsPerHost knobs
        , opPrivateConnectionsPerHost = lkPrivateConnectionsPerHost knobs
        , opScenarios = selected
        , opPatternOverrides = patternOverrides
        }

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
    , pfWindowCgroup :: Maybe CgroupReading
    -- ^ Read as the window closed, while @memory.stat@ still shows what the proxy held.
    , pfWindowThrottledUsec :: Maybe Int
    -- ^ CPU time the quota withheld during the window.
    , pfEnding :: ProxyEnding
    , pfExitedEarly :: Bool
    , pfStderrTail :: Text
    -- ^ Kept only for an ending other than a clean shutdown.
    , pfBootLines :: [Text]
    , pfRuleLines :: [Text]
    -- ^ The rule configuration and rule boot order the proxy logged.
    , pfBootRetries :: [Text]
    -- ^ Failed boots the harness retried, each with its diagnostic.
    , pfInFlight :: GaugeSummary
    -- ^ @ecluse.serve.admission.in_flight@ sampled each second of the window.
    , pfTasks :: GaugeSummary
    -- ^ The proxy cgroup's @pids.current@, its thread count, sampled each second of the window.
    , pfAdmissionSeries :: [Text]
    -- ^ Every admission series at the end of the window.
    , pfCacheWindow :: Maybe [(Text, CacheOutcomes)]
    -- ^ Each metadata cache store's outcomes in the window. 'Nothing' when a scrape failed.
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
    -- ^ One summary per ramp step. 'srLoad' is then the last, highest step.
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
    -- ^ What a replay observed, and any rule failures in the window.
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Measure one fixture. Missing RTS counters fail the harness.
runScenario :: LoadKnobs -> Scenario -> IO ScenarioReport
runScenario knobs s = do
    requireRtsStats "the scenario"
    shape <- podShapeFromEnv
    let scaled = knobs{lkConcurrency = lkConcurrency knobs * max 1 (scenarioConcurrencyScale s)}
    scenarioBoot s scaled (measure scaled s (renderPodShape shape))

measure :: LoadKnobs -> Scenario -> Text -> Target -> IO ScenarioReport
measure knobs s shape (Target proxy driver) = do
    warmUp driver
    settle proxy
    -- This scrape and the one after the window closes sit outside the RTS window, so their own
    -- allocation is not charged to it.
    startScrape <- scrapeOf proxy
    before <- snapshotOf proxy MajorCollection
    cgroupBefore <- cgroupOf proxy
    (outcome, (inFlight, tasks)) <- sampling proxy (drive knobs driver)
    after <- snapshotOf proxy MinorCollection
    cgroupAfter <- cgroupOf proxy
    endScrape <- scrapeOf proxy
    replayed <- case driver of
        DriveReplay replay -> replayEvidence replay
        _ -> pure ""
    let evidence = T.intercalate "\n\n" (filter (not . T.null) [replayed, maybe "" ruleFailureNote (ruleFailuresWindow <$> startScrape <*> endScrape)])
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
            , srProxy = (\(p, end) -> proxyFigures p end cgroupAfter (throttled cgroupBefore cgroupAfter) (inFlight, tasks) (startScrape, endScrape)) <$> ends
            , srEvidence = evidence
            }
  where
    throttled a b = do
        start <- a
        stop <- b
        pure (counter "throttled_usec" (crCpuStat stop) - counter "throttled_usec" (crCpuStat start))

proxyFigures :: ProxyProcess -> ProxyEnd -> Maybe CgroupReading -> Maybe Int -> ([Maybe Double], [Maybe Double]) -> (Maybe [Sample], Maybe [Sample]) -> ProxyFigures
proxyFigures proxy end windowCgroup throttledUsec (inFlight, tasks) (startScrape, endScrape) =
    ProxyFigures
        { pfIdleRts = proxyIdleRts proxy
        , pfIdleCgroupBytes = proxyIdleCgroupBytes proxy
        , pfCgroup = peCgroup end
        , pfWindowCgroup = windowCgroup
        , pfWindowThrottledUsec = throttledUsec
        , pfEnding = peEnding end
        , pfExitedEarly = peExitedEarly end
        , pfStderrTail = if peEnding end == CleanShutdown then "" else peStderrTail end
        , pfBootLines = proxyBootLines proxy
        , pfRuleLines = proxyRuleLines proxy
        , pfBootRetries = proxyBootRetries proxy
        , pfInFlight = summariseGauge inFlight
        , pfTasks = summariseGauge tasks
        , pfAdmissionSeries = maybe ["(the final scrape failed)"] admissionSeries endScrape
        , pfCacheWindow = cacheWindow <$> startScrape <*> endScrape
        }

-- A fail-closed rule answers an undecidable version with 503, which the refusals then include.
ruleFailureNote :: Int -> Text
ruleFailureNote failures
    | failures > 0 = "**Rule failures in the window: " <> show failures <> " undecidable version decisions** (`ecluse.rule.effectful.failures`). A fail-closed rule answers them with 503, which the refusals include."
    | otherwise = ""

-- The proxy's counters over HTTP, or this process's own for in-process work.
snapshotOf :: Maybe ProxyProcess -> Collection -> IO (Maybe RtsSnapshot)
snapshotOf proxy collection = case proxy of
    Just p -> proxySnapshot p collection
    Nothing -> Just <$> snapshotAfter collection

-- Wait up to a minute for the requests the warm-up abandoned at its deadline to leave admission,
-- so they do not spend the window's capacity. A request that never leaves shows in the gauge.
settle :: Maybe ProxyProcess -> IO ()
settle = traverse_ $ \p -> void (pollUntil 300 200_000 (== Just 0) (inFlightNow p))
  where
    inFlightNow p = fmap (fromMaybe 0 . seriesTotal inFlightSeries []) <$> proxyScrape p

cgroupOf :: Maybe ProxyProcess -> IO (Maybe CgroupReading)
cgroupOf = fmap join . traverse proxyCgroupNow

scrapeOf :: Maybe ProxyProcess -> IO (Maybe [Sample])
scrapeOf = fmap join . traverse proxyScrape

-- Sample the in-flight gauge and thread count each second. A failed scrape is a miss, and a gauge
-- not yet created reads zero, since it appears on the first admission.
sampling :: Maybe ProxyProcess -> IO a -> IO (a, ([Maybe Double], [Maybe Double]))
sampling proxy load = case proxy of
    Nothing -> (,([], [])) <$> load
    Just p -> do
        readings <- newIORef []
        let sampleOnce = do
                scraped <- proxyScrape p
                tasks <- proxyTasksNow p
                modifyIORef' readings ((fromMaybe 0 . seriesTotal inFlightSeries [] <$> scraped, fromIntegral <$> tasks) :)
        result <- withAsync (forever (sampleOnce >> threadDelay 1_000_000)) (const load)
        (result,) . unzip . reverse <$> readIORef readings

inFlightSeries :: Text
inFlightSeries = "ecluse_serve_admission_in_flight"

admissionSeries :: [Sample] -> [Text]
admissionSeries samples = map (renderSample (commonLabels samples)) (filter (T.isInfixOf "admission" . sampleName) samples)

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
        let stepSummaries = [summariseOha (show c <> " connections") c r | (c, r) <- reports]
            highest = fromMaybe (summariseOha "" 0 (OhaReport 0 mempty mempty [])) (listToMaybe (reverse stepSummaries))
        pure (DriveOutcome highest Nothing stepSummaries Nothing)
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

-- Every load the RTS window spans: each ramp step, or the measured load and its companion.
windowLoads :: ScenarioReport -> [LoadSummary]
windowLoads r = case srSteps r of
    [] -> srLoad r : maybeToList (srCompanion r)
    steps -> steps

-- | Successful responses across every load the RTS window spans, the divisor for its totals.
windowSuccesses :: ScenarioReport -> Int
windowSuccesses = sum . map lsSuccesses . windowLoads

-- | Completed responses and transport failures across every load the RTS window spans.
windowAttempts :: ScenarioReport -> Int
windowAttempts = sum . map (\l -> lsCompleted l + lsTransportFailures l) . windowLoads

-- | Refusals across every load the RTS window spans.
windowRefusals :: ScenarioReport -> Int
windowRefusals = sum . map lsRefusals . windowLoads

-- | The invariant evidence one report carries: successes per load or step, OOM kills, and the ending.
reportEvidence :: ScenarioReport -> RunEvidence
reportEvidence r =
    RunEvidence
        { reScenario = srName r <> " (" <> srShape r <> ", " <> show (lsConnections (srLoad r)) <> " connections)"
        , reSuccesses = [(lsLabel l, lsSuccesses l) | l <- windowLoads r]
        , reOomKills = maybe 0 (counter "oom_kill" . crMemoryEvents) (pfCgroup =<< srProxy r)
        , reEnding = pfEnding <$> srProxy r
        , reExitedEarly = maybe False pfExitedEarly (srProxy r)
        }
