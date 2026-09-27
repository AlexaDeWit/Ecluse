-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Run the loaded and concurrency-one passes for each ecosystem fixture under one pod shape, or
the GC-thrash probe across a series of memory limits. Each scenario runs in a child process that
prints one JSON report and boots its own proxy process. The driver renders the Markdown artifact,
then fails the run if any invariant broke.
-}
module Main (main) where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumCapabilities, getNumProcessors)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnvironment, getExecutablePath)
import System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), proc, readProcessStdout, setEnv)
import UnliftIO (bracket_)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Harness (
    LoadKnobs (lkUpstreamLatencyMicros),
    Scenario (scenarioInProcess, scenarioName, scenarioServiceTime),
    ScenarioReport (srName),
    UpstreamFixture (fixtureEcosystem, fixtureScenarios),
    loadKnobsFromEnv,
    reportEvidence,
    runScenario,
 )
import Ecluse.BenchLoad.Normalise (BaselineSource (InjectedFallback, MeasuredRtt))
import Ecluse.BenchLoad.Npm (npmFixture)
import Ecluse.BenchLoad.Pod (PodShape (Limited, Unlimited), renderPodShape)
import Ecluse.BenchLoad.ProxyProcess (podShapeFromEnv, serveProxyFlag, sweepProxyCgroups)
import Ecluse.BenchLoad.ProxyServe (runServeProxy)
import Ecluse.BenchLoad.PyPI (pypiFixture, pypiLoadNotes)
import Ecluse.BenchLoad.Report (renderLoadSaturation, renderReports, renderServiceTime, renderThrash, renderVerdict)
import Ecluse.BenchLoad.RtsProbe (rtsStatsFlag)
import Ecluse.BenchLoad.Selection (fixtureBaseline, fixtureSection, scenarioKey, selectScenario)
import Ecluse.BenchLoad.Verdict (runViolations)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Test.RegistryCapture (catBenchPins, fetchPackumentBody, loadCatalogue)

fixtures :: [UpstreamFixture]
fixtures = [npmFixture, pypiFixture]

-- | Run the passes, serve as a scenario's proxy, or run one ecosystem-qualified child scenario.
main :: IO ()
main =
    getArgs >>= \case
        [] -> runDriver
        [flag] | flag == serveProxyFlag -> runServeProxy
        [name] -> runChild (toText name)
        _ -> benchFail "usage: bench-load [<ecosystem>/<scenario-name> | --serve-proxy]"

-- Proxy cgroups a killed run left behind are retired before and after, so none outlives a run.
runDriver :: IO ()
runDriver = bracket_ sweepProxyCgroups sweepProxyCgroups $ do
    knobs <- loadKnobsFromEnv
    shape <- podShapeFromEnv
    selected <- selectedKeys
    thrash <- thrashLimitsFromEnv
    (rendered, violations) <- case thrash of
        Just limits -> runThrashProbe knobs limits
        Nothing -> runPasses knobs shape selected
    -- The probe reads OOM kills and heap overflows as results, so only a broken probe has a verdict.
    let output = T.intercalate "\n" (rendered <> [renderVerdict violations | isNothing thrash || not (null violations)])
    putText output
    lookupEnv "GITHUB_STEP_SUMMARY" >>= traverse_ (`appendFileText` output)
    unless (null violations) $
        benchFail ("the load run broke " <> show (length violations) <> " invariant(s). The verdict section lists them.")

runPasses :: LoadKnobs -> PodShape -> Maybe [Text] -> IO ([Text], [Text])
runPasses knobs shape selected = do
    npmBaseline <- probePublicRtt knobs
    self <- getExecutablePath
    capabilities <- getNumCapabilities
    processors <- getNumProcessors
    sections <- forM fixtures $ \fixture -> do
        let eco = fixtureEcosystem fixture
            chosen = filter (runsHere eco) (fixtureScenarios fixture)
            baseline = fixtureBaseline eco (lkUpstreamLatencyMicros knobs) npmBaseline
            injMs = baselineInjectedMs baseline
            pinChildren = childRts capabilities
            loadOverrides = [latencyOverride injMs, pinChildren]
            c1Overrides = [latencyOverride injMs, ("BENCH_LOAD_CONCURRENCY", "1"), pinChildren]
            loadPassKnobs = knobs{lkUpstreamLatencyMicros = injMs * 1_000}
            notes = [pypiLoadNotes knobs | eco == PyPI]
            keyOf = scenarioKey eco . scenarioName
        if null chosen
            then pure Nothing
            else do
                loaded <- traverse (\s -> (keyOf s,) <$> runScenarioChild self loadOverrides (keyOf s)) chosen
                c1 <- traverse (\s -> (keyOf s,) <$> runScenarioChild self c1Overrides (keyOf s)) (filter scenarioServiceTime chosen)
                let loadedReports = rights (map snd loaded)
                    c1Reports = rights (map snd c1)
                    serviceTimeKeys = map keyOf (filter scenarioServiceTime chosen)
                    violations = concatMap (either pure (runViolations . reportEvidence)) (map labelled loaded <> map labelled c1)
                    body =
                        fixtureSection eco $
                            notes
                                <> [ renderReports loadPassKnobs capabilities processors (renderPodShape shape) eco loadedReports
                                   , renderServiceTime baseline c1Reports
                                   , renderLoadSaturation c1Reports (filter ((`elem` serviceTimeKeys) . srName) loadedReports)
                                   ]
                pure (Just (body, violations))
    let ran = catMaybes sections
    when (null ran) (benchFail "no scenario matched BENCH_LOAD_SCENARIOS under this pod shape")
    pure (map fst ran, concatMap snd ran)
  where
    runsHere eco s =
        maybe True (scenarioKey eco (scenarioName s) `elem`) selected
            && (shape == Unlimited || not (scenarioInProcess s))
    labelled (key, result) = first (\failure -> key <> ": " <> failure) result

{- | Run one scenario at each memory limit. An OOM kill or a heap overflow is the probe's reading,
so the probe fails only when no limit produced a report.
-}
runThrashProbe :: LoadKnobs -> [Int] -> IO ([Text], [Text])
runThrashProbe knobs limitsMib = do
    self <- getExecutablePath
    capabilities <- getNumCapabilities
    cpus <- maybe 2 (fromMaybe 2 . readMaybe) <$> lookupEnv "BENCH_LOAD_THRASH_CPUS"
    key <- maybe "npm/heavy-private" toText <$> lookupEnv "BENCH_LOAD_THRASH_SCENARIO"
    when (isNothing (findScenario key)) (benchFail ("BENCH_LOAD_THRASH_SCENARIO names no scenario: " <> key))
    let shapes = [Limited (max 1 cpus) (mib * 1024 * 1024) | mib <- limitsMib]
        overrides shape =
            [ ("BENCH_LOAD_POD", toString (renderPodShape shape))
            , childRts capabilities
            , latencyOverride (lkUpstreamLatencyMicros knobs `div` 1_000)
            ]
    steps <- traverse (\shape -> (renderPodShape shape,) <$> runScenarioChild self (overrides shape) key) shapes
    pure ([renderThrash key steps], ["the GC-thrash probe produced no report at any memory limit" | null (rights (map snd steps))])

-- The parent's argv RTS flags do not reach a child, so each child gets the statistics flag and the
-- parent's capability count through GHCRTS.
childRts :: Int -> (String, String)
childRts capabilities = ("GHCRTS", rtsStatsFlag <> " -N" <> show capabilities)

-- The scenario keys in BENCH_LOAD_SCENARIOS, or every scenario when it is unset or blank.
selectedKeys :: IO (Maybe [Text])
selectedKeys =
    commaListFromEnv "BENCH_LOAD_SCENARIOS" >>= traverse known
  where
    known keys = do
        let unknown = filter (isNothing . findScenario) keys
        unless (null unknown) (benchFail ("BENCH_LOAD_SCENARIOS names unknown scenarios: " <> T.intercalate ", " unknown))
        pure keys

-- The memory limits in MiB for the thrash probe, highest first, or 'Nothing' for the normal passes.
thrashLimitsFromEnv :: IO (Maybe [Int])
thrashLimitsFromEnv =
    commaListFromEnv "BENCH_LOAD_THRASH_LIMITS_MIB" >>= traverse positive
  where
    positive parts =
        maybe
            (benchFail ("BENCH_LOAD_THRASH_LIMITS_MIB must list positive MiB counts: " <> T.intercalate "," parts))
            pure
            (traverse (mfilter (> 0) . readMaybe . toString) parts)

-- A comma-separated variable's non-blank items, 'Nothing' when it is unset or holds none.
commaListFromEnv :: String -> IO (Maybe [Text])
commaListFromEnv name = do
    raw <- lookupEnv name
    pure $ case filter (not . T.null) (map T.strip (T.splitOn "," (maybe "" toText raw))) of
        [] -> Nothing
        items -> Just items

latencyOverride :: Int -> (String, String)
latencyOverride injMs = ("BENCH_LOAD_UPSTREAM_LATENCY_MS", show injMs)

baselineInjectedMs :: BaselineSource -> Int
baselineInjectedMs = \case
    MeasuredRtt rtt _ -> round rtt
    InjectedFallback ms -> round ms

-- A child that fails leaves its reason on the inherited stderr, and the run carries on.
runScenarioChild :: FilePath -> [(String, String)] -> Text -> IO (Either Text ScenarioReport)
runScenarioChild self overrides name = do
    base <- getEnvironment
    (code, raw) <- readProcessStdout (setEnv (overrideEnv overrides base) (proc self [toString name]))
    pure $ case code of
        ExitFailure n -> Left ("the scenario process exited " <> show n <> ". Its reason is in the job log.")
        ExitSuccess -> first (\err -> "the scenario report did not parse: " <> toText err) (eitherDecode raw)

overrideEnv :: [(String, String)] -> [(String, String)] -> [(String, String)]
overrideEnv overrides base =
    [(k, v) | (k, v) <- base, k `notElem` overriddenKeys] <> overrides
  where
    overriddenKeys = map fst overrides

-- Warm the connection before timing registry fetches. Failed or disabled probes use the configured latency.
probePublicRtt :: LoadKnobs -> IO BaselineSource
probePublicRtt knobs = do
    enabled <- probeEnabled
    if not enabled
        then pure fallback
        else do
            catalogue <- loadCatalogue
            manager <- newManager tlsManagerSettings
            case Map.keys (catBenchPins catalogue) of
                [] -> pure fallback
                names@(warm : _) -> do
                    _ <- fetchPackumentBody manager Npm warm
                    samples <- catMaybes <$> traverse (timeFetch manager) names
                    pure $ case samples of
                        [] -> fallback
                        _ -> MeasuredRtt (meanMs samples) (length samples)
  where
    fallback = InjectedFallback (fromIntegral (lkUpstreamLatencyMicros knobs) / 1_000)

    timeFetch :: Manager -> Text -> IO (Maybe Double)
    timeFetch manager name = do
        t0 <- getMonotonicTime
        mBody <- fetchPackumentBody manager Npm name
        t1 <- getMonotonicTime
        pure (if isJust mBody then Just ((t1 - t0) * 1_000) else Nothing)

    meanMs :: [Double] -> Double
    meanMs samples = fromIntegral (round (sum samples / fromIntegral (length samples)) :: Int)

probeEnabled :: IO Bool
probeEnabled = maybe True ((`notElem` ["0", "false", "no", "off"]) . map toLower) <$> lookupEnv "BENCH_LOAD_PROBE_RTT"

runChild :: Text -> IO ()
runChild name = do
    knobs <- loadKnobsFromEnv
    s <- maybe (benchFail ("unknown scenario: " <> name)) pure (findScenario name)
    report <- runScenario knobs s{scenarioName = name}
    LBSC.putStrLn (encode report)

findScenario :: Text -> Maybe Scenario
findScenario name =
    selectScenario
        name
        [ (fixtureEcosystem fixture, [(scenarioName s, s) | s <- fixtureScenarios fixture])
        | fixture <- fixtures
        ]
