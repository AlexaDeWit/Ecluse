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
import Ecluse.BenchLoad.Floors (
    Enforcement,
    FloorKey,
    Floors (floorsCalibration),
    Pass (ConcurrencyOne, Loaded),
    RunFacts (..),
    enforce,
    loadFloors,
    patternOverridesIn,
    runnerIn,
    triggerIn,
 )
import Ecluse.BenchLoad.Harness (
    LoadKnobs (lkUpstreamLatencyMicros),
    Scenario (scenarioName, scenarioServiceTime),
    ScenarioReport (srName),
    UpstreamFixture (fixtureEcosystem, fixtureScenarios),
    loadKnobsFromEnv,
    operatingPoint,
    reportEvidence,
    runScenario,
 )
import Ecluse.BenchLoad.Normalise (BaselineSource (InjectedFallback, MeasuredRtt))
import Ecluse.BenchLoad.Pod (PodShape (Limited), renderPodShape)
import Ecluse.BenchLoad.ProxyProcess (podShapeFromEnv, serveProxyFlag, sweepProxyCgroups)
import Ecluse.BenchLoad.ProxyServe (runServeProxy)
import Ecluse.BenchLoad.PyPI (pypiLoadNotes)
import Ecluse.BenchLoad.Report (Section (..), renderFloors, renderLoadSaturation, renderReports, renderServiceTime, renderThrash, renderVerdict)
import Ecluse.BenchLoad.RtsProbe (rtsStatsFlag)
import Ecluse.BenchLoad.Scenarios (checkedCounts, findScenario, fixtures, runsUnder)
import Ecluse.BenchLoad.Selection (fixtureBaseline, fixtureSection, scenarioKey)
import Ecluse.BenchLoad.Verdict (RunEvidence, runVerdict)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Test.RegistryCapture (catBenchPins, fetchPackumentBody, loadCatalogue)

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

-- What the passes of every fixture share.
data Setup = Setup
    { setupKnobs :: LoadKnobs
    , setupShape :: PodShape
    , setupSelected :: Maybe [Text]
    , setupEnforcement :: Enforcement
    , setupNpmBaseline :: BaselineSource
    , setupSelf :: FilePath
    , setupCapabilities :: Int
    , setupProcessors :: Int
    }

runPasses :: LoadKnobs -> PodShape -> Maybe [Text] -> IO ([Text], [Text])
runPasses knobs shape selected = do
    -- Read before any scenario runs, so floors that do not decode cost no load.
    floors <- either benchFail pure =<< loadFloors
    environment <- Map.fromList . map (bimap toText toText) <$> getEnvironment
    npmBaseline <- probePublicRtt knobs
    self <- getExecutablePath
    capabilities <- getNumCapabilities
    processors <- getNumProcessors
    let facts =
            RunFacts
                { rfSettings = operatingPoint knobs selected (patternOverridesIn environment)
                , rfRunner = runnerIn environment
                , rfShape = shape
                , rfNpmLatencyMs = baselineInjectedMs (fixtureBaseline Npm (lkUpstreamLatencyMicros knobs) npmBaseline)
                }
        enforcement = enforce floors facts
    ran <- catMaybes <$> traverse (fixturePasses (Setup knobs shape selected enforcement npmBaseline self capabilities processors)) fixtures
    when (null ran) (benchFail "no scenario matched BENCH_LOAD_SCENARIOS under this pod shape")
    pure
        ( map fst ran <> [renderFloors (floorsCalibration floors) enforcement]
        , runVerdict (triggerIn environment) enforcement (checkedCounts shape) (concatMap snd ran)
        )

-- One ecosystem's loaded and concurrency-one passes: its report section, and what each pass of each scenario produced.
fixturePasses :: Setup -> UpstreamFixture -> IO (Maybe (Text, [(FloorKey, Either Text RunEvidence)]))
fixturePasses setup fixture
    | null chosen = pure Nothing
    | otherwise = do
        loaded <- traverse (\s -> (keyOf s,) <$> runScenarioChild self loadOverrides (keyOf s)) chosen
        c1 <- traverse (\s -> (keyOf s,) <$> runScenarioChild self c1Overrides (keyOf s)) serviceTime
        let section =
                Section
                    { sectionKnobs = knobs{lkUpstreamLatencyMicros = injMs * 1_000}
                    , sectionCapabilities = capabilities
                    , sectionProcessors = setupProcessors setup
                    , sectionShape = renderPodShape (setupShape setup)
                    , sectionEcosystem = eco
                    , sectionEnforcement = setupEnforcement setup
                    , sectionConcurrencyOne = rights (map snd c1)
                    , sectionLoaded = rights (map snd loaded)
                    }
            body =
                fixtureSection eco $
                    [pypiLoadNotes knobs | eco == PyPI]
                        <> [ renderReports section
                           , renderServiceTime baseline (sectionConcurrencyOne section)
                           , renderLoadSaturation (sectionConcurrencyOne section) (filter ((`elem` map keyOf serviceTime) . srName) (sectionLoaded section))
                           ]
        pure (Just (body, map (outcomeOf Loaded) loaded <> map (outcomeOf ConcurrencyOne) c1))
  where
    knobs = setupKnobs setup
    self = setupSelf setup
    capabilities = setupCapabilities setup
    eco = fixtureEcosystem fixture
    keyOf = scenarioKey eco . scenarioName
    chosen = filter runsHere (fixtureScenarios fixture)
    serviceTime = filter scenarioServiceTime chosen
    runsHere s = maybe True (keyOf s `elem`) (setupSelected setup) && runsUnder (setupShape setup) s
    baseline = fixtureBaseline eco (lkUpstreamLatencyMicros knobs) (setupNpmBaseline setup)
    injMs = baselineInjectedMs baseline
    loadOverrides = [latencyOverride injMs, childRts capabilities]
    c1Overrides = [latencyOverride injMs, ("BENCH_LOAD_CONCURRENCY", "1"), childRts capabilities]
    outcomeOf whichPass (key, result) = ((key, whichPass), reportEvidence <$> result)

-- Run one scenario at each memory limit. An OOM kill or a heap overflow is the probe's reading,
-- so the probe fails only when no limit produced a report.
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
