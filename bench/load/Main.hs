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
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Core.Registry.PyPI.Wire (SimpleIndex (..), simpleIndexMediaType)
import Ecluse.Test.Corpus (cargoCorpusPackages, cpName, pypiCorpusPackages, rubyGemsCorpusPackages)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumCapabilities)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hContentType, status200)
import System.Environment (getEnvironment, getExecutablePath)
import System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), proc, readProcessStdout, setEnv)
import UnliftIO (bracket_)
import UnliftIO.Exception (tryAny)

import Ecluse.BenchLoad.Cargo (cargoLoadNotes)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Evidence (loadEvidence)
import Ecluse.BenchLoad.Fixture (probeBaseline)
import Ecluse.BenchLoad.Floors (
    Calibration (calConditions, calRetainedConditions),
    Enforcement (NotHeld),
    FloorKey,
    Floors (floorsCalibration),
    Pass (ConcurrencyOne, Loaded),
    PassSelection (EveryPass),
    RunFacts (..),
    Unheld (ThrashProbe),
    enforce,
    loadFloors,
    patternOverridesIn,
    runnerIn,
    triggerIn,
    unheldViolations,
 )
import Ecluse.BenchLoad.Harness (
    LoadKnobs (..),
    Scenario (scenarioName, scenarioServiceTime),
    ScenarioReport (..),
    UpstreamFixture (fixtureEcosystem),
    loadKnobsFromEnv,
    operatingPoint,
    reportEvidence,
    runScenario,
 )
import Ecluse.BenchLoad.Normalise (BaselineSource (InjectedFallback, MeasuredRtt))
import Ecluse.BenchLoad.Oha (ohaPinned)
import Ecluse.BenchLoad.Placement (runnerProcessors, withHarnessPlacement)
import Ecluse.BenchLoad.Pod (PodShape (Limited), ProxyTuning (ptCpus), renderPodShape)
import Ecluse.BenchLoad.Profile (parseLoadProfile, selectProfile)
import Ecluse.BenchLoad.ProxyProcess (podShapeFromEnv, serveProxyFlag, sweepProxyCgroups)
import Ecluse.BenchLoad.ProxyServe (runServeProxy)
import Ecluse.BenchLoad.PyPI (pypiLoadNotes)
import Ecluse.BenchLoad.Report (Section (..), renderFloors, renderLoadSaturation, renderReports, renderServiceTime, renderThrash, renderVerdict)
import Ecluse.BenchLoad.RtsProbe (rtsStatsFlag)
import Ecluse.BenchLoad.RubyGems (rubyGemsLoadNotes)
import Ecluse.BenchLoad.Scenarios (checkFixtureCoverage, checkedCounts, findScenario, fixtures, runsUnder, scenariosForSelection)
import Ecluse.BenchLoad.Selection (fixtureSection, measurementConditionVersion, scenarioKey)
import Ecluse.BenchLoad.Verdict (RunEvidence, runVerdict)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Test.Registry.Cargo.Fixtures (cargoShard)
import Ecluse.Test.Registry.Cargo.Reference (IndexLine (indexValue), referenceIndex)
import Ecluse.Test.Registry.RubyGems.Reference (referenceInfo)
import Ecluse.Test.RegistryCapture (catBenchPins, fetchPackumentBody, loadCatalogue)

-- | Run the passes, serve as a scenario's proxy, or run one ecosystem-qualified child scenario.
main :: IO ()
main =
    getArgs >>= \case
        [] -> runDriver
        [flag] | flag == serveProxyFlag -> runServeProxy
        [name] -> runChild Loaded (toText name)
        ["--service-time", name] -> runChild ConcurrencyOne (toText name)
        _ -> benchFail "usage: bench-load [<ecosystem>/<scenario-name> | --service-time <ecosystem>/<scenario-name> | --serve-proxy]"

-- Proxy cgroups a killed run left behind are retired before and after, so none outlives a run.
runDriver :: IO ()
runDriver = bracket_ sweepProxyCgroups sweepProxyCgroups $ do
    either benchFail pure (checkFixtureCoverage fixtures)
    knobs <- loadKnobsFromEnv
    root <- lookupEnv "BENCH_LOAD_CGROUP"
    pinned <- ohaPinned
    withHarnessPlacement root pinned (ptCpus (lkProxyTuning knobs)) (measureRun knobs)

-- Run the passes or the GC-thrash probe, print the report, and fail on any broken invariant.
measureRun :: LoadKnobs -> IO ()
measureRun knobs = do
    processors <- runnerProcessors
    shape <- podShapeFromEnv
    (profile, selected) <- selectedKeys
    thrash <- thrashLimitsFromEnv
    environment <- Map.fromList . map (bimap toText toText) <$> getEnvironment
    (rendered, violations) <- case thrash of
        -- The probe is never held to the floors, which a scheduled run must be.
        Just limits -> second (unheldViolations (triggerIn environment) (NotHeld (ThrashProbe :| [])) <>) <$> runThrashProbe knobs limits
        Nothing -> runPasses environment knobs processors shape profile selected
    -- The probe reads OOM kills and heap overflows as results, so only a broken probe has a verdict.
    let output = T.intercalate "\n" (["Load profile: `" <> profile <> "`.\n" | isNothing thrash] <> rendered <> [renderVerdict violations | isNothing thrash || not (null violations)])
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
    , setupBaselineOf :: Ecosystem -> BaselineSource
    , setupSelf :: FilePath
    , setupCapabilities :: Int
    , setupProcessors :: Int
    }

runPasses :: Map Text Text -> LoadKnobs -> Int -> PodShape -> Text -> Maybe [Text] -> IO ([Text], [Text])
runPasses environment knobs processors shape profile selected = do
    -- Read before any scenario runs, so floors that do not decode cost no load.
    floors <- either benchFail pure =<< loadFloors
    npmBaseline <- probePublicRtt Npm knobs
    pypiBaseline <- probePublicRtt PyPI knobs
    cargoBaseline <- probePublicRtt Cargo knobs
    rubyGemsBaseline <- probePublicRtt RubyGems knobs
    self <- getExecutablePath
    capabilities <- getNumCapabilities
    -- One source for what each fixture injects, so the latency compared is the latency injected.
    let baselineOf eco = case eco of
            Npm -> npmBaseline
            PyPI -> pypiBaseline
            Cargo -> cargoBaseline
            RubyGems -> rubyGemsBaseline
        facts =
            RunFacts
                { rfSettings = operatingPoint knobs selected (patternOverridesIn environment)
                , rfRunner = runnerIn environment
                , rfShape = shape
                , rfNpmLatencyMs = baselineInjectedMs (baselineOf Npm)
                , rfCargoLatencyMs = Just (baselineInjectedMs (baselineOf Cargo))
                , rfRubyGemsLatencyMs = Just (baselineInjectedMs (baselineOf RubyGems))
                , rfPypiLatencyMs = baselineInjectedMs (baselineOf PyPI)
                , rfConditions = measurementConditionVersion
                }
        enforcement = enforce floors facts
        setup = Setup knobs shape selected enforcement baselineOf self capabilities processors
    ran <- catMaybes <$> traverse (fixturePasses setup) fixtures
    when (null ran) (benchFail "no scenario matched BENCH_LOAD_SCENARIOS under this pod shape")
    let outcomes = concatMap fprOutcomes ran
        violations = runVerdict (triggerIn environment) enforcement (checkedCounts shape) outcomes
    let calibration = floorsCalibration floors
        provenance = Map.fromList [("loadProfile", profile), ("floorMeasurementConditions", calConditions calibration), ("retainedFloorConditions", show (calRetainedConditions calibration))]
    writeLoadObservations (Map.union provenance (measurementConditions setup facts)) enforcement violations outcomes
    pure
        ( map fprBody ran <> [renderFloors (floorsCalibration floors) enforcement]
        , violations
        )

measurementConditions :: Setup -> RunFacts -> Map Text Text
measurementConditions setup facts =
    Map.fromList
        [ ("measurementConditions", measurementConditionVersion)
        , ("npmBaseline", show (setupBaselineOf setup Npm))
        , ("cargoBaseline", show (setupBaselineOf setup Cargo))
        , ("rubygemsBaseline", show (setupBaselineOf setup RubyGems))
        , ("pypiBaseline", show (setupBaselineOf setup PyPI))
        , ("podShape", renderPodShape (setupShape setup))
        , ("operatingPoint", show (rfSettings facts))
        , ("capabilities", show (setupCapabilities setup))
        , ("processors", show (setupProcessors setup))
        , ("npmLatencyMs", show (baselineInjectedMs (setupBaselineOf setup Npm)))
        , ("cargoLatencyMs", show (baselineInjectedMs (setupBaselineOf setup Cargo)))
        , ("rubygemsLatencyMs", show (baselineInjectedMs (setupBaselineOf setup RubyGems)))
        , ("pypiLatencyMs", show (baselineInjectedMs (setupBaselineOf setup PyPI)))
        , ("durationSeconds", show (lkDurationSeconds knobs))
        , ("concurrency", show (lkConcurrency knobs))
        , ("payloadBytes", show (lkPayloadBytes knobs))
        , ("cacheMaxEntries", show (lkCacheMaxEntries knobs))
        , ("workingSet", show (lkWorkingSet knobs))
        , ("serveMaxInFlight", maybe "automatic" show (lkServeMaxInFlight knobs))
        , ("publicConnectionsPerHost", maybe "automatic" show (lkPublicConnectionsPerHost knobs))
        , ("privateConnectionsPerHost", maybe "automatic" show (lkPrivateConnectionsPerHost knobs))
        ]
  where
    knobs = setupKnobs setup

writeLoadObservations :: Map Text Text -> Enforcement -> [Text] -> [(FloorKey, Either Text RunEvidence)] -> IO ()
writeLoadObservations conditions enforcement violations outcomes =
    lookupEnv "ECLUSE_PERFORMANCE_OBSERVATIONS" >>= traverse_ (`writeFileLBS` (encode native <> "\n"))
  where
    native = loadEvidence conditions enforcement violations outcomes

data FixturePassResult = FixturePassResult
    { fprBody :: Text
    , fprOutcomes :: [(FloorKey, Either Text RunEvidence)]
    }

-- One ecosystem's loaded and concurrency-one passes: its report section, and what each pass of each scenario produced.
fixturePasses :: Setup -> UpstreamFixture -> IO (Maybe FixturePassResult)
fixturePasses setup fixture
    | null chosen = pure Nothing
    | otherwise = do
        loaded <- traverse (\s -> (keyOf s,) <$> runScenarioChild Loaded self loadOverrides (keyOf s)) chosen
        c1 <- traverse (\s -> (keyOf s,) <$> runScenarioChild ConcurrencyOne self loadOverrides (keyOf s)) concurrencyOne
        let section =
                Section
                    { sectionKnobs = knobs{lkUpstreamLatencyMicros = injMs * 1_000}
                    , sectionCapabilities = capabilities
                    , sectionProcessors = setupProcessors setup
                    , sectionShape = setupShape setup
                    , sectionEcosystem = eco
                    , sectionEnforcement = setupEnforcement setup
                    , sectionConcurrencyOne = rights (map snd c1)
                    , sectionLoaded = rights (map snd loaded)
                    }
            body =
                fixtureSection eco $
                    [pypiLoadNotes knobs | eco == PyPI]
                        <> [cargoLoadNotes knobs | eco == Cargo]
                        <> [rubyGemsLoadNotes knobs | eco == RubyGems]
                        <> [renderReports section]
                        <> [renderServiceTime baseline (sectionConcurrencyOne section) | everyPass]
                        <> [renderLoadSaturation (sectionConcurrencyOne section) (filter ((`elem` map keyOf serviceTime) . srName) (sectionLoaded section)) | everyPass]
        pure (Just (FixturePassResult body (map (outcomeOf Loaded) loaded <> map (outcomeOf ConcurrencyOne) c1)))
  where
    knobs = setupKnobs setup
    self = setupSelf setup
    capabilities = setupCapabilities setup
    eco = fixtureEcosystem fixture
    keyOf = scenarioKey eco . scenarioName
    chosen = filter runsHere (scenariosForSelection (setupSelected setup) fixture)
    serviceTime = filter scenarioServiceTime chosen
    everyPass = lkPasses knobs == EveryPass
    concurrencyOne = [s | everyPass, s <- serviceTime]
    runsHere s = maybe True (keyOf s `elem`) (setupSelected setup) && runsUnder (setupShape setup) s
    baseline = setupBaselineOf setup eco
    injMs = baselineInjectedMs baseline
    loadOverrides = [latencyOverride injMs, childRts capabilities]
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
    steps <- traverse (\shape -> (renderPodShape shape,) <$> runScenarioChild Loaded self (overrides shape) key) shapes
    pure ([renderThrash key steps], ["the GC-thrash probe produced no report at any memory limit" | null (rights (map snd steps))])

-- The parent's argv RTS flags do not reach a child, so each child gets the statistics flag and the
-- parent's capability count through GHCRTS.
childRts :: Int -> (String, String)
childRts capabilities = ("GHCRTS", rtsStatsFlag <> " -N" <> show capabilities)

-- An explicit selection takes precedence over the profile. Local callers default to full.
selectedKeys :: IO (Text, Maybe [Text])
selectedKeys = do
    profile <- lookupEnv "BENCH_LOAD_PROFILE" >>= either benchFail pure . parseLoadProfile . maybe "full" toText
    explicit <- commaListFromEnv "BENCH_LOAD_SCENARIOS"
    let (name, selected) = selectProfile profile explicit
    (name,) <$> traverse known selected
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
runScenarioChild :: Pass -> FilePath -> [(String, String)] -> Text -> IO (Either Text ScenarioReport)
runScenarioChild whichPass self overrides name = do
    base <- getEnvironment
    let args = ["--service-time" | whichPass == ConcurrencyOne] <> [toString name]
    (code, raw) <- readProcessStdout (setEnv (overrideEnv overrides base) (proc self args))
    pure $ case code of
        ExitFailure n -> Left ("the scenario process exited " <> show n <> ". Its reason is in the job log.")
        ExitSuccess -> first (\err -> "the scenario report did not parse: " <> toText err) (eitherDecode raw)

overrideEnv :: [(String, String)] -> [(String, String)] -> [(String, String)]
overrideEnv overrides base =
    [(k, v) | (k, v) <- base, k `notElem` overriddenKeys] <> overrides
  where
    overriddenKeys = map fst overrides

-- Warm each native listing connection before measuring it. Report failed or disabled probes as fallbacks.
probePublicRtt :: Ecosystem -> LoadKnobs -> IO BaselineSource
probePublicRtt ecosystem knobs = do
    enabled <- probeEnabled
    if not enabled
        then pure (InjectedFallback configured)
        else do
            names <- case ecosystem of
                Npm -> Map.keys . catBenchPins <$> loadCatalogue
                PyPI -> pure (map cpName pypiCorpusPackages)
                Cargo -> pure (map cpName cargoCorpusPackages)
                RubyGems -> pure (map cpName rubyGemsCorpusPackages)
            manager <- newManager tlsManagerSettings
            probeBaseline configured True names (timeFetch manager)
  where
    configured = fromIntegral (lkUpstreamLatencyMicros knobs) / 1_000
    timeFetch :: Manager -> Text -> IO (Maybe Double)
    timeFetch manager name = do
        t0 <- getMonotonicTime
        body <- case ecosystem of
            PyPI -> fetchPublicIndex manager name
            Cargo -> fetchCargoIndex manager name
            RubyGems -> fetchRubyGemsInfo manager name
            _ -> fetchPackumentBody manager ecosystem name
        t1 <- getMonotonicTime
        pure (if isJust body then Just ((t1 - t0) * 1_000) else Nothing)

fetchPublicIndex :: Manager -> Text -> IO (Maybe LByteString)
fetchPublicIndex manager name = do
    result <- tryAny $ do
        request <- HTTP.parseRequest (toString ("https://pypi.org/simple/" <> name <> "/"))
        response <- HTTP.httpLbs request{HTTP.requestHeaders = [("Accept", simpleIndexMediaType)], HTTP.responseTimeout = HTTP.responseTimeoutMicro (30 * 1_000 * 1_000)} manager
        let bytes = HTTP.responseBody response
            native = case eitherDecode bytes of
                Right index -> siName index == name && not (null (siFiles index)) && null (siInvalidEntries index)
                Left _ -> False
        pure [bytes | HTTP.responseStatus response == status200, List.lookup hContentType (HTTP.responseHeaders response) == Just simpleIndexMediaType, native]
    pure (either (const Nothing) listToMaybe result)

fetchCargoIndex :: Manager -> Text -> IO (Maybe LByteString)
fetchCargoIndex = fetchNativeListing (\name -> "https://index.crates.io/" <> cargoShard name) valid
  where
    valid bytes = let rows = referenceIndex bytes in not (null rows) && all (isRight . indexValue) rows

fetchRubyGemsInfo :: Manager -> Text -> IO (Maybe LByteString)
fetchRubyGemsInfo = fetchNativeListing ("https://rubygems.org/info/" <>) (either (const False) (not . null) . referenceInfo)

fetchNativeListing :: (Text -> Text) -> (ByteString -> Bool) -> Manager -> Text -> IO (Maybe LByteString)
fetchNativeListing location native manager name = do
    result <- tryAny $ do
        request <- HTTP.parseRequest (toString (location name))
        response <- HTTP.httpLbs request{HTTP.requestHeaders = [("Accept", "text/plain")], HTTP.responseTimeout = HTTP.responseTimeoutMicro (30 * 1_000 * 1_000)} manager
        let bytes = HTTP.responseBody response
        pure [bytes | HTTP.responseStatus response == status200, native (toStrict bytes)]
    pure (either (const Nothing) listToMaybe result)

probeEnabled :: IO Bool
probeEnabled = maybe True ((`notElem` ["0", "false", "no", "off"]) . map toLower) <$> lookupEnv "BENCH_LOAD_PROBE_RTT"

runChild :: Pass -> Text -> IO ()
runChild whichPass name = do
    knobs <- loadKnobsFromEnv
    s <- maybe (benchFail ("unknown scenario: " <> name)) pure (findScenario name)
    report <- runScenario whichPass knobs s{scenarioName = name}
    LBSC.putStrLn (encode report)
