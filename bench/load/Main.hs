-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Run isolated load and concurrency-one passes for each ecosystem fixture.
Each report keeps its own baseline, throughput, service-time, and saturation sections.
A child prints one JSON report, while the driver writes the combined Markdown artifact.
-}
module Main (main) where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumCapabilities)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnvironment, getExecutablePath)
import System.Process.Typed (proc, readProcessStdout_, setEnv)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Harness (
    LoadKnobs (lkUpstreamLatencyMicros),
    Scenario (scenarioName),
    ScenarioReport,
    UpstreamFixture (fixtureEcosystem, fixtureScenarios),
    loadKnobsFromEnv,
    renderLoadSaturation,
    renderReports,
    renderServiceTime,
    runScenario,
 )
import Ecluse.BenchLoad.Normalise (BaselineSource (InjectedFallback, MeasuredRtt))
import Ecluse.BenchLoad.Npm (npmFixture)
import Ecluse.BenchLoad.PyPI (pypiFixture, pypiLoadNotes)
import Ecluse.BenchLoad.Selection (fixtureBaseline, fixtureSection, scenarioKey, selectScenario)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Test.RegistryCapture (catBenchPins, fetchPackumentBody, loadCatalogue)

fixtures :: [UpstreamFixture]
fixtures = [npmFixture, pypiFixture]

-- | Run both fixture passes, or one ecosystem-qualified child scenario.
main :: IO ()
main =
    getArgs >>= \case
        [] -> runDriver
        [name] -> runChild (toText name)
        _ -> benchFail "usage: bench-load [<ecosystem>/<scenario-name>]"

runDriver :: IO ()
runDriver = do
    knobs <- loadKnobsFromEnv
    npmBaseline <- probePublicRtt knobs
    self <- getExecutablePath
    -- The parent consumes argv RTS flags. Children need the same capability count through GHCRTS.
    capabilities <- getNumCapabilities
    rendered <- forM fixtures $ \fixture -> do
        let eco = fixtureEcosystem fixture
            names = map (scenarioKey eco . scenarioName) (fixtureScenarios fixture)
            baseline = fixtureBaseline eco (lkUpstreamLatencyMicros knobs) npmBaseline
            pinChildren = ("GHCRTS", "-N" <> show capabilities)
            injMs = baselineInjectedMs baseline
            loadOverrides = [latencyOverride injMs, pinChildren]
            c1Overrides = [latencyOverride injMs, ("BENCH_LOAD_CONCURRENCY", "1"), pinChildren]
            loadPassKnobs = knobs{lkUpstreamLatencyMicros = injMs * 1_000}
            notes = case eco of
                PyPI -> [pypiLoadNotes knobs]
                _ -> []
        loadedReports <- traverse (runScenarioChild self loadOverrides) names
        c1Reports <- traverse (runScenarioChild self c1Overrides) names
        pure $
            fixtureSection eco $
                notes
                    <> [ renderReports loadPassKnobs capabilities eco loadedReports
                       , renderServiceTime baseline c1Reports
                       , renderLoadSaturation c1Reports loadedReports
                       ]
    let output = T.intercalate "\n" rendered
    putText output
    lookupEnv "GITHUB_STEP_SUMMARY" >>= traverse_ (`appendFileText` output)

latencyOverride :: Int -> (String, String)
latencyOverride injMs = ("BENCH_LOAD_UPSTREAM_LATENCY_MS", show injMs)

baselineInjectedMs :: BaselineSource -> Int
baselineInjectedMs = \case
    MeasuredRtt rtt _ -> round rtt
    InjectedFallback ms -> round ms

runScenarioChild :: FilePath -> [(String, String)] -> Text -> IO ScenarioReport
runScenarioChild self overrides name = do
    base <- getEnvironment
    raw <- readProcessStdout_ (setEnv (overrideEnv overrides base) (proc self [toString name]))
    either (\err -> benchFail ("bench-load child " <> name <> " report did not parse: " <> toText err)) pure (eitherDecode raw)

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
    scenario <- maybe (benchFail ("unknown scenario: " <> name)) pure (findScenario name)
    report <- runScenario knobs scenario{scenarioName = name}
    LBSC.putStrLn (encode report)

findScenario :: Text -> Maybe Scenario
findScenario name =
    selectScenario
        name
        [ (fixtureEcosystem fixture, [(scenarioName scenario, scenario) | scenario <- fixtureScenarios fixture])
        | fixture <- fixtures
        ]
