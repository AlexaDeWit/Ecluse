-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @bench-load@ entry point: the load benchmarks tier of the benchmark strategy,
throughput and latency under concurrent load against the real composed proxy.

Unlike the work-per-request micro-benches (@ecluse-bench@, @tasty-bench@), this boots the
real 'Ecluse.Server.application' on @warp@ over stub upstreams and drives it with @oha@.
It therefore measures system behaviour (saturation, latency tails, GC pauses) rather than
a pure function's cost. It is __inform-only__ and __never gates__. A human reads and
trends the figures. The only red state is a literal failure: the harness cannot boot,
@oha@ cannot run, or a scenario served nothing. See
@docs\/architecture\/performance.md@.

== Two passes, one baseline

The driver first probes the live public registry for the corpus packages and takes the
mean round trip as the upstream baseline (measure-then-seed). It then drives __two__
passes over every scenario, both with that round trip injected as the stub upstreams'
latency:

  * a __concurrency-1 service pass__, where no request queues, so a measured latency is
    the upstream baseline plus Écluse's per-request overhead: the service-time
    attribution.

  * the __loaded pass__ at the configured concurrency, whose p50 above the service p50 is
    the queuing delay: the load-saturation signal.

The probe is non-gating. The configured injected latency stands in, labelled as a
fallback, when the public registry is unreachable or @BENCH_LOAD_PROBE_RTT=0@ switches the
probe off. Both passes still run.

== Per-scenario process isolation

Peak residency is a process-wide RTS high-water mark, so each scenario runs in its __own
process__ to keep its residency its own. With no argument this binary is the __driver__.
It re-execs itself once per scenario per pass (@bench-load <scenario>@) and collects each
child's machine-readable report. It renders the combined tables to stdout and the GitHub
run summary. With a scenario name it is a __child__: it runs that one scenario and prints
its report as a single JSON line. Both read the load knobs from the environment, so a
child inherits the driver's operating point. It also inherits the per-pass overrides the
driver layers onto its environment: the seeded latency, and concurrency 1 for the service
pass.
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
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Test.RegistryCapture (catBenchPins, fetchPackumentBody, loadCatalogue)

-- | The fixtures driven, one per upstream ecosystem. npm is the only instance today.
fixtures :: [UpstreamFixture]
fixtures = [npmFixture]

main :: IO ()
main =
    getArgs >>= \case
        [] -> runDriver
        [name] -> runChild (toText name)
        _ -> benchFail "usage: bench-load [<scenario-name>]"

-- The driver: probe the public-registry round trip, then for each fixture's scenarios
-- drive the loaded pass and the concurrency-1 service pass. Each scenario runs in its own
-- child process, and both passes inject the probed round trip. The driver renders the
-- per-scenario table, the service-time attribution, and the load-saturation flag to
-- stdout and (when set) the GitHub run summary.
runDriver :: IO ()
runDriver = do
    knobs <- loadKnobsFromEnv
    baseline <- probePublicRtt knobs
    self <- getExecutablePath
    -- Scenario children must run with the driver's capability count. A command-line
    -- @+RTS -N3@ does not survive the re-exec, because the parent's runtime consumes the
    -- argv RTS flags. Without this the children fall back to the baked bare @-N@, claim
    -- every core, and overlap the core the harness pins oha to. The child's RTS reads
    -- GHCRTS at startup, so the driver's count propagates.
    capabilities <- getNumCapabilities
    let pinChildren = ("GHCRTS", "-N" <> show capabilities)
        injMs = baselineInjectedMs baseline
        loadOverrides = [latencyOverride injMs, pinChildren]
        c1Overrides = [latencyOverride injMs, ("BENCH_LOAD_CONCURRENCY", "1"), pinChildren]
        loadPassKnobs = knobs{lkUpstreamLatencyMicros = injMs * 1_000}
    rendered <- forM fixtures $ \fixture -> do
        let names = map scenarioName (fixtureScenarios fixture)
            eco = fixtureEcosystem fixture
        loadedReports <- traverse (runScenarioChild self loadOverrides) names
        c1Reports <- traverse (runScenarioChild self c1Overrides) names
        pure $
            T.intercalate
                "\n"
                [ renderReports loadPassKnobs capabilities eco loadedReports
                , renderServiceTime baseline c1Reports
                , renderLoadSaturation c1Reports loadedReports
                ]
    let output = T.intercalate "\n" rendered
    putText output
    lookupEnv "GITHUB_STEP_SUMMARY" >>= traverse_ (`appendFileText` output)

-- The environment override that seeds a child's injected upstream latency, in whole
-- milliseconds (the knob's unit).
latencyOverride :: Int -> (String, String)
latencyOverride injMs = ("BENCH_LOAD_UPSTREAM_LATENCY_MS", show injMs)

-- The baseline round trip in whole milliseconds, the value injected into both passes.
baselineInjectedMs :: BaselineSource -> Int
baselineInjectedMs = \case
    MeasuredRtt rtt _ -> round rtt
    InjectedFallback ms -> round ms

-- Run one scenario in a child process with the given environment overrides layered onto
-- the driver's environment, and decode its report. The child prints exactly one JSON line
-- (its report), so the captured stdout decodes directly.
runScenarioChild :: FilePath -> [(String, String)] -> Text -> IO ScenarioReport
runScenarioChild self overrides name = do
    base <- getEnvironment
    raw <- readProcessStdout_ (setEnv (overrideEnv overrides base) (proc self [toString name]))
    either (\err -> benchFail ("bench-load child " <> name <> " report did not parse: " <> toText err)) pure (eitherDecode raw)

-- Layer the overrides onto a base environment: keep every base entry the overrides do not
-- name, then append the overrides (which therefore win).
overrideEnv :: [(String, String)] -> [(String, String)] -> [(String, String)]
overrideEnv overrides base =
    [(k, v) | (k, v) <- base, k `notElem` overriddenKeys] <> overrides
  where
    overriddenKeys = map fst overrides

{- Probe the live public registry for each corpus package and take the mean round trip as
the upstream baseline. A first fetch warms the keep-alive connection, which keeps the
handshake out of the samples. The probe then times each package's fetch and averages the
successful samples. Yields the configured injected latency as a labelled fallback when
probing is off, the catalogue is empty, or every fetch fails. An offline run therefore
still produces both passes. -}
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

    -- The mean of the timed samples, rounded to whole milliseconds. The value injected
    -- into the stubs and the value subtracted in attribution are then the same.
    meanMs :: [Double] -> Double
    meanMs samples = fromIntegral (round (sum samples / fromIntegral (length samples)) :: Int)

-- Whether the live probe is enabled, which is the default. @BENCH_LOAD_PROBE_RTT@ set to
-- a falsey value switches it off for a deterministic offline run.
probeEnabled :: IO Bool
probeEnabled = maybe True ((`notElem` ["0", "false", "no", "off"]) . map toLower) <$> lookupEnv "BENCH_LOAD_PROBE_RTT"

-- The child: run the named scenario and print its report as a single JSON line.
runChild :: Text -> IO ()
runChild name = do
    knobs <- loadKnobsFromEnv
    scenario <- maybe (benchFail ("unknown scenario: " <> name)) pure (findScenario name)
    report <- runScenario knobs scenario
    LBSC.putStrLn (encode report)

-- Find a scenario by name across every fixture.
findScenario :: Text -> Maybe Scenario
findScenario name = find ((== name) . scenarioName) (concatMap fixtureScenarios fixtures)
