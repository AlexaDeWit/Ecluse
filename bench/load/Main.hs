{- | The @bench-load@ entry point: Layer B of the benchmark strategy — throughput and
latency under concurrent load against the real composed proxy.

Unlike the Layer A work-per-request micro-benches (@ecluse-bench@, @tasty-bench@), this
boots the real 'Ecluse.Server.application' on @warp@ over stub upstreams and drives it
with @oha@, so it measures system behaviour — saturation, latency tails, GC pauses —
rather than a pure function's cost. It is __inform-only__ and __never gates__: the
figures are reported for a human to read and trend (decision D1); the only red state is a
literal failure (the harness cannot boot, @oha@ cannot run, or a scenario served
nothing). See @docs\/architecture\/performance.md@.

== Per-scenario process isolation

Peak residency is a process-wide RTS high-water mark, so each scenario is run in its
__own process__ to keep its residency its own. With no argument this binary is the
__driver__: it re-execs itself once per scenario (@bench-load <scenario>@), collects each
child's machine-readable report, and renders the combined table to stdout and the GitHub
run summary. With a scenario name it is a __child__: it runs that one scenario and prints
its report as a single JSON line. The load knobs are read from the environment by both,
so the child inherits the driver's operating point.
-}
module Main (main) where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Text qualified as T
import System.Environment (getExecutablePath)
import System.Process.Typed (proc, readProcessStdout_)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Harness (
    Scenario (scenarioName),
    ScenarioReport,
    UpstreamFixture (fixtureEcosystem, fixtureScenarios),
    loadKnobsFromEnv,
    renderReports,
    runScenario,
 )
import Ecluse.BenchLoad.Npm (npmFixture)

-- | The fixtures driven, one per upstream ecosystem. npm is the only instance today.
fixtures :: [UpstreamFixture]
fixtures = [npmFixture]

main :: IO ()
main =
    getArgs >>= \case
        [] -> runDriver
        [name] -> runChild (toText name)
        _ -> benchFail "usage: bench-load [<scenario-name>]"

-- The driver: for each fixture's scenarios, re-exec this binary as a child to run the
-- scenario in isolation, collect its report, then render the combined table to stdout
-- and (when set) the GitHub run summary.
runDriver :: IO ()
runDriver = do
    knobs <- loadKnobsFromEnv
    self <- getExecutablePath
    rendered <- forM fixtures $ \fixture -> do
        reports <- traverse (runScenarioChild self . scenarioName) (fixtureScenarios fixture)
        pure (renderReports knobs (fixtureEcosystem fixture) reports)
    let output = T.intercalate "\n" rendered
    putText output
    lookupEnv "GITHUB_STEP_SUMMARY" >>= traverse_ (`appendFileText` output)

-- Run one scenario in a child process and decode its report. The child prints exactly
-- one JSON line (its report), so the captured stdout decodes directly.
runScenarioChild :: FilePath -> Text -> IO ScenarioReport
runScenarioChild self name = do
    raw <- readProcessStdout_ (proc self [toString name])
    either (\err -> benchFail ("bench-load child " <> name <> " report did not parse: " <> toText err)) pure (eitherDecode raw)

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
