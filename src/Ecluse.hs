-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Écluse: a supply-chain policy proxy for package registries.

Écluse sits between clients and a package registry and applies a configurable resilience policy
before any dependency reaches a build. It hosts no packages: the operator's own backend stores them,
and Écluse governs only what may be fetched from, and mirrored to, those backends. The rules engine
is __deny by default__ and mirroring is demand-driven, so it never runs on a request's critical path.
'run', the entry point the @ecluse@ executable invokes, lives here rather than in @app\/Main.hs@ so
the composition root is one importable unit. This module also holds the typed process perimeter.
-}
module Ecluse (
    -- * Entry point
    run,

    -- * The typed process supervisor
    ProcessOutcome (..),
    superviseProcess,
    exitCodeFor,

    -- * The separately deployable services
    runServer,
    runWorker,

    -- * npm front door
    mountBindingFor,
) where

import Control.Exception (AsyncException (ThreadKilled, UserInterrupt), SomeAsyncException)
import Control.Exception qualified as Exception
import Data.Text.IO qualified as TIO
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

import Ecluse.Boot
import Ecluse.CLI (AppCommand (..), execCLI)
import Ecluse.CheckConfig (runCheckConfig)
import Ecluse.Composition.BootError (renderAdvisory, renderBootErrors)
import Ecluse.Composition.Credential (initTargetCredentialProviders)
import Ecluse.Composition.Executable (
    PrunerWiring,
    RoleWiring (MirrorPipelineWiring, PilotWiring, StorePrunerWiring),
    epRoleWiring,
    planExecutable,
 )
import Ecluse.Composition.Maintenance (storeBuilds)
import Ecluse.Composition.Plan (BootPlan (bpS3Endpoint))
import Ecluse.Composition.Types (
    BootRole (BootMirrorPipeline, BootWithoutPipeline),
    MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly),
 )
import Ecluse.Config (Config (configApp))
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Dredger (runDredger)
import Ecluse.Dredger.Plan (DredgerOptions (doMode), dredgerBootRole)
import Ecluse.Mirror
import Ecluse.Pilot
import Ecluse.Proxy
import Ecluse.Runtime.Telemetry.Tracing (tracingPortOf)
import Ecluse.Service

run :: IO ()
run = do
    cmd <- execCLI
    outcome <- superviseProcess (runCommand cmd)
    -- A non-zero status is representable only beside its reason ('ProcessExit'), so reporting
    -- here covers every one of them.
    traverse_ (TIO.hPutStrLn stderr) (exitReasonFor outcome)
    exitWith (exitCodeFor outcome)

{- Dispatch one subcommand under the process perimeter. Each arm names its role once, and the
plan carries it from there. check-config runs outside 'withBootEnv': no logger, no services. -}
runCommand :: AppCommand -> IO ProcessOutcome
runCommand = \case
    RunCheckConfig -> shutdownAfter runCheckConfig
    RunService role -> withBootEnv (BootMirrorPipeline role) (startPlannedRole noDredgerOptions)
    RunPilot -> withBootEnv BootWithoutPipeline (startPlannedRole noDredgerOptions)
    -- The flags settle which store role the process boots under, so the vetting pass below runs
    -- for the authority this invocation will hold.
    RunDredger opts -> withBootEnv (dredgerBootRole (doMode opts)) (startPlannedRole (Just opts))
    -- A one-shot compile vets under the Pilot's role and then does its own work rather than
    -- that role's long-running one, so it is the one boot whose behaviour the plan cannot name.
    RunPilotCompile opts ->
        withBootEnv BootWithoutPipeline $ \bootEnv ->
            shutdownAfter (void (runPilotCompile (beLogEnv bootEnv) (beTelemetry bootEnv) (bpS3Endpoint (beBootPlan bootEnv)) (configApp (beConfig bootEnv)) opts))
  where
    -- Only 'RunDredger' carries sweep options, and only it boots the deleting role.
    noDredgerOptions = Nothing

{- Plan the role's runtime, then start the behaviour that plan carries. Every role plans through
the one phase, so this is where a boot spends its last refusal whichever role it started. -}
startPlannedRole :: Maybe DredgerOptions -> BootEnv -> IO ProcessOutcome
startPlannedRole dredgerOptions bootEnv = do
    (advisories, outcome) <-
        planExecutable
            (beLogEnv bootEnv)
            (tracingPortOf (beTelemetry bootEnv))
            mountBindingFor
            buildMirrorQueue
            initTargetCredentialProviders
            storeBuilds
            (beBootPlan bootEnv)
    -- A finding about a configuration that will not start is still one its operator must act on,
    -- so this reports beside the refusal rather than instead of it.
    traverse_ (logBootWarning (beLogEnv bootEnv) . renderAdvisory) advisories
    plan <- orExit renderBootErrors outcome
    case epRoleWiring plan of
        MirrorPipelineWiring mirror -> shutdownAfter (withServiceRuntime bootEnv plan mirror runMirrorPipeline)
        -- Only 'RunDredger' names a store role, so it is the only command that reaches here and
        -- the options it settled are always in hand.
        StorePrunerWiring pruner -> maybe (pure ShutdownRequested) (sweepUnder bootEnv pruner) dredgerOptions
        PilotWiring exportPlan -> shutdownAfter (runPilot bootEnv exportPlan)

{- Run the Dredger and report what it ended on. A one-shot cycle that halted is a service ending
rather than a shutdown, so a scheduler reads the outcome from the exit status. -}
sweepUnder :: BootEnv -> PrunerWiring -> DredgerOptions -> IO ProcessOutcome
sweepUnder bootEnv pruner opts = maybe ShutdownRequested ServiceExited <$> runDredger bootEnv opts pruner

shutdownAfter :: IO () -> IO ProcessOutcome
shutdownAfter act = ShutdownRequested <$ act

{- Pick the entry point the assembled runtime's own role names. Both halves run over the one
assembly, so the dedicated worker composes the wiring the serve path embeds. -}
runMirrorPipeline :: ServiceRuntime -> IO ()
runMirrorPipeline runtime = case svcRole runtime of
    MirrorOnly -> runMirror runtime
    ServeAndMirror -> runProxy runtime
    ServeOnly -> runProxy runtime

{- | How one whole service run ended. Each constructor owns one exit code ('exitCodeFor'), so
an orchestrator reads the ending from the status alone.
-}
data ProcessOutcome
    = -- | The services drained and returned (a graceful shutdown): exit 0.
      ShutdownRequested
    | -- | A service failed up with the carried rendered fault: exit 1.
      ServiceExited Text
    | -- | The boot aborted ('BootAborted') with the carried rendered refusal: exit 2.
      BootFault Text
    | -- | The run was cancelled from outside (a kill, an interrupt): exit 3.
      RunCancelled
    deriving stock (Eq, Show)

{- | Run the service under the typed process perimeter and classify its ending. The base
'Exception.try' and 'Exception.throwIO' are deliberate: what leaves here async must leave async.
-}
superviseProcess :: IO ProcessOutcome -> IO ProcessOutcome
superviseProcess service =
    Exception.try service >>= \case
        Right outcome -> pure outcome
        Left err
            | Just (BootAborted rendered) <- fromException err -> pure (BootFault rendered)
            | Just (code :: ExitCode) <- fromException err -> Exception.throwIO code
            | Just (killed :: AsyncException) <- fromException err ->
                pure $ case killed of
                    ThreadKilled -> RunCancelled
                    UserInterrupt -> RunCancelled
                    -- StackOverflow / HeapOverflow: resource exhaustion is a
                    -- fault of the run, not a cancellation.
                    other -> ServiceExited (displayExceptionT other)
            | Just (_ :: SomeAsyncException) <- fromException err -> Exception.throwIO err
            | otherwise -> pure (ServiceExited (displayExceptionT err))

{- How a run ends. A failing status is representable only beside the reason it reports, so
'run' cannot exit non-zero in silence. -}
data ProcessExit
    = ExitedCleanly
    | ExitedWith ExitCode Text

-- The status and the report one outcome owns. Both 'run' and 'exitCodeFor' read the ending here.
processExitFor :: ProcessOutcome -> ProcessExit
processExitFor = \case
    ShutdownRequested -> ExitedCleanly
    ServiceExited detail -> ExitedWith (ExitFailure 1) ("ecluse: service exited: " <> detail)
    -- The boot phase rendered the whole aggregated block, which reports here unprefixed.
    BootFault rendered -> ExitedWith (ExitFailure 2) rendered
    RunCancelled -> ExitedWith (ExitFailure 3) "ecluse: run cancelled"

-- | The process exit status each 'ProcessOutcome' owns.
exitCodeFor :: ProcessOutcome -> ExitCode
exitCodeFor outcome = case processExitFor outcome of
    ExitedCleanly -> ExitSuccess
    ExitedWith code _ -> code

-- What an outcome reports before exiting. A graceful shutdown is the only one with nothing to say.
exitReasonFor :: ProcessOutcome -> Maybe Text
exitReasonFor outcome = case processExitFor outcome of
    ExitedCleanly -> Nothing
    ExitedWith _ reason -> Just reason
