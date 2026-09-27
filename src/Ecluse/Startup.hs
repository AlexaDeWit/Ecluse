-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The start-up every @ecluse@ command runs: the process perimeter, the boot environment, the
executable plan, and the role behaviour that plan carries. The caller chooses the adapter resolver
each mount binds through. The shipped executable passes 'mountBindingFor'. The load harness passes a
loopback resolver, so the proxy it measures boots through this code.
-}
module Ecluse.Startup (runWith) where

import Data.Text.IO qualified as TIO

import Ecluse.Boot
import Ecluse.CLI (AppCommand (..))
import Ecluse.CheckConfig (runCheckConfig)
import Ecluse.Composition (ResolveAdapter)
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
import Ecluse.Dredger (runDredger)
import Ecluse.Dredger.Plan (DredgerOptions (doMode), dredgerBootRole)
import Ecluse.Internal (ProcessOutcome (ServiceExited, ShutdownRequested), exitCodeFor, exitReasonFor, superviseProcess)
import Ecluse.Mirror
import Ecluse.Pilot
import Ecluse.Proxy
import Ecluse.Runtime.Telemetry.Tracing (tracingPortOf)
import Ecluse.Service

-- | Start one command under the process perimeter, then exit with the status its ending owns.
runWith :: ResolveAdapter -> AppCommand -> IO ()
runWith resolveAdapter cmd = do
    outcome <- superviseProcess (runCommand resolveAdapter cmd)
    -- A non-zero status is representable only beside its reason, so reporting here covers
    -- every one of them.
    traverse_ (TIO.hPutStrLn stderr) (exitReasonFor outcome)
    exitWith (exitCodeFor outcome)

{- Each arm names its role once, and the plan carries it from there. check-config runs outside
'withBootEnv': no logger, no services. -}
runCommand :: ResolveAdapter -> AppCommand -> IO ProcessOutcome
runCommand resolveAdapter = \case
    RunCheckConfig -> shutdownAfter runCheckConfig
    RunService role -> withBootEnv (BootMirrorPipeline role) (startPlannedRole resolveAdapter noDredgerOptions)
    RunPilot -> withBootEnv BootWithoutPipeline (startPlannedRole resolveAdapter noDredgerOptions)
    -- The flags settle which store role the process boots under, so the vetting pass below runs
    -- for the authority this invocation will hold.
    RunDredger opts -> withBootEnv (dredgerBootRole (doMode opts)) (startPlannedRole resolveAdapter (Just opts))
    -- A one-shot compile vets under the Pilot's role and then does its own work rather than
    -- that role's long-running one, so it is the one boot whose behaviour the plan cannot name.
    RunPilotCompile opts ->
        withBootEnv BootWithoutPipeline $ \bootEnv ->
            shutdownAfter (void (runPilotCompile (beLogEnv bootEnv) (beTelemetry bootEnv) (bpS3Endpoint (beBootPlan bootEnv)) (beConfig bootEnv) opts))
  where
    -- Only 'RunDredger' carries sweep options, and only it boots the deleting role.
    noDredgerOptions = Nothing

{- Plan the role's runtime, then start the behaviour that plan carries. Every role plans through
the one phase, so this is where a boot spends its last refusal whichever role it started. -}
startPlannedRole :: ResolveAdapter -> Maybe DredgerOptions -> BootEnv -> IO ProcessOutcome
startPlannedRole resolveAdapter dredgerOptions bootEnv = do
    (advisories, outcome) <-
        planExecutable
            (beLogEnv bootEnv)
            (tracingPortOf (beTelemetry bootEnv))
            resolveAdapter
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
