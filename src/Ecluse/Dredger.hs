-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Run the supervised Dredger cycle, advisory synchronisation, and health probes.
"Ecluse.Core.Registry.Sweep" owns the selection and execution decisions.
-}
module Ecluse.Dredger (
    runDredger,
    withSyncTasks,
    dredgerServerConfig,
    dredgerReady,
    latchedStep,
) where

import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)
import Katip (LogEnv, Severity (ErrorS, InfoS, WarningS), SimpleLogPayload, runKatipContextT)
import UnliftIO.Async (link, mapConcurrently_, withAsync)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Boot (BootEnv (..), probeServerConfig)
import Ecluse.Composition.Executable (PrunerWiring (pwCveSync, pwDeferredMetrics, pwMounts))
import Ecluse.Config (AppConfig, Config (configApp))
import Ecluse.Core.Cve.Slot (currentAdvisoryEtag)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Registry.Maintenance (
    RefillPosture (RefillPermitted, RefillRefused),
    StoreFacts (factBackend, factRefill),
    StoreObservation (obFacts),
 )
import Ecluse.Core.Registry.Sweep (sweepCycle)
import Ecluse.Core.Registry.Sweep.Types (
    CycleHalt,
    CycleOutcome (outcomeHalt),
    SweepAudit (SweepAudit, auditError, auditInfo, auditWarn),
    SweepMount (smEcosystem, smStore),
    SweepPacing (swpCyclePause, swpShape),
    SweepPorts (SweepPorts, sweepAdvisoryEtag, sweepAudit, sweepDelay, sweepMetrics, sweepNow, sweepReport),
    SweepReport,
    SweepShape (SweepCandidates, SweepEverything),
    SweepStore (ssObserve),
    latches,
    renderCycleHalt,
    walkMarkerOf,
 )
import Ecluse.Core.Server.Readiness (Readiness (Latched), allMountsReady)
import Ecluse.Core.Supervision (secondsToMicros, superviseLoop, transientPolicy)
import Ecluse.Cve.Sync (
    CveSyncHandle (csEnv),
    backgroundLoopBackoff,
    cveSyncReadiness,
    cveSyncScheduleFor,
    cveSyncTasks,
    registerAdvisoryAges,
 )
import Ecluse.Dredger.Plan (
    DredgerOptions (doMode, doRepetition),
    SweepMode (SweepDeletes, SweepPreviews),
    SweepRepetition (SweepContinuously, SweepOnce),
    advisoryPollMicros,
    advisoryWaitAttempts,
    cycleEnding,
    sweepPacingFor,
    sweepReportFor,
    waitsForAdvisories,
 )
import Ecluse.Runtime.Cve.Sync (SyncEnv (syncSlot))
import Ecluse.Runtime.Log (moduleLog)
import Ecluse.Runtime.Server (
    ServerConfig (scCheckReady, scPort),
    probeOnlyApplication,
    raceServerAgainstLoop,
    runWarp,
 )
import Ecluse.Runtime.Telemetry.Instruments (Metrics, dredgerMetricsPortOf, newMetrics)
import Ecluse.Runtime.Telemetry.Reporters (installMetrics)

data SweepStatus = SweepStatus
    { stLatched :: IORef (Maybe CycleHalt)
    , stFinal :: IORef (Maybe CycleOutcome)
    }

newSweepStatus :: IO SweepStatus
newSweepStatus = SweepStatus <$> newIORef Nothing <*> newIORef Nothing

{- | Run the Dredger. Under @--once@ the sweep returns and the race ends with it, carrying what
that cycle ended on, which is what makes the role scriptable.
-}
runDredger :: BootEnv -> DredgerOptions -> PrunerWiring -> IO (Maybe Text)
runDredger bootEnv opts pruner = do
    metrics <- newMetrics telemetry
    -- The instruments exist now, so installing them makes the credential providers' and the
    -- effectful rules' deferred reporters live for the rest of the run.
    installMetrics (pwDeferredMetrics pruner) metrics
    registerAdvisoryAges metrics (pwCveSync pruner)
    status <- newSweepStatus
    moduleLog logEnv dredgerModule InfoS capLine
    when (doMode opts == SweepDeletes) $
        moduleLog logEnv dredgerModule InfoS "this command deletes only mirrorTarget versions. privateUpstream inventory is available through --dry-run"
    traverse_ (logBlastRadius logEnv opts pacing) mounts
    moduleLog logEnv dredgerModule InfoS ("Dredger starting up, health probes on port " <> show (scPort (cfg status)))
    raceServerAgainstLoop
        (runWarp (cfg status) probeOnlyApplication)
        (withSyncTasks (syncTasks metrics) (sweepTask logEnv opts pacing (portsOver metrics) syncReady status mounts))
    (>>= cycleEnding (doMode opts)) <$> readIORef (stFinal status)
  where
    logEnv = beLogEnv bootEnv
    telemetry = beTelemetry bootEnv
    appConfig = configApp (beConfig bootEnv)
    (pacing, capLine) = sweepPacingFor appConfig (length (pwMounts pruner))
    -- The boot built each mount's own execution, so the loop never asks which run it is in.
    mounts = pwMounts pruner
    syncReady = cveSyncReadiness (pwCveSync pruner)
    cfg status = dredgerServerConfig appConfig (dredgerReady syncReady (readIORef (stLatched status)))
    syncTasks metrics = cveSyncTasks logEnv metrics telemetry (cveSyncScheduleFor appConfig) (pwCveSync pruner)
    portsOver metrics = sweepPortsFor logEnv metrics (sweepReportFor (doMode opts)) (pwCveSync pruner)

{- | The Dredger's health surface: the shared @server.port@, and a readiness the advisory sync
opens and a latched halt closes for good. A latch never fails liveness, so nothing restarts it.
-}
dredgerServerConfig :: AppConfig -> IO Readiness -> ServerConfig
dredgerServerConfig appConfig checkReady = (probeServerConfig appConfig){scCheckReady = checkReady}

{- | The advisory sync's own verdict until a halt latches, and 'Latched' for good after one.
Liveness stays untouched, because a restart would begin sweeping the generation that latched it.
-}
dredgerReady :: IO Readiness -> IO (Maybe CycleHalt) -> IO Readiness
dredgerReady checkReady readLatched = readLatched >>= maybe checkReady (const (pure Latched))

{- Run the sweep on the invocation's repetition. A cycle is one supervised step, so a fault that
escapes a store handle's typed contract backs off and the next cycle runs. -}
sweepTask :: LogEnv -> DredgerOptions -> SweepPacing -> SweepPorts -> IO Readiness -> SweepStatus -> [SweepMount] -> IO ()
sweepTask logEnv opts pacing ports checkReady status mounts = case doRepetition opts of
    SweepOnce -> awaitAdvisories >> onceCycle
    SweepContinuously ->
        void . runKatipContextT logEnv (mempty :: SimpleLogPayload) "dredger" $ do
            liftIO awaitAdvisories
            superviseLoop (transientPolicy "dredger-sweep" backgroundLoopBackoff) (liftIO step)
  where
    -- Only a one-shot run reports its cycle's halt as the process ending. A cycling Dredger stops
    -- by being asked to, whatever its last cycle did, so a supervisor does not restart it.
    onceCycle = do
        outcome <- sweepCycle pacing ports mounts
        writeIORef (stFinal status) (Just outcome)
        when (any latches (outcomeHalt outcome)) (writeIORef (stLatched status) (outcomeHalt outcome))

    step = latchedStep pacing ports mounts (stLatched status)

    {- Give every mount's first sync a bounded chance to land, because a sweep reads each
    mount's own database and partial readiness is not enough. Past the bound the cycle runs. -}
    awaitAdvisories = when (waitsForAdvisories mounts) (poll (advisoryWaitAttempts pacing))

    poll remaining
        | remaining <= (0 :: Int) = pass
        | otherwise = checkReady >>= bool (threadDelay advisoryPollMicros >> poll (remaining - 1)) pass . allMountsReady

{- | Run the sweep with the advisory sync tasks beside it. The sweep alone decides when the run
ends, and a task that faults still brings the run down with it.
-}
withSyncTasks :: [IO ()] -> IO a -> IO a
withSyncTasks tasks act = withAsync (mapConcurrently_ id tasks) (\syncs -> link syncs >> act)

{- | One step of the cycling Dredger: run a cycle, or report the halt that latched instead, then
wait the cycle pause. A latched Dredger touches no store and keeps reporting until it is restarted.
-}
latchedStep :: SweepPacing -> SweepPorts -> [SweepMount] -> IORef (Maybe CycleHalt) -> IO ()
latchedStep pacing ports mounts latched = do
    readIORef latched >>= maybe runCycle (reportLatched ports)
    sweepDelay ports (swpCyclePause pacing)
  where
    runCycle = do
        halt <- outcomeHalt <$> sweepCycle pacing ports mounts
        when (any latches halt) (writeIORef latched halt)

reportLatched :: SweepPorts -> CycleHalt -> IO ()
reportLatched ports halt =
    auditError (sweepAudit ports) ("the mirror sweep is halted and runs no cycle: " <> renderCycleHalt halt)

sweepPortsFor :: LogEnv -> Metrics -> SweepReport -> Map Ecosystem CveSyncHandle -> SweepPorts
sweepPortsFor logEnv metrics report cveSync =
    SweepPorts
        { sweepNow = getCurrentTime
        , sweepAdvisoryEtag = \eco ->
            maybe (pure Nothing) (currentAdvisoryEtag . syncSlot . csEnv) (Map.lookup eco cveSync)
        , sweepDelay = threadDelay . secondsToMicros
        , sweepMetrics = dredgerMetricsPortOf metrics
        , sweepAudit =
            SweepAudit
                { auditInfo = moduleLog logEnv dredgerModule InfoS
                , auditWarn = moduleLog logEnv dredgerModule WarningS
                , auditError = moduleLog logEnv dredgerModule ErrorS
                }
        , sweepReport = report
        }

{- One boot line per store, putting the Dredger's blast radius on record: which backend holds it,
whether a deleted version can come back, what this run does, and whether a walk over it resumes. -}
logBlastRadius :: LogEnv -> DredgerOptions -> SweepPacing -> SweepMount -> IO ()
logBlastRadius logEnv opts pacing mount =
    moduleLog logEnv dredgerModule InfoS $
        "sweeping the "
            <> ecosystemName (smEcosystem mount)
            <> " mirror store on "
            <> factBackend facts
            <> ", "
            <> refill
            <> ", "
            <> disposition
            <> resumption
  where
    facts = obFacts (ssObserve (smStore mount))
    refill = case factRefill facts of
        RefillPermitted -> "which accepts a re-publication of a version it deleted"
        RefillRefused -> "which refuses a re-publication, so a delete retires the version for good"
    disposition = case doMode opts of
        SweepDeletes -> "deleting what a named decisive deny condemns"
        SweepPreviews -> "previewing only: this run holds nothing that could delete"
    resumption = case (swpShape pacing, walkMarkerOf (smStore mount)) of
        (SweepCandidates, _) -> ""
        (SweepEverything, Just _) -> "; the full walk resumes from this store's own marker"
        (SweepEverything, Nothing) -> "; this run keeps no marker, so the full walk starts at the first bucket"

dredgerModule :: Text
dredgerModule = "Ecluse.Dredger"
