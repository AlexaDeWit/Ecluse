-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The Pilot role's front door. Every decision it acts on is planned before it starts, in
"Ecluse.Pilot.Plan" and the boot arm that spends the plan's refusal, so what is left here is
the effect each plan names.
-}
module Ecluse.Pilot (
    runPilot,
    runExportLoop,
    superviseExportCycles,

    -- * One-shot compilation
    PilotCompileOptions (..),
    runPilotCompile,
    PilotUploadUnconfigured (..),
) where

import Conduit (MonadResource, runResourceT)
import Control.Monad.Catch (MonadMask)
import Katip (KatipContext, LogEnv, Severity (InfoS, WarningS), logFM, ls)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (mapConcurrently_)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)

import Ecluse.Boot (BootEnv (..), probeServerConfig)
import Ecluse.Composition.Plan (BootPlan (bpS3Endpoint))
import Ecluse.Config (
    AdvisoriesSettings (advDataDir, advUrl),
    AdvisoryStoreUrl,
    AppConfig (cfgAdvisories),
    Config (configApp, configMounts),
    advisoryStoreUrlText,
 )
import Ecluse.Core.Ecosystem (ecosystemName, parseEcosystem)
import Ecluse.Core.Osv.Compile (compileOsvToSqlite)
import Ecluse.Core.Osv.Ecosystem (osvEcosystemFor, osvEcosystemNamed)
import Ecluse.Core.Supervision (
    BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros),
    superviseLoop,
    transientPolicy,
 )
import Ecluse.Pilot.Plan (
    ExportLoopPlan (ExportIdle, ExportTo),
    ExportTarget (etEcosystem, etEpss),
    PilotCompileOptions (..),
    PilotUploadUnconfigured (..),
    UploadPlan (UploadSkipped, UploadTo),
    compileEpssRequirement,
    configuredSources,
    exportCadenceMicros,
    idleCadenceMicros,
    quietTimeFor,
    unmountedCompileWarning,
    uploadPlan,
    uploadTarget,
 )
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Log (moduleContext)
import Ecluse.Runtime.Pilot.Export (exportToS3)
import Ecluse.Runtime.Server (ServerConfig (scPort), probeOnlyApplication, raceServerAgainstLoop, runWarp)
import Ecluse.Runtime.Telemetry (Telemetry, telemetryTracerProvider)
import Ecluse.Runtime.Telemetry.Instruments (Metrics, advisoryCompileMetricsPortOf, newMetrics)

{- | The entry point for the Pilot worker mode. The export loop never returns, so the
server's graceful return on shutdown must cancel it, resuming from the remote artifact next boot.
-}
runPilot :: BootEnv -> ExportLoopPlan -> IO ()
runPilot bootEnv exportPlan = do
    let cfg = probeServerConfig (configApp (beConfig bootEnv))
    moduleContext (beLogEnv bootEnv) "Ecluse.Pilot" $ do
        logFM InfoS (ls ("Pilot mode starting up on port " <> show (scPort cfg) :: String))
        raceServerAgainstLoop
            (liftIO $ runWarp cfg probeOnlyApplication)
            (runExportLoop (beTelemetry bootEnv) (bpS3Endpoint (beBootPlan bootEnv)) (beConfig bootEnv) exportPlan)

{- | Run the loop the boot planned, never returning. Every fault inside a cycle is transient,
because a cycle has no wiring fault to fail up on.
-}
runExportLoop :: (MonadMask m, MonadUnliftIO m, KatipContext m) => Telemetry -> Maybe AwsEndpoint -> Config -> ExportLoopPlan -> m ()
runExportLoop telemetry s3Endpoint config = \case
    ExportIdle -> do
        logFM InfoS "No advisory store configured for OSV database export; export loop disabled."
        forever (threadDelay idleCadenceMicros)
    ExportTo store targets -> do
        logFM InfoS (ls ("Export loop starting up. Target store: " <> advisoryStoreUrlText store))
        -- One instrument set for the whole loop. Rebuilding it per cycle would register
        -- the catalogue again and split each signal across two streams.
        metrics <- liftIO (newMetrics telemetry)
        superviseExportCycles schedule targets $ \target -> do
            runResourceT (exportEcosystem metrics target telemetry s3Endpoint advisories store)
            threadDelay cadence
  where
    advisories = cfgAdvisories (configApp config)
    cadence = exportCadenceMicros advisories
    schedule = BackoffSchedule{bsBaseMicros = cadence, bsCapMicros = cadence}

{- | Run one supervised cycle loop per ecosystem, side by side. Each keeps its own backoff, so a
feed that fails costs its own cadence and holds back no other ecosystem's artifact.
-}
superviseExportCycles :: (MonadUnliftIO m, KatipContext m) => BackoffSchedule -> NonEmpty ExportTarget -> (ExportTarget -> m ()) -> m ()
superviseExportCycles schedule targets runCycle =
    mapConcurrently_ (\target -> superviseLoop (policyFor target) (runCycle target)) targets
  where
    policyFor target = transientPolicy ("pilot-export-" <> ecosystemName (etEcosystem target)) schedule

-- One full cycle for one ecosystem: compile its OSV artifact and upload it.
exportEcosystem :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => Metrics -> ExportTarget -> Telemetry -> Maybe AwsEndpoint -> AdvisoriesSettings -> AdvisoryStoreUrl -> m ()
exportEcosystem metrics target telemetry s3Endpoint advisories store = do
    logFM InfoS (ls ("Starting " <> ecosystemName eco <> " OSV database compilation"))
    dbPath <-
        compileOsvToSqlite
            (advisoryCompileMetricsPortOf metrics (Just eco))
            (telemetryTracerProvider telemetry)
            (advDataDir advisories)
            osvEco
            (etEpss target)
            (configuredSources advisories osvEco)
            (quietTimeFor advisories (Just eco))
    uploadToStore telemetry s3Endpoint store dbPath
  where
    eco = etEcosystem target
    osvEco = osvEcosystemFor eco

-- Upload one artifact to the address 'uploadTarget' derives, so it lands where the sync reads.
uploadToStore :: (MonadResource m, MonadUnliftIO m, MonadMask m, KatipContext m) => Telemetry -> Maybe AwsEndpoint -> AdvisoryStoreUrl -> FilePath -> m ()
uploadToStore telemetry s3Endpoint store dbPath =
    exportToS3 (telemetryTracerProvider telemetry) s3Endpoint bucket key dbPath
  where
    (bucket, key) = uploadTarget store dbPath

-- Act on a planned upload. A skipped one is the caller's success path, not an error.
runUploadPlan :: (MonadResource m, MonadUnliftIO m, MonadMask m, KatipContext m) => Telemetry -> Maybe AwsEndpoint -> UploadPlan -> FilePath -> m ()
runUploadPlan telemetry s3Endpoint plan dbPath = case plan of
    UploadSkipped -> pass
    UploadTo store -> uploadToStore telemetry s3Endpoint store dbPath

{- | Run a single OSV compilation, optionally upload the artifact, and return its path. An OSV
failure or a required EPSS failure propagates, so the command exits non-zero and stays scriptable.
-}
runPilotCompile :: LogEnv -> Telemetry -> Maybe AwsEndpoint -> Config -> PilotCompileOptions -> IO FilePath
runPilotCompile logEnv telemetry s3Endpoint config opts = do
    metrics <- newMetrics telemetry
    -- The metric label domain is the closed 'Ecosystem' enum. A one-shot compile of a name
    -- outside it still writes its artifact, and records no series.
    let compileMetrics = advisoryCompileMetricsPortOf metrics (parseEcosystem (pcoEcosystem opts))
    moduleContext logEnv "Ecluse.Pilot" $
        runResourceT $ do
            -- Raised before the compile, so an upload the wiring cannot satisfy fails
            -- without first compiling the artifact it could never publish.
            plan <- either throwIO pure planned
            traverse_ (logFM WarningS . ls) (unmountedCompileWarning (configMounts config) opts)
            dbFile <-
                compileOsvToSqlite
                    compileMetrics
                    (telemetryTracerProvider telemetry)
                    (pcoOutDir opts)
                    osvEco
                    (compileEpssRequirement (configMounts config) opts)
                    (configuredSources advisories osvEco)
                    (quietTimeFor advisories (parseEcosystem (pcoEcosystem opts)))
            runUploadPlan telemetry s3Endpoint plan dbFile
            pure dbFile
  where
    advisories = cfgAdvisories (configApp config)
    osvEco = osvEcosystemNamed (pcoEcosystem opts)
    planned = uploadPlan opts (advUrl advisories)
