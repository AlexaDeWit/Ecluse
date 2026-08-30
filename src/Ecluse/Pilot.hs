-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The Pilot role's front door. Every decision it acts on is planned in
"Ecluse.Pilot.Plan", so what is left here is the effect each plan names.
-}
module Ecluse.Pilot (
    runPilot,

    -- * One-shot compilation
    PilotCompileOptions (..),
    runPilotCompile,
    PilotUploadUnconfigured (..),
) where

import Conduit (MonadResource, runResourceT)
import Control.Monad.Catch (MonadMask)
import Katip (KatipContext, LogEnv, Severity (InfoS), logFM, ls)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)

import Ecluse.Boot (BootEnv (..), probeServerConfig)
import Ecluse.Config (
    AdvisoriesSettings (advDataDir, advUrl),
    AdvisoryStoreUrl,
    AppConfig (cfgAdvisories),
    Config (configApp),
    advisoryStoreUrlText,
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName, parseEcosystem)
import Ecluse.Core.Osv.Compile (compileOsvToSqlite)
import Ecluse.Core.Supervision (
    BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros),
    superviseLoop,
    transientPolicy,
 )
import Ecluse.Pilot.Plan (
    ExportLoopPlan (ExportIdle, ExportTo),
    PilotCompileOptions (..),
    PilotUploadUnconfigured (..),
    UploadPlan (UploadSkipped, UploadTo),
    compileSources,
    configuredSources,
    exportCadenceMicros,
    exportLoopPlan,
    idleCadenceMicros,
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
runPilot :: BootEnv -> IO ()
runPilot bootEnv = do
    let cfg = probeServerConfig (configApp (beConfig bootEnv))
    moduleContext (beLogEnv bootEnv) "Ecluse.Pilot" $ do
        logFM InfoS (ls ("Pilot mode starting up on port " <> show (scPort cfg) :: String))
        raceServerAgainstLoop
            (liftIO $ runWarp cfg probeOnlyApplication)
            (runExportLoop (beTelemetry bootEnv) (beS3Endpoint bootEnv) (beConfig bootEnv))

-- Run the loop 'exportLoopPlan' names. Every fault inside it is transient, because a cycle
-- has no wiring fault to fail up on.
runExportLoop :: (MonadMask m, MonadUnliftIO m, KatipContext m) => Telemetry -> Maybe AwsEndpoint -> Config -> m ()
runExportLoop telemetry s3Endpoint config = case exportLoopPlan advisories of
    ExportIdle -> do
        logFM InfoS "No advisory store configured for OSV database export; export loop disabled."
        forever (threadDelay idleCadenceMicros)
    ExportTo store -> do
        logFM InfoS (ls ("Export loop starting up. Target store: " <> advisoryStoreUrlText store))
        -- One instrument set for the whole loop. Rebuilding it per cycle would register
        -- the catalogue again and split each signal across two streams.
        metrics <- liftIO (newMetrics telemetry)
        void $ superviseLoop (transientPolicy "pilot-export" schedule) $ do
            runResourceT (exportEcosystem metrics Npm telemetry s3Endpoint advisories store)
            threadDelay cadence
  where
    advisories = cfgAdvisories (configApp config)
    cadence = exportCadenceMicros advisories
    schedule = BackoffSchedule{bsBaseMicros = cadence, bsCapMicros = cadence}

-- One full cycle for one ecosystem: compile its OSV artifact and upload it. osv.dev spells
-- @npm@ as 'ecosystemName' does, so an ecosystem it spells differently needs its own spelling.
exportEcosystem :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => Metrics -> Ecosystem -> Telemetry -> Maybe AwsEndpoint -> AdvisoriesSettings -> AdvisoryStoreUrl -> m ()
exportEcosystem metrics eco telemetry s3Endpoint advisories store = do
    logFM InfoS (ls ("Starting " <> ecosystemName eco <> " OSV database compilation"))
    dbPath <-
        compileOsvToSqlite
            (advisoryCompileMetricsPortOf metrics (Just eco))
            (telemetryTracerProvider telemetry)
            (advDataDir advisories)
            (ecosystemName eco)
            (configuredSources advisories (ecosystemName eco))
    uploadToStore telemetry s3Endpoint store dbPath

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

{- | Run a single OSV compilation, optionally upload the artifact, and return its path. An
unfetchable or unparseable source propagates, so the command exits non-zero and stays scriptable.
-}
runPilotCompile :: LogEnv -> Telemetry -> Maybe AwsEndpoint -> AppConfig -> PilotCompileOptions -> IO FilePath
runPilotCompile logEnv telemetry s3Endpoint appCfg opts = do
    metrics <- newMetrics telemetry
    -- The metric label domain is the closed 'Ecosystem' enum. A one-shot compile of a name
    -- outside it still writes its artifact, and records no series.
    let compileMetrics = advisoryCompileMetricsPortOf metrics (parseEcosystem (pcoEcosystem opts))
    moduleContext logEnv "Ecluse.Pilot" $
        runResourceT $ do
            -- Raised before the compile, so an upload the wiring cannot satisfy fails
            -- without first compiling the artifact it could never publish.
            plan <- either throwIO pure planned
            dbFile <-
                compileOsvToSqlite
                    compileMetrics
                    (telemetryTracerProvider telemetry)
                    (pcoOutDir opts)
                    (pcoEcosystem opts)
                    (compileSources advisories opts)
            runUploadPlan telemetry s3Endpoint plan dbFile
            pure dbFile
  where
    advisories = cfgAdvisories appCfg
    planned = uploadPlan opts (advUrl advisories)
