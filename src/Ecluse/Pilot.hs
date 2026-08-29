-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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
    AdvisoriesSettings (advBucket, advCompileInterval, advDataDir, advOsvExportBaseUrl),
    AppConfig (cfgAdvisories),
    Config (configApp),
    unUrl,
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName, parseEcosystem)
import Ecluse.Core.Osv.Advisory (osvExportUrl)
import Ecluse.Core.Osv.Compile (compileOsvToSqlite)
import Ecluse.Core.Supervision (
    BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros),
    FaultDisposition (Transient),
    SupervisionPolicy (SupervisionPolicy, spBackoff, spClassify, spLabel),
    superviseLoop,
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

{- | Compile and upload one OSV artifact per sync interval, or idle with no bucket configured.
Every fault is transient, because a cycle has no wiring fault to fail up on.
-}
runExportLoop :: (MonadMask m, MonadUnliftIO m, KatipContext m) => Telemetry -> Maybe AwsEndpoint -> Config -> m ()
runExportLoop telemetry s3Endpoint config = do
    let appCfg = configApp config
        intervalMicros = (round (advCompileInterval (cfgAdvisories appCfg)) :: Int) * 1000000
    case advBucket (cfgAdvisories appCfg) of
        Nothing -> do
            logFM InfoS "No S3 bucket configured for OSV database export; export loop disabled."
            forever $ threadDelay (24 * 60 * 60 * 1000000)
        Just bucketName -> do
            logFM InfoS (ls ("S3 export loop starting up. Target bucket: " <> bucketName))
            -- One instrument set for the whole loop. Rebuilding it per cycle would register
            -- the catalogue again and split each signal across two streams.
            metrics <- liftIO (newMetrics telemetry)
            void
                $ superviseLoop
                    SupervisionPolicy
                        { spLabel = "pilot-export"
                        , spClassify = const Transient
                        , spBackoff = BackoffSchedule{bsBaseMicros = intervalMicros, bsCapMicros = intervalMicros}
                        }
                $ do
                    runResourceT (exportEcosystem metrics Npm telemetry s3Endpoint appCfg bucketName)
                    threadDelay intervalMicros

{- | One full cycle for one ecosystem: compile its OSV artifact and upload it. osv.dev spells
@npm@ as 'ecosystemName' does, so an ecosystem it spells differently needs its own spelling.
-}
exportEcosystem :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => Metrics -> Ecosystem -> Telemetry -> Maybe AwsEndpoint -> AppConfig -> Text -> m ()
exportEcosystem metrics eco telemetry s3Endpoint appCfg bucketName = do
    logFM InfoS (ls ("Starting " <> ecosystemName eco <> " OSV database compilation"))
    dbPath <-
        compileOsvToSqlite
            (advisoryCompileMetricsPortOf metrics (Just eco))
            (telemetryTracerProvider telemetry)
            (advDataDir (cfgAdvisories appCfg))
            (ecosystemName eco)
            (osvExportUrl (unUrl (advOsvExportBaseUrl (cfgAdvisories appCfg))) (ecosystemName eco))
    exportToS3 (telemetryTracerProvider telemetry) s3Endpoint bucketName dbPath

-- | Options for the one-shot 'runPilotCompile' mode.
data PilotCompileOptions = PilotCompileOptions
    { pcoEcosystem :: Text
    , pcoSource :: Maybe String
    {- ^ Overrides the export URL. 'Nothing' selects the configured export
    base for the ecosystem ('osvExportUrl' under @osvExportBaseUrl@).
    -}
    , pcoOutDir :: FilePath
    , pcoUpload :: Bool
    -- ^ Upload the compiled artifact to the configured vulnerability-database bucket.
    }
    deriving stock (Eq, Show)

{- | Requesting an upload without a configured vulnerability-database bucket.

This is a wiring fault at the composition root, so it throws rather than returning a
value the caller could only re-raise.
-}
data PilotUploadUnconfigured = PilotUploadUnconfigured
    deriving stock (Eq, Show)

instance Exception PilotUploadUnconfigured

{- | Run a single OSV compilation, optionally upload the artifact, and return its path. An
unfetchable or unparseable source propagates, so the command exits non-zero and stays scriptable.
-}
runPilotCompile :: LogEnv -> Telemetry -> Maybe AwsEndpoint -> AppConfig -> PilotCompileOptions -> IO FilePath
runPilotCompile logEnv telemetry s3Endpoint appCfg opts = do
    let url = fromMaybe (osvExportUrl (unUrl (advOsvExportBaseUrl (cfgAdvisories appCfg))) (pcoEcosystem opts)) (pcoSource opts)
    metrics <- newMetrics telemetry
    -- The metric label domain is the closed 'Ecosystem' enum. A one-shot compile of a name
    -- outside it still writes its artifact, and records no series.
    let compileMetrics = advisoryCompileMetricsPortOf metrics (parseEcosystem (pcoEcosystem opts))
    moduleContext logEnv "Ecluse.Pilot" $
        runResourceT $ do
            dbFile <- compileOsvToSqlite compileMetrics (telemetryTracerProvider telemetry) (pcoOutDir opts) (pcoEcosystem opts) url
            when (pcoUpload opts) $
                case advBucket (cfgAdvisories appCfg) of
                    Nothing -> throwIO PilotUploadUnconfigured
                    Just bucket -> exportToS3 (telemetryTracerProvider telemetry) s3Endpoint bucket dbFile
            pure dbFile
