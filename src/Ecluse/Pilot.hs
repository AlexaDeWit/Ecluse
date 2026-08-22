-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Pilot (
    runPilot,
    pilotApplication,

    -- * One-shot compilation
    PilotCompileOptions (..),
    runPilotCompile,
    PilotUploadUnconfigured (..),
) where

import Conduit (MonadResource, runResourceT)
import Control.Monad.Catch (MonadMask)
import Katip (KatipContext, LogEnv, Severity (InfoS), logFM, ls)
import Katip.Monadic (runKatipContextT)
import Network.Wai (Application)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)

import Ecluse.Boot (BootEnv (..))
import Ecluse.Config (
    AdvisoriesSettings (advBucket, advCompileInterval, advDataDir, advOsvExportBaseUrl),
    AppConfig (cfgAdvisories, cfgServer),
    Config (configApp),
    ServerSettings (srvPort),
    unUrl,
 )
import Ecluse.Config.Ambient (AmbientAws (ambientAwsEndpointUrl), parseEndpointUrl)
import Ecluse.Core.Ecosystem (Ecosystem (Npm), parseEcosystem)
import Ecluse.Core.Osv.Advisory (osvExportUrl)
import Ecluse.Core.Osv.Compile (compileOsvToSqlite)
import Ecluse.Core.Supervision (
    BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros),
    FaultDisposition (Transient),
    SupervisionPolicy (SupervisionPolicy, spBackoff, spClassify, spLabel),
    superviseLoop,
 )
import Ecluse.Core.Telemetry.Record (AdvisoryCompileMetricsPort)
import Ecluse.Runtime.Log (moduleField)
import Ecluse.Runtime.Pilot.Export (exportToS3)
import Ecluse.Runtime.Server (ServerConfig (scCheckReady, scDrain, scPort), mkServerConfig, probeApplication, raceServerAgainstLoop, runWarp, serverMiddleware)
import Ecluse.Runtime.Telemetry (Telemetry, telemetryTracerProvider)
import Ecluse.Runtime.Telemetry.Instruments (advisoryCompileMetricsPortOf, newMetrics)

-- | The WAI application for the Pilot worker mode: the liveness and readiness probes.
pilotApplication :: ServerConfig -> IO Application
pilotApplication cfg = pure (serverMiddleware cfg (probeApplication (scDrain cfg) (scCheckReady cfg) (pure True)))

{- | The entry point for the Pilot worker mode.

The export loop never returns, so the server's graceful return on shutdown must cancel
it. A cancelled export cycle resumes from the remote artifact on the next boot.
-}
runPilot :: BootEnv -> IO ()
runPilot bootEnv = do
    let logEnv = beLogEnv bootEnv
        port = srvPort (cfgServer (beConfig bootEnv))
        cfg = (mkServerConfig []){scPort = port}

    runKatipContextT logEnv (moduleField "Ecluse.Pilot") mempty $ do
        logFM InfoS (ls ("Pilot mode starting up on port " <> show port :: String))
        raceServerAgainstLoop
            (liftIO $ runWarp cfg pilotApplication)
            (runExportLoop (beTelemetry bootEnv) (beAmbient bootEnv) (beConfigFull bootEnv))

{- | Compile the npm OSV artifact and upload it to the configured bucket, once per sync
interval, or idle when no bucket is configured.

Every fault is transient here because a cycle has no wiring fault to fail up on, and the
backoff is pinned at the sync interval on both ends.
-}
runExportLoop :: (MonadMask m, MonadUnliftIO m, KatipContext m) => Telemetry -> AmbientAws -> Config -> m ()
runExportLoop telemetry ambient config = do
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
                    runResourceT (exportNpm (advisoryCompileMetricsPortOf metrics (Just Npm)) telemetry ambient appCfg bucketName)
                    threadDelay intervalMicros

-- | Compile the npm OSV artifact and upload it to the given bucket: one full cycle.
exportNpm :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => AdvisoryCompileMetricsPort -> Telemetry -> AmbientAws -> AppConfig -> Text -> m ()
exportNpm compileMetrics telemetry ambient appCfg bucketName = do
    logFM InfoS "Starting npm OSV database compilation"
    dbPath <- compileOsvToSqlite compileMetrics (telemetryTracerProvider telemetry) (advDataDir (cfgAdvisories appCfg)) "npm" (osvExportUrl (unUrl (advOsvExportBaseUrl (cfgAdvisories appCfg))) "npm")
    exportToS3 (telemetryTracerProvider telemetry) (ambientAwsEndpointUrl ambient >>= parseEndpointUrl) bucketName dbPath

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

{- | Run a single OSV compilation, optionally upload the artifact, and return its path.

A source that cannot be fetched or parsed propagates as an exception, so the process
exits non-zero and the command stays safe to script.
-}
runPilotCompile :: LogEnv -> Telemetry -> AmbientAws -> AppConfig -> PilotCompileOptions -> IO FilePath
runPilotCompile logEnv telemetry ambient appCfg opts = do
    let url = fromMaybe (osvExportUrl (unUrl (advOsvExportBaseUrl (cfgAdvisories appCfg))) (pcoEcosystem opts)) (pcoSource opts)
    metrics <- newMetrics telemetry
    -- The metric label domain is the closed 'Ecosystem' enum. A one-shot compile of a name
    -- outside it still writes its artifact, and records no series.
    let compileMetrics = advisoryCompileMetricsPortOf metrics (parseEcosystem (pcoEcosystem opts))
    runKatipContextT logEnv (moduleField "Ecluse.Pilot") mempty $
        runResourceT $ do
            dbFile <- compileOsvToSqlite compileMetrics (telemetryTracerProvider telemetry) (pcoOutDir opts) (pcoEcosystem opts) url
            when (pcoUpload opts) $
                case advBucket (cfgAdvisories appCfg) of
                    Nothing -> throwIO PilotUploadUnconfigured
                    Just bucket -> exportToS3 (telemetryTracerProvider telemetry) (ambientAwsEndpointUrl ambient >>= parseEndpointUrl) bucket dbFile
            pure dbFile
