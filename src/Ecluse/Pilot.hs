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
import UnliftIO.Async (concurrently_)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)

import Ecluse.Boot (BootEnv (..))
import Ecluse.Composition.MirrorQueue (parseEndpointUrl)
import Ecluse.Config (
    AppConfig (cfgAwsEndpointUrl, cfgCveSyncInterval, cfgOsvDataDir, cfgOsvExportBaseUrl, cfgPort, cfgVulnerabilityDatabaseBucket),
    Config (configApp),
 )
import Ecluse.Core.Osv.Advisory (osvExportUrl)
import Ecluse.Core.Osv.Compile (compileOsvToSqlite)
import Ecluse.Core.Supervision (
    BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros),
    FaultDisposition (Transient),
    SupervisionPolicy (SupervisionPolicy, spBackoff, spClassify, spLabel),
    superviseLoop,
 )
import Ecluse.Runtime.Log (moduleField)
import Ecluse.Runtime.Pilot.Export (exportToS3)
import Ecluse.Runtime.Server (ServerConfig (scCheckReady, scDrain, scPort), mkServerConfig, probeApplication, runWarp, serverMiddleware)
import Ecluse.Runtime.Telemetry (Telemetry, telemetryTracerProvider)

{- | The WAI application for the Pilot worker mode.
It exposes liveness and readiness probes.
-}
pilotApplication :: ServerConfig -> IO Application
pilotApplication cfg = pure (serverMiddleware cfg (probeApplication (scDrain cfg) (scCheckReady cfg) (pure True)))

{- | The entry point for the Pilot worker mode.
Pilot runs as a standalone HTTP server that only exposes liveness and readiness
probes, while it concurrently runs the OSV export loop.
-}
runPilot :: BootEnv -> IO ()
runPilot bootEnv = do
    let logEnv = beLogEnv bootEnv
        port = cfgPort (beConfig bootEnv)
        cfg = (mkServerConfig []){scPort = port}

    runKatipContextT logEnv (moduleField "Ecluse.Pilot") mempty $ do
        logFM InfoS (ls ("Pilot mode starting up on port " <> show port :: String))
        concurrently_
            (runExportLoop (beTelemetry bootEnv) (beConfigFull bootEnv))
            (liftIO $ runWarp cfg (pilotApplication cfg))

{- | The Pilot steady-state export loop: compile the npm OSV artifact and upload it
to the configured S3 bucket, then wait the configured sync interval and repeat. With
no bucket configured the loop idles. Orchestration over the OSV producer (the compile
in "Ecluse.Core.Osv.Compile") and the S3 adapter ("Ecluse.Runtime.Pilot.Export"); it
lives in the shell because it reads the composed configuration.

The cycle runs under the shared supervision combinator: every fault is transient
here (a failed export is retried; there is no per-cycle wiring fault to fail up
on), and the backoff is pinned at the sync interval on both ends, so a failing
export retries at exactly the cadence a succeeding one repeats at.
-}
runExportLoop :: (MonadMask m, MonadUnliftIO m, KatipContext m) => Telemetry -> Config -> m ()
runExportLoop telemetry config = do
    let appCfg = configApp config
        intervalMicros = (round (cfgCveSyncInterval appCfg) :: Int) * 1000000
    case cfgVulnerabilityDatabaseBucket appCfg of
        Nothing -> do
            logFM InfoS "No S3 bucket configured for OSV database export; export loop disabled."
            forever $ threadDelay (24 * 60 * 60 * 1000000)
        Just bucketName -> do
            logFM InfoS (ls ("S3 export loop starting up. Target bucket: " <> bucketName))
            void
                $ superviseLoop
                    SupervisionPolicy
                        { spLabel = "pilot-export"
                        , spClassify = const Transient
                        , spBackoff = BackoffSchedule{bsBaseMicros = intervalMicros, bsCapMicros = intervalMicros}
                        }
                $ do
                    runResourceT (exportNpm telemetry appCfg bucketName)
                    threadDelay intervalMicros

-- | Compile the npm OSV artifact and upload it to the given bucket: one full cycle.
exportNpm :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => Telemetry -> AppConfig -> Text -> m ()
exportNpm telemetry appCfg bucketName = do
    logFM InfoS "Starting npm OSV database compilation"
    dbPath <- compileOsvToSqlite (telemetryTracerProvider telemetry) (cfgOsvDataDir appCfg) "npm" (osvExportUrl (cfgOsvExportBaseUrl appCfg) "npm")
    exportToS3 (telemetryTracerProvider telemetry) (cfgAwsEndpointUrl appCfg >>= parseEndpointUrl) bucketName dbPath

{- | Options for the one-shot 'runPilotCompile' mode: which ecosystem's export
to compile, where to fetch it from, and where the artifact lands.
-}
data PilotCompileOptions = PilotCompileOptions
    { pcoEcosystem :: Text
    , pcoSource :: Maybe String
    {- ^ Overrides the export URL; 'Nothing' selects the configured export
    base for the ecosystem ('osvExportUrl' under @osvExportBaseUrl@).
    -}
    , pcoOutDir :: FilePath
    , pcoUpload :: Bool
    {- ^ After compiling, upload the artifact to the configured
    vulnerability-database bucket, completing one full sync cycle.
    -}
    }
    deriving stock (Eq, Show)

{- | Requesting an upload without a configured vulnerability-database bucket.

This is a wiring fault at the composition root: there is no per-run decision a
caller could make about it, so it throws rather than returning a value the
caller could only re-raise.
-}
data PilotUploadUnconfigured = PilotUploadUnconfigured
    deriving stock (Eq, Show)

instance Exception PilotUploadUnconfigured

{- | Run a single OSV compilation, optionally upload it, and return the
artifact's path.

The same bounded-retry pipeline the export loop runs, without the loop or the
probe server: point it at an export (or a stub serving a fixture zip) and it
writes the artifact into the requested directory, then uploads it when
'pcoUpload' asks for one full sync cycle. A source that cannot be fetched or
parsed propagates as an exception, so the process exits non-zero, which makes
the command safe to script and to schedule.
-}
runPilotCompile :: LogEnv -> Telemetry -> AppConfig -> PilotCompileOptions -> IO FilePath
runPilotCompile logEnv telemetry appCfg opts = do
    let url = fromMaybe (osvExportUrl (cfgOsvExportBaseUrl appCfg) (pcoEcosystem opts)) (pcoSource opts)
    runKatipContextT logEnv (moduleField "Ecluse.Pilot") mempty $
        runResourceT $ do
            dbFile <- compileOsvToSqlite (telemetryTracerProvider telemetry) (pcoOutDir opts) (pcoEcosystem opts) url
            when (pcoUpload opts) $
                case cfgVulnerabilityDatabaseBucket appCfg of
                    Nothing -> throwIO PilotUploadUnconfigured
                    Just bucket -> exportToS3 (telemetryTracerProvider telemetry) (cfgAwsEndpointUrl appCfg >>= parseEndpointUrl) bucket dbFile
            pure dbFile
