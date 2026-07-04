module Ecluse.Pilot (
    runPilot,
    pilotApplication,

    -- * One-shot compilation
    PilotCompileOptions (..),
    runPilotCompile,
    PilotUploadUnconfigured (..),
) where

import Conduit (runResourceT)
import Katip (LogEnv, Severity (InfoS), logFM, ls)
import Katip.Monadic (runKatipContextT)
import Network.Wai (Application)

import UnliftIO.Async (concurrently_)
import UnliftIO.Exception (throwIO)

import Ecluse.Boot (BootEnv (..))
import Ecluse.Config (AppConfig (cfgOsvExportBaseUrl, cfgPort, cfgVulnerabilityDatabaseBucket))
import Ecluse.Log (moduleField)
import Ecluse.Pilot.Export (exportToS3, runExportLoop)
import Ecluse.Pilot.Osv (osvExportUrl)
import Ecluse.Pilot.Osv.Compile (compileOsvToSqlite)
import Ecluse.Server (ServerConfig (scDrain, scPort), mkServerConfig, probeApplication, runWarp, serverMiddleware)
import Ecluse.Telemetry (Telemetry)

{- | The WAI application for the Pilot worker mode.
It exposes liveness and readiness probes.
-}
pilotApplication :: ServerConfig -> IO Application
pilotApplication cfg = pure (serverMiddleware cfg (probeApplication (scDrain cfg) (pure True)))

{- | The entry point for the Pilot worker mode.
Pilot runs as a standalone HTTP server that only exposes liveness and readiness
probes. Its actual worker loop will ingest advisory databases.
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
            dbFile <- compileOsvToSqlite telemetry (pcoOutDir opts) (pcoEcosystem opts) url
            when (pcoUpload opts) $
                case cfgVulnerabilityDatabaseBucket appCfg of
                    Nothing -> throwIO PilotUploadUnconfigured
                    Just bucket -> exportToS3 appCfg bucket dbFile
            pure dbFile
