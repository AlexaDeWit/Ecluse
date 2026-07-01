{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}

module Ecluse.Pilot (
    runPilot,
    runCompileOsv,
    pilotApplication,
) where

import Conduit (runConduit, (.|))
import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.Trans.Resource (runResourceT)
import Katip (Severity (InfoS), logFM, ls)
import Katip.Monadic (KatipContextT, runKatipContextT)
import Network.Wai (Application)
import UnliftIO (async)

import Ecluse.Boot (BootEnv (..))
import Ecluse.Config (AppConfig (cfgOsvDbPath, cfgOsvSyncInterval, cfgOsvUrl, cfgPort))
import Ecluse.Log (moduleField)
import Ecluse.Osv.Database (compileToSqlite)
import Ecluse.Osv.Stream (streamOsvUrl)
import Ecluse.Server (ServerConfig (scDrain, scPort), mkServerConfig, probeApplication, runWarp, serverMiddleware)
import Ecluse.Telemetry (telemetryTracerProvider)

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
        config = beConfig bootEnv
        port = cfgPort config
        cfg = (mkServerConfig []){scPort = port}

    runKatipContextT logEnv (moduleField "Ecluse.Pilot") mempty $ do
        logFM InfoS (ls ("Pilot mode starting up on port " <> show port :: String))

        -- Start background compilation loop if configured
        case cfgOsvUrl config of
            Nothing -> logFM InfoS "No OSV URL configured, background compilation disabled."
            Just url -> do
                let dbPath = cfgOsvDbPath config
                    -- Floor the interval at 1 hour (3600s) to avoid API hammering
                    interval = max 3600 (cfgOsvSyncInterval config)
                void . liftIO . async $ forever $ do
                    runKatipContextT logEnv (moduleField "Ecluse.Pilot") mempty $ do
                        logFM InfoS (ls ("Starting background OSV compilation from " <> url))
                        runCompileOsv bootEnv url dbPath
                    threadDelay (round (realToFrac interval * 1000000 :: Double))

    runWarp cfg (pilotApplication cfg)

{- | Run the OSV compilation pipeline.
It fetches the OSV dataset from the given URL and compiles it into a SQLite
database at the specified path.
-}
runCompileOsv :: BootEnv -> String -> FilePath -> IO ()
runCompileOsv bootEnv url dbPath = do
    let logEnv = beLogEnv bootEnv
        telemetry = beTelemetry bootEnv

    runResourceT $
        runKatipContextT logEnv (moduleField "Ecluse.Pilot") mempty $ do
            logFM InfoS (ls ("Starting OSV compilation from " <> url <> " to " <> dbPath))
            runConduit $
                streamOsvUrl telemetry url
                    .| compileToSqlite dbPath
