{-# LANGUAGE ImportQualifiedPost #-}

module Ecluse.Pilot (
    runPilot,
    runCompileOsv,
    pilotApplication,
) where

import Conduit (runConduitRes, (.|))
import Control.Monad.Primitive (PrimMonad (..))
import Katip (Severity (InfoS), logFM, ls)
import Katip.Monadic (KatipContextT, runKatipContextT)
import Network.Wai (Application)

import Ecluse.Boot (BootEnv (..))
import Ecluse.Config (AppConfig (cfgPort))
import Ecluse.Log (moduleField)
import Ecluse.Pilot.Osv.Database (compileToSqlite)
import Ecluse.Pilot.Osv.Stream (streamOsvUrl)
import Ecluse.Server (ServerConfig (scDrain, scPort), mkServerConfig, probeApplication, runWarp, serverMiddleware)
import Ecluse.Telemetry (telemetryTracerProvider)

instance (PrimMonad m) => PrimMonad (KatipContextT m) where
    type PrimState (KatipContextT m) = PrimState m
    primitive = lift . primitive

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

    runWarp cfg (pilotApplication cfg)

{- | Run the OSV compilation pipeline.
It fetches the OSV dataset from the given URL and compiles it into a SQLite
database at the specified path.
-}
runCompileOsv :: BootEnv -> String -> FilePath -> IO ()
runCompileOsv bootEnv url dbPath = do
    let logEnv = beLogEnv bootEnv
        telemetry = beTelemetry bootEnv

    runKatipContextT logEnv (moduleField "Ecluse.Pilot") mempty $ do
        logFM InfoS (ls ("Starting OSV compilation from " <> url <> " to " <> dbPath))
        runConduitRes $
            streamOsvUrl telemetry url
                .| compileToSqlite dbPath
