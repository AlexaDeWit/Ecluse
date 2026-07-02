module Ecluse.Pilot (
    runPilot,
    pilotApplication,
) where

import Katip (Severity (InfoS), logFM, ls)
import Katip.Monadic (runKatipContextT)
import Network.Wai (Application)

import UnliftIO.Async (concurrently_)

import Ecluse.Boot (BootEnv (..))
import Ecluse.Config (AppConfig (cfgPort))
import Ecluse.Log (moduleField)
import Ecluse.Pilot.Export (runExportLoop)
import Ecluse.Server (ServerConfig (scDrain, scPort), mkServerConfig, probeApplication, runWarp, serverMiddleware)

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
