module Ecluse.Pilot (
    runPilot,
) where

import Katip (Severity (InfoS), logFM, ls)
import Katip.Monadic (runKatipContextT)

import Ecluse.Boot (BootEnv (..))
import Ecluse.Config (AppConfig (cfgPort))
import Ecluse.Log (moduleField)
import Ecluse.Server (ServerConfig (scDrain, scPort), mkServerConfig, probeApplication, runWarp, serverMiddleware)

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

    -- Start the probe server. Since scMounts is empty, it will only serve
    -- /livez and /readyz, using a dummy heartbeat that is always healthy.
    runWarp cfg (pure (serverMiddleware cfg (probeApplication (scDrain cfg) (pure True))))
