module Ecluse.Dredger (
    runDredger,
) where

import Katip (Severity (InfoS), logFM, ls)
import Katip.Monadic (runKatipContextT)

import Ecluse.Boot (BootEnv (..))
import Ecluse.Config (AppConfig (cfgPort))
import Ecluse.Log (moduleField)
import Ecluse.Server (ServerConfig (scDrain, scPort), mkServerConfig, probeApplication, runWarp, serverMiddleware)

{- | The entry point for the Dredger worker mode.
Dredger runs as a standalone HTTP server that only exposes liveness and readiness
probes. Its actual worker loop will scan upstream mirrors and garbage collect.
-}
runDredger :: BootEnv -> IO ()
runDredger bootEnv = do
    let logEnv = beLogEnv bootEnv
        port = cfgPort (beConfig bootEnv)
        cfg = (mkServerConfig []){scPort = port}

    runKatipContextT logEnv (moduleField "Ecluse.Dredger") mempty $ do
        logFM InfoS (ls ("Dredger mode starting up on port " <> show port :: String))

    -- Start the probe server. Since scMounts is empty, it will only serve
    -- /livez and /readyz, using a dummy heartbeat that is always healthy.
    runWarp cfg (pure (serverMiddleware cfg (probeApplication (scDrain cfg) (pure True))))
