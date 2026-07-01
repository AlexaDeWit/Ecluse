module Ecluse.Dredger (
    runDredger,
    dredgerApplication,
) where

import Katip (Severity (InfoS), logFM, ls)
import Katip.Monadic (runKatipContextT)
import Network.Wai (Application)

import Ecluse.Boot (BootEnv (..))
import Ecluse.Config (AppConfig (cfgPort))
import Ecluse.Log (moduleField)
import Ecluse.Server (ServerConfig (scDrain, scPort), mkServerConfig, probeApplication, runWarp, serverMiddleware)

{- | The WAI application for the Dredger worker mode.
It exposes liveness and readiness probes.
-}
dredgerApplication :: ServerConfig -> IO Application
dredgerApplication cfg = pure (serverMiddleware cfg (probeApplication (scDrain cfg) (pure True)))

{- | The entry point for the Dredger worker mode.
Dredger runs as a standalone HTTP server that only exposes liveness and readiness
probes. Its actual worker loop will clean up upstream mirrors.
-}
runDredger :: BootEnv -> IO ()
runDredger bootEnv = do
    let logEnv = beLogEnv bootEnv
        port = cfgPort (beConfig bootEnv)
        cfg = (mkServerConfig []){scPort = port}

    runKatipContextT logEnv (moduleField "Ecluse.Dredger") mempty $ do
        logFM InfoS (ls ("Dredger mode starting up on port " <> show port :: String))

    runWarp cfg (dredgerApplication cfg)
