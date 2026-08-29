-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Dredger (
    runDredger,
) where

import Katip (Severity (InfoS))

import Ecluse.Boot (BootEnv (..), probeServerConfig)
import Ecluse.Config (Config (configApp))
import Ecluse.Runtime.Log (moduleLog)
import Ecluse.Runtime.Server (ServerConfig (scPort), probeOnlyApplication, runWarp)

{- | The entry point for the Dredger worker mode, an HTTP server that serves only the
liveness and readiness probes.
-}
runDredger :: BootEnv -> IO ()
runDredger bootEnv = do
    let cfg = probeServerConfig (configApp (beConfig bootEnv))
    moduleLog (beLogEnv bootEnv) "Ecluse.Dredger" InfoS ("Dredger mode starting up on port " <> show (scPort cfg))
    runWarp cfg probeOnlyApplication
