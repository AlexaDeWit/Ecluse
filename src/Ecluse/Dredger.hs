-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Dredger (
    runDredger,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Katip (Severity (InfoS))

import Ecluse.Boot (BootEnv (..), orExit, probeServerConfig)
import Ecluse.Composition.BootError (renderBootError)
import Ecluse.Composition.Endpoints (MirrorStore, mirrorStoreUrl, vetMirrorStores)
import Ecluse.Config (AppConfig (cfgMounts), Config (configApp))
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Runtime.Log (moduleLog)
import Ecluse.Runtime.Server (ServerConfig (scPort), probeOnlyApplication, runWarp)

{- | The Dredger worker mode: a probe-only HTTP server over the vetted mirror stores. It deletes
from each mount's mirror target, so an endpoint another role holds refuses here, not at a delete.
-}
runDredger :: BootEnv -> IO ()
runDredger bootEnv = do
    let appConfig = configApp (beConfig bootEnv)
    stores <- orExit (T.unlines . map renderBootError) (vetMirrorStores (cfgMounts appConfig))
    traverse_ (dredgerLog . vettedStoreLine) (Map.toAscList stores)
    let cfg = probeServerConfig appConfig
    dredgerLog ("Dredger mode starting up on port " <> show (scPort cfg))
    runWarp cfg probeOnlyApplication
  where
    dredgerLog = moduleLog (beLogEnv bootEnv) "Ecluse.Dredger" InfoS

-- One boot line per store the Dredger may delete from, so its blast radius is on the record.
vettedStoreLine :: (Ecosystem, MirrorStore) -> Text
vettedStoreLine (eco, store) =
    "mount \""
        <> ecosystemName eco
        <> "\": mirror store "
        <> registryUrlText (mirrorStoreUrl store)
        <> " holds no other registry role"
