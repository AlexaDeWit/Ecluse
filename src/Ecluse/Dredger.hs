-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Dredger (
    runDredger,
) where

import Data.Map.Strict qualified as Map
import Katip (Severity (InfoS))

import Ecluse.Boot (BootEnv (..), probeServerConfig)
import Ecluse.Composition.Endpoints (MirrorStore, mirrorStoreUrl)
import Ecluse.Composition.Plan (BootPlan (bpValidated))
import Ecluse.Composition.Validate (ValidatedPlan (vpMirrorStores, vpSettings))
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Runtime.Log (moduleLog)
import Ecluse.Runtime.Server (ServerConfig (scPort), probeOnlyApplication, runWarp)

{- | The Dredger worker mode: a probe-only HTTP server over the vetted mirror stores. It deletes
from each mount's mirror target, so the boot's own pass refuses an endpoint another role holds
and this mode never sees one.
-}
runDredger :: BootEnv -> IO ()
runDredger bootEnv = do
    let validated = bpValidated (beBootPlan bootEnv)
    traverse_ (dredgerLog . vettedStoreLine) (Map.toAscList (vpMirrorStores validated))
    let cfg = probeServerConfig (vpSettings validated)
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
