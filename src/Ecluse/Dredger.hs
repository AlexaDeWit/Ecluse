-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Dredger (
    runDredger,
) where

import Data.Map.Strict qualified as Map
import Katip (Severity (InfoS))

import Ecluse.Boot (BootEnv (..), orExit, probeServerConfig)
import Ecluse.Composition.BootError (renderBootErrors)
import Ecluse.Composition.Endpoints (MirrorStore, mirrorStoreUrl)
import Ecluse.Composition.Maintenance (resolveStoreMaintenance)
import Ecluse.Composition.Plan (BootPlan (bpValidated))
import Ecluse.Composition.Validate (ValidatedPlan (vpMirrorStores, vpSettings))
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Fault (tfDetail)
import Ecluse.Core.Registry.Maintenance (
    StoreFacts (factBackend),
    StoreFault (faultTransport),
    StoreMaintenance (classifyStore, storeFacts),
    renderStoreClass,
 )
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Runtime.Log (moduleLog)
import Ecluse.Runtime.Server (ServerConfig (scPort), probeOnlyApplication, runWarp)

{- | The Dredger worker mode: a probe-only HTTP server over the vetted mirror stores. It deletes
from each mount's mirror target, so the boot's own pass refuses an endpoint another role holds.
-}
runDredger :: BootEnv -> IO ()
runDredger bootEnv = do
    let validated = bpValidated (beBootPlan bootEnv)
    traverse_ (dredgerLog . vettedStoreLine) (Map.toAscList (vpMirrorStores validated))
    maintenance <- resolveStoreMaintenance (vpMirrorStores validated) >>= orExit renderBootErrors
    traverse_ (dredgerLog <=< backendLine) (Map.toAscList maintenance)
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

{- One boot line per store, naming the backend that would delete from it and what such a
delete would destroy, because a store that refills itself is not worth sweeping. -}
backendLine :: (Ecosystem, StoreMaintenance) -> IO Text
backendLine (eco, handle) = do
    verdict <- classifyStore handle
    pure
        ( "mount \""
            <> ecosystemName eco
            <> "\": maintenance backend "
            <> factBackend (storeFacts handle)
            <> ", store "
            <> either unclassified renderStoreClass verdict
        )
  where
    unclassified fault = "unclassified: " <> tfDetail (faultTransport fault)
