-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bind durable PyPI inventories and source-only retention to each configured mirror target.
module Ecluse.Composition.MirrorState (
    MirrorStateBinding (..),
    newMirrorStates,
    bindMirrorStates,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Config (
    AdvisoryStoreTarget (S3Store),
    AppConfig (cfgMirrorState, cfgMounts),
    MirrorEndpoint (meUrl),
    MountConfig (mntMirrorTarget),
    advisoryStoreTarget,
 )
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Ecosystem (Ecosystem (PyPI), parseEcosystem)
import Ecluse.Core.Mirror.State (MirrorState, StateTarget, stateTarget)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Cache (CacheConfig (cacheTtl), StoreBudget (StoreBudget))
import Ecluse.Core.Server.Cache.Backend.Local (LocalPool)
import Ecluse.Core.Server.Context (MountBinding (bindingPackumentDeps, bindingPrefix), PackumentDeps (pdMirrorState))
import Ecluse.Core.Server.MirrorView (MirrorView, newMirrorRetention, newMirrorView)
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Mirror.State.S3 (newS3MirrorState)

-- | Foreground and worker roles share one target identity and durable state port.
data MirrorStateBinding = MirrorStateBinding
    { msbStore :: MirrorState
    , msbTarget :: StateTarget
    , msbView :: MirrorView
    }

-- | Reuse the metadata pool's aggregate bounds. Unconfigured mounts allocate no state client or retention.
newMirrorStates :: Maybe AwsEndpoint -> Deadlines -> AppConfig -> Limits -> CacheConfig -> LocalPool -> IO (Map Ecosystem MirrorStateBinding)
newMirrorStates endpoint deadlines app limits cacheConfig pool = case configured of
    Nothing -> pure Map.empty
    Just (storeUrl, mirror) -> do
        let S3Store bucket prefix = advisoryStoreTarget storeUrl
            target = stateTarget (registryUrlText (meUrl mirror))
        store <- newS3MirrorState endpoint deadlines limits bucket (fromMaybe "" prefix) target
        retention <- newMirrorRetention pool (cacheTtl cacheConfig) (StoreBudget 0 0)
        pure (Map.singleton PyPI (MirrorStateBinding store target (newMirrorView limits target store retention)))
  where
    configured = (,) <$> cfgMirrorState app <*> (mntMirrorTarget =<< Map.lookup PyPI (cfgMounts app))

-- | Attach state only to the matching mount. First-party handling remains in the read pipeline.
bindMirrorStates :: Map Ecosystem MirrorStateBinding -> [MountBinding] -> [MountBinding]
bindMirrorStates states = map bind
  where
    bind mount = mount{bindingPackumentDeps = deps{pdMirrorState = msbView <$> selected}}
      where
        deps = bindingPackumentDeps mount
        prefix :| _ = bindingPrefix mount
        selected = parseEcosystem prefix >>= (`Map.lookup` states)
