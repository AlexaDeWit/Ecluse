-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bind durable mirror inventories and source-only retention to each configured mirror target.
module Ecluse.Composition.MirrorState (
    MirrorStateBinding (..),
    newMirrorStates,
    bindMirrorStates,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Composition.MirrorState.Internal (MirrorStateBinding (..), newMirrorStatesWith)
import Ecluse.Composition.Validate (VettedMount)
import Ecluse.Config (AppConfig)
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Ecosystem (Ecosystem, parseEcosystem)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Server.Cache (CacheConfig)
import Ecluse.Core.Server.Cache.Backend.Local (LocalPool)
import Ecluse.Core.Server.Context (MountBinding (bindingPackumentDeps, bindingPrefix), PackumentDeps (pdMirrorState))
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Mirror.State.S3 (newS3MirrorState)

-- | Reuse the metadata pool's aggregate bounds. Unconfigured mounts allocate no state client or retention.
newMirrorStates :: Maybe AwsEndpoint -> Deadlines -> AppConfig -> [VettedMount] -> Limits -> CacheConfig -> LocalPool -> IO (Map Ecosystem MirrorStateBinding)
newMirrorStates endpoint deadlines app mounts limits cacheConfig pool =
    newMirrorStatesWith (newS3MirrorState endpoint deadlines limits) app mounts limits cacheConfig pool

-- | Attach state only to the matching mount. First-party handling remains in the read pipeline.
bindMirrorStates :: Map Ecosystem MirrorStateBinding -> [MountBinding] -> [MountBinding]
bindMirrorStates states = map bind
  where
    bind mount = mount{bindingPackumentDeps = deps{pdMirrorState = msbView <$> selected}}
      where
        deps = bindingPackumentDeps mount
        prefix :| _ = bindingPrefix mount
        selected = parseEcosystem prefix >>= (`Map.lookup` states)
