-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Allocate state for vetted mirror capabilities, with storage construction supplied by the application edge.
module Ecluse.Composition.MirrorState.Internal (
    MirrorStateBinding (..),
    newMirrorStatesWith,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Composition.Validate (VettedMount (vmAdapter, vmConfig, vmEcosystem))
import Ecluse.Config (
    AdvisoryStoreTarget (S3Store),
    AppConfig (cfgMirrorState),
    MirrorEndpoint (meUrl),
    MountConfig (mntMirrorTarget),
    advisoryStoreTarget,
 )
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Mirror.State (MirrorState, StateTarget, stateTarget)
import Ecluse.Core.Registry.Adapter (adapterPublish)
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (publishMirrorRequirements), MirrorRequirements (mirrorStateRequirement), durableMirrorDocuments)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Cache (CacheConfig (cacheTtl), StoreBudget (StoreBudget))
import Ecluse.Core.Server.Cache.Backend.Local (LocalPool)
import Ecluse.Core.Server.MirrorView (MirrorView, newMirrorRetention, newMirrorView)

-- | Foreground and worker roles share one target identity and durable state port.
data MirrorStateBinding = MirrorStateBinding
    { msbStore :: MirrorState
    , msbTarget :: StateTarget
    , msbView :: MirrorView
    }

-- | Allocate only declared durable mirrors. The caller owns the shared retention pool and storage constructor.
newMirrorStatesWith :: (Text -> Text -> StateTarget -> IO MirrorState) -> AppConfig -> [VettedMount] -> Limits -> CacheConfig -> LocalPool -> IO (Map Ecosystem MirrorStateBinding)
newMirrorStatesWith allocate app mounts limits cacheConfig pool = case cfgMirrorState app of
    Nothing -> pure Map.empty
    Just storeUrl -> fmap Map.fromList . forM configured $ \(eco, documents, mirror) -> do
        let S3Store bucket prefix = advisoryStoreTarget storeUrl
            target = stateTarget (registryUrlText (meUrl mirror))
        store <- allocate bucket (fromMaybe "" prefix) target
        retention <- newMirrorRetention pool (cacheTtl cacheConfig) (StoreBudget 0 0)
        pure (eco, MirrorStateBinding store target (newMirrorView limits target store retention documents))
  where
    configured =
        [ (vmEcosystem mount, documents, mirror)
        | mount <- mounts
        , Just publish <- [adapterPublish (vmAdapter mount)]
        , Just documents <- [durableMirrorDocuments (mirrorStateRequirement (publishMirrorRequirements publish))]
        , Just mirror <- [mntMirrorTarget (vmConfig mount)]
        ]
