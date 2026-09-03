-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The boot's validate phase: one pass over the loaded configuration that yields every pure
refusal and every advisory a role earns, and the plan a cleared configuration reifies from.

The composition root builds from 'ValidatedPlan' alone, so a mount binding, a publish relay, or a
store sweep cannot be assembled out of a configuration this pass never cleared. What no rule
decides stays on 'vpSettings', where reading it raw is honest rather than a hole.
-}
module Ecluse.Composition.Validate (
    -- * The validate phase
    ValidatedPlan (vpMounts, vpPublications, vpMirrorStores, vpSettings),
    vetBoot,

    -- * What it clears
    VettedMount (vmEcosystem, vmAdapter, vmMount, vmConfig),
    VettedPublication (vpubTarget, vpubAllow, vpubStaticToken),
    VettedStore (vsStore, vsBackend),
) where

import Data.Map.Strict qualified as Map

import Ecluse.Composition.BootError (
    BootError (MissingAdapter, PublicationAllowMissing, PublishStaticCredentialNeedsEdge),
 )
import Ecluse.Composition.Endpoints (
    MirrorStore,
    PublicationTarget,
    VettedEndpoints (veMirrorStores, vePublicationTargets),
    vetEndpoints,
 )
import Ecluse.Composition.Maintenance (ClearedBackend, vetStoreBackends)
import Ecluse.Composition.Vet (Severity (Refuse), Vet, rule)
import Ecluse.Config (
    AppConfig (cfgMounts, cfgServer),
    Config (configApp, configMounts),
    Mount,
    MountConfig (mntPublicationAllow, mntPublicationTarget, mntPublicationTargetToken),
    PublicationAllow,
    ServerSettings (srvAuthToken),
 )
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Registry.Adapter (RegistryAdapter, adapterFor)

{- | What the pure boot pass cleared: the mounts a role may serve, the endpoints it may use, and
the settings no rule vets.
-}
data ValidatedPlan = ValidatedPlan
    { vpMounts :: [VettedMount]
    -- ^ Every active mount, in ascending ecosystem order, with the adapter that serves it.
    , vpPublications :: Map Ecosystem VettedPublication
    -- ^ Each mount's cleared publish path, absent where the mount declares no target.
    , vpMirrorStores :: Map Ecosystem VettedStore
    {- ^ The stores a sweep may delete from, each with the backend that sweeps it. Only
    @ecluse dredger@'s pass clears one.
    -}
    , vpSettings :: AppConfig
    {- ^ The settings no rule vets. The mounts it still carries are the raw declarations, and
    'vpMounts' holds the vetted ones the runtime reads.
    -}
    }

-- | One active mount, paired with the adapter this build ships for its ecosystem.
data VettedMount = VettedMount
    { vmEcosystem :: Ecosystem
    , vmAdapter :: RegistryAdapter
    , vmMount :: Mount
    , vmConfig :: MountConfig
    }

{- | A mount's cleared publish path: the vetted endpoint, the allow-list the anti-shadowing guard
enforces, and the static credential the inbound edge gate covers.
-}
data VettedPublication = VettedPublication
    { vpubTarget :: PublicationTarget
    , vpubAllow :: PublicationAllow
    , vpubStaticToken :: Maybe Secret
    }

{- | A store the deleting role's pass cleared: the endpoint no other registry role holds, and the
backend this build sweeps it with.
-}
data VettedStore = VettedStore
    { vsStore :: MirrorStore
    , vsBackend :: ClearedBackend
    }

{- | Vet the whole loaded configuration for one role. The four groups compose with '<*>', so one
run reports every refusal and every advisory rather than the first group's alone.
-}
vetBoot :: Config -> Vet ValidatedPlan
vetBoot config =
    assemble
        <$> vetMounts config
        <*> vetPublishPolicy app
        <*> vetEndpoints (cfgMounts app)
        <*> vetStoreBackends (cfgMounts app)
  where
    app = configApp config

    -- Both groups enumerate the mounts declaring a mirrorTarget, and a target the backend rule
    -- refuses yields no plan at all, so under the deleting role the two maps share a keyset.
    assemble mounts policies endpoints backends =
        ValidatedPlan
            { vpMounts = mounts
            , vpPublications = Map.intersectionWith cleared (vePublicationTargets endpoints) policies
            , vpMirrorStores = Map.intersectionWith VettedStore (veMirrorStores endpoints) backends
            , vpSettings = app
            }

    cleared target (allow, staticToken) = VettedPublication target allow staticToken

{- Every active mount, refusing the ecosystems this build ships no adapter for. Serving one would
answer every route with a stub, which is a wiring fault rather than a posture an operator chose. -}
vetMounts :: Config -> Vet [VettedMount]
vetMounts config = catMaybes <$> traverse vetMount (activeMounts config)

{- 'Ecluse.Config.loadConfig' derives 'configMounts' from 'cfgMounts' entry for entry, so the two
maps share a keyset and this pairing is total. -}
activeMounts :: Config -> [(Ecosystem, (Mount, MountConfig))]
activeMounts config =
    Map.toAscList (Map.intersectionWith (,) (configMounts config) (cfgMounts (configApp config)))

-- 'Nothing' only where the rule refused, and a refused pass yields no plan to carry it into.
vetMount :: (Ecosystem, (Mount, MountConfig)) -> Vet (Maybe VettedMount)
vetMount (eco, (mount, mcfg)) =
    vetted <$ rule (const (Refuse MissingAdapter)) unservedEcosystem eco
  where
    vetted = adapterFor eco <&> \adapter -> VettedMount eco adapter mount mcfg

    unservedEcosystem e
        | isNothing (adapterFor e) = Just e
        | otherwise = Nothing

{- The two couplings a declared publication target carries: the allow-list the anti-shadowing
guard enforces, and the verifiable inbound edge a static publish credential needs. -}
vetPublishPolicy :: AppConfig -> Vet (Map Ecosystem (PublicationAllow, Maybe Secret))
vetPublishPolicy app =
    Map.fromList . catMaybes <$> traverse (vetPublication (srvAuthToken (cfgServer app))) publishingMounts
  where
    publishingMounts =
        [ (eco, mcfg)
        | (eco, mcfg) <- Map.toAscList (cfgMounts app)
        , isJust (mntPublicationTarget mcfg)
        ]

vetPublication :: Maybe Secret -> (Ecosystem, MountConfig) -> Vet (Maybe (Ecosystem, (PublicationAllow, Maybe Secret)))
vetPublication inboundToken subject@(eco, mcfg) =
    cleared
        <$ rule (const (Refuse PublicationAllowMissing)) allowMissing subject
        <* rule (const (Refuse PublishStaticCredentialNeedsEdge)) (staticWithoutEdge inboundToken) subject
  where
    cleared = mntPublicationAllow mcfg <&> \allow -> (eco, (allow, mntPublicationTargetToken mcfg))

allowMissing :: (Ecosystem, MountConfig) -> Maybe Ecosystem
allowMissing (eco, mcfg)
    | isNothing (mntPublicationAllow mcfg) = Just eco
    | otherwise = Nothing

-- Any unauthenticated client could otherwise publish within scope under Écluse's own credential.
staticWithoutEdge :: Maybe Secret -> (Ecosystem, MountConfig) -> Maybe Ecosystem
staticWithoutEdge inboundToken (eco, mcfg)
    | isJust (mntPublicationTargetToken mcfg) && isNothing inboundToken = Just eco
    | otherwise = Nothing
