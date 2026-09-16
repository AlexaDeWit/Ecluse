-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The pure boot pass accumulates refusals and advisories for each role.
The composition root builds only from 'ValidatedPlan'. Unvetted settings remain on 'vpSettings'.
-}
module Ecluse.Composition.Validate (
    -- * The validate phase
    ValidatedPlan (vpMounts, vpPublications, vpMirrorStores, vpPrivateCaches, vpSettings),
    vetBoot,

    -- * What it clears
    VettedMount (vmEcosystem, vmAdapter, vmMount, vmConfig),
    VettedPublication (vpubTarget, vpubFirstParty, vpubStaticToken),
) where

import Data.Map.Strict qualified as Map

import Ecluse.Composition.BootError (
    Advisory (DredgerQuotaOverrideUnmatched),
    BootError (
        AdvisoryDenyWithoutStore,
        DredgerChunkPauseBeneathFloor,
        FirstPartyMissing,
        FirstPartyWithoutPrivateUpstream,
        MirrorTargetWithoutPublish,
        MissingAdapter,
        PublicationTargetWithoutPublish,
        PublishStaticCredentialNeedsEdge
    ),
 )
import Ecluse.Composition.Endpoints (
    PublicationTarget,
    VettedEndpoints (vePublicationTargets),
    vetEndpoints,
 )
import Ecluse.Composition.Maintenance (ClearedBackend, overrideKey, vetPrivateCaches, vetStoreBackends)
import Ecluse.Composition.Types (RegistryRole (MirrorPreviewer, MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (Severity (Advise, Ignore, Refuse), Vet, rule)
import Ecluse.Config (
    AdvisoriesSettings (advUrl),
    AppConfig (cfgAdvisories, cfgDredger, cfgMounts, cfgServer),
    Config (configApp, configMounts),
    DredgerSettings (drgChunkPause, drgQuotaOverrides),
    FirstParty,
    MirrorTarget (mtUrl),
    Mount,
    MountConfig (mntFirstParty, mntPrivateUpstream, mntPublicationTarget),
    PrivateEndpoint (preTarget),
    PublicationEndpoint (peTarget, peToken),
    ServerSettings (srvAuthToken),
    StoreBackend,
    StoreTag,
    Target (tgtTag, tgtUrl),
    mountAdvisoryDenials,
    mountRegistries,
    regMirrorTarget,
 )
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Registry.Adapter (RegistryAdapter, adapterFor, adapterPublish)
import Ecluse.Core.Registry.Sweep.Types (minimumChunkPause)
import Ecluse.Core.Security.Egress (registryUrlText)

{- | What the pure boot pass cleared: the mounts a role may serve, the endpoints it may use, and
the settings no rule vets.
-}
data ValidatedPlan = ValidatedPlan
    { vpMounts :: [VettedMount]
    -- ^ Every active mount, in ascending ecosystem order, with the adapter that serves it.
    , vpPublications :: Map Ecosystem VettedPublication
    -- ^ Each mount's cleared publish path, absent where the mount declares no target.
    , vpMirrorStores :: Map Ecosystem ClearedBackend
    {- ^ The backend for each store a sweep may delete from. Only @ecluse dredger@'s pass
    clears one.
    -}
    , vpPrivateCaches :: Map Ecosystem (Maybe StoreBackend, ClearedBackend)
    -- ^ Private caches cleared for this role with their own credential plans.
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

{- | A mount's cleared publish path: the vetted endpoint, the first-party namespaces the
anti-shadowing guard enforces, and the static credential the inbound edge gate covers.
-}
data VettedPublication = VettedPublication
    { vpubTarget :: PublicationTarget
    , vpubFirstParty :: FirstParty
    , vpubStaticToken :: Maybe Secret
    }

-- | Accumulate every pure refusal and advisory for one role before constructing its plan.
vetBoot :: Config -> Vet ValidatedPlan
vetBoot config =
    assemble
        <$> vetMounts config
        <*> vetPublishPolicy app
        <*> vetEndpoints (cfgMounts app)
        <*> vetStoreBackends adapterFor (configMounts config)
        <*> vetPrivateCaches adapterFor (cfgMounts app) (configMounts config)
        <* vetSweepPacing app
        <* vetAdvisoryStore config
        <* vetQuotaOverrides config
  where
    app = configApp config

    assemble mounts policies endpoints backends caches =
        ValidatedPlan
            { vpMounts = mounts
            , vpPublications = Map.intersectionWith cleared (vePublicationTargets endpoints) policies
            , vpMirrorStores = backends
            , vpPrivateCaches = caches
            , vpSettings = app
            }

    cleared target (firstParty, staticToken) = VettedPublication target firstParty staticToken

{- The floor under the sweep's own pace. Deletion is permanent, so both store roles refuse a
pause that would sweep faster than an operator can stop it, and every other role reads none. -}
vetSweepPacing :: AppConfig -> Vet ()
vetSweepPacing app = rule severity beneathFloor (drgChunkPause (cfgDredger app))
  where
    severity = \case
        MirrorPruner -> Refuse (`DredgerChunkPauseBeneathFloor` minimumChunkPause)
        MirrorPreviewer -> Refuse (`DredgerChunkPauseBeneathFloor` minimumChunkPause)
        MirrorWriter -> Ignore

    beneathFloor configured = configured <$ guard (configured < minimumChunkPause)

{- An advisory deny cannot decide without a database, whatever its onUnavailable says, so every
role that evaluates rules refuses the pairing rather than serving a mount that denies everything. -}
vetAdvisoryStore :: Config -> Vet ()
vetAdvisoryStore config =
    traverse_ (rule (const (Refuse (uncurry AdvisoryDenyWithoutStore))) denyingWithoutStore) mounts
  where
    stored = isJust (advUrl (cfgAdvisories (configApp config)))
    mounts = Map.toAscList (configMounts config)

    denyingWithoutStore (eco, mount)
        | stored = Nothing
        | otherwise = (,) eco <$> nonEmpty (mountAdvisoryDenials mount)

{- A declared capacity that matches no store paces nothing. It advises rather than refuses,
because an endpoint renamed under a running Dredger would otherwise stop the role outright. -}
vetQuotaOverrides :: Config -> Vet ()
vetQuotaOverrides config = traverse_ (rule severity unmatched) declaredKeys
  where
    severity = \case
        MirrorPruner -> Advise DredgerQuotaOverrideUnmatched
        MirrorPreviewer -> Advise DredgerQuotaOverrideUnmatched
        MirrorWriter -> Ignore
    declaredKeys = Map.keys (drgQuotaOverrides (cfgDredger (configApp config)))
    unmatched key = key <$ guard (overrideKey key `notElem` storeKeys)
    storeKeys = map overrideKey (declaredStoreUrls config)

-- Every store URL a mount declares as a sweep target: its mirror target and its private cache.
declaredStoreUrls :: Config -> [Text]
declaredStoreUrls config =
    [registryUrlText (mtUrl target) | mount <- Map.elems (configMounts config), Just target <- [regMirrorTarget (mountRegistries mount)]]
        <> [registryUrlText (tgtUrl (preTarget endpoint)) | mcfg <- Map.elems (cfgMounts (configApp config)), Just endpoint <- [mntPrivateUpstream mcfg]]

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
    vetted
        <$ rule (const (Refuse MissingAdapter)) unservedEcosystem eco
        <* rule (const (Refuse MirrorTargetWithoutPublish)) (declaredWithoutPublish mirrors) eco
        <* rule (const (Refuse PublicationTargetWithoutPublish)) (declaredWithoutPublish publishes) eco
        <* rule (const (Refuse FirstPartyWithoutPrivateUpstream)) firstPartyWithoutPrivateUpstream (eco, mcfg)
  where
    vetted = adapterFor eco <&> \adapter -> VettedMount eco adapter mount mcfg

    unservedEcosystem e
        | isNothing (adapterFor e) = Just e
        | otherwise = Nothing

    mirrors = isJust (regMirrorTarget (mountRegistries mount))
    publishes = isJust (mntPublicationTarget mcfg)

    declaredWithoutPublish declared e = do
        guard declared
        adapter <- adapterFor e
        guard (isNothing (adapterPublish adapter))
        pure e

firstPartyWithoutPrivateUpstream :: (Ecosystem, MountConfig) -> Maybe Ecosystem
firstPartyWithoutPrivateUpstream (eco, mcfg) =
    eco <$ guard (isJust (mntFirstParty mcfg) && isNothing (mntPrivateUpstream mcfg))

{- The two couplings a declared publication target carries: the first-party namespaces the
guard enforces, and the inbound edge a static publish credential needs. -}
vetPublishPolicy :: AppConfig -> Vet (Map Ecosystem (FirstParty, Maybe Secret))
vetPublishPolicy app =
    Map.fromList . catMaybes <$> traverse (vetPublication (srvAuthToken (cfgServer app))) publishingMounts
  where
    publishingMounts =
        [ (eco, mcfg)
        | (eco, mcfg) <- Map.toAscList (cfgMounts app)
        , isJust (mntPublicationTarget mcfg)
        ]

vetPublication :: Maybe Secret -> (Ecosystem, MountConfig) -> Vet (Maybe (Ecosystem, (FirstParty, Maybe Secret)))
vetPublication inboundToken subject@(eco, mcfg) =
    cleared
        <$ rule (const (Refuse FirstPartyMissing)) firstPartyMissing subject
        <* rule (const (Refuse (uncurry PublishStaticCredentialNeedsEdge))) (staticWithoutEdge inboundToken) subject
  where
    cleared = mntFirstParty mcfg <&> \firstParty -> (eco, (firstParty, publicationToken mcfg))

publicationToken :: MountConfig -> Maybe Secret
publicationToken mcfg = peToken =<< mntPublicationTarget mcfg

firstPartyMissing :: (Ecosystem, MountConfig) -> Maybe Ecosystem
firstPartyMissing (eco, mcfg)
    | isNothing (mntFirstParty mcfg) = Just eco
    | otherwise = Nothing

{- Any unauthenticated client could otherwise publish within scope under Écluse's own credential.
The tag rides along, because the credential's key path nests below it. -}
staticWithoutEdge :: Maybe Secret -> (Ecosystem, MountConfig) -> Maybe (Ecosystem, StoreTag)
staticWithoutEdge inboundToken (eco, mcfg) = do
    guard (isJust (publicationToken mcfg) && isNothing inboundToken)
    endpoint <- mntPublicationTarget mcfg
    pure (eco, tgtTag (peTarget endpoint))
