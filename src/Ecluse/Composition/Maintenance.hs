-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's store maintenance build, split across the boot's two tiers. The pure
half reads each mount's resolved store backend ("Ecluse.Config.Target") and its ecosystem adapter
as one rule in the vetting pass, so @ecluse dredger@ refuses a store this build cannot sweep and
@ecluse check-config@ names that refusal. Only that pass issues a 'ClearedBackend', and the
effectful half, in the pruner's arm of the planning phase ("Ecluse.Composition.Executable"),
builds a handle from that witness alone.
-}
module Ecluse.Composition.Maintenance (
    -- * The config-decidable half
    ClearedBackend (..),
    ClearedProtocolStore (..),
    ResolveMaintenanceAdapter,
    vetStoreBackends,

    -- * The environment-dependent half
    BuildStoreMaintenance,
    buildStoreMaintenance,
    planStoreMaintenance,
) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Validation (eitherToValidation, validationToEither)

import Ecluse.Composition.BootError (
    BootError (StoreMaintenanceUnavailable),
    StoreMaintenanceReason (ClientBuildFailed, DeletionNotPermitted, NoControlPlane, NoProtocolMaintenance),
    refuseOnThrow,
 )
import Ecluse.Composition.Sizing (newPooledManager)
import Ecluse.Composition.Types (RegistryRole (MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (Severity (Ignore, Refuse), Vet, rule, vetRole)
import Ecluse.Config (
    ControlPlane (ControlCodeArtifact, ControlNone, ControlProtocol),
    DeletionConsent (DeletionWithheld),
    MirrorTarget (mtBackend, mtUrl),
    Mount (mountRegistries),
    MountMap,
    StoreBackend,
    StoreTag,
    regMirrorTarget,
    sbControl,
    sbTag,
    storeTagName,
 )
import Ecluse.Config.Resolve (mountKeyRef)
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Registry.Adapter (
    RegistryAdapter,
    adapterMaintenance,
    adapterPublish,
    publishCodec,
 )
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterMaintenance (maintenanceListing, maintenanceVersionDelete),
    StoreListing,
    VersionDelete,
 )
import Ecluse.Core.Registry.Maintenance (StoreMaintenance)
import Ecluse.Core.Registry.Maintenance.Protocol (ProtocolStore (..), newProtocolMaintenance)
import Ecluse.Core.Registry.Origin (OriginClient (OriginClient, ocBaseUrl, ocLimits, ocManager, ocToken))
import Ecluse.Core.Registry.Publish (PublishCodec)
import Ecluse.Core.Security (Limits)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Runtime.Maintenance.CodeArtifact (newCodeArtifactMaintenance)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore)

{- | A store the deleting role's pass cleared, one arm per backend kind. Only 'vetStoreBackends'
issues one, so a handle that can delete is built for no store that pass did not clear.
-}
data ClearedBackend
    = -- | A CodeArtifact repository, deleted through the vendor's own control plane.
      ClearedCodeArtifact CodeArtifactStore
    | -- | A store with no vendor control plane, deleted through the ecosystem protocol.
      ClearedProtocol ClearedProtocolStore

{- | Everything the protocol leaf needs but the live environment: the endpoint, its write
credential, the operator's consent, and the ecosystem verbs the pass proved present.
-}
data ClearedProtocolStore = ClearedProtocolStore
    { cpsUrl :: RegistryUrl
    , cpsToken :: Secret
    , cpsTag :: StoreTag
    -- ^ The tag the store was declared under, which names the backend and its consent key.
    , cpsEcosystem :: Ecosystem
    , cpsListing :: StoreListing
    , cpsDelete :: VersionDelete
    , cpsCodec :: PublishCodec
    }

{- | How the pass resolves a mount's ecosystem to the adapter this build ships, injected so a
spec drives the protocol rule over an adapter that fills no maintenance slice.
-}
type ResolveMaintenanceAdapter = Ecosystem -> Maybe RegistryAdapter

{- | The rule every declared mirror target meets: its resolved backend offers a control plane this
build can sweep. The deleting role refuses a target that fails it, and a writing role ignores it.
-}
vetStoreBackends :: ResolveMaintenanceAdapter -> MountMap -> Vet (Map Ecosystem ClearedBackend)
vetStoreBackends resolveAdapter mounts = clearedFor <$> vetRole <* traverse_ (rule severity unmaintained) resolved
  where
    resolved =
        [ (eco, sweepableStore (resolveAdapter eco) eco target)
        | (eco, mount) <- Map.toAscList mounts
        , Just target <- [regMirrorTarget (mountRegistries mount)]
        ]

    severity = \case
        MirrorPruner -> Refuse (uncurry StoreMaintenanceUnavailable)
        MirrorWriter -> Ignore

    unmaintained (eco, outcome) = (eco,) <$> leftToMaybe outcome

    -- A refused pass yields no plan, so a target the rule refused never reaches this map.
    clearedFor = \case
        MirrorWriter -> Map.empty
        MirrorPruner -> Map.fromList [(eco, backend) | (eco, Right backend) <- resolved]

-- The store a resolved backend lets the Dredger delete from, or why this build reaches none.
sweepableStore :: Maybe RegistryAdapter -> Ecosystem -> MirrorTarget -> Either StoreMaintenanceReason ClearedBackend
sweepableStore mAdapter eco target = case sbControl backend of
    ControlCodeArtifact store -> Right (ClearedCodeArtifact store)
    ControlNone -> Left (NoControlPlane (sbTag backend))
    ControlProtocol token consent -> do
        -- Consent is the operator's own key, so it is reported ahead of what this build ships.
        when (consent == DeletionWithheld) (Left (DeletionNotPermitted (sbTag backend)))
        adapter <- maybeToRight NoProtocolMaintenance mAdapter
        listing <- maybeToRight NoProtocolMaintenance (maintenanceListing (adapterMaintenance adapter))
        delete <- maybeToRight NoProtocolMaintenance (maintenanceVersionDelete (adapterMaintenance adapter))
        Right
            ( ClearedProtocol
                ClearedProtocolStore
                    { cpsUrl = mtUrl target
                    , cpsToken = token
                    , cpsTag = sbTag backend
                    , cpsEcosystem = eco
                    , cpsListing = listing
                    , cpsDelete = delete
                    , cpsCodec = publishCodec (adapterPublish adapter)
                    }
            )
  where
    backend :: StoreBackend
    backend = mtBackend target

{- | How a boot builds one store's maintenance handle, under the response bound the plan resolved.
Injected, as the queue builder is, so a spec drives the pruner's arm of the planning phase without
discovering an AWS identity.
-}
type BuildStoreMaintenance = Limits -> ClearedBackend -> IO StoreMaintenance

{- | The live handle for a cleared store. CodeArtifact discovers its credentials the standard AWS
way, and a protocol store dials its endpoint over a manager of its own, as that client does.
-}
buildStoreMaintenance :: BuildStoreMaintenance
buildStoreMaintenance limits = \case
    ClearedCodeArtifact store -> newCodeArtifactMaintenance store
    ClearedProtocol store -> newProtocolMaintenance . protocolStore limits store <$> protocolManager

{- The maintenance control plane is not the data plane, so this manager carries no data-plane
tracing, exactly as the vendor client's own does not. -}
protocolManager :: IO Manager
protocolManager = newPooledManager protocolConnections tlsManagerSettings

-- One store, swept package by package, so the pool holds what one in-flight request needs.
protocolConnections :: Int
protocolConnections = 4

protocolStore :: Limits -> ClearedProtocolStore -> Manager -> ProtocolStore
protocolStore limits store manager =
    ProtocolStore
        { psOrigin =
            OriginClient
                { ocBaseUrl = cpsUrl store
                , ocManager = manager
                , ocToken = Just (cpsToken store)
                , ocLimits = limits
                }
        , psListing = cpsListing store
        , psDelete = cpsDelete store
        , psCodec = cpsCodec store
        , psBackendName = storeTagName (cpsTag store)
        , psPermitDeletion = True
        , psConsentDescriptor = consentDescriptor (cpsEcosystem store) (cpsTag store)
        }

{- The key an operator sets, which the handle's own withheld verdict names. A handle this root
built always carries consent, because the pass above refuses the store that does not. -}
consentDescriptor :: Ecosystem -> StoreTag -> Text
consentDescriptor eco tag =
    "set "
        <> mountKeyRef eco ("mirrorTarget." <> storeTagName tag <> ".permitDeletion")
        <> " to true: the Dredger deletes nothing from a store that does not carry it"

{- | Build one handle per cleared store, or every refusal the live environment earns. The builds
accumulate, so one launch reports every store whose client cannot be built.
-}
planStoreMaintenance ::
    BuildStoreMaintenance ->
    Limits ->
    Map Ecosystem ClearedBackend ->
    IO (Either [BootError] (Map Ecosystem StoreMaintenance))
planStoreMaintenance build limits backends =
    validationToEither . traverse eitherToValidation <$> Map.traverseWithKey planOne backends
  where
    planOne eco backend =
        refuseOnThrow (StoreMaintenanceUnavailable eco . ClientBuildFailed) (build limits backend)
