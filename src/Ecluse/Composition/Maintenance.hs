-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Vet store backends, then build the boot role's maintenance or observation capabilities.
The preview observes the declared private cache without constructing a deletion handle.
-}
module Ecluse.Composition.Maintenance (
    -- * The config-decidable half
    ClearedBackend (..),
    ClearedControl (..),
    ClearedProtocolStore (..),
    ResolveMaintenanceAdapter,
    vetStoreBackends,
    vetPreviewCaches,

    -- * The environment-dependent half
    StorePorts (..),
    BuildStoreMaintenance,
    BuildStoreObservation,
    StoreBuilds (..),
    storeBuilds,
    buildStoreMaintenance,
    buildStoreObservation,
    planStoreMaintenance,
    planStoreMaintenanceFor,
) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Validation (eitherToValidation, validationToEither)

import Ecluse.Composition.BootError (
    BootError (StoreMaintenanceUnavailable),
    StoreMaintenanceReason (ClientBuildFailed, DeletionNotPermitted, NoControlPlane, NoProtocolMaintenance, PrivateCacheUnavailable),
    refuseOnThrow,
 )
import Ecluse.Composition.Credential (CredentialProviders, CredentialTarget (..), lookupTargetProvider)
import Ecluse.Composition.Sizing (newPooledManager)
import Ecluse.Composition.Types (RegistryRole (MirrorPreviewer, MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (Severity (Ignore, Refuse), Vet, rule, withRole)
import Ecluse.Config (
    ControlPlane (ControlCodeArtifact, ControlNone, ControlProtocol),
    DeletionConsent (DeletionPermitted, DeletionWithheld),
    MirrorTarget (MirrorTarget, mtBackend, mtUrl),
    Mount (mountRegistries),
    MountConfig (mntPrivateUpstream),
    MountMap,
    StoreBackend,
    StoreTag (TagCodeArtifact, TagRegistry, TagVerdaccio),
    Target (tgtTag, tgtUrl),
    regMirrorTarget,
    sbControl,
    sbTag,
    storeTagName,
 )
import Ecluse.Config.Resolve (mountKeyRef)
import Ecluse.Config.Target (resolvePrivateBackend)
import Ecluse.Core.Credential (ClientCredential, CredentialProvider, Secret, bareCredential, mintSecret)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Fault (TransportCause (TransportProtocol), transportFault)
import Ecluse.Core.Registry (FetchFault (FetchTransport))
import Ecluse.Core.Registry.Adapter (
    RegistryAdapter,
    adapterMaintenance,
    adapterMetadata,
    adapterPublish,
    publishCodec,
 )
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterMaintenance (maintenanceAlphabet, maintenanceListing, maintenanceVersionDelete),
    AdapterMetadata (metadataFetchManifest),
    ManifestFetch,
    StoreListing,
    VersionDelete,
 )
import Ecluse.Core.Registry.Maintenance (
    NameAlphabet,
    StoreMaintenance,
    StoreManifestRead,
    StoreObservation,
    noNameAlphabet,
    storeFaultOfMetadata,
 )
import Ecluse.Core.Registry.Maintenance.Protocol (
    ProtocolRead (..),
    ProtocolStore (ProtocolStore, psDelete, psRead),
    newProtocolMaintenance,
    newProtocolObservation,
 )
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataFetch))
import Ecluse.Core.Registry.Origin (OriginClient, originClient)
import Ecluse.Core.Registry.Publish (PublishCodec)
import Ecluse.Core.Security (Limits (maxVersionCount))
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Runtime.Maintenance.CodeArtifact (newCodeArtifactMaintenance, newCodeArtifactObservation)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore)

{- | A store the deleting role's pass cleared, one arm per backend kind. Only 'vetStoreBackends'
issues one, so a handle that can delete is built for no store that pass did not clear.
-}
data ClearedBackend = ClearedBackend
    { cbUrl :: RegistryUrl
    -- ^ Where the store answers, which is both what a sweep reads and what it deletes from.
    , cbAlphabet :: NameAlphabet
    -- ^ The characters a full walk partitions this store's names by.
    , cbFetchManifest :: ManifestFetch
    {- ^ The mount ecosystem's own manifest read, which the root leads over the store's endpoint
    rather than over the public upstream.
    -}
    , cbControl :: ClearedControl
    -- ^ The backend control plane.
    }

-- | The control plane a cleared store offers, one arm per backend kind.
data ClearedControl
    = -- | A CodeArtifact repository, deleted through the vendor's own control plane.
      ClearedCodeArtifact CodeArtifactStore
    | -- | A store with no vendor control plane, deleted through the ecosystem protocol.
      ClearedProtocol ClearedProtocolStore

-- | Vetted protocol operations and target-local authentication. An absent token means anonymous reads.
data ClearedProtocolStore = ClearedProtocolStore
    { cpsToken :: Maybe Secret
    , cpsTag :: StoreTag
    -- ^ The tag the store was declared under, which names the backend and its consent key.
    , cpsConsent :: DeletionConsent
    -- ^ What the operator wrote under that key, which the handle's own verdict reads.
    , cpsConsentKey :: Text
    , cpsListing :: StoreListing
    , cpsDelete :: VersionDelete
    , cpsCodec :: PublishCodec
    }

{- | How the pass resolves a mount's ecosystem to the adapter this build ships, injected so a
spec drives the protocol rule over an adapter that fills no maintenance slice.
-}
type ResolveMaintenanceAdapter = Ecosystem -> Maybe RegistryAdapter

{- | The rule every declared mirror target meets: its resolved backend offers a control plane this
build can sweep. Both store roles refuse a target that fails it, and a writing role ignores it.
-}
vetStoreBackends :: ResolveMaintenanceAdapter -> MountMap -> Vet (Map Ecosystem ClearedBackend)
vetStoreBackends resolveAdapter mounts = withRole $ \role ->
    let resolved = resolvedFor role
     in clearedFor role resolved <$ traverse_ (rule severity unmaintained) resolved
  where
    resolvedFor role =
        [ (eco, sweepableStore role (resolveAdapter eco) eco target)
        | (eco, mount) <- Map.toAscList mounts
        , Just target <- [regMirrorTarget (mountRegistries mount)]
        ]

    severity = \case
        MirrorPruner -> Refuse (uncurry StoreMaintenanceUnavailable)
        MirrorPreviewer -> Refuse (uncurry StoreMaintenanceUnavailable)
        MirrorWriter -> Ignore

    unmaintained (eco, outcome) = (eco,) <$> leftToMaybe outcome

    -- A refused pass yields no plan, so a target the rule refused never reaches this map.
    clearedFor role resolved = case role of
        MirrorWriter -> Map.empty
        MirrorPruner -> swept resolved
        MirrorPreviewer -> swept resolved

    swept resolved = Map.fromList [(eco, backend) | (eco, Right backend) <- resolved]

-- | Vet declared private caches only for preview, using the existing backend capability checks.
vetPreviewCaches :: ResolveMaintenanceAdapter -> Map Ecosystem MountConfig -> MountMap -> Vet (Map Ecosystem (Maybe StoreBackend, ClearedBackend))
vetPreviewCaches resolveAdapter configured mounts = withRole $ \case
    MirrorWriter -> pure Map.empty
    MirrorPruner -> pure Map.empty
    MirrorPreviewer ->
        Map.fromList [(eco, backend) | (eco, Right backend) <- resolved]
            <$ traverse_ (rule (const (Refuse (uncurry StoreMaintenanceUnavailable))) unmaintained) resolved
  where
    resolved =
        [ (eco, resolve eco target)
        | (eco, mount) <- Map.toAscList mounts
        , isJust (regMirrorTarget (mountRegistries mount))
        , Just config <- [Map.lookup eco configured]
        , Just target <- [mntPrivateUpstream config]
        ]
    unmaintained (eco, outcome) = (eco,) <$> leftToMaybe outcome
    resolve eco target = case tgtTag target of
        TagCodeArtifact -> do
            backend <- first (PrivateCacheUnavailable . show) (resolvePrivateBackend eco target)
            cleared <- sweepableStore MirrorPreviewer (resolveAdapter eco) eco (MirrorTarget (tgtUrl target) backend)
            pure (Just backend, cleared)
        TagRegistry -> Left (PrivateCacheUnavailable "registry has no inventory control plane")
        TagVerdaccio -> do
            store <-
                protocolStoreFor
                    (resolveAdapter eco)
                    TagVerdaccio
                    Nothing
                    DeletionWithheld
                    "privateUpstream.verdaccio declares no deletion consent. This preview reads anonymously"
            pure (Nothing, clearedBackend (tgtUrl target) (resolveAdapter eco) (ClearedProtocol store))

{- Whether this role's boot needs the operator's own deletion key in hand. A preview reads the
store and changes nothing, so the key is a finding it reports rather than one it refuses on. -}
refusesWithoutConsent :: RegistryRole -> Bool
refusesWithoutConsent = \case
    MirrorWriter -> False
    MirrorPruner -> True
    MirrorPreviewer -> False

-- The store a resolved backend lets the Dredger reach, or why this build reaches none.
sweepableStore :: RegistryRole -> Maybe RegistryAdapter -> Ecosystem -> MirrorTarget -> Either StoreMaintenanceReason ClearedBackend
sweepableStore role mAdapter eco target = clearedBackend (mtUrl target) mAdapter <$> control
  where
    backend = mtBackend target
    control = case sbControl backend of
        ControlCodeArtifact store -> Right (ClearedCodeArtifact store)
        ControlNone -> Left (NoControlPlane (sbTag backend))
        ControlProtocol token consent -> do
            when (consent == DeletionWithheld && refusesWithoutConsent role) (Left (DeletionNotPermitted (sbTag backend)))
            ClearedProtocol <$> protocolStoreFor mAdapter (sbTag backend) (Just token) consent (consentDescriptor eco (sbTag backend))

clearedBackend :: RegistryUrl -> Maybe RegistryAdapter -> ClearedControl -> ClearedBackend
clearedBackend url adapter control =
    ClearedBackend
        { cbUrl = url
        , cbAlphabet = maybe noNameAlphabet (maintenanceAlphabet . adapterMaintenance) adapter
        , cbFetchManifest = maybe absentManifestRead (metadataFetchManifest . adapterMetadata) adapter
        , cbControl = control
        }

protocolStoreFor :: Maybe RegistryAdapter -> StoreTag -> Maybe Secret -> DeletionConsent -> Text -> Either StoreMaintenanceReason ClearedProtocolStore
protocolStoreFor mAdapter tag token consent descriptor = do
    adapter <- maybeToRight NoProtocolMaintenance mAdapter
    listing <- maybeToRight NoProtocolMaintenance (maintenanceListing (adapterMaintenance adapter))
    delete <- maybeToRight NoProtocolMaintenance (maintenanceVersionDelete (adapterMaintenance adapter))
    publish <- maybeToRight NoProtocolMaintenance (adapterPublish adapter)
    pure
        ClearedProtocolStore
            { cpsToken = token
            , cpsTag = tag
            , cpsConsent = consent
            , cpsConsentKey = descriptor
            , cpsListing = listing
            , cpsDelete = delete
            , cpsCodec = publishCodec publish
            }

-- The read a store cleared without an adapter would make, which no boot reaches.
absentManifestRead :: ManifestFetch
absentManifestRead _ _ _ =
    pure (Left (MetadataFetch (FetchTransport (transportFault TransportProtocol absentAdapterDetail))))

absentAdapterDetail :: Text
absentAdapterDetail = "this build serves the mount's ecosystem no metadata read"

-- | Per-target tracing and authentication, resolved before the store handles are built.
data StorePorts = StorePorts
    { spTracing :: TracingPort
    -- ^ The tracing port the manifest read is bracketed by.
    , spCredential :: Maybe CredentialProvider
    -- ^ The credential for this exact target. An anonymous protocol observation holds none.
    }

{- | How a boot builds one store's maintenance handle, under the response bound the plan resolved.
Injected, as the queue builder is, so a spec drives the pruner's arm without an AWS identity.
-}
type BuildStoreMaintenance = StorePorts -> Limits -> ClearedBackend -> IO StoreMaintenance

{- | How a boot builds the observing calls for a cleared store, under the same bound. Nothing it
builds can delete, write a marker, or publish.
-}
type BuildStoreObservation = StorePorts -> Limits -> ClearedBackend -> IO StoreObservation

{- | The two builds a boot chooses between, one per authority a Dredger role holds. The role picks
its own, so a preview's boot never runs the build that holds a delete.
-}
data StoreBuilds = StoreBuilds
    { sbDeleting :: BuildStoreMaintenance
    , sbObserving :: BuildStoreObservation
    }

-- | The shipped pair.
storeBuilds :: StoreBuilds
storeBuilds = StoreBuilds{sbDeleting = buildStoreMaintenance, sbObserving = buildStoreObservation}

{- | The live handle for a cleared store. CodeArtifact discovers its credentials the standard AWS
way, and both arms read and dial over one manager of the store's own.
-}
buildStoreMaintenance :: BuildStoreMaintenance
buildStoreMaintenance ports limits cleared = do
    (readManifest, manager) <- storeAccess ports limits cleared
    case cbControl cleared of
        ClearedCodeArtifact store -> newCodeArtifactMaintenance (cbAlphabet cleared) readManifest store
        ClearedProtocol store ->
            pure (newProtocolMaintenance (protocolStore limits cleared store readManifest manager))

{- | The observing calls for a cleared store, built from the backend's own read capability rather
than from a handle with its writes taken away.
-}
buildStoreObservation :: BuildStoreObservation
buildStoreObservation ports limits cleared = do
    (readManifest, manager) <- storeAccess ports limits cleared
    case cbControl cleared of
        ClearedCodeArtifact store -> newCodeArtifactObservation (maxVersionCount limits) (cbAlphabet cleared) readManifest store
        ClearedProtocol store ->
            pure (newProtocolObservation (protocolRead limits cleared store readManifest manager))

-- One manager of the store's own, and the manifest read that leads over it.
storeAccess :: StorePorts -> Limits -> ClearedBackend -> IO (StoreManifestRead, Manager)
storeAccess ports limits cleared = do
    manager <- storeManager
    pure (storeManifestRead ports limits cleared manager, manager)

{- One package's metadata as the store serves it, through the ecosystem's own codec. The token is
minted per read, because a store that mints its own hands out a short-lived one. -}
storeManifestRead :: StorePorts -> Limits -> ClearedBackend -> Manager -> StoreManifestRead
storeManifestRead ports limits cleared manager name = do
    token <- traverse mintSecret (spCredential ports)
    -- A minted store token carries no username: the store's own control plane, not a caller's.
    first storeFaultOfMetadata
        <$> cbFetchManifest cleared (spTracing ports) (storeOrigin limits cleared manager (bareCredential <$> token)) name

storeOrigin :: Limits -> ClearedBackend -> Manager -> Maybe ClientCredential -> OriginClient
storeOrigin limits cleared manager = originClient limits manager (cbUrl cleared)

{- The maintenance calls are not the proxy's data plane, so this manager carries none of its
tracing, exactly as the vendor client's own does not. -}
storeManager :: IO Manager
storeManager = newPooledManager storeConnections tlsManagerSettings

-- One store, swept package by package, so the pool holds what one in-flight request needs.
storeConnections :: Int
storeConnections = 4

protocolStore :: Limits -> ClearedBackend -> ClearedProtocolStore -> StoreManifestRead -> Manager -> ProtocolStore
protocolStore limits cleared store readManifest manager =
    ProtocolStore
        { psRead = protocolRead limits cleared store readManifest manager
        , psDelete = cpsDelete store
        }

protocolRead :: Limits -> ClearedBackend -> ClearedProtocolStore -> StoreManifestRead -> Manager -> ProtocolRead
protocolRead limits cleared store readManifest manager =
    ProtocolRead
        { prOrigin = storeOrigin limits cleared manager (bareCredential <$> cpsToken store)
        , prReadManifest = readManifest
        , prListing = cpsListing store
        , prCodec = cpsCodec store
        , prBackendName = storeTagName (cpsTag store)
        , prPermitDeletion = cpsConsent store == DeletionPermitted
        , prConsentDescriptor = cpsConsentKey store
        }

{- The key an operator sets, which the store's own withheld verdict names. The deleting role's
pass refuses a store without it, and its preview reports the verdict instead. -}
consentDescriptor :: Ecosystem -> StoreTag -> Text
consentDescriptor eco tag =
    "set "
        <> mountKeyRef eco ("mirrorTarget." <> storeTagName tag <> ".permitDeletion")
        <> " to true: the Dredger deletes nothing from a store that does not carry it"

{- | Build the booting role's own capabilities for each cleared store, or every refusal the live
environment earns. The builds accumulate, so one launch reports every store that cannot be built.
-}
planStoreMaintenance ::
    (StorePorts -> Limits -> ClearedBackend -> IO store) ->
    TracingPort ->
    CredentialProviders ->
    Limits ->
    Map Ecosystem ClearedBackend ->
    IO (Either [BootError] (Map Ecosystem store))
planStoreMaintenance = planStoreMaintenanceFor MirrorCredential

-- | Plan a target slot with its own credentials and accumulate all backend construction refusals.
planStoreMaintenanceFor ::
    CredentialTarget ->
    (StorePorts -> Limits -> ClearedBackend -> IO store) ->
    TracingPort ->
    CredentialProviders ->
    Limits ->
    Map Ecosystem ClearedBackend ->
    IO (Either [BootError] (Map Ecosystem store))
planStoreMaintenanceFor target build tracing credentials limits backends =
    validationToEither . traverse eitherToValidation <$> Map.traverseWithKey planOne backends
  where
    planOne eco backend = refuseOnThrow (StoreMaintenanceUnavailable eco . reason) (build (portsFor eco) limits backend)
    reason = case target of
        MirrorCredential -> ClientBuildFailed
        PrivateCacheCredential -> PrivateCacheUnavailable . ("client build failed: " <>)
    portsFor eco = StorePorts{spTracing = tracing, spCredential = lookupTargetProvider target eco credentials}
