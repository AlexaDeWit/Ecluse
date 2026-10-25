-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pure role-specific store vetting. Only a successful pass constructs a cleared backend.
module Ecluse.Composition.Maintenance.Plan (
    ClearedBackend (cbUrl, cbAlphabet, cbFetchManifest, cbUsername, cbControl),
    ClearedControl (..),
    ClearedProtocolStore (..),
    ResolveMaintenanceAdapter,
    vetStoreBackends,
    vetPrivateCaches,
    overrideKey,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Composition.BootError (
    BootError (StoreMaintenanceUnavailable),
    StoreMaintenanceReason (DeletionNotPermitted, NoControlPlane, NoProtocolMaintenance, PrivateCacheUnavailable),
 )
import Ecluse.Composition.Types (RegistryRole (MirrorPreviewer, MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (Severity (Ignore, Refuse), Vet, byStoreRole, rule, withRole)
import Ecluse.Config (
    ControlPlane (ControlCodeArtifact, ControlNone, ControlProtocol),
    DeletionConsent (DeletionWithheld),
    MirrorTarget (mtBackend, mtUrl, mtUsername),
    Mount (mountRegistries),
    MountConfig (mntPrivateUpstream),
    MountMap,
    PrivateEndpoint (..),
    StoreBackend (BackendVerdaccio),
    StoreTag (TagCodeArtifact, TagRegistry, TagVerdaccio),
    Target (tgtTag, tgtUrl),
    regMirrorTarget,
    sbControl,
    sbTag,
    storeTagName,
 )
import Ecluse.Config.Resolve (mountKeyRef)
import Ecluse.Config.Target (resolvePrivateBackend)
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Fault (TransportCause (TransportProtocol), transportFault)
import Ecluse.Core.Registry (FetchFault (FetchTransport))
import Ecluse.Core.Registry.Adapter (RegistryAdapter, adapterMaintenance, adapterMetadata, adapterPublish)
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterMaintenance (maintenanceAlphabet, maintenanceListing, maintenanceVersionDelete),
    AdapterMetadata (metadataRead),
    AdapterPublish (publishCodec),
    StoreListing,
    VersionDelete,
 )
import Ecluse.Core.Registry.Maintenance.NameSpace (NameAlphabet, noNameAlphabet)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataFetch))
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (prepareManifest)
import Ecluse.Core.Registry.Metadata.Fetch.Types (ManifestFetch)
import Ecluse.Core.Registry.Publish (PublishCodec)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore)

-- | A store cleared by its role-specific pass. Its constructor stays private.
data ClearedBackend = ClearedBackend
    { cbUrl :: RegistryUrl
    -- ^ Where the store answers, which is both what a sweep reads and what it deletes from.
    , cbAlphabet :: NameAlphabet
    -- ^ The characters a full walk partitions this store's names by.
    , cbFetchManifest :: ManifestFetch
    -- ^ The ecosystem's metadata read, directed at this store.
    , cbUsername :: Maybe Text
    -- ^ The target's resolved credential username, paired with each refreshed secret.
    , cbControl :: ClearedControl
    -- ^ The backend control plane.
    }

-- | The control plane a cleared store offers, one arm per backend kind.
data ClearedControl
    = -- | A CodeArtifact repository, deleted through the vendor's own control plane.
      ClearedCodeArtifact CodeArtifactStore
    | -- | The configured cache permits refill without weakening ordinary mirror classification.
      ClearedCodeArtifactCache CodeArtifactStore
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

-- | Resolve each ecosystem to the adapter whose maintenance capabilities the pass checks.
type ResolveMaintenanceAdapter = Ecosystem -> Maybe RegistryAdapter

-- | Require a sweepable backend for both Dredger modes. Writing roles skip this check.
vetStoreBackends :: ResolveMaintenanceAdapter -> MountMap -> Vet (Map Ecosystem ClearedBackend)
vetStoreBackends resolveAdapter mounts = withRole $ \role ->
    let resolved = resolvedFor role
     in clearedFor role resolved <$ traverse_ (rule severity unmaintainedOf) resolved
  where
    resolvedFor role =
        [ (eco, sweepableStore role (resolveAdapter eco) eco target)
        | (eco, mount) <- Map.toAscList mounts
        , Just target <- [regMirrorTarget (mountRegistries mount)]
        ]

    severity = byStoreRole (Refuse (uncurry StoreMaintenanceUnavailable)) Ignore

    clearedFor role resolved = case role of
        MirrorWriter -> Map.empty
        MirrorPruner -> clearedOf resolved
        MirrorPreviewer -> clearedOf resolved

-- | Vet each private cache under its own backend declaration and maintenance authority.
vetPrivateCaches :: ResolveMaintenanceAdapter -> Map Ecosystem MountConfig -> MountMap -> Vet (Map Ecosystem (Maybe StoreBackend, ClearedBackend))
vetPrivateCaches resolveAdapter configured mounts = withRole $ \case
    MirrorWriter -> pure Map.empty
    MirrorPruner -> clearedFor MirrorPruner
    MirrorPreviewer -> clearedFor MirrorPreviewer
  where
    clearedFor role =
        let resolved = resolvedFor role
         in clearedOf resolved
                <$ traverse_ (rule (const (Refuse (uncurry StoreMaintenanceUnavailable))) unmaintainedOf) resolved
    resolvedFor role =
        [ (eco, resolve role eco endpoint)
        | (eco, mount) <- Map.toAscList mounts
        , isJust (regMirrorTarget (mountRegistries mount))
        , Just config <- [Map.lookup eco configured]
        , Just endpoint <- [mntPrivateUpstream config]
        ]
    resolve role eco endpoint = case tgtTag target of
        TagCodeArtifact -> do
            (backend, store) <- first (PrivateCacheUnavailable . show) (resolvePrivateBackend eco target)
            pure (Just backend, clearedBackend (tgtUrl target) (Just "aws") adapter (ClearedCodeArtifactCache store))
        TagRegistry -> Left (PrivateCacheUnavailable "registry has no inventory control plane")
        TagVerdaccio -> do
            when (refusesWithoutConsent role && preConsent endpoint == DeletionWithheld) $
                Left (PrivateCacheUnavailable (descriptor <> " is not set"))
            when (role == MirrorPruner && isNothing (preToken endpoint)) $
                Left (PrivateCacheUnavailable (mountKeyRef eco "privateUpstream.verdaccio.token" <> " is not set"))
            store <- protocolStoreFor adapter TagVerdaccio (preToken endpoint) (preConsent endpoint) descriptor
            pure (fmap (`BackendVerdaccio` preConsent endpoint) (preToken endpoint), clearedBackend (tgtUrl target) Nothing adapter (ClearedProtocol store))
      where
        target = preTarget endpoint
        adapter = resolveAdapter eco
        descriptor = mountKeyRef eco "privateUpstream.verdaccio.permitDeletion"

-- A resolved entry that failed, named by the ecosystem that declared it.
unmaintainedOf :: (Ecosystem, Either reason a) -> Maybe (Ecosystem, reason)
unmaintainedOf (eco, outcome) = (eco,) <$> leftToMaybe outcome

-- A refused pass yields no plan, so a target the rule refused never reaches this map.
clearedOf :: [(Ecosystem, Either reason a)] -> Map Ecosystem a
clearedOf resolved = Map.fromList [(eco, backend) | (eco, Right backend) <- resolved]

-- A preview reports missing consent instead of refusing boot.
refusesWithoutConsent :: RegistryRole -> Bool
refusesWithoutConsent = \case
    MirrorWriter -> False
    MirrorPruner -> True
    MirrorPreviewer -> False

-- The store a resolved backend lets the Dredger reach, or why this build reaches none.
sweepableStore :: RegistryRole -> Maybe RegistryAdapter -> Ecosystem -> MirrorTarget -> Either StoreMaintenanceReason ClearedBackend
sweepableStore role mAdapter eco target = clearedBackend (mtUrl target) (mtUsername target) mAdapter <$> control
  where
    backend = mtBackend target
    control = case sbControl backend of
        ControlCodeArtifact store -> Right (ClearedCodeArtifact store)
        ControlNone -> Left (NoControlPlane (sbTag backend))
        ControlProtocol token consent -> do
            when (consent == DeletionWithheld && refusesWithoutConsent role) (Left (DeletionNotPermitted (sbTag backend)))
            ClearedProtocol <$> protocolStoreFor mAdapter (sbTag backend) (Just token) consent (consentDescriptor eco (sbTag backend))

clearedBackend :: RegistryUrl -> Maybe Text -> Maybe RegistryAdapter -> ClearedControl -> ClearedBackend
clearedBackend url username adapter control =
    ClearedBackend
        { cbUrl = url
        , cbAlphabet = maybe noNameAlphabet (maintenanceAlphabet . adapterMaintenance) adapter
        , cbFetchManifest = maybe absentManifestRead (prepareManifest . metadataRead . adapterMetadata) adapter
        , cbUsername = username
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

-- | Describe the mirror target consent required for deletion. Preview reports withheld consent instead.
consentDescriptor :: Ecosystem -> StoreTag -> Text
consentDescriptor eco tag =
    "set "
        <> mountKeyRef eco ("mirrorTarget." <> storeTagName tag <> ".permitDeletion")
        <> " to true: the Dredger deletes nothing from a store that does not carry it"

-- | Match capacity declarations without case or trailing-slash differences.
overrideKey :: Text -> Text
overrideKey = T.dropWhileEnd (== '/') . T.toLower . T.strip
