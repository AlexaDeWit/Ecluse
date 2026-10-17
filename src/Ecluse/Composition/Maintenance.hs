-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Vet store backends and build role-specific maintenance, observation and upstream probes.
module Ecluse.Composition.Maintenance (
    -- * The config-decidable half
    ClearedBackend (cbUrl, cbAlphabet, cbFetchManifest),
    ResolveMaintenanceAdapter,
    vetStoreBackends,
    vetPrivateCaches,

    -- * The environment-dependent half
    StorePorts (..),
    BudgetPorts (..),
    storeScope,
    overrideKey,
    resolvedBudget,
    BuildUpstreamProbe,
    StoreBuilds (..),
    storeBuilds,
    buildStoreMaintenance,
    buildStoreObservation,
    buildUpstreamProbe,
    planStoreMaintenance,
    planStoreMaintenanceFor,

    -- * What the private upstream answers about public content
    readUpstreamSafety,
    upstreamFindings,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client (Manager)
import Validation (eitherToValidation, validationToEither)

import Ecluse.Composition.BootError (
    Advisory (PrivateUpstreamUndecided),
    BootError (PrivateUpstreamProbeFailed, PrivateUpstreamUnsafe, StoreMaintenanceUnavailable),
    StoreMaintenanceReason (ClientBuildFailed, DeletionNotPermitted, NoControlPlane, NoProtocolMaintenance, PrivateCacheUnavailable),
    refuseOnThrow,
 )
import Ecluse.Composition.Credential (CredentialProviders, CredentialTarget (..), lookupTargetProvider)
import Ecluse.Composition.Sizing (newPooledManager)
import Ecluse.Composition.Types (RegistryRole (MirrorPreviewer, MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (Severity (Ignore, Refuse), Vet, byStoreRole, rule, withRole)
import Ecluse.Config (
    ControlPlane (ControlCodeArtifact, ControlNone, ControlProtocol),
    DeletionConsent (DeletionPermitted, DeletionWithheld),
    MirrorTarget (mtBackend, mtUrl, mtUsername),
    Mount (mountRegistries),
    MountConfig (mntPrivateUpstream),
    MountMap,
    PrivateEndpoint (..),
    QuotaOverride (qoQuotas, qoScope, qoWeights),
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
import Ecluse.Core.Credential (ClientCredential (ClientCredential), CredentialProvider, Secret, bareCredential, mintSecret)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Fault (TransportCause (TransportProtocol), transportFault)
import Ecluse.Core.Fault.Http (typedTlsManagerSettings)
import Ecluse.Core.Registry (FetchFault (FetchTransport))
import Ecluse.Core.Registry.Adapter (
    RegistryAdapter,
    adapterMaintenance,
    adapterMetadata,
    adapterPublish,
 )
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterMaintenance (maintenanceAlphabet, maintenanceListing, maintenanceVersionDelete),
    AdapterMetadata (metadataRead),
    AdapterPublish (publishCodec),
    StoreListing,
    VersionDelete,
 )
import Ecluse.Core.Registry.Exchange (singleAttemptSettings)
import Ecluse.Core.Registry.Maintenance (
    StoreFacts (factBudget),
    StoreMaintenance (storeFacts),
    StoreManifestRead,
    StoreObservation (obFacts),
    meteredMaintenance,
    meteredObservation,
    storeFaultOfMetadata,
 )
import Ecluse.Core.Registry.Maintenance.Budget (
    QuotaOrigin (QuotaDeclared),
    QuotaScope,
    RequestGate,
    StoreBudget (bgCosts, bgOrigin, bgQuotas, bgScope),
    mkQuotaScope,
 )
import Ecluse.Core.Registry.Maintenance.NameSpace (
    NameAlphabet,
    noNameAlphabet,
 )
import Ecluse.Core.Registry.Maintenance.Protocol (
    ProtocolRead (..),
    ProtocolStore (ProtocolStore, psDelete, psDeleteOrigin, psRead),
    newProtocolMaintenance,
    newProtocolObservation,
 )
import Ecluse.Core.Registry.Maintenance.Upstream (
    UpstreamSafety (Undecidable, Unsafe),
    noUpstreamMechanism,
 )
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataFetch))
import Ecluse.Core.Registry.Metadata.Fetch (fetchManifest)
import Ecluse.Core.Registry.Metadata.Fetch.Types (ManifestFetch)
import Ecluse.Core.Registry.Origin (OriginClient, originClient)
import Ecluse.Core.Registry.Publish (PublishCodec)
import Ecluse.Core.Registry.Sweep.Pacing (derivedCapacity)
import Ecluse.Core.Security (Limits (maxVersionCount), authorityLabel)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Runtime.Maintenance.CodeArtifact (newCodeArtifactCacheMaintenance, newCodeArtifactCacheObservation, newCodeArtifactMaintenance, newCodeArtifactObservation, newCodeArtifactUpstreamProbe)
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
        , cbFetchManifest = maybe absentManifestRead (fetchManifest . metadataRead . adapterMetadata) adapter
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

-- | Per-target tracing and authentication, resolved before the store handles are built.
data StorePorts = StorePorts
    { spTracing :: TracingPort
    -- ^ The tracing port the manifest read is bracketed by.
    , spCredential :: Maybe CredentialProvider
    -- ^ The credential for this exact target. An anonymous protocol observation holds none.
    , spBudget :: BudgetPorts
    -- ^ What this store's requests are counted and paced through.
    }

-- | Capacity declarations and request gates resolved before store construction.
data BudgetPorts = BudgetPorts
    { bpGateFor :: QuotaScope -> RequestGate
    , bpOverrides :: Map Text QuotaOverride
    , bpNominalPace :: Rational
    -- ^ The sweep's own package pace, which a backend publishing no quota is taken to run at.
    }

-- | Build maintenance for a cleared store under the resolved response limits.
type BuildStoreMaintenance = StorePorts -> Limits -> ClearedBackend -> IO StoreMaintenance

-- | Build observations without deletion, marker writes or publication.
type BuildStoreObservation = StorePorts -> Limits -> ClearedBackend -> IO StoreObservation

-- | Role-specific constructors. Preview construction never creates deletion handles.
data StoreBuilds = StoreBuilds
    { sbDeleting :: BuildStoreMaintenance
    , sbObserving :: BuildStoreObservation
    , sbProbing :: BuildUpstreamProbe
    }

-- | The shipped builds.
storeBuilds :: StoreBuilds
storeBuilds =
    StoreBuilds
        { sbDeleting = buildStoreMaintenance
        , sbObserving = buildStoreObservation
        , sbProbing = buildUpstreamProbe
        }

-- | Build the probe for one private upstream under the role's own authority.
type BuildUpstreamProbe = Ecosystem -> PrivateEndpoint -> IO UpstreamSafety

-- | Probe the declared backend with the role's ambient identity, never a caller's credential.
buildUpstreamProbe :: BuildUpstreamProbe
buildUpstreamProbe eco endpoint = case tgtTag target of
    -- 'vetPrivateRepository' refuses an endpoint addressing no repository, so this resolution
    -- fails only for a value no loaded configuration carries, and there is nothing to ask about.
    TagCodeArtifact -> either (const noUpstreamMechanism) (newCodeArtifactUpstreamProbe . snd) (resolvePrivateBackend eco target)
    TagRegistry -> noUpstreamMechanism
    TagVerdaccio -> noUpstreamMechanism
  where
    target = preTarget endpoint

-- | Collect probe answers and refuse the affected mounts when a probe throws.
readUpstreamSafety :: [(Ecosystem, IO UpstreamSafety)] -> IO ([Advisory], Either [BootError] ())
readUpstreamSafety probes = settled . partitionEithers <$> traverse answer probes
  where
    answer (eco, probe) = fmap (eco,) <$> refuseOnThrow (PrivateUpstreamProbeFailed eco) probe

    settled (thrown, answers) =
        let (advisories, refused) = upstreamFindings answers
         in (advisories, refusals (concat thrown <> fromLeft [] refused))

-- | Refuse unsafe upstreams, advise on undecidable ones, and accept safe ones.
upstreamFindings :: [(Ecosystem, UpstreamSafety)] -> ([Advisory], Either [BootError] ())
upstreamFindings answers = (advisories, refusals unsafeMounts)
  where
    advisories = [PrivateUpstreamUndecided eco reason | (eco, Undecidable reason) <- answers]
    unsafeMounts = [PrivateUpstreamUnsafe eco reason | (eco, Unsafe reason) <- answers]

-- Nothing found is nothing refused, which is what lets a whole pass report at once.
refusals :: [BootError] -> Either [BootError] ()
refusals found = if null found then Right () else Left found

-- | Build maintenance using store-local metadata reads and backend-owned control credentials.
buildStoreMaintenance :: BuildStoreMaintenance
buildStoreMaintenance ports limits cleared = budgeted ports cleared <$> built
  where
    built = do
        (readManifest, manager) <- storeAccess ports limits cleared
        case cbControl cleared of
            ClearedCodeArtifact store -> newCodeArtifactMaintenance (maxVersionCount limits) (cbAlphabet cleared) readManifest store
            ClearedCodeArtifactCache store -> newCodeArtifactCacheMaintenance (maxVersionCount limits) (cbAlphabet cleared) readManifest store
            ClearedProtocol store -> do
                deletionManager <- newPooledManager storeConnections (singleAttemptSettings typedTlsManagerSettings)
                pure (newProtocolMaintenance (protocolStore limits cleared store readManifest manager deletionManager))

-- Resolve the capacity pool before metering requests against it.
budgeted :: StorePorts -> ClearedBackend -> StoreMaintenance -> StoreMaintenance
budgeted ports cleared handle =
    (meteredMaintenance (gateOver ports facts) handle){storeFacts = facts}
  where
    facts = resolvedFacts ports cleared (storeFacts handle)

-- | Build the cleared backend's observation capability without constructing deletion handles.
buildStoreObservation :: BuildStoreObservation
buildStoreObservation ports limits cleared = observed <$> built
  where
    observed handle =
        let facts = resolvedFacts ports cleared (obFacts handle)
         in (meteredObservation (gateOver ports facts) handle){obFacts = facts}
    built = do
        (readManifest, manager) <- storeAccess ports limits cleared
        case cbControl cleared of
            ClearedCodeArtifact store -> newCodeArtifactObservation (maxVersionCount limits) (cbAlphabet cleared) readManifest store
            ClearedCodeArtifactCache store -> newCodeArtifactCacheObservation (maxVersionCount limits) (cbAlphabet cleared) readManifest store
            ClearedProtocol store ->
                pure (newProtocolObservation (protocolRead limits cleared store readManifest manager))

resolvedFacts :: StorePorts -> ClearedBackend -> StoreFacts -> StoreFacts
resolvedFacts ports cleared facts =
    facts{factBudget = resolvedBudget (spBudget ports) (cbUrl cleared) (factBudget facts)}

-- The gate for the pool this store's resolved capacity landed in.
gateOver :: StorePorts -> StoreFacts -> RequestGate
gateOver ports facts = bpGateFor (spBudget ports) (bgScope (factBudget facts))

-- | Default to one capacity pool per authority, shared by paths on that host.
storeScope :: RegistryUrl -> QuotaScope
storeScope = mkQuotaScope . authorityLabel . registryUrlText

-- | Match capacity declarations without case or trailing-slash differences.
overrideKey :: Text -> Text
overrideKey = T.dropWhileEnd (== '/') . T.toLower . T.strip

-- What the operator declared about this exact store, where they declared anything.
matchingOverride :: Map Text QuotaOverride -> RegistryUrl -> Maybe QuotaOverride
matchingOverride overrides url = Map.lookup (overrideKey (registryUrlText url)) keyed
  where
    keyed = Map.fromList [(overrideKey key, override) | (key, override) <- Map.toList overrides]

-- | Apply declared capacity over backend defaults, deriving any remaining quota from the sweep pace.
resolvedBudget :: BudgetPorts -> RegistryUrl -> StoreBudget -> StoreBudget
resolvedBudget ports url budget =
    -- The derivation runs last and only bites where nothing else declared a quota, so an entry
    -- naming a scope or a weight alone still leaves the store a capacity.
    derivedCapacity (bpNominalPace ports) (maybe located (declared located) (matchingOverride (bpOverrides ports) url))
  where
    -- A backend that names its own pool keeps it. One that names none is pooled by its authority.
    located
        | bgScope budget == mkQuotaScope "" = budget{bgScope = storeScope url}
        | otherwise = budget

-- A declared capacity wins per dimension, and a declared weight scales that kind's own costs.
declared :: StoreBudget -> QuotaOverride -> StoreBudget
declared budget override =
    budget
        { bgScope = maybe (bgScope budget) mkQuotaScope (qoScope override)
        , bgQuotas = Map.union (qoQuotas override) (bgQuotas budget)
        , bgOrigin = if Map.null (qoQuotas override) then bgOrigin budget else QuotaDeclared
        , bgCosts = Map.mapWithKey weighted (bgCosts budget)
        }
  where
    weighted kind costs = maybe costs (\weight -> Map.map (* weight) costs) (Map.lookup kind (qoWeights override))

-- One manager of the store's own, and the manifest read that leads over it.
storeAccess :: StorePorts -> Limits -> ClearedBackend -> IO (StoreManifestRead, Manager)
storeAccess ports limits cleared = do
    manager <- storeManager
    pure (storeManifestRead ports limits cleared manager, manager)

-- Resolve the current secret for each read because managed tokens expire.
storeManifestRead :: StorePorts -> Limits -> ClearedBackend -> Manager -> StoreManifestRead
storeManifestRead ports limits cleared manager name = do
    token <- traverse mintSecret (spCredential ports)
    first storeFaultOfMetadata
        <$> cbFetchManifest cleared (spTracing ports) (storeOrigin limits cleared manager (ClientCredential (cbUsername cleared) <$> token)) name

storeOrigin :: Limits -> ClearedBackend -> Manager -> Maybe ClientCredential -> OriginClient
storeOrigin limits cleared manager = originClient limits manager (cbUrl cleared)

-- Disable hidden HTTP replay so every attempt consumes the store's request budget.
storeManager :: IO Manager
storeManager = newPooledManager storeConnections (singleAttemptSettings typedTlsManagerSettings)

-- One store, swept package by package, so the pool holds what one in-flight request needs.
storeConnections :: Int
storeConnections = 4

protocolStore :: Limits -> ClearedBackend -> ClearedProtocolStore -> StoreManifestRead -> Manager -> Manager -> ProtocolStore
protocolStore limits cleared store readManifest manager deletionManager =
    ProtocolStore
        { psRead = protocolRead limits cleared store readManifest manager
        , psDeleteOrigin = storeOrigin limits cleared deletionManager (bareCredential <$> cpsToken store)
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

-- Deletion requires consent at boot. Preview reports withheld consent instead.
consentDescriptor :: Ecosystem -> StoreTag -> Text
consentDescriptor eco tag =
    "set "
        <> mountKeyRef eco ("mirrorTarget." <> storeTagName tag <> ".permitDeletion")
        <> " to true: the Dredger deletes nothing from a store that does not carry it"

-- | Build each cleared store and collect all construction refusals before returning.
planStoreMaintenance ::
    (StorePorts -> Limits -> ClearedBackend -> IO store) ->
    TracingPort ->
    BudgetPorts ->
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
    BudgetPorts ->
    CredentialProviders ->
    Limits ->
    Map Ecosystem ClearedBackend ->
    IO (Either [BootError] (Map Ecosystem store))
planStoreMaintenanceFor target build tracing budget credentials limits backends =
    validationToEither . traverse eitherToValidation <$> Map.traverseWithKey planOne backends
  where
    planOne eco backend =
        refuseOnThrow (StoreMaintenanceUnavailable eco . reason) (build (portsFor eco) limits backend)
    reason = case target of
        MirrorCredential -> ClientBuildFailed
        PrivateCacheCredential -> PrivateCacheUnavailable . ("client build failed: " <>)
    portsFor eco =
        StorePorts
            { spTracing = tracing
            , spCredential = lookupTargetProvider target eco credentials
            , spBudget = budget
            }
