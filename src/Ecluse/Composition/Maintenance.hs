-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Vet store backends, then build the boot role's maintenance or observation capabilities.
The preview observes the declared private cache without constructing a deletion handle. No cleared
type's constructor is exported, so a value a deletion handle builds from exists only where a pass
here issued it.
-}
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
    BuildStoreMaintenance,
    BuildStoreObservation,
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
import Network.HTTP.Client.TLS (tlsManagerSettings)
import UnliftIO (tryAny)
import Validation (eitherToValidation, validationToEither)

import Ecluse.Composition.BootError (
    Advisory (PrivateUpstreamUndecided),
    BootError (PrivateUpstreamUnsafe, StoreMaintenanceUnavailable),
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
    MirrorTarget (mtBackend, mtUrl),
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
import Ecluse.Core.Registry.Exchange (singleAttemptSettings)
import Ecluse.Core.Registry.Maintenance (
    NameAlphabet,
    StoreFacts (factBudget),
    StoreMaintenance (storeFacts),
    StoreManifestRead,
    StoreObservation (obFacts),
    meteredMaintenance,
    meteredObservation,
    noNameAlphabet,
    storeFaultOfMetadata,
 )
import Ecluse.Core.Registry.Maintenance.Budget (
    QuotaOrigin (QuotaDeclared),
    QuotaScope,
    RequestGate,
    StoreBudget (bgCosts, bgOrigin, bgQuotas, bgScope),
    mkQuotaScope,
 )
import Ecluse.Core.Registry.Maintenance.Protocol (
    ProtocolRead (..),
    ProtocolStore (ProtocolStore, psDelete, psDeleteOrigin, psRead),
    newProtocolMaintenance,
    newProtocolObservation,
 )
import Ecluse.Core.Registry.Maintenance.Upstream (
    UndecidabilityReason (NetworkFailure),
    UpstreamSafety (Undecidable, Unsafe),
    noUpstreamMechanism,
 )
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataFetch))
import Ecluse.Core.Registry.Origin (OriginClient, originClient)
import Ecluse.Core.Registry.Publish (PublishCodec)
import Ecluse.Core.Registry.Sweep.Pacing (derivedCapacity)
import Ecluse.Core.Security (Limits (maxVersionCount), authorityLabel)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Runtime.Maintenance.CodeArtifact (newCodeArtifactCacheMaintenance, newCodeArtifactCacheObservation, newCodeArtifactMaintenance, newCodeArtifactObservation, newCodeArtifactUpstreamProbe)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore)

{- | A store a deleting role's pass cleared, one arm per backend kind. Only 'vetStoreBackends' and
'vetPrivateCaches' issue one, so no store their passes did not clear gets a handle that can delete.
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

-- | Vet each private cache under its own backend declaration and maintenance authority.
vetPrivateCaches :: ResolveMaintenanceAdapter -> Map Ecosystem MountConfig -> MountMap -> Vet (Map Ecosystem (Maybe StoreBackend, ClearedBackend))
vetPrivateCaches resolveAdapter configured mounts = withRole $ \case
    MirrorWriter -> pure Map.empty
    MirrorPruner -> clearedFor MirrorPruner
    MirrorPreviewer -> clearedFor MirrorPreviewer
  where
    clearedFor role =
        let resolved = resolvedFor role
         in Map.fromList [(eco, backend) | (eco, Right backend) <- resolved]
                <$ traverse_ (rule (const (Refuse (uncurry StoreMaintenanceUnavailable))) unmaintained) resolved
    resolvedFor role =
        [ (eco, resolve role eco endpoint)
        | (eco, mount) <- Map.toAscList mounts
        , isJust (regMirrorTarget (mountRegistries mount))
        , Just config <- [Map.lookup eco configured]
        , Just endpoint <- [mntPrivateUpstream config]
        ]
    unmaintained (eco, outcome) = (eco,) <$> leftToMaybe outcome
    resolve role eco endpoint = case tgtTag target of
        TagCodeArtifact -> do
            (backend, store) <- first (PrivateCacheUnavailable . show) (resolvePrivateBackend eco target)
            pure (Just backend, clearedBackend (tgtUrl target) adapter (ClearedCodeArtifactCache store))
        TagRegistry -> Left (PrivateCacheUnavailable "registry has no inventory control plane")
        TagVerdaccio -> do
            when (refusesWithoutConsent role && preConsent endpoint == DeletionWithheld) $
                Left (PrivateCacheUnavailable (descriptor <> " is not set"))
            when (role == MirrorPruner && isNothing (preToken endpoint)) $
                Left (PrivateCacheUnavailable (mountKeyRef eco "privateUpstream.verdaccio.token" <> " is not set"))
            store <- protocolStoreFor adapter TagVerdaccio (preToken endpoint) (preConsent endpoint) descriptor
            pure (fmap (`BackendVerdaccio` preConsent endpoint) (preToken endpoint), clearedBackend (tgtUrl target) adapter (ClearedProtocol store))
      where
        target = preTarget endpoint
        adapter = resolveAdapter eco
        descriptor = mountKeyRef eco "privateUpstream.verdaccio.permitDeletion"

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
    , spBudget :: BudgetPorts
    -- ^ What this store's requests are counted and paced through.
    }

{- | What the boot knows about request capacity before any store is built: the gate for each
capacity pool, and what the operator declared about those pools.
-}
data BudgetPorts = BudgetPorts
    { bpGateFor :: QuotaScope -> RequestGate
    , bpOverrides :: Map Text QuotaOverride
    , bpNominalPace :: Rational
    -- ^ The sweep's own package pace, which a backend publishing no quota is taken to run at.
    }

{- | How a boot builds one store's maintenance handle, under the response bound the plan resolved.
Injected, as the queue builder is, so a spec drives the pruner's arm without an AWS identity.
-}
type BuildStoreMaintenance = StorePorts -> Limits -> ClearedBackend -> IO StoreMaintenance

{- | How a boot builds the observing calls for a cleared store, under the same bound. Nothing it
builds can delete, write a marker, or publish.
-}
type BuildStoreObservation = StorePorts -> Limits -> ClearedBackend -> IO StoreObservation

{- | The builds a boot chooses between: one per Dredger authority, and the serving role's probe.
The role picks its own, so a preview's boot never runs the build that holds a delete.
-}
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

{- | How a boot builds the probe for one mount's private upstream. Injected, as the store builds
are, so a spec drives what a boot does with an answer without an AWS identity.
-}
type BuildUpstreamProbe = Ecosystem -> PrivateEndpoint -> IO UpstreamSafety

{- | The shipped build. It asks the backend the mount declared, over the role's own ambient
identity, so no caller's credential reaches the call.
-}
buildUpstreamProbe :: BuildUpstreamProbe
buildUpstreamProbe eco endpoint = case tgtTag target of
    -- A URL naming no repository this build can address leaves it nothing to ask about.
    TagCodeArtifact -> either (const noUpstreamMechanism) (newCodeArtifactUpstreamProbe . snd) (resolvePrivateBackend eco target)
    TagRegistry -> noUpstreamMechanism
    TagVerdaccio -> noUpstreamMechanism
  where
    target = preTarget endpoint

{- | Read every probe's answer. A backend reads its own identity and its own faults, so a throw
that reaches here is one no backend read, and it ends this boot's line rather than the boot.
-}
readUpstreamSafety :: [(Ecosystem, IO UpstreamSafety)] -> IO ([Advisory], Either [BootError] ())
readUpstreamSafety probes = upstreamFindings <$> traverse answer probes
  where
    answer (eco, probe) = (eco,) . fromRight (Undecidable NetworkFailure) <$> tryAny probe

{- | What a boot does about the answers it read: an unsafe repository refuses the role, an open
question advises, and a safe one says nothing.
-}
upstreamFindings :: [(Ecosystem, UpstreamSafety)] -> ([Advisory], Either [BootError] ())
upstreamFindings answers = (advisories, if null refusals then Right () else Left refusals)
  where
    advisories = [PrivateUpstreamUndecided eco reason | (eco, Undecidable reason) <- answers]
    refusals = [PrivateUpstreamUnsafe eco reason | (eco, Unsafe reason) <- answers]

{- | The live handle for a cleared store. CodeArtifact discovers its credentials the standard AWS
way, and both arms read and dial over one manager of the store's own.
-}
buildStoreMaintenance :: BuildStoreMaintenance
buildStoreMaintenance ports limits cleared = budgeted ports cleared <$> built
  where
    built = do
        (readManifest, manager) <- storeAccess ports limits cleared
        case cbControl cleared of
            ClearedCodeArtifact store -> newCodeArtifactMaintenance (maxVersionCount limits) (cbAlphabet cleared) readManifest store
            ClearedCodeArtifactCache store -> newCodeArtifactCacheMaintenance (maxVersionCount limits) (cbAlphabet cleared) readManifest store
            ClearedProtocol store -> do
                deletionManager <- newPooledManager storeConnections (singleAttemptSettings tlsManagerSettings)
                pure (newProtocolMaintenance (protocolStore limits cleared store readManifest manager deletionManager))

{- The handle under this store's resolved capacity, with every request it makes counted and paced.
The capacity resolves first, because the pool it names is the pool the gate meters in. -}
budgeted :: StorePorts -> ClearedBackend -> StoreMaintenance -> StoreMaintenance
budgeted ports cleared handle =
    (meteredMaintenance (gateOver ports facts) handle){storeFacts = facts}
  where
    facts = resolvedFacts ports cleared (storeFacts handle)

{- | The observing calls for a cleared store, built from the backend's own read capability rather
than from a handle with its writes taken away.
-}
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

{- | The pool a store falls in when its backend names none of its own: the store's own authority,
so two paths on one host share it.
-}
storeScope :: RegistryUrl -> QuotaScope
storeScope = mkQuotaScope . authorityLabel . registryUrlText

{- | The spelling a declared capacity's key and a store URL are compared under, so a trailing
slash or a difference of case cannot miss a match.
-}
overrideKey :: Text -> Text
overrideKey = T.dropWhileEnd (== '/') . T.toLower . T.strip

-- What the operator declared about this exact store, where they declared anything.
matchingOverride :: Map Text QuotaOverride -> RegistryUrl -> Maybe QuotaOverride
matchingOverride overrides url = Map.lookup (overrideKey (registryUrlText url)) keyed
  where
    keyed = Map.fromList [(overrideKey key, override) | (key, override) <- Map.toList overrides]

{- | The store's capacity as this boot resolves it: the backend's own description, the operator's
declaration where there is one, else the pace a backend publishing no quota is derived to run at.
-}
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

{- No proxy tracing: these calls are not the data plane. Dropping http-client's hidden replay on
a reused connection keeps every attempt one the request budget counted. -}
storeManager :: IO Manager
storeManager = newPooledManager storeConnections (singleAttemptSettings tlsManagerSettings)

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
