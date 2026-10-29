-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Build role-specific maintenance, observations and upstream probes from cleared backends.
module Ecluse.Composition.Maintenance (
    -- * The environment-dependent half
    StorePorts (..),
    BudgetPorts (..),
    storeScope,
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
import Validation (eitherToValidation, validationToEither)

import Ecluse.Composition.BootError (
    Advisory (PrivateUpstreamUndecided),
    BootError (PrivateUpstreamProbeFailed, PrivateUpstreamUnsafe, StoreMaintenanceUnavailable),
    StoreMaintenanceReason (ClientBuildFailed, PrivateCacheUnavailable),
    refuseOnThrow,
 )
import Ecluse.Composition.Credential (CredentialProviders, CredentialTarget (..), lookupTargetProvider)
import Ecluse.Composition.Maintenance.Plan (
    ClearedBackend (cbAlphabet, cbArtifact, cbControl, cbFetchManifest, cbUrl, cbUsername),
    ClearedControl (..),
    ClearedProtocolStore (..),
    overrideKey,
 )
import Ecluse.Composition.Sizing (newPooledManager)
import Ecluse.Config (
    DeletionConsent (DeletionPermitted),
    PrivateEndpoint (preTarget),
    QuotaOverride (qoQuotas, qoScope, qoWeights),
    StoreTag (TagCodeArtifact, TagRegistry, TagVerdaccio),
    Target (tgtTag),
    storeTagName,
 )
import Ecluse.Config.Target (resolvePrivateBackend)
import Ecluse.Core.BuildIdentity (identityManagerSettings)
import Ecluse.Core.Credential (ClientCredential (ClientCredential), CredentialProvider, bareCredential, mintSecret)
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Fault.Http (typedTlsManagerSettings)
import Ecluse.Core.Package (artUrl)
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl))
import Ecluse.Core.Registry.Exchange (singleAttemptSettings)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ExchangeClient), untracedExchange)
import Ecluse.Core.Registry.InstallEvidence (boundedArtifactLimits, readInstallArtifact)
import Ecluse.Core.Registry.Maintenance (
    StoreFacts (factBudget),
    StoreMaintenance (storeFacts, storeReadArtifact),
    StoreManifestRead,
    StoreObservation (obFacts, obReadArtifact),
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
import Ecluse.Core.Registry.Maintenance.Protocol (
    ProtocolRead (..),
    ProtocolStore (ProtocolStore, psDelete, psDeleteOrigin, psRead),
    newProtocolMaintenance,
    newProtocolObservation,
 )
import Ecluse.Core.Registry.Maintenance.Upstream (UpstreamSafety (Undecidable, Unsafe), noUpstreamMechanism)
import Ecluse.Core.Registry.Origin (OriginClient, originClient)
import Ecluse.Core.Registry.Public (ReadControl (Uncontrolled))
import Ecluse.Core.Registry.Sweep.Pacing (derivedCapacity)
import Ecluse.Core.Rules.Install (InstallArtifactRead, unreadInstallSignal)
import Ecluse.Core.Security (Limits (maxVersionCount), authorityLabel, hostPortAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Runtime.Maintenance.CodeArtifact (newCodeArtifactCacheMaintenance, newCodeArtifactCacheObservation, newCodeArtifactMaintenance, newCodeArtifactObservation, newCodeArtifactUpstreamProbe)

-- | Per-target tracing and authentication, resolved before the store handles are built.
data StorePorts = StorePorts
    { spTracing :: TracingPort
    -- ^ The tracing port the manifest read is bracketed by.
    , spCredential :: Maybe CredentialProvider
    -- ^ The credential for this exact target. An anonymous protocol observation holds none.
    , spUserAgent :: ByteString
    -- ^ The precomputed identity shared by this store's registry requests.
    , spDeadlines :: Deadlines
    -- ^ The process's sweeper, which bounds this store's registry requests.
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
        (readManifest, client) <- storeAccess ports limits cleared
        handle <- case cbControl cleared of
            ClearedCodeArtifact store -> newCodeArtifactMaintenance (maxVersionCount limits) (cbAlphabet cleared) readManifest store
            ClearedCodeArtifactCache store -> newCodeArtifactCacheMaintenance (maxVersionCount limits) (cbAlphabet cleared) readManifest store
            ClearedProtocol store -> do
                deletionClient <- storeClient ports
                pure (newProtocolMaintenance (protocolStore limits cleared store readManifest client deletionClient))
        pure handle{storeReadArtifact = storeArtifactRead ports limits cleared client}

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
        (readManifest, client) <- storeAccess ports limits cleared
        handle <- case cbControl cleared of
            ClearedCodeArtifact store -> newCodeArtifactObservation (maxVersionCount limits) (cbAlphabet cleared) readManifest store
            ClearedCodeArtifactCache store -> newCodeArtifactCacheObservation (maxVersionCount limits) (cbAlphabet cleared) readManifest store
            ClearedProtocol store ->
                pure (newProtocolObservation (protocolRead limits cleared store readManifest client))
        pure handle{obReadArtifact = storeArtifactRead ports limits cleared client}

resolvedFacts :: StorePorts -> ClearedBackend -> StoreFacts -> StoreFacts
resolvedFacts ports cleared facts =
    facts{factBudget = resolvedBudget (spBudget ports) (cbUrl cleared) (factBudget facts)}

-- The gate for the pool this store's resolved capacity landed in.
gateOver :: StorePorts -> StoreFacts -> RequestGate
gateOver ports facts = bpGateFor (spBudget ports) (bgScope (factBudget facts))

-- | Default to one capacity pool per authority, shared by paths on that host.
storeScope :: RegistryUrl -> QuotaScope
storeScope = mkQuotaScope . authorityLabel . registryUrlText

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

-- One client of the store's own, and the manifest read that leads over it.
storeAccess :: StorePorts -> Limits -> ClearedBackend -> IO (StoreManifestRead, ExchangeClient)
storeAccess ports limits cleared = do
    client <- storeClient ports
    pure (storeManifestRead ports limits cleared client, client)

-- Resolve the current secret for each read because managed tokens expire.
storeManifestRead :: StorePorts -> Limits -> ClearedBackend -> ExchangeClient -> StoreManifestRead
storeManifestRead ports limits cleared client name = do
    token <- traverse mintSecret (spCredential ports)
    first storeFaultOfMetadata
        <$> cbFetchManifest cleared (spTracing ports) (storeOrigin limits cleared client (ClientCredential (cbUsername cleared) <$> token)) name

-- Private credentials remain under the configured store authority. No public fallback exists.
storeArtifactRead :: StorePorts -> Limits -> ClearedBackend -> ExchangeClient -> InstallArtifactRead
storeArtifactRead ports limits cleared client bounded artifact pay =
    case cbArtifact cleared of
        Nothing -> pure (Left unreadInstallSignal)
        Just native
            | hostPortAddress (artUrl artifact) /= hostPortAddress (registryUrlText (cbUrl cleared)) -> pure (Left unreadInstallSignal)
            | otherwise -> do
                token <- traverse mintSecret (spCredential ports)
                let credential = case cbControl cleared of
                        ClearedProtocol store -> (ClientCredential (cbUsername cleared) <$> token) <|> (bareCredential <$> cpsToken store)
                        _ -> ClientCredential (cbUsername cleared) <$> token
                readInstallArtifact (boundedArtifactLimits limits bounded) client Uncontrolled (artifactByUrl native credential (artUrl artifact)) pay

storeOrigin :: Limits -> ClearedBackend -> ExchangeClient -> Maybe ClientCredential -> OriginClient
storeOrigin limits cleared client = originClient limits client (cbUrl cleared)

-- Disable hidden HTTP replay so every attempt consumes the store's request budget.
storeClient :: StorePorts -> IO ExchangeClient
storeClient ports = do
    manager <- newPooledManager storeConnections (identityManagerSettings (spUserAgent ports) (singleAttemptSettings typedTlsManagerSettings))
    pure (ExchangeClient manager (spDeadlines ports) untracedExchange)

-- One store, swept package by package, so the pool holds what one in-flight request needs.
storeConnections :: Int
storeConnections = 4

protocolStore :: Limits -> ClearedBackend -> ClearedProtocolStore -> StoreManifestRead -> ExchangeClient -> ExchangeClient -> ProtocolStore
protocolStore limits cleared store readManifest client deletionClient =
    ProtocolStore
        { psRead = protocolRead limits cleared store readManifest client
        , psDeleteOrigin = storeOrigin limits cleared deletionClient (bareCredential <$> cpsToken store)
        , psDelete = cpsDelete store
        }

protocolRead :: Limits -> ClearedBackend -> ClearedProtocolStore -> StoreManifestRead -> ExchangeClient -> ProtocolRead
protocolRead limits cleared store readManifest client =
    ProtocolRead
        { prOrigin = storeOrigin limits cleared client (bareCredential <$> cpsToken store)
        , prReadManifest = readManifest
        , prListing = cpsListing store
        , prCodec = cpsCodec store
        , prBackendName = storeTagName (cpsTag store)
        , prPermitDeletion = cpsConsent store == DeletionPermitted
        , prConsentDescriptor = cpsConsentKey store
        }

-- | Build each cleared store and collect all construction refusals before returning.
planStoreMaintenance ::
    (StorePorts -> Limits -> ClearedBackend -> IO store) ->
    TracingPort ->
    Deadlines ->
    ByteString ->
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
    Deadlines ->
    ByteString ->
    BudgetPorts ->
    CredentialProviders ->
    Limits ->
    Map Ecosystem ClearedBackend ->
    IO (Either [BootError] (Map Ecosystem store))
planStoreMaintenanceFor target build tracing deadlines renderedAgent budget credentials limits backends =
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
            , spUserAgent = renderedAgent
            , spDeadlines = deadlines
            , spCredential = lookupTargetProvider target eco credentials
            , spBudget = budget
            }
