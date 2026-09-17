-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The boot's effectful planning phase: take the cleared 'BootPlan' and yield the
'ExecutablePlan' the booting role assembles its runtime from.

Every role plans through here, so a refusal only a live environment can settle is spent at one
gate whatever the role, and holding an 'ExecutablePlan' means nothing downstream refuses to
boot. A listener that fails to bind is a runtime fault for supervision, not a refusal.
@ecluse check-config@ makes no cloud call, so it stops at the 'BootPlan' and never reaches here.
-}
module Ecluse.Composition.Executable (
    ExecutablePlan (epBootPlan, epRoleWiring),
    RoleWiring (..),
    MirrorWiring (mwRole, mwBootWiring, mwCveSync, mwQueue, mwDeferredMetrics),
    PrunerWiring (pwBudget, pwCveSync, pwDeferredMetrics, pwMounts),
    BuildMirrorQueue,
    BuildCredentials,
    planExecutable,
) where

import Data.Time (getCurrentTime)
import Katip (LogEnv)
import Validation (Validation (Failure), eitherToValidation, validationToEither)

import Data.Map.Strict qualified as Map

import Ecluse.Composition (
    BootWiring,
    PublishBudget (PublishBudget, pbBodyBudget, pbMaxRequestBytes),
    ResolveAdapter,
    WiringPorts (WiringPorts, wpBuildCredentials, wpClock, wpReporters, wpResolveAdapter, wpRuleDeps),
    firstPartyName,
    resolveBootWiring,
 )
import Ecluse.Composition.BootError (
    BootError (AdvisorySyncUnavailable, MirrorQueueUnavailable, PilotWithoutEcosystem, StoreMaintenanceUnavailable),
    StoreMaintenanceReason (PrivateCacheUnavailable),
    refuseOnThrow,
 )
import Ecluse.Composition.Credential (BuildCredentials, CredentialTarget (..), mirrorBackends, noCredentialProviders, providerLabel)
import Ecluse.Composition.Maintenance (
    BudgetPorts (BudgetPorts, bpGateFor, bpNominalPace, bpOverrides),
    ClearedBackend (cbUrl),
    StoreBuilds (sbDeleting, sbObserving),
    StorePorts,
    planStoreMaintenance,
    planStoreMaintenanceFor,
 )
import Ecluse.Composition.MemoryPlan (
    MemoryPlan (mpMaxRequestBytes, mpPublishTenant, mpQueueMemoryMaxDepth),
    PublishTenant (ptAggregateBytes),
 )
import Ecluse.Composition.MirrorQueue (
    MirrorQueuePlan,
    MirrorRuntimePlan (MirrorWith, NoMirroring),
 )
import Ecluse.Composition.MirrorRole (mirrorMintPlan)
import Ecluse.Composition.Plan (
    BootPlan (bpLimits, bpMemoryPlan, bpMirrorRuntime, bpRole, bpS3Endpoint, bpValidated),
 )
import Ecluse.Composition.Types (
    BootRole (BootMirrorPipeline, BootStorePreview, BootStorePruner, BootWithoutPipeline),
    MirrorRole,
 )
import Ecluse.Composition.Validate (
    ValidatedPlan (vpMirrorStores, vpMounts, vpPrivateCaches, vpSettings),
    VettedMount (vmAdapter, vmConfig, vmEcosystem, vmMount),
 )
import Ecluse.Config (AppConfig (cfgAdvisories, cfgDredger), DredgerSettings (drgChunkPause, drgChunkSize, drgQuotaOverrides), Mount (mountPolicy), MountConfig (mntFirstParty), StoreTag, mountAdvisoryAge, mountDatabaseRequirement, mountEpssRequirement)
import Ecluse.Core.Clock (waitSeconds)
import Ecluse.Core.Credential.Refresh (CredentialReporters (CredentialReporters, crBreakerReporter, crRefreshReporter))
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Queue (MirrorQueue, noMirrorQueue)
import Ecluse.Core.Registry.Adapter (ProjectName, adapterProjectName)
import Ecluse.Core.Registry.Maintenance (StoreFacts (factBackend), StoreObservation (obFacts))
import Ecluse.Core.Registry.Maintenance.Budget (BudgetPort, newBudgetMeter)
import Ecluse.Core.Registry.Sweep.Pacing (nominalPackagePace)
import Ecluse.Core.Registry.Sweep.Types (SweepCache (..), SweepMount (..), SweepStore, deletingCache, pairedStore, previewCache)
import Ecluse.Core.Rules (PreparedRule, RuleDeps, prepare)
import Ecluse.Core.Rules.Types (PrecededRule (prRule), Rule)
import Ecluse.Core.Security (Limits (maxVersionCount))
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Admission.Bytes (newByteAdmission)
import Ecluse.Core.Telemetry.Metrics (BreakerSource (CredentialMint, EffectfulRule))
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Cve.Sync (AdvisoryNeed (AdvisoryNeed, anDatabase, anEcosystem, anEpss, anMaxAge), CveSyncHandle, cveRuleDepsFor, katipFaultReporter, planCveSync)
import Ecluse.Pilot.Plan (ExportLoopPlan, exportLoopPlan)
import Ecluse.Runtime.Telemetry.Reporters (
    DeferredMetrics,
    deferredBreakerReporter,
    deferredRefreshReporter,
    newDeferredMetrics,
 )

{- | The boot's post-gating artefact: the cleared plan, and the wiring only a live environment
could settle. 'planExecutable' is its one producer, so a role cannot assemble an unvetted one.
-}
data ExecutablePlan = ExecutablePlan
    { epBootPlan :: BootPlan
    -- ^ The config-decidable plan every decision below was planned against.
    , epRoleWiring :: RoleWiring
    -- ^ What the booting role's own arm of this phase settled.
    }

{- | What each role's arm settled. A role starts from its own arm, so wiring one role's boot
planned cannot reach another role's runtime.
-}
data RoleWiring
    = -- | @ecluse proxy@, @ecluse proxy --no-worker@ and @ecluse mirror@.
      MirrorPipelineWiring MirrorWiring
    | -- | @ecluse dredger@: the stores it sweeps, and what decides for each of them.
      StorePrunerWiring PrunerWiring
    | -- | @ecluse pilot@: the export loop the advisory settings and the vetted mounts name.
      PilotWiring ExportLoopPlan

-- | What a mirror-pipeline role's arm settled, and all "Ecluse.Service" assembles its runtime from.
data MirrorWiring = MirrorWiring
    { mwRole :: MirrorRole
    -- ^ The pipeline half the plan vetted, so the severities it cleared and the runtime agree.
    , mwBootWiring :: BootWiring
    -- ^ The mounts the front door serves, and the publish targets the worker writes through.
    , mwCveSync :: Map Ecosystem CveSyncHandle
    -- ^ One advisory-sync handle per mount ecosystem, empty where no advisory store is configured.
    , mwQueue :: MirrorQueue
    -- ^ The mirror-queue backend the plan selected, inert where no mount mirrors.
    , mwDeferredMetrics :: DeferredMetrics
    {- ^ The metric handle the credential providers and the rule breakers already record through.
    The assembly makes those recordings live once the instruments exist.
    -}
    }

{- | What the store pruner's arm settled: one sweepable mount per cleared store, and the advisory
sync the sweep's rules read. "Ecluse.Dredger" assembles the whole role from it.
-}
data PrunerWiring = PrunerWiring
    { pwMounts :: [SweepMount]
    {- ^ One entry per store the pass cleared, carrying its maintenance handle, its own prepared
    rule set, and the shared first-party predicate its belt reads.
    -}
    , pwCveSync :: Map Ecosystem CveSyncHandle
    -- ^ One advisory-sync handle per mount ecosystem, empty where no advisory store is configured.
    , pwDeferredMetrics :: DeferredMetrics
    {- ^ The metric handle the credential providers and the sweep's rule breakers already record
    through. The role makes those recordings live once the instruments exist.
    -}
    , pwBudget :: BudgetPort
    {- ^ The cycle's end of the request budget every store handle above was built against, so the
    sweep reads what a cycle cost and installs the next one's rate.
    -}
    }

{- | How a boot builds the selected mirror-queue backend. Injected, as the adapter resolver is,
so a spec can drive this phase's refusals without reaching a cloud.
-}
type BuildMirrorQueue = LogEnv -> Int -> MirrorQueuePlan -> IO MirrorQueue

{- How the booting role builds one store's halves as the sweep holds them. Both Dredger roles plan
through the one arm below and differ only in which of 'StoreBuilds' they ran. -}
type BuildSweepCache = StorePorts -> Limits -> ClearedBackend -> IO SweepCache

{- | Plan the runtime the cleared plan's role starts, or report every refusal only a live
environment can settle. Each role has one arm here, and a refusal is spent once for all of them.
-}
planExecutable ::
    LogEnv ->
    TracingPort ->
    ResolveAdapter ->
    BuildMirrorQueue ->
    BuildCredentials ->
    StoreBuilds ->
    BootPlan ->
    IO (Either [BootError] ExecutablePlan)
planExecutable logEnv tracing resolveAdapter buildQueue buildCredentials builds bootPlan = case bpRole bootPlan of
    BootMirrorPipeline role ->
        fmap (executablePlan . MirrorPipelineWiring)
            <$> planMirrorWiring logEnv resolveAdapter buildQueue buildCredentials role bootPlan
    BootStorePruner -> prunerArm (deleting (sbDeleting builds))
    BootStorePreview -> prunerArm (previewing (sbObserving builds))
    BootWithoutPipeline -> pure (executablePlan . PilotWiring <$> pilotExportPlan (bpValidated bootPlan))
  where
    executablePlan wiring = ExecutablePlan{epBootPlan = bootPlan, epRoleWiring = wiring}

    prunerArm build =
        fmap (executablePlan . StorePrunerWiring)
            <$> planPrunerWiring logEnv tracing buildCredentials build bootPlan

    deleting build ports limits cleared = deletingCache <$> build ports limits cleared
    previewing build ports limits cleared = previewCache <$> build ports limits cleared

{- The store roles' shared arm: the advisory sync their rules read, the credential their stores
answer to, and one store per cleared target. All three refusable steps accumulate. -}
planPrunerWiring :: LogEnv -> TracingPort -> BuildCredentials -> BuildSweepCache -> BootPlan -> IO (Either [BootError] PrunerWiring)
planPrunerWiring logEnv tracing buildCredentials buildStore bootPlan = do
    deferredMetrics <- newDeferredMetrics getCurrentTime
    cveSync <- planAdvisorySync logEnv bootPlan
    credentials <- buildCredentials (credentialReportersOver deferredMetrics) credentialBackends
    -- 'waitSeconds' keeps the sub-second part: every pace this budget produces is well under one.
    (budgetPort, gateFor) <- newBudgetMeter waitSeconds
    let budget =
            BudgetPorts
                { bpGateFor = gateFor
                , bpOverrides = drgQuotaOverrides dredger
                , bpNominalPace = nominalPackagePace (drgChunkSize dredger) (drgChunkPause dredger)
                }
    stores <-
        planStoreMaintenance
            buildStore
            tracing
            budget
            (fromRight noCredentialProviders credentials)
            (bpLimits bootPlan)
            (vpMirrorStores validated)
    caches <-
        planStoreMaintenanceFor
            PrivateCacheCredential
            buildStore
            tracing
            budget
            (fromRight noCredentialProviders credentials)
            (bpLimits bootPlan)
            (Map.map snd (vpPrivateCaches validated))
    -- A refused sync leaves the rules abstaining, so the policies below still prepare and still
    -- report. The accumulation then discards them along with the sync.
    let ruleDepsFor =
            cveRuleDepsFor
                (fromRight mempty cveSync)
                (deferredBreakerReporter deferredMetrics EffectfulRule)
                (katipFaultReporter logEnv)
    policies <- Map.fromList <$> traverse (sweepPolicyFor ruleDepsFor) (vpMounts validated)
    pure . validationToEither $
        prunerWiringFrom deferredMetrics budgetPort policies
            <$> eitherToValidation cveSync
            <* eitherToValidation credentials
            <*> eitherToValidation (stores >>= pairEach (fromRight mempty caches))
            <* eitherToValidation caches
  where
    validated = bpValidated bootPlan
    dredger = cfgDredger (vpSettings validated)
    prunerMounts = map vmMount (vpMounts validated)
    credentialBackends =
        [((eco, MirrorCredential), backend) | (eco, backend) <- mirrorBackends prunerMounts]
            <> [((eco, PrivateCacheCredential), backend) | (eco, (Just backend, _)) <- Map.toAscList (vpPrivateCaches validated)]
    -- Every mirror store beside the cache it is swept with, reporting each that has none.
    pairEach caches = validationToEither . Map.traverseWithKey (pairWithCache caches)

    {- Both of a mount's stores under the bound they share. A mirrored mount is vetted with its
    private cache, so a mirror store with none here is a refusal rather than a mount swept alone. -}
    pairWithCache caches eco mirror =
        maybe (Failure [unpaired eco]) pure $ do
            cache <- Map.lookup eco caches
            clearedCache <- snd <$> Map.lookup eco (vpPrivateCaches validated)
            clearedMirror <- Map.lookup eco (vpMirrorStores validated)
            pure $
                pairedStore
                    (maxVersionCount (bpLimits bootPlan))
                    (labelCache "mirrorTarget" clearedMirror mirror)
                    (labelCache "privateUpstream" clearedCache cache)

    unpaired eco = StoreMaintenanceUnavailable eco (PrivateCacheUnavailable "no private cache was cleared to sweep beside this mirror target")

labelCache :: Text -> ClearedBackend -> SweepCache -> SweepCache
labelCache role backend cache = cache{scObserve = labelObservation role backend (scObserve cache)}

labelObservation :: Text -> ClearedBackend -> StoreObservation -> StoreObservation
labelObservation role backend observation =
    observation
        { obFacts = (obFacts observation){factBackend = role <> " " <> registryUrlText (cbUrl backend)}
        }

{- What decides for one mount's store: its own rule set, prepared as the serve path prepares its,
and the shared first-party predicate. A mount declaring no namespaces owns none. -}
sweepPolicyFor :: (Ecosystem -> RuleDeps) -> VettedMount -> IO (Ecosystem, SweepPolicy)
sweepPolicyFor ruleDepsFor vetted = do
    prepared <- prepare deps configured
    pure (eco, SweepPolicy{spRules = prepared, spConfigured = map prRule configured, spDeps = deps, spProject = project, spFirstParty = firstParty})
  where
    eco = vmEcosystem vetted
    deps = ruleDepsFor eco
    configured = mountPolicy (vmMount vetted)
    project = adapterProjectName (vmAdapter vetted)
    firstParty = maybe (const False) firstPartyName (mntFirstParty (vmConfig vetted))

{- One mount's half of a sweepable store. The configured rules ride beside the prepared ones,
because a prepared rule no longer carries the identity a deny names. -}
data SweepPolicy = SweepPolicy
    { spRules :: [PreparedRule]
    , spConfigured :: [Rule]
    , spDeps :: RuleDeps
    , spProject :: ProjectName
    , spFirstParty :: PackageName -> Bool
    }

{- The artefact the arm yields. The join is on the ecosystem, and only a mount declaring a mirror
target reaches the store map, so a store with no policy cannot arise. -}
prunerWiringFrom ::
    DeferredMetrics ->
    BudgetPort ->
    Map Ecosystem SweepPolicy ->
    Map Ecosystem CveSyncHandle ->
    Map Ecosystem SweepStore ->
    PrunerWiring
prunerWiringFrom deferredMetrics budgetPort policies cveSync stores =
    PrunerWiring
        { pwMounts =
            [ SweepMount
                { smEcosystem = eco
                , smStore = store
                , smRules = spRules policy
                , smConfigured = spConfigured policy
                , smRuleDeps = spDeps policy
                , smProjectName = spProject policy
                , smFirstParty = spFirstParty policy
                }
            | (eco, store) <- Map.toAscList stores
            , Just policy <- [Map.lookup eco policies]
            ]
        , pwCveSync = cveSync
        , pwDeferredMetrics = deferredMetrics
        , pwBudget = budgetPort
        }

{- The Pilot publishes one artifact per vetted mount, so a configured store with no mount leaves
it nothing to compile, and a role with no runtime behaviour refuses rather than idling. -}
pilotExportPlan :: ValidatedPlan -> Either [BootError] ExportLoopPlan
pilotExportPlan validated = maybeToRight [PilotWithoutEcosystem] (exportLoopPlan advisories mounted)
  where
    advisories = cfgAdvisories (vpSettings validated)
    mounted = map vmEcosystem (vpMounts validated)

{- The mirror pipeline's arm: the advisory sync, the queue backend, and the mount wiring. The three
refusable steps accumulate, so one launch reports every one rather than the earliest alone. -}
planMirrorWiring :: LogEnv -> ResolveAdapter -> BuildMirrorQueue -> BuildCredentials -> MirrorRole -> BootPlan -> IO (Either [BootError] MirrorWiring)
planMirrorWiring logEnv resolveAdapter buildQueue buildCredentials role bootPlan = do
    -- The metric instruments do not exist until the assembly builds the telemetry substrate. The
    -- credential providers minted below record through reporters 'installMetrics' makes live.
    deferredMetrics <- newDeferredMetrics getCurrentTime
    cveSync <- planAdvisorySync logEnv bootPlan
    publishBudget <- planPublishBudget memoryPlan
    queue <- planMirrorQueue buildQueue logEnv (mpQueueMemoryMaxDepth memoryPlan) (bpMirrorRuntime bootPlan)
    -- A refused sync leaves the rules abstaining, so the wiring below still builds and still
    -- reports what it refuses. The accumulation then discards it along with the sync.
    let ruleDeps =
            cveRuleDepsFor
                (fromRight mempty cveSync)
                (deferredBreakerReporter deferredMetrics EffectfulRule)
                (katipFaultReporter logEnv)
        ports =
            WiringPorts
                { wpReporters = credentialReportersOver deferredMetrics
                , wpBuildCredentials = buildCredentials
                , wpResolveAdapter = resolveAdapter
                , wpClock = getCurrentTime
                , wpRuleDeps = ruleDeps
                }
    -- The wiring reads the rule deps and the publish budget above, so it follows them rather than
    -- accumulating with them.
    wiring <- resolveBootWiring ports (mirrorMintPlan role) (bpLimits bootPlan) publishBudget validated
    pure . validationToEither $
        mirrorWiringFrom role deferredMetrics
            <$> eitherToValidation cveSync
            <*> eitherToValidation queue
            <*> eitherToValidation wiring
  where
    validated = bpValidated bootPlan
    memoryPlan = bpMemoryPlan bootPlan

-- The artefact the arm yields once its refusable steps cleared.
mirrorWiringFrom :: MirrorRole -> DeferredMetrics -> Map Ecosystem CveSyncHandle -> MirrorQueue -> BootWiring -> MirrorWiring
mirrorWiringFrom role deferredMetrics cveSync queue wiring =
    MirrorWiring
        { mwRole = role
        , mwBootWiring = wiring
        , mwCveSync = cveSync
        , mwQueue = queue
        , mwDeferredMetrics = deferredMetrics
        }

{- It creates the local data directory and discovers the advisory store's credentials, so an
environment that can do neither refuses here rather than at first sync. -}
planAdvisorySync :: LogEnv -> BootPlan -> IO (Either [BootError] (Map Ecosystem CveSyncHandle))
planAdvisorySync logEnv bootPlan =
    refuseOnThrow AdvisorySyncUnavailable $
        planCveSync logEnv (bpS3Endpoint bootPlan) settings requirements
  where
    validated = bpValidated bootPlan
    settings = vpSettings validated
    requirements =
        [ AdvisoryNeed
            { anEcosystem = vmEcosystem vetted
            , anMaxAge = mountAdvisoryAge (cfgAdvisories settings) mount
            , anEpss = mountEpssRequirement mount
            , anDatabase = mountDatabaseRequirement mount
            }
        | vetted <- vpMounts validated
        , let mount = vmMount vetted
        ]

{- Build the selected queue backend. It dials the provider to read the queue's redrive policy, so
an environment that cannot reach it refuses here rather than failing the running worker. -}
planMirrorQueue :: BuildMirrorQueue -> LogEnv -> Int -> MirrorRuntimePlan -> IO (Either [BootError] MirrorQueue)
planMirrorQueue buildQueue logEnv memoryDepth = \case
    -- Under NoMirroring nothing enqueues, so the inert queue is unreachable.
    NoMirroring -> pure (Right noMirrorQueue)
    MirrorWith queuePlan -> refuseOnThrow MirrorQueueUnavailable (buildQueue logEnv memoryDepth queuePlan)

{- One process-wide byte aggregate serves every publishing mount. It exists exactly when a
publication target is configured, the same predicate the plan's tenant derives from. -}
planPublishBudget :: MemoryPlan -> IO (Maybe PublishBudget)
planPublishBudget memoryPlan =
    forM (mpPublishTenant memoryPlan) $ \tenant -> do
        bodyBudget <- newByteAdmission (ptAggregateBytes tenant)
        pure PublishBudget{pbBodyBudget = bodyBudget, pbMaxRequestBytes = mpMaxRequestBytes memoryPlan}

-- Where a store's mirror-write credential provider records its mint breaker and refresh outcomes.
credentialReportersOver :: DeferredMetrics -> Ecosystem -> StoreTag -> CredentialReporters
credentialReportersOver deferredMetrics credentialIdentity tag =
    CredentialReporters
        { crBreakerReporter = deferredBreakerReporter deferredMetrics CredentialMint
        , crRefreshReporter = deferredRefreshReporter deferredMetrics credentialIdentity (providerLabel tag)
        }
