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
    BuildMirrorQueue,
    BuildCredentials,
    planExecutable,
) where

import Data.Time (getCurrentTime)
import Katip (LogEnv)
import Validation (eitherToValidation, validationToEither)

import Ecluse.Composition (
    BootWiring,
    PublishBudget (PublishBudget, pbBodyBudget, pbMaxRequestBytes),
    ResolveAdapter,
    WiringPorts (WiringPorts, wpClock, wpReporters, wpResolveAdapter, wpRuleDeps),
    resolveBootWiring,
 )
import Ecluse.Composition.BootError (
    BootError (AdvisorySyncUnavailable, MirrorQueueUnavailable, StorePrunerWithoutSweep),
    refuseOnThrow,
 )
import Ecluse.Composition.Credential (CredentialProviders, noCredentialProviders, providerLabel)
import Ecluse.Composition.Maintenance (BuildStoreMaintenance, planStoreMaintenance)
import Ecluse.Composition.MemoryPlan (
    MemoryPlan (mpMaxRequestBytes, mpPublishTenant, mpQueueMemoryMaxDepth),
    PublishTenant (ptAggregateBytes),
 )
import Ecluse.Composition.MirrorQueue (
    MirrorQueuePlan,
    MirrorRuntimePlan (MirrorWith, NoMirroring),
 )
import Ecluse.Composition.Plan (
    BootPlan (bpLimits, bpMemoryPlan, bpMirrorRuntime, bpRole, bpS3Endpoint, bpValidated),
 )
import Ecluse.Composition.Types (
    BootRole (BootMirrorPipeline, BootStorePruner, BootWithoutPipeline),
    MirrorRole,
 )
import Ecluse.Composition.Validate (
    ValidatedPlan (vpMirrorStores, vpMounts, vpSettings),
    VettedMount (vmEcosystem, vmMount),
 )
import Ecluse.Config (Mount, StoreTag)
import Ecluse.Core.Credential.Refresh (CredentialReporters (CredentialReporters, crBreakerReporter, crRefreshReporter))
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Queue (MirrorQueue, noMirrorQueue)
import Ecluse.Core.Server.Admission.Bytes (newByteAdmission)
import Ecluse.Core.Telemetry.Metrics (BreakerSource (CredentialMint, EffectfulRule))
import Ecluse.Core.Telemetry.Span (TracingPort)
import Ecluse.Cve.Sync (CveSyncHandle, cveRuleDepsFor, katipFaultReporter, planCveSync)
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
    | -- | @ecluse pilot@: it settles nothing a live environment decides.
      PilotWiring

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

{- | How a boot builds the selected mirror-queue backend. Injected, as the adapter resolver is,
so a spec can drive this phase's refusals without reaching a cloud.
-}
type BuildMirrorQueue = LogEnv -> Int -> MirrorQueuePlan -> IO MirrorQueue

{- | How a boot builds the mirror-write credential providers. Injected, as the queue and store
builders are, so a spec drives this phase without minting against a cloud.
-}
type BuildCredentials = (StoreTag -> CredentialReporters) -> [Mount] -> IO (Either [BootError] CredentialProviders)

{- | Plan the runtime the cleared plan's role starts, or report every refusal only a live
environment can settle. Each role has one arm here, and a refusal is spent once for all of them.
-}
planExecutable ::
    LogEnv ->
    TracingPort ->
    ResolveAdapter ->
    BuildMirrorQueue ->
    BuildCredentials ->
    BuildStoreMaintenance ->
    BootPlan ->
    IO (Either [BootError] ExecutablePlan)
planExecutable logEnv tracing resolveAdapter buildQueue buildCredentials buildStore bootPlan = case bpRole bootPlan of
    BootMirrorPipeline role ->
        fmap (executablePlan . MirrorPipelineWiring)
            <$> planMirrorWiring logEnv resolveAdapter buildQueue role bootPlan
    -- This build carries no sweep, so the arm plans its handles and then refuses.
    BootStorePruner -> planPrunerWiring tracing buildCredentials buildStore bootPlan
    BootWithoutPipeline -> pure (Right (executablePlan PilotWiring))
  where
    executablePlan wiring = ExecutablePlan{epBootPlan = bootPlan, epRoleWiring = wiring}

{- The deleting role's arm: the credential its stores answer to, then a handle per store. The
credential is the mirror write's own, so a sweep and a mirror write cannot reach one store as two
identities. Both halves plan before the refusal folds in, so one launch reports every problem. -}
planPrunerWiring :: TracingPort -> BuildCredentials -> BuildStoreMaintenance -> BootPlan -> IO (Either [BootError] ExecutablePlan)
planPrunerWiring tracing buildCredentials buildStore bootPlan = do
    deferredMetrics <- newDeferredMetrics
    credentials <- buildCredentials (credentialReportersOver deferredMetrics) prunerMounts
    stores <-
        planStoreMaintenance
            buildStore
            tracing
            (fromRight noCredentialProviders credentials)
            (bpLimits bootPlan)
            (vpMirrorStores (bpValidated bootPlan))
    pure (idlePrunerRefusal credentials stores)
  where
    prunerMounts = map vmMount (vpMounts (bpValidated bootPlan))

{- The refusal is folded in after the credential and the handles are planned, so a mint or a client
this environment refuses reports beside it rather than the refusal standing alone. -}
idlePrunerRefusal :: Either [BootError] a -> Either [BootError] b -> Either [BootError] ExecutablePlan
idlePrunerRefusal credentials stores =
    Left (fromLeft [] credentials <> fromLeft [] stores <> [StorePrunerWithoutSweep])

{- The mirror pipeline's arm: the advisory sync, the queue backend, and the mount wiring. The three
refusable steps accumulate, so one launch reports every one rather than the earliest alone. -}
planMirrorWiring :: LogEnv -> ResolveAdapter -> BuildMirrorQueue -> MirrorRole -> BootPlan -> IO (Either [BootError] MirrorWiring)
planMirrorWiring logEnv resolveAdapter buildQueue role bootPlan = do
    -- The metric instruments do not exist until the assembly builds the telemetry substrate. The
    -- credential providers minted below record through reporters 'installMetrics' makes live.
    deferredMetrics <- newDeferredMetrics
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
                , wpResolveAdapter = resolveAdapter
                , wpClock = getCurrentTime
                , wpRuleDeps = ruleDeps
                }
    -- The wiring reads the rule deps and the publish budget above, so it follows them rather than
    -- accumulating with them.
    wiring <- resolveBootWiring ports (bpLimits bootPlan) publishBudget validated
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
        planCveSync logEnv (bpS3Endpoint bootPlan) (vpSettings validated) (map vmEcosystem (vpMounts validated))
  where
    validated = bpValidated bootPlan

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
credentialReportersOver :: DeferredMetrics -> StoreTag -> CredentialReporters
credentialReportersOver deferredMetrics tag =
    CredentialReporters
        { crBreakerReporter = deferredBreakerReporter deferredMetrics CredentialMint
        , crRefreshReporter = deferredRefreshReporter deferredMetrics (providerLabel tag)
        }
