-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The boot's effectful planning phase: take the cleared 'BootPlan' and yield the
'ExecutablePlan' a mirror-pipeline role assembles its runtime from.

Every refusal a live environment can still settle is spent here, so holding an 'ExecutablePlan'
means nothing downstream can refuse to boot. A listener that fails to bind or an upstream that
stops answering is a runtime fault for supervision, not a refusal. @ecluse check-config@ makes no
cloud call, so it stops at the 'BootPlan' and never reaches this phase.
-}
module Ecluse.Composition.Executable (
    ExecutablePlan (epBootPlan, epWiring, epCveSync, epDeferredMetrics),
    planExecutable,
) where

import Data.Time (getCurrentTime)
import Katip (LogEnv)

import Ecluse.Composition (
    BootWiring,
    PublishBudget (PublishBudget, pbBodyBudget, pbMaxRequestBytes),
    ResolveAdapter,
    WiringPorts (WiringPorts, wpClock, wpReporters, wpResolveAdapter, wpRuleDeps),
    resolveBootWiring,
 )
import Ecluse.Composition.BootError (BootError)
import Ecluse.Composition.MemoryPlan (
    MemoryPlan (mpMaxRequestBytes, mpPublishTenant),
    PublishTenant (ptAggregateBytes),
 )
import Ecluse.Composition.Plan (
    BootPlan (bpLimits, bpMemoryPlan, bpS3Endpoint, bpValidated),
 )
import Ecluse.Composition.Validate (ValidatedPlan (vpMounts, vpSettings), VettedMount (vmEcosystem))
import Ecluse.Core.Credential.Refresh (CredentialReporters (CredentialReporters, crBreakerReporter, crRefreshReporter))
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Server.Admission.Bytes (newByteAdmission)
import Ecluse.Core.Telemetry.Metrics (BreakerSource (CredentialMint, EffectfulRule), Provider (CodeArtifact))
import Ecluse.Cve.Sync (CveSyncHandle, cveRuleDepsFor, katipFaultReporter, planCveSync)
import Ecluse.Runtime.Telemetry.Reporters (
    DeferredMetrics,
    deferredBreakerReporter,
    deferredRefreshReporter,
    newDeferredMetrics,
 )

{- | The boot's post-gating artefact: the cleared plan, and everything only a live environment
could settle. 'planExecutable' is its one producer, so a role cannot assemble an unvetted one.
-}
data ExecutablePlan = ExecutablePlan
    { epBootPlan :: BootPlan
    -- ^ The config-decidable plan every decision below was planned against.
    , epWiring :: BootWiring
    -- ^ The mounts the front door serves, and the publish targets the worker writes through.
    , epCveSync :: Map Ecosystem CveSyncHandle
    -- ^ One advisory-sync handle per mount ecosystem, empty where no advisory store is configured.
    , epDeferredMetrics :: DeferredMetrics
    {- ^ The metric handle the credential providers and the rule breakers already record through.
    The assembly makes those recordings live once the instruments exist.
    -}
    }

{- | Plan the runtime a mirror-pipeline role starts, or report every refusal only a live
environment can settle. The adapter resolver arrives injected, as it does one tier below.
-}
planExecutable :: LogEnv -> ResolveAdapter -> BootPlan -> IO (Either [BootError] ExecutablePlan)
planExecutable logEnv resolveAdapter bootPlan = do
    -- The metric instruments do not exist until the assembly builds the telemetry substrate. The
    -- credential providers minted below record through reporters 'installMetrics' makes live.
    deferredMetrics <- newDeferredMetrics
    -- Each mount ecosystem syncs independently, so one missing artifact never holds back another.
    -- Without a store the map is empty, rules abstain, and readiness is ungated.
    cveSync <- planCveSync logEnv (bpS3Endpoint bootPlan) (vpSettings validated) (map vmEcosystem (vpMounts validated))
    publishBudget <- planPublishBudget (bpMemoryPlan bootPlan)
    let ports =
            WiringPorts
                { wpReporters = credentialReportersOver deferredMetrics
                , wpResolveAdapter = resolveAdapter
                , wpClock = getCurrentTime
                , wpRuleDeps =
                    cveRuleDepsFor
                        cveSync
                        (deferredBreakerReporter deferredMetrics EffectfulRule)
                        (katipFaultReporter logEnv)
                }
    -- The wiring reads the rule deps and the publish budget above, so it follows them rather than
    -- accumulating with them. It is also the one step here that can refuse.
    fmap (executablePlanFrom bootPlan deferredMetrics cveSync)
        <$> resolveBootWiring ports (bpLimits bootPlan) publishBudget validated
  where
    validated = bpValidated bootPlan

-- The artefact the phase yields once its one refusable step cleared.
executablePlanFrom :: BootPlan -> DeferredMetrics -> Map Ecosystem CveSyncHandle -> BootWiring -> ExecutablePlan
executablePlanFrom bootPlan deferredMetrics cveSync wiring =
    ExecutablePlan
        { epBootPlan = bootPlan
        , epWiring = wiring
        , epCveSync = cveSync
        , epDeferredMetrics = deferredMetrics
        }

{- One process-wide byte aggregate serves every publishing mount. It exists exactly when a
publication target is configured, the same predicate the plan's tenant derives from. -}
planPublishBudget :: MemoryPlan -> IO (Maybe PublishBudget)
planPublishBudget memoryPlan =
    forM (mpPublishTenant memoryPlan) $ \tenant -> do
        bodyBudget <- newByteAdmission (ptAggregateBytes tenant)
        pure PublishBudget{pbBodyBudget = bodyBudget, pbMaxRequestBytes = mpMaxRequestBytes memoryPlan}

-- Where the mirror-write credential providers record their mint breaker and refresh outcomes.
credentialReportersOver :: DeferredMetrics -> CredentialReporters
credentialReportersOver deferredMetrics =
    CredentialReporters
        { crBreakerReporter = deferredBreakerReporter deferredMetrics CredentialMint
        , crRefreshReporter = deferredRefreshReporter deferredMetrics CodeArtifact
        }
