-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's memory-plan derivation, shared by the two entry points
that must agree on it: the boot ('Ecluse.Proxy.runProxy') and the dry-run checker
('Ecluse.CheckConfig.runCheckConfig'). @check-config@'s contract is to resolve
exactly as a boot would, so this owns the @publishConfigured@ predicate and the
settings projection in one place rather than as parallel plumbing across the two
files, the memory-plan analogue of the structural guarantee
'Ecluse.Composition.validateComposition' already gives the pure half of the
composition.
-}
module Ecluse.Composition.Plan (
    resolveMemoryPlanFor,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Composition.MemoryPlan (MemoryPlan, queueTenantDemand, resolveMemoryPlan)
import Ecluse.Composition.MirrorQueue (MirrorRuntimePlan)
import Ecluse.Config (
    AppConfig (cfgCache, cfgLimits, cfgMounts, cfgQueue, cfgRuntime),
    MountConfig (mntPublicationTarget),
    RuntimeSettings (rtServeMaxInFlight),
 )
import Ecluse.Rts (EffectiveRuntimePlan)

{- | Resolve the memory plan and its boot lines from the application config, the
effective runtime plan, and the resolved mirror runtime. The 'EffectiveRuntimePlan'
is the one input the two callers deliberately vary (the boot's applied plan versus
the checker's predicted one); everything else is projected here.
-}
resolveMemoryPlanFor :: AppConfig -> EffectiveRuntimePlan -> MirrorRuntimePlan -> (MemoryPlan, [Text])
resolveMemoryPlanFor appConfig effective mirrorRuntime =
    resolveMemoryPlan
        (cfgCache appConfig)
        (cfgLimits appConfig)
        (cfgQueue appConfig)
        (rtServeMaxInFlight (cfgRuntime appConfig))
        effective
        (queueTenantDemand mirrorRuntime)
        publishConfigured
  where
    publishConfigured = any (isJust . mntPublicationTarget) (Map.elems (cfgMounts appConfig))
