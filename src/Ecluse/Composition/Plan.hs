-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's memory-plan derivation, shared by the two entry points
that must agree on it: the boot ('Ecluse.Proxy.runProxy') and the dry-run checker
('Ecluse.CheckConfig.runCheckConfig'). The @check-config@ command must resolve
exactly as a boot does. This module therefore owns the @publishConfigured@ predicate
and the settings projection in one place, not as parallel plumbing across the two
files. It is the memory-plan analogue of the structural guarantee
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

{- | Resolve the memory plan and its boot lines. The 'EffectiveRuntimePlan' is the one
input the two callers vary: boot passes the applied plan, the checker the predicted one.
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
