-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The boot lines the memory plan emits: one per resolved bound, tagged with its
provenance, and one warning per rung the shed ladder took. The boot logs them and the
dry-run checker ('Ecluse.CheckConfig.runCheckConfig') prints the same text, so an operator
reads one plan whichever path produced it.
-}
module Ecluse.Composition.MemoryPlan.Render (
    renderPlanLines,
    renderDegradations,
) where

import Ecluse.Composition.MemoryPlan.Internal (
    OverridePins (opAdmission, opArtifact, opCache, opDepth, opRequest, opResponse),
    PlanInputs (piAllocAreaBytes, piCapabilities, piCeilingClause, piCpuAdmissionLine),
    ShedOutcomes (..),
    TenantDemands (..),
 )
import Ecluse.Composition.MemoryPlan.Override (overrideFreeOvershoot)
import Ecluse.Composition.MemoryPlan.Shed (cacheEntryBound)
import Ecluse.Composition.Sizing (renderSized)

{- | The ordered boot lines check-config prints: one per resolved bound, tagged with its
provenance (an explicit config value, or the ceiling it was computed from).
-}
renderPlanLines :: PlanInputs -> TenantDemands -> ShedOutcomes -> [Text]
renderPlanLines inputs d o =
    [ planLine "runtime reserve" (tdReserve d) Nothing
    , piCpuAdmissionLine inputs
    , planLine "admission capacity" (soAdmissionFinal o) (opAdmission pins)
    , planLine "material aggregate" (soMaterialFinal o) Nothing
    , planLine "response byte cap" (soResponseFinal o) (opResponse pins)
    , planLine "request byte cap" (tdRequestFinal d) (opRequest pins)
    , planLine "cache byte bound" (soCacheFinal o) (opCache pins)
    , planLine "cache entry bound" (cacheEntryBound d o) (tdCacheEntriesExplicit d)
    ]
        <> [planLine "publish aggregate" (soPublishFinal o) Nothing | tdPublishConfigured d]
        <> [planLine "memory-queue depth" (soDepthFinal o) (opDepth pins) | tdMemoryBacked d]
        <> [planLine "mirror artifact byte cap" (soArtifactCapFinal o) (opArtifact pins) | tdMirrors d]
  where
    pins = tdPins d
    planLine name value explicit = renderSized ("memory plan: " <> name) value explicit computedClause
    computedClause = "computed from heap ceiling " <> show (tdCeiling d) <> ", " <> piCeilingClause inputs

-- | The shed-ladder warnings, in ladder order, each naming what was given up and why.
renderDegradations :: PlanInputs -> TenantDemands -> ShedOutcomes -> Maybe Int -> [Text]
renderDegradations inputs d o shedCaps =
    catMaybes
        [ listToMaybe
            [ shedWarning "mirror artifact byte cap" (tdArtifactCapDesired d) (soArtifactCapFinal o) "this pod mirrors no artifact it cannot buffer safely"
            | soMirrorShed o > 0
            ]
        , listToMaybe
            [ shedWarning "cache aggregate" (tdCacheDesired d) (soCacheFinal o) "the proxy serves uncached"
            | soCacheShed o > 0
            ]
        , listToMaybe
            [ "memory plan: admission shed to "
                <> show (soAdmissionFinal o)
                <> " in-flight operation(s) (the material share cannot hold more at the floor response cap)"
            | soMaterialShed o > 0
            ]
        , capabilityShedWarning inputs <$> shedCaps
        , listToMaybe
            [ "memory plan: publish aggregate shed to one maximum request (" <> show (soPublishFinal o) <> " bytes)"
            | soPublishShed o > 0
            ]
        , listToMaybe
            ["memory plan: memory-queue depth shed to " <> show (soDepthFinal o) | soQueueShedBytes o > 0]
        , listToMaybe
            [irreducibleMinimumWarning freeOvershoot | soResidualOvershoot o > 0 && freeOvershoot > 0]
        ]
  where
    freeOvershoot = overrideFreeOvershoot d

-- A tenant that gave up bytes, with the consequence it carries once it reaches zero.
shedWarning :: Text -> Int -> Int -> Text -> Text
shedWarning tenant desired final atZero =
    "memory plan: "
        <> tenant
        <> " shed from "
        <> show desired
        <> " to "
        <> show final
        <> " bytes to fit the heap ceiling"
        <> (if final == 0 then " (" <> atZero <> ")" else "")

capabilityShedWarning :: PlanInputs -> Int -> Text
capabilityShedWarning inputs shedTo =
    "memory plan: capability count shed to "
        <> show shedTo
        <> " (the nursery of "
        <> show (piCapabilities inputs)
        <> " capabilities x "
        <> show (piAllocAreaBytes inputs)
        <> " bytes allocation area is the memory pressure; fewer, or a smaller GHCRTS -A, fits this pod)"

irreducibleMinimumWarning :: Int -> Text
irreducibleMinimumWarning overshoot =
    "memory plan: the irreducible minimum (one operation on one capability, no cache) still exceeds the heap ceiling by "
        <> show overshoot
        <> " bytes; booting anyway with the container limit as the only backstop -- give this pod more memory"
