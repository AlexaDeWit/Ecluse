-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The one refusal: an explicit override that breaks the plan. A pinned bound never sheds,
so it answers for its own bytes. The plan re-derives the override-free minimum, with every
pin substituted out and every computed tenant at the floor the shed ladder reaches, and
refuses just when that minimum fits the ceiling while the pinned plan does not. It names the
pins whose individual removal would fit, or all of them when only their combination
overshoots. A pod too small even without the pins degrades through
"Ecluse.Composition.MemoryPlan.Shed" like any other.
-}
module Ecluse.Composition.MemoryPlan.Override (
    -- * The pin set
    noOverridePins,
    configuredPins,

    -- * The substitution arithmetic
    overrideMinShedSum,
    overrideSubstitutions,
    overrideFreeOvershoot,

    -- * The refusal
    overrideViolationsFor,
    attributeOverrideViolations,
) where

import Data.Text qualified as T

import Ecluse.Composition.MemoryPlan.Bounds (mirrorArtifactEnvelopeMultiplier, queueDepthFloor, responseBytesFloor)
import Ecluse.Composition.MemoryPlan.Internal (
    OverridePins (..),
    PlanInputs (piCache, piExplicitAdmission, piLimits, piQueue),
    ShedOutcomes (soResidualOvershoot),
    TenantDemands (..),
 )
import Ecluse.Config (CacheSettings (csMaxBytes), LimitsSettings (limMaxArtifactBytes, limMaxRequestBytes, limMaxResponseBytes), QueueSettings (qsMemoryMaxDepth))
import Ecluse.Core.Server.MemoryModel (expandWireBytes, mirrorJobEstimatedBytes, packumentOriginFanout)

-- | Every override substituted out: the pin set the override-free minimum resolves from.
noOverridePins :: OverridePins
noOverridePins =
    OverridePins
        { opCache = Nothing
        , opAdmission = Nothing
        , opResponse = Nothing
        , opRequest = Nothing
        , opDepth = Nothing
        , opArtifact = Nothing
        }

-- | The pin set the operator's configuration actually carries.
configuredPins :: PlanInputs -> OverridePins
configuredPins inputs =
    OverridePins
        { opCache = csMaxBytes (piCache inputs)
        , opAdmission = piExplicitAdmission inputs
        , opResponse = limMaxResponseBytes limits
        , opRequest = limMaxRequestBytes limits
        , opDepth = qsMemoryMaxDepth (piQueue inputs)
        , opArtifact = limMaxArtifactBytes limits
        }
  where
    limits = piLimits inputs

{- | The fully-shed minimum tenant sum for a pin set. Each unpinned tenant contributes
the floor the shed ladder reaches. Comparing pin sets attributes an overshoot to its pins.
-}
overrideMinShedSum :: TenantDemands -> OverridePins -> Int
overrideMinShedSum d pins =
    tdReserve d
        + tdFixedBuffers d
        + fromMaybe 0 (opCache pins)
        + materialFloor
        + (if tdPublishConfigured d then fromMaybe (tdRequestComputed d) (opRequest pins) else 0)
        + (if tdMemoryBacked d then fromMaybe queueDepthFloor (opDepth pins) * mirrorJobEstimatedBytes else 0)
        + (if tdMirrors d then maybe 0 (* mirrorArtifactEnvelopeMultiplier) (opArtifact pins) else 0)
  where
    materialFloor = fromMaybe 1 (opAdmission pins) * packumentOriginFanout * expandWireBytes (fromMaybe responseBytesFloor (opResponse pins))

{- | Each explicit override present in the pin set, paired with the pin set that
substitutes only it out, and with the operator's config-key name.
-}
overrideSubstitutions :: OverridePins -> [(Text, OverridePins)]
overrideSubstitutions pins =
    catMaybes
        [ ("cache.maxBytes", pins{opCache = Nothing}) <$ opCache pins
        , ("runtime.serveMaxInFlight", pins{opAdmission = Nothing}) <$ opAdmission pins
        , ("limits.maxResponseBytes", pins{opResponse = Nothing}) <$ opResponse pins
        , ("limits.maxRequestBytes", pins{opRequest = Nothing}) <$ opRequest pins
        , ("queue.memoryMaxDepth", pins{opDepth = Nothing}) <$ opDepth pins
        , ("limits.maxArtifactBytes", pins{opArtifact = Nothing}) <$ opArtifact pins
        ]

-- How far a pin set's fully-shed minimum overshoots the ceiling.
overshootFor :: TenantDemands -> OverridePins -> Int
overshootFor d pins = max 0 (overrideMinShedSum d pins - tdCeiling d)

{- | The overshoot with every pin substituted out. Above zero, the pod is too small
whatever the operator configured, so no pin is to blame.
-}
overrideFreeOvershoot :: TenantDemands -> Int
overrideFreeOvershoot d = overshootFor d noOverridePins

-- | The plan's own refusal decision, over its resolved demands and shed outcomes.
overrideViolationsFor :: TenantDemands -> ShedOutcomes -> [Text]
overrideViolationsFor d o =
    attributeOverrideViolations
        (tdCeiling d)
        (soResidualOvershoot o)
        (overrideFreeOvershoot d)
        [(name, overshootFor d pins) | (name, pins) <- overrideSubstitutions (tdPins d)]

{- | Decide the override refusal and name the culprits. A pod too small even without the
pins degrades instead of refusing. When no single pin flips the verdict, name them all.
-}
attributeOverrideViolations :: Int -> Int -> Int -> [(Text, Int)] -> [Text]
attributeOverrideViolations heapCeiling overriddenOvershoot freeOvershoot perOverrideOvershoot
    | overriddenOvershoot > 0 && freeOvershoot <= 0 =
        [ "explicit override(s) "
            <> T.intercalate ", " culprits
            <> " push the combined memory plan "
            <> show overriddenOvershoot
            <> " bytes past the effective heap ceiling "
            <> show heapCeiling
            <> "; the override-free minimum fits within it, so lower them or raise the ceiling"
        ]
    | otherwise = []
  where
    culprits = case [name | (name, o) <- perOverrideOvershoot, o <= 0] of
        [] -> map fst perOverrideOvershoot
        flips -> flips
