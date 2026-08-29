-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The solver's working records, the vocabulary "Ecluse.Composition.MemoryPlan" passes between
resolving the tenant demands, walking the shed ladder, and rendering the boot lines. Their
constructors stay out of that module's public contract, so this is the only module exporting them,
for its own cluster and for the specs that build a record directly. Importing it opts out of the
public module's stability promise, as @text@ does.
-}
module Ecluse.Composition.MemoryPlan.Internal (
    PlanInputs (..),
    TenantDemands (..),
    OverridePins (..),
    ShedOutcomes (..),
) where

import Ecluse.Composition.MemoryPlan.Types (QueueTenantDemand)
import Ecluse.Config (CacheSettings, LimitsSettings, QueueSettings)

{- | Everything resolved before the plan knows whether a heap ceiling exists. The solved
path and the fallback path both read it.
-}
data PlanInputs = PlanInputs
    { piCache :: CacheSettings
    , piLimits :: LimitsSettings
    , piQueue :: QueueSettings
    , piExplicitAdmission :: Maybe Int
    , piPublishConfigured :: Bool
    , piQueueDemand :: QueueTenantDemand
    , piCapabilities :: Int
    , piAllocAreaBytes :: Int
    , piCpuAdmission :: Int
    , piCpuAdmissionLine :: Text
    , piCeilingClause :: Text
    }

{- | Every tenant's demand before the shed ladder walks it, resolved from the ceiling: the
byte charges, plus the counts, pins, and flags the ladder's steps read.
-}
data TenantDemands = TenantDemands
    { tdCeiling :: Int
    , tdReserve :: Int
    , tdFixedBuffers :: Int
    , tdPins :: OverridePins
    , tdCacheDesired :: Int
    , tdCacheEntriesExplicit :: Maybe Int
    -- ^ Cache entries, not bytes.
    , tdMaterialDesired :: Int
    , tdMaterialMinimum :: Int
    , tdAdmissionDesired :: Int
    -- ^ Concurrent serve operations, not bytes.
    , tdPublishConfigured :: Bool
    , tdPublishDesired :: Int
    , tdRequestFinal :: Int
    , tdRequestComputed :: Int
    , tdDepthDesired :: Int
    -- ^ Queued jobs, not bytes.
    , tdMemoryBacked :: Bool
    , tdMirrors :: Bool
    , tdArtifactCapDesired :: Int
    , tdMirrorChargeDesired :: Int
    }

{- | Explicit overrides, each pinned ('Just') or substituted out ('Nothing'), in the plan's
allocation order. A pinned bound never sheds and answers for its own overshoot.
-}
data OverridePins = OverridePins
    { opCache :: Maybe Int
    , opAdmission :: Maybe Int
    , opResponse :: Maybe Int
    , opRequest :: Maybe Int
    , opDepth :: Maybe Int
    , opArtifact :: Maybe Int
    }
    deriving stock (Eq, Show)

{- | Every tenant's post-shed value plus the residual overshoot the ladder could not
reclaim. The combined invariant is a pure function of this record.
-}
data ShedOutcomes = ShedOutcomes
    { soMirrorShed :: Int
    , soArtifactCapFinal :: Int
    , soCacheShed :: Int
    , soCacheFinal :: Int
    , soMaterialShed :: Int
    , soMaterialFinal :: Int
    , soAdmissionFinal :: Int
    , soResponseFinal :: Int
    , soPublishShed :: Int
    , soPublishFinal :: Int
    , soQueueShedBytes :: Int
    , soDepthFinal :: Int
    , soQueueTenantBytes :: Int
    , soResidualOvershoot :: Int
    }
