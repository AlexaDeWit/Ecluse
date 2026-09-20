-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Static material estimates for metadata work, independent of CPU and ingest limits.
module Ecluse.Core.Server.Admission.Material (
    MaterialAdmission,
    MaterialAllowances (..),
    MaterialWork (..),
    newMaterialAdmission,
    newMaterialAdmissionTuned,
    withMaterialAdmission,
) where

import UnliftIO (MonadUnliftIO)

import Ecluse.Core.Server.Admission.Weighted (
    AdmissionObservers (..),
    WeightedAdmission,
    admissionWaitMicros,
    newWeightedAdmission,
    withWeightedAdmission,
 )
import Ecluse.Core.Server.Cache.Store (MaterialReuse (..))

-- | Reviewed static costs for a declared workload, not worst-case heap bounds.
data MaterialAllowances = MaterialAllowances
    { maColdSelectedBytes :: Int
    -- ^ Selected metadata materialisation and fresh policy evaluation.
    , maRetainedSelectedBytes :: Int
    -- ^ Policy evaluation over the captured selected value, including cached absence.
    , maFullOriginBytes :: Int
    -- ^ One configured, permitted full metadata read, including external lookup and fallback.
    , maListingOutputBytes :: Int
    -- ^ Listing policy, merge and output work after the origins resolve.
    }
    deriving stock (Eq, Show)

-- | The work that the bracket covers, determined before expensive reads start.
data MaterialWork
    = SelectedMaterial MaterialReuse
    | -- | Number of full origins this request may read.
      ListingMaterial Int
    deriving stock (Eq, Show)

-- | Process-wide material capacity with a separately bounded waiting room.
data MaterialAdmission = MaterialAdmission
    { maCore :: WeightedAdmission
    , maCapacity :: Int
    , maAllowances :: MaterialAllowances
    }

-- | Build the material gate with a waiter count independent of byte capacity.
newMaterialAdmission :: Int -> Int -> MaterialAllowances -> IO MaterialAdmission
newMaterialAdmission capacity room = newMaterialAdmissionTuned capacity room admissionWaitMicros

-- | Build a gate with an explicit wait budget in microseconds for lifecycle tests.
newMaterialAdmissionTuned :: Int -> Int -> Int -> MaterialAllowances -> IO MaterialAdmission
newMaterialAdmissionTuned capacity room waitMicros allowances = do
    let bounded = max 1 capacity
    core <- newWeightedAdmission bounded room waitMicros
    pure (MaterialAdmission core bounded allowances)

-- | Hold the estimate through metadata and policy work, capped for one request to make progress.
withMaterialAdmission :: (MonadUnliftIO m) => MaterialAdmission -> MaterialWork -> m a -> m (Maybe a)
withMaterialAdmission admission work =
    withWeightedAdmission observers (maCore admission) weight
  where
    observers = AdmissionObservers pass pass (const pass)
    allowances = maAllowances admission
    positive = toInteger . max 1
    estimated = case work of
        SelectedMaterial KnownLocalReuse -> positive (maRetainedSelectedBytes allowances)
        SelectedMaterial NeedsMaterialisation -> positive (maColdSelectedBytes allowances)
        ListingMaterial origins ->
            positive (maListingOutputBytes allowances)
                + toInteger (max 0 origins) * positive (maFullOriginBytes allowances)
    weight = fromInteger (min (toInteger (maCapacity admission)) estimated)
