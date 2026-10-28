-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Verified artifact bodies share bounded byte capacity independently from publication.
module Ecluse.Core.Server.Admission.Artifact (
    VerifiedArtifactBudget,
    newVerifiedArtifactBudget,
    verifiedArtifactMaxBytes,
    withVerifiedArtifactAdmission,
) where

import UnliftIO (MonadUnliftIO)

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Server.Admission.Artifact.Internal (VerifiedArtifactBudget (..))
import Ecluse.Core.Server.Admission.Weighted (AdmissionObservers (..), admissionWaitMicros, newWeightedAdmission, withWeightedAdmission)
import Ecluse.Core.Telemetry.Record (MetricsPort (..))

{- | Refuse a non-positive body cap or aggregate capacity below it.
Both limits count wire bytes. Waiting uses the bounded serve-admission timing.
-}
newVerifiedArtifactBudget :: Deadlines -> Int -> Int -> IO (Either Text VerifiedArtifactBudget)
newVerifiedArtifactBudget deadlines body aggregate
    | body <= 0 || aggregate < body = pure (Left "verified artifact limits require a positive body cap within aggregate capacity")
    | otherwise = Right . VerifiedArtifactBudget body aggregate <$> newWeightedAdmission deadlines aggregate 8 admissionWaitMicros

-- | The resolved body ceiling applies to both known and unknown source lengths.
verifiedArtifactMaxBytes :: VerifiedArtifactBudget -> Int
verifiedArtifactMaxBytes (VerifiedArtifactBudget body _ _) = body

-- | Oversized reservations refuse explicitly and never clamp to smaller weights.
withVerifiedArtifactAdmission :: (MonadUnliftIO m) => MetricsPort -> VerifiedArtifactBudget -> Int -> m a -> m (Maybe a)
withVerifiedArtifactAdmission metrics (VerifiedArtifactBudget cap aggregate gate) weight action
    | weight < 0 || weight > cap || weight > aggregate = liftIO (mpArtifactBodyShed metrics) $> Nothing
    | otherwise = rightToMaybe <$> withWeightedAdmission observers gate weight action
  where
    observers = AdmissionObservers (mpArtifactBodyQueued metrics) (mpArtifactBodyShed metrics) (mpArtifactBodyInFlightBytes metrics)
