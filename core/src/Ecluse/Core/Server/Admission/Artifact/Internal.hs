-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The verified-read budget representation supports pipeline code and admission probes.
module Ecluse.Core.Server.Admission.Artifact.Internal (VerifiedArtifactBudget (..)) where

import Ecluse.Core.Server.Admission.Weighted (WeightedAdmission)

-- | Per-body and aggregate byte limits share their admission gate.
data VerifiedArtifactBudget = VerifiedArtifactBudget Int Int WeightedAdmission
