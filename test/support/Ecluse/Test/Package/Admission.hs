-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Admission fixtures read skipped rule evidence beside the production gate's verdict.
module Ecluse.Test.Package.Admission (
    admitArtifactWithEvidence,
    admitArtifactWithDecision,
) where

import Ecluse.Core.Package (PackageDetails)
import Ecluse.Core.Package.Admission (ArtifactAdmission, admitArtifactWithDecision)
import Ecluse.Core.Package.Integrity (MinIntegrity)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Rules.Types (EvalContext, SkippedCheck, skippedChecks)
import Ecluse.Core.Server.Path (Filename)

-- | Retain skipped rule evidence beside the fixture's production admission verdict.
admitArtifactWithEvidence :: EvalContext -> [PreparedRule] -> MinIntegrity -> Filename -> PackageDetails -> IO (ArtifactAdmission, [SkippedCheck])
admitArtifactWithEvidence ctx rules minIntegrity file details = do
    (admission, decision) <- admitArtifactWithDecision ctx rules minIntegrity file details
    pure (admission, skippedChecks decision)
