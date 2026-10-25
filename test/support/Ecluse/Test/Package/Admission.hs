-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Admission fixtures use the production gate without native install evidence.
module Ecluse.Test.Package.Admission (
    admitArtifactWithEvidence,
    admitArtifactWithDecision,
) where

import Ecluse.Core.Package (PackageDetails)
import Ecluse.Core.Package.Admission (ArtifactAdmission, admitArtifactWithInstallRead)
import Ecluse.Core.Package.Integrity (MinIntegrity)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Rules.Install (InstallEvidenceMode (ProjectedInstallFacts))
import Ecluse.Core.Rules.Types (Decision, EvalContext, SkippedCheck, skippedChecks)
import Ecluse.Core.Server.Path (Filename)

-- | Retain skipped rule evidence beside the fixture's production admission verdict.
admitArtifactWithEvidence :: EvalContext -> [PreparedRule] -> MinIntegrity -> Filename -> PackageDetails -> IO (ArtifactAdmission, [SkippedCheck])
admitArtifactWithEvidence ctx rules minIntegrity file details = do
    (admission, decision) <- admitArtifactWithDecision ctx rules minIntegrity file details
    pure (admission, skippedChecks decision)

-- | Retain the production decision without running the rules a second time.
admitArtifactWithDecision :: EvalContext -> [PreparedRule] -> MinIntegrity -> Filename -> PackageDetails -> IO (ArtifactAdmission, Decision)
admitArtifactWithDecision = admitArtifactWithInstallRead ProjectedInstallFacts Nothing
