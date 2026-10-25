-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Decide public artifact admission and retain skipped or unreached rule evidence.
The serve path and worker share rules, filename selection, and integrity-floor checks.
-}
module Ecluse.Core.Package.Admission (
    ArtifactAdmission (..),
    admissionTransience,
    admitArtifactWithEvidence,
    admitArtifactWithDecision,
    admitArtifactWithInstallRead,
) where

import Ecluse.Core.Package (Artifact, Hash, PackageDetails, artFilename, artHashes, pkgArtifacts)
import Ecluse.Core.Package.Integrity (
    MinIntegrity,
    VersionIntegrity (BelowFloor, MeetsFloor, NoIntegrity),
    classifyDigests,
 )
import Ecluse.Core.Rules (PreparedRule, evalRulesWithInstallRead)
import Ecluse.Core.Rules.Install (InstallEvidenceMode (ProjectedInstallFacts), InstallEvidenceRead)
import Ecluse.Core.Rules.Types (
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
    EvalContext,
    SkippedCheck,
    Transience (WontResolve),
    completeEvidence,
    skippedChecks,
 )
import Ecluse.Core.Server.Path (Filename, unFilename)

-- | A public artifact verdict. Inability retains the shared serve and worker retry classification.
data ArtifactAdmission
    = -- | The matched filename and artifact, with digests that meet the integrity floor.
      AdmissionAdmit Filename Artifact (NonEmpty Hash)
    | -- | A decisive rule or deny-by-default refusal.
      AdmissionDenied Decision
    | -- | A fail-closed inability whose decision carries retry advice.
      AdmissionUndecidable Decision
    | -- | Current metadata contains no artifact with the requested filename.
      AdmissionFileAbsent
    | -- | The selected artifact has no digest.
      AdmissionIntegrityMissing
    | -- | The selected artifact has digests, but none meets the public floor.
      AdmissionBelowFloor
    deriving stock (Show)

-- | Decide one public artifact and retain the rule evidence beside its verdict.
admitArtifactWithEvidence ::
    EvalContext ->
    [PreparedRule] ->
    MinIntegrity ->
    Filename ->
    PackageDetails ->
    IO (ArtifactAdmission, [SkippedCheck])
admitArtifactWithEvidence ctx rules minIntegrity file details = do
    (admission, decision) <- admitArtifactWithDecision ctx rules minIntegrity file details
    pure (admission, skippedChecks decision)

-- | Retain the evaluated decision for a mirror publication audit without running the rules again.
admitArtifactWithDecision ::
    EvalContext ->
    [PreparedRule] ->
    MinIntegrity ->
    Filename ->
    PackageDetails ->
    IO (ArtifactAdmission, Decision)
admitArtifactWithDecision = admitArtifactWithInstallRead ProjectedInstallFacts Nothing

-- | Retain the selected version and the closed evidence read through the shared admission gate.
admitArtifactWithInstallRead :: InstallEvidenceMode -> Maybe InstallEvidenceRead -> EvalContext -> [PreparedRule] -> MinIntegrity -> Filename -> PackageDetails -> IO (ArtifactAdmission, Decision)
admitArtifactWithInstallRead mode installRead ctx rules minIntegrity file details = do
    decision <- evalRulesWithInstallRead mode ctx rules (completeEvidence details) installRead
    pure (admissionOf minIntegrity file details decision, decision)

-- The filename and integrity steps over a settled rules decision.
admissionOf :: MinIntegrity -> Filename -> PackageDetails -> Decision -> ArtifactAdmission
admissionOf minIntegrity file details decision =
    case decision of
        Admitted{} -> admitFile minIntegrity file details
        Blocked{} -> AdmissionDenied decision
        BlockedByDefault{} -> AdmissionDenied decision
        Undecidable{} -> AdmissionUndecidable decision

-- The filename and integrity steps, once the rules engine has admitted the version.
admitFile :: MinIntegrity -> Filename -> PackageDetails -> ArtifactAdmission
admitFile minIntegrity file details = case artifactFor file details of
    Nothing -> AdmissionFileAbsent
    Just artifact -> case classifyDigests minIntegrity (artHashes artifact) of
        MeetsFloor ->
            -- 'MeetsFloor' guarantees a digest is present, but 'artHashes' is a plain list.
            -- The unreachable empty case fails closed, as if no digest existed.
            maybe AdmissionIntegrityMissing (AdmissionAdmit file artifact) (nonEmpty (artHashes artifact))
        BelowFloor -> AdmissionBelowFloor
        NoIntegrity -> AdmissionIntegrityMissing

-- | Retry advice for an inability, shared by the serve and worker paths.
admissionTransience :: ArtifactAdmission -> Maybe Transience
admissionTransience = \case
    AdmissionUndecidable (Undecidable transience _) -> Just transience
    -- 'AdmissionUndecidable' carries only an 'Undecidable' here, so another decision is a
    -- construction fault. Fail closed: an inability no retry clears.
    AdmissionUndecidable _ -> Just WontResolve
    AdmissionAdmit{} -> Nothing
    AdmissionDenied{} -> Nothing
    AdmissionFileAbsent -> Nothing
    AdmissionIntegrityMissing -> Nothing
    AdmissionBelowFloor -> Nothing

-- 'Nothing' when no artifact carries the filename: a forwarded miss, never a fabricated location.
artifactFor :: Filename -> PackageDetails -> Maybe Artifact
artifactFor file details =
    find ((== unFilename file) . artFilename) (pkgArtifacts details)
