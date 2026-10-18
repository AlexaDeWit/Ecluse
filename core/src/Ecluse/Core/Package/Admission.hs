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
) where

import Ecluse.Core.Package (Artifact, Hash, PackageDetails, artFilename, artHashes, pkgArtifacts)
import Ecluse.Core.Package.Integrity (
    MinIntegrity,
    VersionIntegrity (BelowFloor, MeetsFloor, NoIntegrity),
    classifyDigests,
 )
import Ecluse.Core.Rules (PreparedRule, evalRules)
import Ecluse.Core.Rules.Types (
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
    EvalContext,
    SkippedCheck,
    Transience (WontResolve),
    completeEvidence,
    skippedChecks,
 )
import Ecluse.Core.Server.Path (Filename, unFilename)

{- | The admission verdict for one requested artifact. An inability to decide is no refusal: serve
renders @503@\/@500@, and the worker redelivers or drops per 'admissionTransience'.
-}
data ArtifactAdmission
    = {- | Admitted, with digests clearing the integrity floor. Carries the 'Filename' the gate
      matched against current metadata, and the floor-checked digest set.
      -}
      AdmissionAdmit Filename Artifact (NonEmpty Hash)
    | {- | A rule, or deny-by-default, blocked the version. Carries the 'Decision' so each consumer
      renders the deciding rule and reason on its own surface.
      -}
      AdmissionDenied Decision
    | {- | A fail-closed rule could not vet the version. Carries the 'Undecidable' 'Decision', whose
      transience 'admissionTransience' reads out for both consumers.
      -}
      AdmissionUndecidable Decision
    | {- | Admitted, but no artifact carries the requested filename: a forwarded miss on serve, a
      withdrawn-file drop at the worker, never a fabricated location.
      -}
      AdmissionFileAbsent
    | {- | The selected artifact carries no digest at all, so nothing ties its bytes to a
      fingerprint. Kept apart from 'AdmissionBelowFloor' so the refusal can say which.
      -}
      AdmissionIntegrityMissing
    | {- | The selected artifact carries digests, but none meets the configured public-integrity
      floor (a legacy SHA-1 shasum only, under a SHA-256 floor).
      -}
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
admitArtifactWithDecision ctx rules minIntegrity file details = do
    decision <- evalRules ctx rules (completeEvidence details)
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

{- | The transience of a verdict no rule could decide, and 'Nothing' for a settled one. The
serve gate renders it as a @503@ or a @500@, and the mirror worker redelivers or drops on it.
-}
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
