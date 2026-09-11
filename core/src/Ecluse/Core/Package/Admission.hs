-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The single public-version admission gate, shared by the serve path and the mirror worker.
Admitting a version to a concrete artifact request is a three-step decision: the rules engine
decides the __version__, the requested 'Filename' selects the __artifact__, and the integrity
floor decides whether that artifact's digests are __strong enough to gate__. The serve path's
public tarball gate and the worker's ingest re-evaluation both call the one 'admitArtifact', so
a version the worker would freeze into the rule-exempt mirror is exactly a version the serve
gate would admit. Neither context decides for itself whether a retry could change a refusal:
that is 'admissionTransience', read by both.
-}
module Ecluse.Core.Package.Admission (
    ArtifactAdmission (..),
    admissionTransience,
    admitArtifact,
) where

import Ecluse.Core.Package (Artifact, Hash, PackageDetails, artFilename, artHashes, pkgArtifacts)
import Ecluse.Core.Package.Integrity (
    MinIntegrity,
    VersionIntegrity (BelowFloor, MeetsFloor, NoIntegrity),
    classifyArtifacts,
 )
import Ecluse.Core.Rules (PreparedRule, evalRules)
import Ecluse.Core.Rules.Types (
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
    EvalContext,
    Transience (WontResolve),
    completeEvidence,
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

{- | Decide one requested artifact under current policy: the rules, then the filename, then the
integrity floor. Serve and worker pass the same inputs, so re-evaluation can only /narrow/.
-}
admitArtifact ::
    EvalContext ->
    [PreparedRule] ->
    MinIntegrity ->
    -- | The requested artifact filename (the client's, or the mirror job's).
    Filename ->
    PackageDetails ->
    IO ArtifactAdmission
admitArtifact ctx rules minIntegrity file details = do
    decision <- evalRules ctx rules (completeEvidence details)
    pure $ case decision of
        Admitted{} -> case artifactFor file details of
            Nothing -> AdmissionFileAbsent
            Just artifact -> case classifyArtifacts minIntegrity (artifact :| []) of
                MeetsFloor ->
                    -- 'MeetsFloor' guarantees a digest is present, but 'artHashes' is a plain list.
                    -- The unreachable empty case fails closed, as if no digest existed.
                    maybe AdmissionIntegrityMissing (AdmissionAdmit file artifact) (nonEmpty (artHashes artifact))
                BelowFloor -> AdmissionBelowFloor
                NoIntegrity -> AdmissionIntegrityMissing
        Blocked{} -> AdmissionDenied decision
        BlockedByDefault{} -> AdmissionDenied decision
        Undecidable{} -> AdmissionUndecidable decision

{- | The transience of a verdict no rule could decide, and 'Nothing' for a settled one. The
serve gate renders it as a @503@ or a @500@, and the mirror worker redelivers or drops on it.
-}
admissionTransience :: ArtifactAdmission -> Maybe Transience
admissionTransience = \case
    AdmissionUndecidable (Undecidable transience _) -> Just transience
    -- 'admitArtifact' carries only an 'Undecidable' here, so another decision is a
    -- construction fault. Fail closed: an inability no retry clears.
    AdmissionUndecidable _ -> Just WontResolve
    AdmissionAdmit{} -> Nothing
    AdmissionDenied{} -> Nothing
    AdmissionFileAbsent -> Nothing
    AdmissionIntegrityMissing -> Nothing
    AdmissionBelowFloor -> Nothing

{- Select the artifact a request's filename names. 'Nothing' when none carries that filename:
a forwarded miss, never a fabricated location. -}
artifactFor :: Filename -> PackageDetails -> Maybe Artifact
artifactFor file details =
    find ((== unFilename file) . artFilename) (pkgArtifacts details)
