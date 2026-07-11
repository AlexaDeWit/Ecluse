-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The single public-version admission gate, shared by the serve path and the
mirror worker.

Admitting a public version to a concrete artifact request is a three-step decision:
the rules engine decides the __version__ ('Ecluse.Core.Rules.evalRules'), the
requested filename selects the __artifact__ ('artifactFor'), and the
integrity-floor admission policy decides whether that artifact's digests are
__strong enough to gate__ ('Ecluse.Core.Package.Integrity.classifyArtifacts'). Both
consumers of that decision -- the serve pipeline's public tarball gate and the
mirror worker's ingest-time re-evaluation -- call the one 'admitArtifact' here, so
the two contexts cannot drift: a version the worker would freeze into the
rule-exempt mirror is exactly a version the serve gate would admit, and a
tightened policy (a new deny rule, a raised floor, a withdrawn file) refuses it in
both places for the same reason. Each context projects the shared
'ArtifactAdmission' onto its own surface (an HTTP status, a queue ack/redeliver),
and those projections are total, so a new admission outcome cannot be silently
ignored by either.
-}
module Ecluse.Core.Package.Admission (
    ArtifactAdmission (..),
    admitArtifact,
    artifactFor,
) where

import Ecluse.Core.Package (Artifact, PackageDetails, artFilename, pkgArtifacts)
import Ecluse.Core.Package.Integrity (
    MinIntegrity,
    VersionIntegrity (BelowFloor, MeetsFloor, NoIntegrity),
    classifyArtifacts,
 )
import Ecluse.Core.Rules (PreparedRule, evalRules)
import Ecluse.Core.Rules.Types (
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
    EvalContext,
 )

{- | The admission verdict for one requested artifact of one public version -- the
shared vocabulary both the serve gate and the worker's ingest re-evaluation project
onto their own surfaces.

The constructors separate the /deliberate/ refusals (a rule denial, an
integrity-policy refusal, an absent file) from the /inability/ to decide
('AdmissionUndecidable'), because the two consumers act on that split differently:
serve renders a denial @403@ and an inability @503@\/@500@, the worker retires a
denied job (ack, never publish) and leaves an undecidable one to redeliver.
-}
data ArtifactAdmission
    = {- | The rules admitted the version, the requested filename selected an
      artifact, and its digests clear the integrity floor: serve it / mirror it.
      -}
      AdmissionAdmit Artifact
    | {- | A rule (or deny-by-default) blocked the version. Carries the 'Blocked' \/
      'BlockedByDefault' 'Decision' so each consumer renders the deciding rule and
      reason on its own surface.
      -}
      AdmissionDenied Decision
    | {- | The version could not be decided (a fail-closed rule whose evaluation was
      unavailable). Carries the 'Undecidable' 'Decision' with its
      'Ecluse.Core.Rules.Types.Transience', so serve can choose @503@ vs @500@ and
      the worker can leave the job to redeliver.
      -}
      AdmissionUndecidable Decision
    | {- | The rules admitted the version but no artifact carries the requested
      filename: a forwarded miss on the serve path, a withdrawn-file drop at the
      worker -- never a fabricated location.
      -}
      AdmissionFileAbsent
    | {- | The selected artifact carries no integrity digest of any kind, so its
      bytes cannot be tied to a tamper-evident fingerprint. Refused by the
      admission policy (deny-by-default), distinct from 'AdmissionBelowFloor' so
      the refusal can say which.
      -}
      AdmissionIntegrityMissing
    | {- | The selected artifact carries digests, but none meets the configured
      public-integrity floor (e.g. a legacy SHA-1 shasum only, under the SHA-256
      floor). Refused by the admission policy.
      -}
      AdmissionBelowFloor
    deriving stock (Show)

{- | Decide one requested artifact of one public version under current policy: the
rules first (the engine's first decisive verdict), then artifact selection by the
requested filename, then the integrity-floor admission policy over the selected
artifact.

The rules run first so an artifact-level refusal never masks a version-level
denial, and no integrity classification is paid for a version a rule already
denies. The floor is applied to the __selected__ artifact only (the one whose bytes
would be served or mirrored), exactly as the serve path has always gated it.

This is the one admission decision for both contexts. The serve pipeline calls it
on a public tarball request; the mirror worker calls it at ingest with the same
prepared rules, the same clock, the same configured floor, and the job's own
filename -- so the enqueue → process window can only ever /narrow/ what is mirrored
(policy tightened, file withdrawn), never admit past the serve gate.
-}
admitArtifact ::
    EvalContext ->
    [PreparedRule] ->
    MinIntegrity ->
    -- | The requested artifact filename (the client's, or the mirror job's).
    Text ->
    PackageDetails ->
    IO ArtifactAdmission
admitArtifact ctx rules minIntegrity file details = do
    decision <- evalRules ctx rules details
    pure $ case decision of
        Admitted{} -> case artifactFor file details of
            Nothing -> AdmissionFileAbsent
            Just artifact -> case classifyArtifacts minIntegrity (artifact :| []) of
                MeetsFloor -> AdmissionAdmit artifact
                BelowFloor -> AdmissionBelowFloor
                NoIntegrity -> AdmissionIntegrityMissing
        Blocked{} -> AdmissionDenied decision
        BlockedByDefault{} -> AdmissionDenied decision
        Undecidable{} -> AdmissionUndecidable decision

{- | Select the artifact a request's filename names from a version's distribution
files. npm has exactly one artifact per version, so the match is the single file; a
many-per-version ecosystem (PyPI) would select the wheel\/sdist whose filename the
client requested. 'Nothing' when no artifact carries the requested filename -- a
forwarded miss, never a fabricated location.
-}
artifactFor :: Text -> PackageDetails -> Maybe Artifact
artifactFor file details =
    find ((== file) . artFilename) (pkgArtifacts details)
