-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The single public-version admission gate, shared by the serve path and the
mirror worker.

Admitting a public version to a concrete artifact request is a three-step decision.
The rules engine decides the __version__ ('Ecluse.Core.Rules.evalRules'). The
requested filename selects the __artifact__ ('artifactFor'). The integrity-floor
admission policy decides whether that artifact's digests are __strong enough to
gate__ ('Ecluse.Core.Package.Integrity.classifyArtifacts').

The serve pipeline's public tarball gate and the mirror worker's ingest-time
re-evaluation both call the one 'admitArtifact' here. The two contexts therefore
cannot drift. A version the worker would freeze into the rule-exempt mirror is exactly
a version the serve gate would admit. A tightened policy refuses it in both places for
the same reason: a new deny rule, a raised floor, or a withdrawn file. Each context
projects the shared 'ArtifactAdmission' onto its own surface, an HTTP status or a queue
ack/redeliver. Those projections are total, so neither consumer can silently ignore a
new admission outcome.
-}
module Ecluse.Core.Package.Admission (
    ArtifactAdmission (..),
    admitArtifact,
    artifactFor,
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
 )

{- | The admission verdict for one requested artifact of one public version. It is the
shared vocabulary that the serve gate and the worker's ingest re-evaluation each project
onto their own surface.

The constructors separate the /deliberate/ refusals from the /inability/ to decide
('AdmissionUndecidable'), because the two consumers act on that split differently. A
deliberate refusal is a rule denial, an integrity-policy refusal, or an absent file.
Serve renders a denial @403@ and an inability @503@\/@500@. The worker retires a
denied job (ack, never publish) and leaves an undecidable one to redeliver.
-}
data ArtifactAdmission
    = {- | The rules admitted the version, the requested filename selected an
      artifact, and its digests clear the integrity floor: serve it or mirror it.
      Carries the artifact and its integrity digests exactly as the floor checked
      them, non-empty as a fact of admission, so both consumers act on this one
      floor-checked set rather than each re-deriving and re-guarding it from the
      artifact. The serve pipeline captures the set on the mirror job it enqueues,
      and the worker's tamper gate verifies the fetched bytes against it.
      -}
      AdmissionAdmit Artifact (NonEmpty Hash)
    | {- | A rule (or deny-by-default) blocked the version. Carries the 'Blocked' \/
      'BlockedByDefault' 'Decision' so each consumer renders the deciding rule and
      reason on its own surface.
      -}
      AdmissionDenied Decision
    | {- | The version could not be decided: a fail-closed rule whose evaluation was
      unavailable. Carries the 'Undecidable' 'Decision' with its
      'Ecluse.Core.Rules.Types.Transience', so serve can choose @503@ vs @500@ and
      the worker can leave the job to redeliver.
      -}
      AdmissionUndecidable Decision
    | {- | The rules admitted the version but no artifact carries the requested
      filename: a forwarded miss on the serve path, or a withdrawn-file drop at the
      worker. Never a fabricated location.
      -}
      AdmissionFileAbsent
    | {- | The selected artifact carries no integrity digest of any kind, so nothing
      ties its bytes to a tamper-evident fingerprint. The admission policy refuses it
      (deny-by-default), distinct from 'AdmissionBelowFloor' so the refusal can say
      which.
      -}
      AdmissionIntegrityMissing
    | {- | The selected artifact carries digests, but none meets the configured
      public-integrity floor (e.g. a legacy SHA-1 shasum only, under the SHA-256
      floor). The admission policy refuses it.
      -}
      AdmissionBelowFloor
    deriving stock (Show)

{- | Decide one requested artifact of one public version under current policy. The rules
decide the version first, and the engine's first decisive verdict wins. The requested
filename then selects the artifact. The integrity-floor admission policy then judges
that artifact.

The rules run first, so an artifact-level refusal never masks a version-level
denial and a version a rule already denies costs no integrity classification. The
floor applies to the __selected__ artifact only: the one whose bytes would be served
or mirrored.

This is the one admission decision for both contexts. The serve pipeline calls it on
a public tarball request. The mirror worker calls it at ingest with the same prepared
rules, the same clock, the same configured floor, and the job's own filename. The
enqueue → process window can therefore only ever /narrow/ what the worker mirrors, when
policy tightens or a file is withdrawn. It never admits past the serve gate.
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
                MeetsFloor ->
                    -- 'MeetsFloor' guarantees a digest is present, but 'artHashes'
                    -- is a plain list. The unreachable empty case therefore fails
                    -- closed here, in the one place that extracts the digest set. It
                    -- yields the same refusal a digest-less artifact receives.
                    -- Neither consumer re-derives or re-guards the set.
                    maybe AdmissionIntegrityMissing (AdmissionAdmit artifact) (nonEmpty (artHashes artifact))
                BelowFloor -> AdmissionBelowFloor
                NoIntegrity -> AdmissionIntegrityMissing
        Blocked{} -> AdmissionDenied decision
        BlockedByDefault{} -> AdmissionDenied decision
        Undecidable{} -> AdmissionUndecidable decision

{- | Select the artifact a request's filename names from a version's distribution
files. An npm version has exactly one artifact, so the match is that single file. A
many-per-version ecosystem (PyPI) would select the wheel\/sdist whose filename the
client requested. 'Nothing' when no artifact carries the requested filename: a
forwarded miss, never a fabricated location.
-}
artifactFor :: Text -> PackageDetails -> Maybe Artifact
artifactFor file details =
    find ((== file) . artFilename) (pkgArtifacts details)
