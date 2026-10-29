-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Gate public artifacts through the worker's admission oracle. An admitted GET enqueues mirroring.
module Ecluse.Core.Server.Pipeline.Tarball.Public (
    servePublicArtifact,
    servePublicArtifactWithCoverage,

    -- * The public artifact gate (exposed for direct testing)
    PublicArtifactGate (..),
    publicArtifactGate,
) where

import Katip (Severity (ErrorS), logFM, ls)
import Network.HTTP.Types (status404)
import Network.Wai (ResponseReceived)
import UnliftIO (withRunInIO)

import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Fault (TransportFault, tfDetail)
import Ecluse.Core.Package (Artifact (artUrl), PackageDetails)
import Ecluse.Core.Package.Admission (
    ArtifactAdmission (
        AdmissionAdmit,
        AdmissionBelowFloor,
        AdmissionDenied,
        AdmissionFileAbsent,
        AdmissionIntegrityMissing,
        AdmissionUndecidable
    ),
    admissionTransience,
    admitArtifactWithInstallRead,
 )
import Ecluse.Core.Queue (
    MirrorJob (MirrorJob, jobPackage, jobTraceContext, jobTriggerFilename, jobVersion),
    RemoteSpanContext,
    enqueue,
 )
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl))
import Ecluse.Core.Registry.Metadata (
    MetadataError (MetadataAbsent),
    VersionDoc (vdDetails),
    VersionEvaluation (VersionCooling, VersionMetadataUnavailable, VersionMissing, VersionPresent),
    VersionRead,
    versionEvaluation,
 )
import Ecluse.Core.Rules.Effectful (ReadFault (rfDetail))
import Ecluse.Core.Rules.Install (InstallEvidenceRead, InstallEvidenceRole (InstallPublicAdmission), installEvidenceMode)
import Ecluse.Core.Rules.Render (renderDecision, renderInability)
import Ecluse.Core.Rules.Types (EvalContext, SkippedCheck, completeEvidence, mkEvalContext, skippedChecks)
import Ecluse.Core.Security (Limits (progressFloor), Origin (UntrustedOrigin), hostPortAddress, thgPublicHostPort)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket)
import Ecluse.Core.Server.Context (
    Handler,
    PackumentDeps (..),
    ReadSourceSelection (..),
    ServeRuntime (..),
    pdMirror,
    pdTarballHostGate,
    tarballHostHonoured,
 )
import Ecluse.Core.Server.InstallEvidence (newPublicInstallEvidenceReads)
import Ecluse.Core.Server.Path (Filename)
import Ecluse.Core.Server.Pipeline.Internal (
    VersionVerdict (..),
    evalTier,
    logDenials,
    logSkippedChecksOnce,
    recordDenials,
    serveDecisionClass,
 )
import Ecluse.Core.Server.Pipeline.Origin (withPublicMetadata)
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Pipeline.Tarball.Refusal (
    artifactError,
    crossHostRefused,
    internalArtifactError,
    upstreamUnavailable,
    versionAbsent,
 )
import Ecluse.Core.Server.Pipeline.Tarball.Relay (
    ArtifactServe (ServeFull, ServeHead),
    RelayVerdict (RelayedArtifact, RelayedNonSuccess, RelayedOddShape),
    observeRelayAnomaly,
    relayJudged,
    relayPublicResult,
    withMethod,
    withValidators,
 )
import Ecluse.Core.Server.Pipeline.Tarball.Types (ArtifactRequest (..), TarballReplies (..), relayResponder)
import Ecluse.Core.Server.Response (
    ServeDecision (Admit),
    Transience (WontResolve),
    mkRefusal,
    rejectUnavailable,
    serveDecisionOf,
 )
import Ecluse.Core.Server.Stream (UpstreamResult (..))
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit, NoMirrorWrite))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (spanMirrorEnqueue, spanRuleEval)
import Ecluse.Core.Telemetry.Upstream qualified as PublicRead
import Ecluse.Core.Version (renderVersion)

-- | Gate the requested version under the mount's admission budget, then relay it.
servePublicArtifact :: ArtifactRequest response -> Handler ResponseReceived
servePublicArtifact = servePublicArtifactWithCoverage (const True)

-- | A managed fallback must cover the whole expected version before its requested file is admitted.
servePublicArtifactWithCoverage :: (Maybe PackageDetails -> Bool) -> ArtifactRequest response -> Handler ResponseReceived
servePublicArtifactWithCoverage covered ctx
    | pdReadSourceSelection (arDeps ctx) == PrivateOnly || (pdReadSourceSelection (arDeps ctx) == ManagedMirrorAndPublic && pdFirstParty (arDeps ctx) (arPackage ctx)) =
        liftIO (arRespond ctx (tarballError (arReplies ctx) status404 [] (mkRefusal (pdHelp (arDeps ctx)) "this registry view has no public fallback")))
    | otherwise = servePublicWithCoverage covered ctx

servePublicWithCoverage :: (Maybe PackageDetails -> Bool) -> ArtifactRequest response -> Handler ResponseReceived
servePublicWithCoverage covered ctx = do
    advisoryEtag <- liftIO (pdAdvisoryEtag (arDeps ctx))
    withMetadataAdmission
        (arRuntime ctx)
        (\headers -> liftIO (arRespond ctx (tarballError (arReplies ctx) shedStatus headers (mkRefusal Nothing shedMessage))))
        (\ticket -> withPublicMetadata (arRuntime ctx) (arDeps ctx) ticket (arPackage ctx) (arVersion ctx) (gatePublicVersion ticket covered ctx advisoryEtag))
        $ \case
            Admitted artifact skipped -> serveAdmitted ctx advisoryEtag artifact skipped
            Refused decision -> refusePublic ctx advisoryEtag decision

serveAdmitted :: ArtifactRequest response -> Maybe DbEtag -> Artifact -> [SkippedCheck] -> Handler ResponseReceived
serveAdmitted ctx advisoryEtag artifact skipped = do
    let metrics = srMetrics (arRuntime ctx)
    liftIO (mpServeDecision metrics Metric.Admit)
    logSkippedChecksOnce (pdNoteAdmission (arDeps ctx)) (arPackage ctx) (renderVersion (arVersion ctx)) advisoryEtag skipped
    withRunInIO $ \runInIO ->
        streamPublicArtifact ctx artifact (runInIO . observeRelayAnomaly metrics (arPackage ctx) (arVersion ctx))

refusePublic :: ArtifactRequest response -> Maybe DbEtag -> ServeDecision -> Handler ResponseReceived
refusePublic ctx advisoryEtag decision = do
    let metrics = srMetrics (arRuntime ctx)
    liftIO (mpServeDecision metrics (serveDecisionClass decision))
    logDenials (arPackage ctx) advisoryEtag [VersionVerdict (renderVersion (arVersion ctx)) decision]
    liftIO (recordDenials metrics [decision])
    liftIO (arRespond ctx (artifactError (arReplies ctx) (arDeps ctx) decision))

-- | Preserve the admitted artifact's authoritative location through the public gate.
data PublicArtifactGate
    = -- | The gate admitted the version: the artifact selected by filename, and the checks the admission skipped.
      Admitted Artifact [SkippedCheck]
    | -- | The gate refused the version: a policy denial, an upstream outage, or absence.
      Refused ServeDecision

gatePublicVersion :: MemoryTicket -> (Maybe PackageDetails -> Bool) -> ArtifactRequest response -> Maybe DbEtag -> IO (Either MetadataError VersionRead) -> Handler PublicArtifactGate
gatePublicVersion ticket covered ctx advisoryEtag readSelected = do
    evalCtx <- liftIO (mkEvalContext (pdNow deps) (pure advisoryEtag))
    eval <-
        liftIO $
            readSelected <&> \case
                Left MetadataAbsent -> VersionMissing
                result -> versionEvaluation result
    case eval of
        VersionCooling retry -> pure (Refused (upstreamCooling retry))
        VersionMetadataUnavailable -> pure (Refused upstreamUnavailable)
        VersionMissing -> pure (Refused (if covered Nothing then versionAbsent else upstreamUnavailable))
        VersionPresent doc _ | not (covered (Just (vdDetails doc))) -> pure (Refused upstreamUnavailable)
        VersionPresent doc _ -> withRunInIO $ \run -> do
            let evaluateWith installRead = spanRuleEval (srTracing rt) (arPackage ctx) (arVersion ctx) $ do
                    (gate, seconds) <- timedSeconds (gateVersion evalCtx deps installRead (arFile ctx) (vdDetails doc))
                    mpRuleEvalDuration (srMetrics rt) (evalTier (pdRules deps)) seconds
                    pure (gate, gateVerdict gate)
            case pdInstallEvidence deps of
                Nothing -> evaluateWith Nothing
                Just _ -> do
                    evidenceReads <- newPublicInstallEvidenceReads deps (srPublicClient rt) (srPublicControl rt) InstallPublicAdmission ticket (run . logFM ErrorS . ls . renderInability . rfDetail)
                    evidenceReads (vdDetails doc) >>= evaluateWith
  where
    rt = arRuntime ctx
    deps = arDeps ctx

gateVerdict :: PublicArtifactGate -> ServeDecision
gateVerdict = \case
    Admitted{} -> Admit
    Refused decision -> decision

gateVersion :: EvalContext -> PackumentDeps -> Maybe InstallEvidenceRead -> Filename -> PackageDetails -> IO PublicArtifactGate
gateVersion ctx deps installRead file details = do
    (admission, decision) <- admitArtifactWithInstallRead (installEvidenceMode (pdInstallEvidence deps)) installRead ctx (pdRules deps) (pdMinIntegrity deps) file details
    pure (publicArtifactGate details admission (skippedChecks decision))

-- | Render the shared admission verdict on the serve surface.
publicArtifactGate :: PackageDetails -> ArtifactAdmission -> [SkippedCheck] -> PublicArtifactGate
publicArtifactGate details admission skipped = case admission of
    AdmissionAdmit _ artifact _ -> Admitted artifact skipped
    AdmissionDenied decision -> Refused (serveDecisionOf (completeEvidence details) decision)
    AdmissionUndecidable decision -> Refused (rejectUnavailable transience (renderDecision (completeEvidence details) decision))
    AdmissionFileAbsent -> Refused versionAbsent
    AdmissionBelowFloor -> Refused integrityBelowFloor
    AdmissionIntegrityMissing -> Refused integrityMissing
  where
    -- The @503@-versus-@500@ transience is the shared projection's, the one the worker's
    -- retry-versus-drop reads. A settled verdict cannot be waited out.
    transience = fromMaybe WontResolve (admissionTransience admission)

streamPublicArtifact ::
    ArtifactRequest response ->
    Artifact ->
    -- | Observe the relay verdict (the anomaly log line and metric).
    (RelayVerdict -> IO ()) ->
    IO ResponseReceived
streamPublicArtifact ctx artifact observeVerdict
    | not hostHonoured = respond (crossHostRefused replies)
    | otherwise = case publicRequest of
        Left _ -> respond (internalArtifactError replies)
        Right req ->
            relayPublicResult (srPublicControl (arRuntime ctx)) PublicRead.Artifact (arMode ctx) (srPublicClient (arRuntime ctx)) (progressFloor (pdLimits (arDeps ctx))) req (const True) relayJudged (relayResponder replies respond) >>= \case
                UpstreamRelayed (verdict, received) -> do
                    observeVerdict verdict
                    mirrorOnAdmit ctx artifact verdict
                    pure received
                UpstreamCooling retry -> respond (artifactError replies deps (upstreamCooling retry))
                _ -> respond (artifactError replies deps upstreamUnavailable)
  where
    deps = arDeps ctx
    replies = arReplies ctx
    respond = arRespond ctx

    hostHonoured = tarballHostHonoured UntrustedOrigin deps (thgPublicHostPort (pdTarballHostGate deps)) (hostPortAddress (artUrl artifact))

    shape request = withValidators (arValidators ctx) (withMethod (arMode ctx) request)
    publicRequest = shape <$> artifactByUrl (pdArtifact deps) Nothing (artUrl artifact)

{- Back-fill the mirror only for a relayed artifact on the @GET@ path. A @HEAD@ served no bytes,
and an odd-shaped or non-success relay is not the artifact. -}
mirrorOnAdmit :: ArtifactRequest response -> Artifact -> RelayVerdict -> IO ()
mirrorOnAdmit ctx artifact verdict = case (verdict, pdMirror (arDeps ctx)) of
    (RelayedArtifact, MirrorOnAdmit _) -> case arMode ctx of
        ServeFull -> enqueueMirror ctx artifact
        ServeHead -> pass
    (RelayedArtifact, NoMirrorWrite) -> pass
    (RelayedOddShape _, _) -> pass
    (RelayedNonSuccess _, _) -> pass

enqueueMirror :: ArtifactRequest response -> Artifact -> IO ()
enqueueMirror ctx artifact =
    void . spanMirrorEnqueue (srTracing (arRuntime ctx)) (arPackage ctx) (arVersion ctx) (artUrl artifact) enqueueErrorDetail $
        enqueueJob ctx

-- Count the hand-off outcome and hand it back, never propagating it. The composition root's
-- buffer callbacks count drops and backend delivery failures behind the hand-off.
enqueueJob :: ArtifactRequest response -> Maybe RemoteSpanContext -> IO (Either TransportFault ())
enqueueJob ctx traceContext = do
    let metrics = srMetrics (arRuntime ctx)
    enqueued <-
        enqueue
            (srQueue (arRuntime ctx))
            MirrorJob
                { jobPackage = arPackage ctx
                , jobVersion = arVersion ctx
                , jobTriggerFilename = Just (arFile ctx)
                , -- The enqueueing span's trace context, captured by the span bracket, so
                  -- the worker's per-job span links back across the hop.
                  jobTraceContext = traceContext
                }
    either (const (mpMirrorEnqueueFailure metrics)) (const (mpMirrorEnqueued metrics)) enqueued
    -- The span bracket marks a swallowed failure errored on the producer span.
    pure enqueued

enqueueErrorDetail :: Either TransportFault () -> Maybe Text
enqueueErrorDetail = either (Just . enqueueFailureDetail) (const Nothing)

enqueueFailureDetail :: TransportFault -> Text
enqueueFailureDetail fault = "mirror enqueue failed: " <> tfDetail fault
