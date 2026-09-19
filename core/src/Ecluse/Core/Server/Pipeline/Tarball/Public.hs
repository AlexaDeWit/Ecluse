-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The gated leg of the artifact path: vet the requested version, then relay the public upstream.

The gate runs the same admission oracle the worker's ingest re-evaluation runs, so a version the
worker would refuse is refused here too. An admitted @GET@ enqueues the demand-driven mirror.
-}
module Ecluse.Core.Server.Pipeline.Tarball.Public (
    servePublicArtifact,

    -- * The public artifact gate (exposed for direct testing)
    PublicArtifactGate (..),
    publicArtifactGate,
) where

import Network.Wai (ResponseReceived)

import Ecluse.Core.Cve (DbEtag)
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
    admitArtifactWithEvidence,
 )
import Ecluse.Core.Queue (
    MirrorJob (MirrorJob, jobArtifactFilename, jobArtifactUrl, jobPackage, jobTraceContext, jobVersion),
    RemoteSpanContext,
    enqueue,
 )
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl))
import Ecluse.Core.Registry.Metadata (
    VersionDoc (vdDetails),
    VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent),
    fetchVersionDetails,
 )
import Ecluse.Core.Rules (renderDecision)
import Ecluse.Core.Rules.Types (EvalContext, SkippedCheck, completeEvidence, mkEvalContext)
import Ecluse.Core.Security (Origin (UntrustedOrigin), hostPortAddress, thgPublicHostPort)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Server.Context (
    Handler,
    PackumentDeps (..),
    ServeRuntime (..),
    pdMirror,
    pdPublicBaseUrl,
    pdTarballHostGate,
    tarballHostHonoured,
 )
import Ecluse.Core.Server.Path (Filename)
import Ecluse.Core.Server.Pipeline.Internal (
    VersionVerdict (..),
    evalTier,
    logDenials,
    logSkippedChecksOnce,
    recordDenials,
    serveDecisionClass,
 )
import Ecluse.Core.Server.Pipeline.Origin (withPublicMetadataClient)
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
    relayUpstreamWhen,
    withMethod,
    withValidators,
 )
import Ecluse.Core.Server.Pipeline.Tarball.Types (ArtifactRequest (..), TarballReplies (..))
import Ecluse.Core.Server.Response (
    ServeDecision (Admit),
    Transience (WontResolve),
    mkRefusal,
    rejectUnavailable,
    serveDecisionOf,
 )
import Ecluse.Core.Server.Stream (RelayResponder (RelayResponder))
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit, NoMirrorWrite))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (spanMirrorEnqueue, spanRuleEval)
import Ecluse.Core.Version (renderVersion)
import UnliftIO (withRunInIO)

-- | Gate the requested version under the mount's admission budget, then relay it.
servePublicArtifact :: ArtifactRequest response -> Handler ResponseReceived
servePublicArtifact ctx = do
    let metrics = srMetrics (arRuntime ctx)
    -- The advisory database active for this request, resolved once and used both for the
    -- version's evaluation and for a denial's audit line.
    advisoryEtag <- liftIO (pdAdvisoryEtag (arDeps ctx))
    withAdmissionOrShed
        metrics
        (srAdmission (arRuntime ctx))
        (liftIO (arRespond ctx (tarballError (arReplies ctx) shedStatus [shedRetryAfter] (mkRefusal Nothing shedMessage))))
        (gatePublicVersion ctx advisoryEtag)
        $ \case
            Admitted artifact skipped -> serveAdmitted ctx advisoryEtag artifact skipped
            Refused decision -> refusePublic ctx advisoryEtag decision

-- Stream an admitted artifact, recording the admission and the checks the gate had to skip.
serveAdmitted :: ArtifactRequest response -> Maybe DbEtag -> Artifact -> [SkippedCheck] -> Handler ResponseReceived
serveAdmitted ctx advisoryEtag artifact skipped = do
    let metrics = srMetrics (arRuntime ctx)
    liftIO (mpServeDecision metrics Metric.Admit)
    logSkippedChecksOnce (pdNoteAdmission (arDeps ctx)) (arPackage ctx) (renderVersion (arVersion ctx)) advisoryEtag skipped
    withRunInIO $ \runInIO ->
        streamPublicArtifact ctx artifact (runInIO . observeRelayAnomaly metrics (arPackage ctx) (arVersion ctx))

-- Answer a gate refusal, recording the denial on the metrics and the audit log.
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

{- Gate the requested version and select its artifact. The single-version read resolves the full
packument through the shared metadata cache, so a packument @GET@ and this gate are one call. -}
gatePublicVersion :: ArtifactRequest response -> Maybe DbEtag -> Handler PublicArtifactGate
gatePublicVersion ctx advisoryEtag = do
    evalCtx <- liftIO (mkEvalContext (pdNow deps) (pure advisoryEtag))
    eval <-
        withPublicMetadataClient rt deps (pdPublicBaseUrl deps) $ \client ->
            liftIO (fetchVersionDetails client (arPackage ctx) (arVersion ctx))
    case eval of
        VersionMetadataUnavailable -> pure (Refused upstreamUnavailable)
        VersionMissing -> pure (Refused versionAbsent)
        VersionPresent doc _ ->
            liftIO $
                spanRuleEval (srTracing rt) (arPackage ctx) (arVersion ctx) $ do
                    (gate, seconds) <- timedSeconds (gateVersion evalCtx deps (arFile ctx) (vdDetails doc))
                    mpRuleEvalDuration (srMetrics rt) (evalTier (pdRules deps)) seconds
                    pure (gate, gateVerdict gate)
  where
    rt = arRuntime ctx
    deps = arDeps ctx

-- The serve verdict a gate outcome carries, for the rule-eval span.
gateVerdict :: PublicArtifactGate -> ServeDecision
gateVerdict = \case
    Admitted{} -> Admit
    Refused decision -> decision

{- Gate one requested artifact through the shared admission oracle the worker's ingest
re-evaluation also runs. The trusted private leg never reaches this gate. -}
gateVersion :: EvalContext -> PackumentDeps -> Filename -> PackageDetails -> IO PublicArtifactGate
gateVersion ctx deps file details =
    uncurry (publicArtifactGate details) <$> admitArtifactWithEvidence ctx (pdRules deps) (pdMinIntegrity deps) file details

-- | Render the shared admission verdict on the serve surface.
publicArtifactGate :: PackageDetails -> ArtifactAdmission -> [SkippedCheck] -> PublicArtifactGate
publicArtifactGate details admission skipped = case admission of
    -- The carried floor-checked digest set is the worker's ingest concern. The serve path
    -- streams without rehashing, so it has no consumer for the set.
    AdmissionAdmit _ artifact _ -> Admitted artifact skipped
    AdmissionDenied decision -> Refused (serveDecisionOf details decision)
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
            relayUpstreamWhen (arMode ctx) (srPublicManager (arRuntime ctx)) req (const True) relayJudged (relayResponder replies respond) >>= \case
                Just (verdict, received) -> do
                    observeVerdict verdict
                    mirrorOnAdmit ctx artifact verdict
                    pure received
                Nothing -> respond (artifactError replies deps upstreamUnavailable)
  where
    deps = arDeps ctx
    replies = arReplies ctx
    respond = arRespond ctx

    hostHonoured = tarballHostHonoured UntrustedOrigin deps (thgPublicHostPort (pdTarballHostGate deps)) (hostPortAddress (artUrl artifact))

    publicRequest = withValidators (arValidators ctx) . withMethod (arMode ctx) <$> artifactByUrl (pdArtifact deps) Nothing (artUrl artifact)

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

-- Adapt the route's typed response constructors to the streaming helper's callback. The
-- upstream connection stays open until the selected response completes.
relayResponder :: TarballReplies response -> (response -> IO received) -> RelayResponder received
relayResponder replies respond =
    RelayResponder
        (\status headers body -> respond (tarballStream replies status headers body))
        (\status headers -> respond (tarballEmpty replies status headers))

enqueueMirror :: ArtifactRequest response -> Artifact -> IO ()
enqueueMirror ctx artifact =
    case pdEgressUrl (arDeps ctx) (artUrl artifact) of
        Left _ -> mpMirrorEnqueueFailure (srMetrics (arRuntime ctx))
        Right egressUrl ->
            void . spanMirrorEnqueue (srTracing (arRuntime ctx)) (arPackage ctx) (arVersion ctx) (artUrl artifact) enqueueErrorDetail $
                enqueueJob ctx egressUrl

-- Count the hand-off outcome and hand it back, never propagating it. The composition root's
-- buffer callbacks count drops and backend delivery failures behind the hand-off.
enqueueJob :: ArtifactRequest response -> RegistryUrl -> Maybe RemoteSpanContext -> IO (Either TransportFault ())
enqueueJob ctx egressUrl traceContext = do
    let metrics = srMetrics (arRuntime ctx)
    enqueued <-
        enqueue
            (srQueue (arRuntime ctx))
            MirrorJob
                { jobPackage = arPackage ctx
                , jobVersion = arVersion ctx
                , jobArtifactUrl = egressUrl
                , jobArtifactFilename = arFile ctx
                , -- The enqueueing span's trace context, captured by the span bracket, so
                  -- the worker's per-job span links back across the hop.
                  jobTraceContext = traceContext
                }
    either (const (mpMirrorEnqueueFailure metrics)) (const (mpMirrorEnqueued metrics)) enqueued
    -- The span bracket marks a swallowed failure errored on the producer span.
    pure enqueued

-- Project the swallowed enqueue outcome onto the producer span's status, so a trace explains
-- why the mirror was not enqueued.
enqueueErrorDetail :: Either TransportFault () -> Maybe Text
enqueueErrorDetail = either (Just . enqueueFailureDetail) (const Nothing)

enqueueFailureDetail :: TransportFault -> Text
enqueueFailureDetail fault = "mirror enqueue failed: " <> tfDetail fault
