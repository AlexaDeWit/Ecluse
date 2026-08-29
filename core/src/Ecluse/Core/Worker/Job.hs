-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Deciding one mirror job: probe the mirror target, re-run current policy, fetch, verify, and
publish. Every step reports its verdict as a 'JobOutcome' value, which
"Ecluse.Core.Worker.Realise" realises at the queue handle.

A received message is hidden only for the queue's visibility window, so before a publish that
may run long the worker holds it ('Ecluse.Core.Queue.extendVisibility'). Nothing here acks: a
transient failure simply reports 'Retried', and the un-acked message redelivers.
-}
module Ecluse.Core.Worker.Job (
    JobOutcome (..),
    outcomeOfAdmission,
    outcomeOfFetchFault,
    processJob,
    workerPublishVisibilityBudget,
) where

import Data.Map.Strict qualified as Map
import Katip (Severity (DebugS, ErrorS, InfoS), katipAddNamespace, logFM, ls)
import UnliftIO (withRunInIO)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Fault (tfCause)
import Ecluse.Core.Package (Artifact (artSize), Hash, pkgEcosystem)
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
    admitArtifact,
 )
import Ecluse.Core.Queue (MirrorJob (jobArtifactFilename, jobArtifactUrl, jobPackage, jobTraceContext, jobVersion), MirrorQueue (extendVisibility), ReceiptHandle, Seconds (Seconds))
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    PublishFault (PublishFetch, PublishRejected),
    renderUrlFormationError,
 )
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl))
import Ecluse.Core.Registry.Metadata (VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent), versionTransience)
import Ecluse.Core.Registry.Publish (MirrorPublish (mpParseVersionList, mpProbeMetadata, mpPublishArtifact))
import Ecluse.Core.Rules.Types (Decision (Blocked, Undecidable), Transience (WillResolve, WontResolve), mkEvalContext)
import Ecluse.Core.Security (authorityLabel, hostPortAddress)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Path (Filename)
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (JobSpanOutcome (JobSpanOutcome), WorkerTracingPort (..))
import Ecluse.Core.Worker.Fetch (fetchArtifactBytes)
import Ecluse.Core.Worker.Integrity (IntegrityResult (..), verifyIntegrity)
import Ecluse.Core.Worker.Types

{- | The terminal outcome of processing one mirror job. It decides whether the worker
acks the message or leaves it to redeliver.
-}
data JobOutcome
    = {- | The publish succeeded, so the job is acked. This covers an idempotent
      redelivery too: a version already present at the mirror target answers a
      status the ecosystem's codec classifies as success (npm's @409@), so it
      surfaces here as 'Succeeded' rather than a distinct case. So does the same
      presence confirmed by the pre-fetch probe, before any bytes moved.
      -}
      Succeeded
    | {- | A __non-retryable__ rejection (a tampered artifact, an unformable request URL).
      Redelivery cannot help, so the job is acked to retire it after alarming.
      -}
      Dropped Text
    | {- | A __terminal__ fault handed to 'Ecluse.Core.Queue.deadLetter' rather than acked,
      because a plain delete would silently discard it on a durable queue.
      -}
      DeadLettered Text
    | {- | A __transient__ fault: a fetch failure, or a registry rejection worth
      retrying. The message is left un-acked so it redelivers. Carries the reason.
      -}
      Retried Text
    deriving stock (Eq, Show)

{- | Process one mirror job end to end and return the 'JobOutcome' that decides whether the worker
acks the message or lets it redeliver. The worker re-runs current policy before publishing, because
the enqueue-to-process window is unbounded and the mirror is later served without the rules.
-}
processJob :: ReceiptHandle -> MirrorJob -> WorkerM JobOutcome
processJob receipt job = katipAddNamespace "job" $ do
    logFM DebugS (ls ("starting mirror job for " <> renderJob job))
    tracing <- asks wrTracing
    runtime <- ask
    withRunInIO $ \runInIO ->
        wtpMirrorJobSpan tracing (jobPackage job) (jobVersion job) (jobTraceContext job) jobSpanOutcome $
            runInIO $
                wrInjectTraceContext runtime (reevaluateThenMirror receipt job)
  where
    -- The failure detail marks the span errored, so only a job that did not publish carries one.
    jobSpanOutcome :: JobOutcome -> JobSpanOutcome
    jobSpanOutcome = \case
        Succeeded -> JobSpanOutcome "succeeded" Nothing
        Dropped reason -> JobSpanOutcome "dropped" (Just reason)
        DeadLettered reason -> JobSpanOutcome "dead-lettered" (Just reason)
        Retried reason -> JobSpanOutcome "retried" (Just reason)

-- Order the steps cheapest first: a duplicate retires for one metadata round trip and a now-denied
-- job drops before its bytes are downloaded. Every step past the lookup rides the ecosystem's own
-- bundle, so no job can consult a foreign ecosystem's probe, rules, or publish.
reevaluateThenMirror :: ReceiptHandle -> MirrorJob -> WorkerM JobOutcome
reevaluateThenMirror receipt job = do
    policies <- asks wrPolicies
    case Map.lookup (pkgEcosystem (jobPackage job)) policies of
        Nothing ->
            -- Structurally unreachable: only an activated ecosystem enqueues jobs, and
            -- activation implies a bundle. Kept as the fail-closed drop.
            pure (Dropped ("no rule policy is configured for the " <> ecosystemName (pkgEcosystem (jobPackage job)) <> " ecosystem; refusing to mirror " <> renderJob job))
        Just policy ->
            alreadyMirrored policy job >>= \case
                True -> do
                    logFM InfoS (ls ("already present at the mirror target, acking without re-publish: " <> renderJob job))
                    pure Succeeded
                False ->
                    reevaluatePolicy policy job >>= \case
                        Right admitted -> mirrorArtifact policy receipt job admitted
                        Left outcome -> pure outcome

{- Confirm presence positively only: a fetch fault or an unparseable body answers 'False', so the
job falls through to the full gated pipeline. The probe never admits an unvetted job. -}
alreadyMirrored :: WorkerPolicy -> MirrorJob -> WorkerM Bool
alreadyMirrored policy job = do
    probed <- liftIO (mpProbeMetadata (wpPublish policy) (jobPackage job))
    case probed of
        Left fault -> do
            -- DebugS keeps a persistently-failing probe diagnosable rather than silent.
            logFM DebugS (ls ("mirror presence probe did not confirm " <> renderJob job <> "; falling through to full re-evaluation: " <> show fault))
            pure False
        Right response -> case mpParseVersionList (wpPublish policy) response of
            Left _ -> pure False
            Right versions -> pure (jobVersion job `elem` versions)

{- Re-check the fetch URL against the mount's tarball-host gate, because the queue payload is a
trust boundary. Then re-run current policy through 'Ecluse.Core.Package.Admission.admitArtifact'. -}
reevaluatePolicy :: WorkerPolicy -> MirrorJob -> WorkerM (Either JobOutcome MirrorArtifact)
reevaluatePolicy policy job
    | not (wpArtifactHostHonoured policy (hostPortAddress (registryUrlText (jobArtifactUrl job)))) =
        pure (Left (Dropped ("the tarball-host policy refuses the artifact host of " <> renderJob job <> " (" <> jobArtifactAuthority job <> "); refusing to fetch or mirror it")))
    | otherwise = do
        evaluation <- liftIO (wpResolveVersion policy (jobPackage job) (jobVersion job))
        case evaluation of
            VersionMetadataUnavailable ->
                pure (Left (retryOrDrop (versionTransience evaluation) ("could not re-fetch metadata to re-evaluate current policy for " <> renderJob job)))
            VersionMissing ->
                pure (Left (retryOrDrop (versionTransience evaluation) ("the public upstream no longer offers " <> renderJob job <> "; refusing to mirror a withdrawn version")))
            VersionPresent details -> do
                -- The back-fill path emits no per-decision audit line, so the
                -- audit-only advisory ETag is not resolved for its context.
                ctx <- liftIO (mkEvalContext (wpNow policy) (pure Nothing))
                admission <-
                    liftIO
                        ( admitArtifact
                            ctx
                            (wpRules policy)
                            (wpMinIntegrity policy)
                            (jobArtifactFilename job)
                            details
                        )
                pure (outcomeOfAdmission job admission)

{- | Render the shared 'ArtifactAdmission' as the descriptor to publish, or the outcome the queue
realises. 'admissionTransience' alone splits retry from drop, so no path can diverge from the gate.
-}
outcomeOfAdmission :: MirrorJob -> ArtifactAdmission -> Either JobOutcome MirrorArtifact
outcomeOfAdmission job admission = case admission of
    AdmissionAdmit filename artifact digests -> Right (readmittedDescriptor filename artifact digests)
    AdmissionDenied (Blocked ruleName reason) ->
        refused ("current policy denies " <> renderJob job <> ": blocked by " <> ruleName <> " (" <> reason <> ")")
    AdmissionDenied _ ->
        refused ("current policy denies " <> renderJob job <> ": no rule admits it")
    AdmissionUndecidable (Undecidable _ reason) ->
        refused ("current policy could not be evaluated for " <> renderJob job <> ": " <> reason)
    AdmissionUndecidable _ ->
        refused ("current policy could not be evaluated for " <> renderJob job)
    AdmissionFileAbsent ->
        refused ("the public upstream no longer offers the admitted artifact file of " <> renderJob job <> "; refusing to mirror a withdrawn artifact")
    AdmissionBelowFloor ->
        refused ("current admission policy refuses " <> renderJob job <> ": its strongest integrity digest is below the configured public floor")
    AdmissionIntegrityMissing ->
        refused ("current admission policy refuses " <> renderJob job <> ": it no longer carries any integrity digest")
  where
    refused :: Text -> Either JobOutcome MirrorArtifact
    refused = Left . retryOrDrop (admissionTransience admission)

{- The worker's one retry-versus-drop rule, over the shared transience. Only an inability the
evaluator expects to clear redelivers: the rest drop through the terminal path. -}
retryOrDrop :: Maybe Transience -> Text -> JobOutcome
retryOrDrop transience reason = case transience of
    Just (WillResolve _) -> Retried reason
    Just WontResolve -> Dropped reason
    Nothing -> Dropped reason

{- Derive the publish descriptor from what the gate settled, so nothing the queue payload asserted
reaches the trusted-tier publish document unchecked. The size is current metadata's. -}
readmittedDescriptor :: Filename -> Artifact -> NonEmpty Hash -> MirrorArtifact
readmittedDescriptor filename artifact digests =
    MirrorArtifact
        { maFilename = filename
        , maHashes = digests
        , maSize = artSize artifact
        }

{- | The worker's terminal-versus-transient split over the shared exchange-fault channel.
The artifact fetch and the mirror write read this one table, so no fault splits between them.
-}
outcomeOfFetchFault :: (FetchFault -> Text) -> FetchFault -> JobOutcome
outcomeOfFetchFault render fault = verdict (render fault)
  where
    verdict = case fault of
        FetchUrlUnformable _ -> Dropped
        FetchBoundExceeded _ -> DeadLettered
        FetchTransport _ -> Retried

-- A tampered artifact must never reach the private upstream, which later serves it without the
-- rules, so the bytes are verified against the re-admitted digests before any publish.
mirrorArtifact :: WorkerPolicy -> ReceiptHandle -> MirrorJob -> MirrorArtifact -> WorkerM JobOutcome
mirrorArtifact policy receipt job admitted = do
    logFM DebugS (ls ("fetching artifact bytes from " <> jobArtifactAuthority job))
    fetched <- fetchArtifactBytes (wpArtifactLimits policy) (artifactByUrl (wpArtifact policy)) (jobArtifactUrl job)
    case fetched of
        -- 'outcomeOfFetchFault' makes the terminal-versus-transient split, and
        -- 'processMessage' logs the reason at the queue-realisation site.
        Left fault -> pure (outcomeOfFetchFault (artifactFetchReason job) fault)
        Right bytes ->
            case verifyIntegrity (maHashes admitted) bytes of
                IntegrityMismatch detail -> do
                    logFM ErrorS (ls ("artifact integrity mismatch, refusing to publish: " <> detail))
                    pure (Dropped ("integrity mismatch: " <> detail))
                IntegrityVerified -> publishVerified policy receipt job admitted bytes

-- The client's rendered exception would print the request path, query, and headers, so a
-- transport reason names only the authority and the cause.
artifactFetchReason :: MirrorJob -> FetchFault -> Text
artifactFetchReason job = \case
    FetchUrlUnformable urlErr -> "unformable artifact URL: " <> renderUrlFormationError urlErr
    FetchBoundExceeded limitErr -> "artifact exceeded the response bound: " <> show limitErr
    FetchTransport fault -> "artifact fetch from " <> jobArtifactAuthority job <> " failed: " <> show (tfCause fault)

-- The mirror target is operator-configured, so its rendered transport detail is diagnosable
-- rather than attacker-supplied.
publishFaultReason :: FetchFault -> Text
publishFaultReason = \case
    FetchUrlUnformable urlErr -> "unformable publish URL: " <> renderUrlFormationError urlErr
    FetchBoundExceeded limitErr -> "the publication target's response exceeded the response bound: " <> show limitErr
    FetchTransport fault -> "publish transport failure: " <> show fault

-- Publish already-verified bytes to the mirror target. The publish document is assembled from the
-- re-admitted descriptor, so no queue-payload text reaches the trusted-tier packument.
publishVerified :: WorkerPolicy -> ReceiptHandle -> MirrorJob -> MirrorArtifact -> ByteString -> WorkerM JobOutcome
publishVerified policy receipt job admitted bytes = do
    holdForLongPublish receipt
    metrics <- asks wrMetrics
    -- The publish is the long, network-bound step. Time it for the publish-latency
    -- histogram whichever way the registry responds.
    (result, seconds) <- timedSeconds (liftIO (mpPublishArtifact (wpPublish policy) (jobPackage job) (jobVersion job) admitted bytes))
    liftIO (wmpMirrorPublishDuration metrics seconds)
    case result of
        Right () -> do
            logFM InfoS (ls ("mirrored artifact published: " <> renderJob job))
            pure Succeeded
        Left (PublishRejected err) -> do
            releaseForRetry receipt
            pure (Retried ("registry rejected publish: " <> show err))
        Left (PublishFetch fault) -> case outcomeOfFetchFault publishFaultReason fault of
            -- Reset the hold only when a redelivery is actually coming. A terminal outcome is
            -- acked or dead-lettered, so there is nothing to hasten and the hold can stand.
            Retried reason -> do
                releaseForRetry receipt
                pure (Retried reason)
            outcome -> pure outcome

-- Hold the message past its visibility window before a publish that may run long. A mid-publish
-- redelivery only wastes a re-fetch, so a failed extend is swallowed, never failing the job.
holdForLongPublish :: ReceiptHandle -> WorkerM ()
holdForLongPublish receipt =
    queueOp (\queue -> extendVisibility queue receipt workerPublishVisibilityBudget) (const pass)

{- | The visibility window one publish gets before its message could redeliver mid-write, sized to
upload the largest artifact the memory plan admits (512 MiB) over a 2 MiB-per-second link.
@Ecluse.Worker.LivenessSpec@ pins it under the liveness staleness bound so the two cannot drift.
-}
workerPublishVisibilityBudget :: Seconds
workerPublishVisibilityBudget = Seconds 300

-- Reset the message to visible, so a failed publish redelivers at once instead of waiting out
-- 'holdForLongPublish'. Best effort: a missed reset only delays the redelivery.
releaseForRetry :: ReceiptHandle -> WorkerM ()
releaseForRetry receipt =
    queueOp (\queue -> extendVisibility queue receipt (Seconds 0)) (const pass)

{- The job's artifact location as a log-safe authority. The queue payload's URL can carry userinfo
or a pre-signed query, so a log line names only the host and port the worker dials. -}
jobArtifactAuthority :: MirrorJob -> Text
jobArtifactAuthority = authorityLabel . registryUrlText . jobArtifactUrl
