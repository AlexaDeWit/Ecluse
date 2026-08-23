-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Ack within the visibility budget during job processing. A received message is hidden only
for the queue's visibility window, so before a publish that may run long the worker calls
'Ecluse.Core.Queue.extendVisibility' to hold it. On a transient failure it does __not__ ack, so
the message redelivers. A batch is processed __sequentially__, so each job has the full
visibility budget rather than competing with its batch-mates. A delivery that already spent the
queue's redelivery budget is retired before the job runs, so a message nothing else captures
stops cycling instead of re-fetching its artifact on every redelivery.
-}
module Ecluse.Core.Worker.Job (
    JobOutcome (..),
    outcomeOfFetchFault,
    processJob,
    processBatch,
    workerPublishVisibilityBudget,
) where

import Data.Map.Strict qualified as Map
import Katip (Severity (DebugS, ErrorS, InfoS, WarningS), katipAddNamespace, logFM, ls)
import UnliftIO (withRunInIO)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Fault (tfCause, tfDetail)
import Ecluse.Core.Package (Artifact (artFilename, artSize), Hash, pkgEcosystem, renderPackageName)
import Ecluse.Core.Package.Admission (
    ArtifactAdmission (
        AdmissionAdmit,
        AdmissionBelowFloor,
        AdmissionDenied,
        AdmissionFileAbsent,
        AdmissionIntegrityMissing,
        AdmissionUndecidable
    ),
    admitArtifact,
 )
import Ecluse.Core.Queue (DeliveryBudget, MirrorJob (jobArtifactFilename, jobArtifactUrl, jobPackage, jobTraceContext, jobVersion), MirrorQueue (ack, deadLetter, deliveryBudget, extendVisibility), QueueMessage (msgJob, msgReceipt, msgReceiveCount), ReceiptHandle, Seconds (Seconds), deliveryBudgetSpent, retiringDelivery)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    PublishFault (PublishFetch, PublishRejected),
    renderUrlFormationError,
 )
import Ecluse.Core.Registry.Metadata (VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent))
import Ecluse.Core.Registry.Publish (MirrorPublish (mpParseVersionList, mpProbeMetadata, mpPublishArtifact))
import Ecluse.Core.Rules.Types (Decision (Blocked, Undecidable), mkEvalContext)
import Ecluse.Core.Security (authorityLabel, hostPortAddress)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (JobSpanOutcome (JobSpanOutcome), WorkerTracingPort (..))
import Ecluse.Core.Version (renderVersion)
import Ecluse.Core.Worker.Fetch (fetchArtifactBytes)
import Ecluse.Core.Worker.Integrity (IntegrityResult (..), verifyIntegrity)
import Ecluse.Core.Worker.Types

{- | Process one batch sequentially, so each job gets the full visibility budget. The heartbeat
advances per job, so 'Ecluse.Core.Worker.Liveness.workerHeartbeatStaleAfter' covers one job.
-}
processBatch :: [QueueMessage] -> WorkerM ()
processBatch = traverse_ $ \message -> do
    processMessage message
    recordWorkerProgress

{- Check the queue's delivery budget before running the job, so a poison message retires without
re-fetching its artifact, even on a queue with no dead-letter terminus. -}
processMessage :: QueueMessage -> WorkerM ()
processMessage message = do
    budget <- asks (deliveryBudget . wrQueue)
    if deliveryBudgetSpent budget message
        then do
            metrics <- asks wrMetrics
            liftIO (wmpMirrorJobProcessed metrics Metric.Discarded)
            retireTerminally (budgetSpentReason budget message) (msgReceipt message)
        else processDelivery message

-- Run the job and realise its outcome for a delivery still within the queue's budget.
processDelivery :: QueueMessage -> WorkerM ()
processDelivery message = do
    metrics <- asks wrMetrics
    outcome <- processJob (msgReceipt message) (msgJob message)
    liftIO (wmpMirrorJobProcessed metrics (jobResultMetric outcome))
    case outcome of
        Succeeded -> ackMessage (msgReceipt message)
        Dropped reason ->
            -- Non-retryable, and not worth a dead-letter forensic trail, so retire it instead.
            retireTerminally ("dropping unrecoverable mirror job: " <> reason) (msgReceipt message)
        DeadLettered reason -> do
            -- Alarm first: on the in-memory backend the log and metric are the only record.
            logFM ErrorS (ls ("dead-lettering unmirrorable mirror job (rides the backend's dead-letter terminus): " <> reason))
            deadLetterMessage (msgReceipt message)
        Retried reason ->
            logFM WarningS (ls ("leaving mirror job un-acked for retry (redelivered by a durable queue, re-mirrored on next demand by the in-memory one): " <> reason))

{- Retire a message the worker will never mirror: alarm, then ack so it stops cycling. On a
durable queue that ack is the delete that finally kills the message. -}
retireTerminally :: Text -> ReceiptHandle -> WorkerM ()
retireTerminally reason receipt = do
    logFM ErrorS (ls reason)
    ackMessage receipt

-- On a queue with no dead-letter terminus this line is the only record the message ever leaves.
budgetSpentReason :: DeliveryBudget -> QueueMessage -> Text
budgetSpentReason budget message =
    "discarding a mirror job after "
        <> show (msgReceiveCount message)
        <> " deliveries (this queue retires one on delivery "
        <> show (retiringDelivery budget)
        <> "): "
        <> renderJob (msgJob message)
        <> ". No dead-letter queue captured it, so it is retired here rather than left to"
        <> " cycle until the queue's retention window drops it unseen. Attach a redrive"
        <> " policy to retain it for inspection."

-- Classify a job outcome for the @ecluse.mirror.jobs.processed@ metric. 'Metric.Discarded' is
-- absent here on purpose: the worker counts a budget-spent delivery at its retirement.
jobResultMetric :: JobOutcome -> Metric.MirrorResult
jobResultMetric = \case
    Succeeded -> Metric.Published
    Dropped _ -> Metric.Failed
    DeadLettered _ -> Metric.Failed
    Retried _ -> Metric.Failed

ackMessage :: ReceiptHandle -> WorkerM ()
ackMessage receipt = do
    queue <- asks wrQueue
    acked <- liftIO (ack queue receipt)
    whenLeft_ acked $ \fault ->
        logFM WarningS (ls ("ack failed; the processed message will redeliver (harmless, publishing is idempotent): " <> tfDetail fault))

-- Hand the message to the queue's dead-letter terminus, never a plain delete, which would
-- silently discard it on a durable queue.
deadLetterMessage :: ReceiptHandle -> WorkerM ()
deadLetterMessage receipt = do
    queue <- asks wrQueue
    outcome <- liftIO (deadLetter queue receipt)
    whenLeft_ outcome $ \fault ->
        logFM WarningS (ls ("dead-letter realisation failed; the message redelivers and re-fails terminally (harmless): " <> tfDetail fault))

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

-- The policy re-evaluation's verdict, decided before any artifact fetch. 'ReevalAdmit' carries the
-- floor-checked digest set the tamper gate verifies the fetched bytes against.
data ReevalOutcome
    = ReevalAdmit MirrorArtifact
    | ReevalDrop Text
    | ReevalRetry Text

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
                        ReevalAdmit admitted -> mirrorArtifact policy receipt job admitted
                        ReevalDrop reason -> pure (Dropped reason)
                        ReevalRetry reason -> pure (Retried reason)

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
reevaluatePolicy :: WorkerPolicy -> MirrorJob -> WorkerM ReevalOutcome
reevaluatePolicy policy job
    | not (wpArtifactHostHonoured policy (hostPortAddress (registryUrlText (jobArtifactUrl job)))) =
        pure (ReevalDrop ("the tarball-host policy refuses the artifact host of " <> renderJob job <> " (" <> jobArtifactAuthority job <> "); refusing to fetch or mirror it"))
    | otherwise = do
        evaluation <- liftIO (wpResolveVersion policy (jobPackage job) (jobVersion job))
        case evaluation of
            VersionMetadataUnavailable ->
                pure (ReevalRetry ("could not re-fetch metadata to re-evaluate current policy for " <> renderJob job))
            VersionMissing ->
                pure (ReevalDrop ("the public upstream no longer offers " <> renderJob job <> "; refusing to mirror a withdrawn version"))
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

-- Project the shared 'ArtifactAdmission' onto the worker's outcome. A deliberate refusal drops,
-- never frozen into the rule-exempt mirror store. An undecidable verdict retries, so an advisory
-- source outage neither drops a serviceable job nor publishes it unvetted.
outcomeOfAdmission :: MirrorJob -> ArtifactAdmission -> ReevalOutcome
outcomeOfAdmission job = \case
    AdmissionAdmit artifact digests -> ReevalAdmit (readmittedDescriptor artifact digests)
    AdmissionDenied (Blocked ruleName reason) ->
        ReevalDrop ("current policy denies " <> renderJob job <> ": blocked by " <> ruleName <> " (" <> reason <> ")")
    AdmissionDenied _ ->
        ReevalDrop ("current policy denies " <> renderJob job <> ": no rule admits it")
    AdmissionUndecidable (Undecidable _ reason) ->
        ReevalRetry ("current policy could not be evaluated for " <> renderJob job <> ": " <> reason)
    AdmissionUndecidable _ ->
        ReevalRetry ("current policy could not be evaluated for " <> renderJob job)
    AdmissionFileAbsent ->
        ReevalDrop ("the public upstream no longer offers the admitted artifact file of " <> renderJob job <> "; refusing to mirror a withdrawn artifact")
    AdmissionBelowFloor ->
        ReevalDrop ("current admission policy refuses " <> renderJob job <> ": its strongest integrity digest is below the configured public floor")
    AdmissionIntegrityMissing ->
        ReevalDrop ("current admission policy refuses " <> renderJob job <> ": it no longer carries any integrity digest")

-- Derive the publish descriptor from current metadata alone: the floor-checked digests the
-- tamper gate verifies against, plus the filename and declared size. The payload's filename only
-- selects the artifact, so no queue-payload text reaches the trusted-tier publish document.
readmittedDescriptor :: Artifact -> NonEmpty Hash -> MirrorArtifact
readmittedDescriptor artifact digests =
    MirrorArtifact
        { maFilename = artFilename artifact
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
    fetched <- fetchArtifactBytes (wpArtifactLimits policy) (wpBuildArtifactRequest policy) (jobArtifactUrl job)
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
holdForLongPublish receipt = do
    queue <- asks wrQueue
    -- The fault channel is a value, and a failed extend is the swallowed 'Left'.
    _ <- liftIO (extendVisibility queue receipt workerPublishVisibilityBudget)
    pass

{- | The visibility window one publish gets before its message could redeliver mid-write, sized to
upload the largest artifact the memory plan admits (512 MiB) over a 2 MiB-per-second link.
@Ecluse.Worker.LivenessSpec@ pins it under the liveness staleness bound so the two cannot drift.
-}
workerPublishVisibilityBudget :: Seconds
workerPublishVisibilityBudget = Seconds 300

-- Reset the message to visible, so a failed publish redelivers at once instead of waiting out
-- 'holdForLongPublish'. Best effort: a missed reset only delays the redelivery.
releaseForRetry :: ReceiptHandle -> WorkerM ()
releaseForRetry receipt = do
    queue <- asks wrQueue
    -- The fault channel is a value, and a failed reset is the swallowed 'Left'.
    _ <- liftIO (extendVisibility queue receipt (Seconds 0))
    pass

-- A one-line identifier for a job, for log lines.
renderJob :: MirrorJob -> Text
renderJob job = renderPackageName (jobPackage job) <> "@" <> renderVersion (jobVersion job)

{- The job's artifact location as a log-safe authority. The queue payload's URL can carry userinfo
or a pre-signed query, so a log line names only the host and port the worker dials. -}
jobArtifactAuthority :: MirrorJob -> Text
jobArtifactAuthority = authorityLabel . registryUrlText . jobArtifactUrl
