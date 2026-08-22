-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Ack within the visibility budget during job processing.

A received message is hidden only for the queue's visibility window. The worker acks
on success. Before a publish that may run long it calls
'Ecluse.Core.Queue.extendVisibility' to hold the message before the window lapses. On a
transient failure it does __not__ ack, so the message redelivers. The worker processes a
batch __sequentially__, so each job has the full visibility budget rather than competing
with its batch-mates for it.

The worker retires a delivery that already spent the queue's redelivery budget (see
"Ecluse.Core.Queue"). The check runs before the job, so a message nothing else captures
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
import Ecluse.Core.Queue (DeliveryBudget, MirrorJob (jobArtifactFilename, jobArtifactUrl, jobPackage, jobTraceContext, jobVersion), MirrorQueue (ack, deadLetter, deliveryBudget, extendVisibility), QueueMessage (msgJob, msgReceipt, msgReceiveCount), ReceiptHandle, Seconds (Seconds), deliveryBudgetSpent, qfDetail, retiringDelivery)
import Ecluse.Core.Registry (MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize), PublishFault (PublishRejected, PublishTransport, PublishUrlUnformable))
import Ecluse.Core.Registry.Metadata (VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent))
import Ecluse.Core.Registry.Publish (MirrorPublish (mpParseVersionList, mpProbeMetadata, mpPublishArtifact))
import Ecluse.Core.Rules.Types (Decision (Blocked, Undecidable), mkEvalContext)
import Ecluse.Core.Security (authorityLabel, hostPortAddress)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (JobSpanOutcome (JobSpanOutcome), WorkerTracingPort (..))
import Ecluse.Core.Version (renderVersion)
import Ecluse.Core.Worker.Fetch (ArtifactFetchFault (ArtifactOverCap, ArtifactUnavailable), fetchArtifactBytes)
import Ecluse.Core.Worker.Integrity (IntegrityResult (..), verifyIntegrity)
import Ecluse.Core.Worker.Types

{- | Process one received batch __sequentially__, so each job gets the full visibility
budget rather than competing with its batch-mates for it. A batch is at most the
queue's configured batch size (≤ 10), so sequential processing is a deliberate
throughput-versus-budget choice, not a scaling bottleneck.

The liveness heartbeat advances after __each__ completed job, not once for the whole
batch. The @\/livez@ staleness bound
('Ecluse.Core.Worker.Liveness.workerHeartbeatStaleAfter') therefore need only cover one
job's worst case, not a whole sequential batch of large-artifact publishes. Nothing
mistakes a healthy worker mid-batch for a stalled one.
-}
processBatch :: [QueueMessage] -> WorkerM ()
processBatch = traverse_ $ \message -> do
    processMessage message
    recordWorkerProgress

{- Process one message: run the job, then realise its terminal outcome. Ack a success
or a clean non-retryable drop. Dead-letter a terminal fault, which the backend routes
to its own terminus. Leave a transient failure un-acked so the queue redelivers it
("retry is don't ack").

The worker retires a delivery that spent the queue's redelivery budget __before__ it
runs the job. A message the worker returns only ends somewhere if something captures
it. On a queue with no dead-letter terminus a poison message would otherwise cycle
until the retention window dropped it unseen. Every redelivery would re-fetch the
artifact. The budget makes the delivery terminal instead, and checking it first is what
spares the re-fetch. -}
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
            -- A clean non-retryable rejection (a tampered artifact, an unformable
            -- publish URL). The job can never succeed and is not worth a dead-letter
            -- forensic trail. Retire it, having already alarmed.
            retireTerminally ("dropping unrecoverable mirror job: " <> reason) (msgReceipt message)
        DeadLettered reason -> do
            -- A terminal fault the backend routes to its own dead-letter terminus.
            -- The in-memory backend drops it, its only terminus. A durable queue
            -- returns it to ride the operator's redrive policy to the dead-letter
            -- queue. The alarm goes first, since on the memory backend the log and
            -- metric are the only observability there is.
            logFM ErrorS (ls ("dead-lettering unmirrorable mirror job (rides the backend's dead-letter terminus): " <> reason))
            deadLetterMessage (msgReceipt message)
        Retried reason ->
            -- A transient fault: leave the message un-acked. The retry is
            -- backend-dependent. A durable queue redelivers it once the visibility
            -- window lapses, while the in-memory backend (no redelivery) re-mirrors
            -- it on the next demand. Either way it is not lost.
            logFM WarningS (ls ("leaving mirror job un-acked for retry (redelivered by a durable queue, re-mirrored on next demand by the in-memory one): " <> reason))

{- Retire a message the worker will never mirror: alarm, then ack so it stops cycling.
One terminal path serves both a job that can never succeed and a delivery that spent
the queue's budget. The two cannot drift on what retiring means. On the in-memory
backend the ack is the no-op a delivered job already is. On a durable queue it is the
delete that finally kills the message. -}
retireTerminally :: Text -> ReceiptHandle -> WorkerM ()
retireTerminally reason receipt = do
    logFM ErrorS (ls reason)
    ackMessage receipt

-- The alarm for a delivery past the queue's budget. On a queue with no dead-letter
-- terminus this line is the only record the message ever leaves. It names the job,
-- what it cost, and what the operator can do about it.
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

-- Classify a terminal job outcome into the bounded @ecluse.mirror.jobs.processed@
-- result. A successful publish counts as published, the idempotent already-present
-- 409 included. A dropped, dead-lettered, or retried job is a failure. A message the
-- worker retires for spending the queue's budget counts as 'Metric.Discarded'. The
-- worker counts it at the retirement, not here, because a delivery it never runs has
-- no job outcome.
jobResultMetric :: JobOutcome -> Metric.MirrorResult
jobResultMetric = \case
    Succeeded -> Metric.Published
    Dropped _ -> Metric.Failed
    DeadLettered _ -> Metric.Failed
    Retried _ -> Metric.Failed

-- Acknowledge a terminally-processed message. This absorbs a failed ack after a
-- warning. The message stays un-acked and redelivers, and idempotent publishing makes
-- the repeat harmless. That is the retry-is-don't-ack shape, arrived at by accident
-- rather than decision.
ackMessage :: ReceiptHandle -> WorkerM ()
ackMessage receipt = do
    queue <- asks wrQueue
    acked <- liftIO (ack queue receipt)
    whenLeft_ acked $ \fault ->
        logFM WarningS (ls ("ack failed; the processed message will redeliver (harmless, publishing is idempotent): " <> qfDetail fault))

-- Realise a terminal fault through the queue's dead-letter capability. The in-memory
-- backend drops it. A durable backend returns it to ride the operator's redrive policy
-- to the dead-letter queue, never a plain delete, which would silently discard it.
-- This absorbs a 'Left' after a warning. The message redelivers and re-fails
-- terminally either way, so the fault is not lost.
deadLetterMessage :: ReceiptHandle -> WorkerM ()
deadLetterMessage receipt = do
    queue <- asks wrQueue
    outcome <- liftIO (deadLetter queue receipt)
    whenLeft_ outcome $ \fault ->
        logFM WarningS (ls ("dead-letter realisation failed; the message redelivers and re-fails terminally (harmless): " <> qfDetail fault))

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
    | {- | A __non-retryable__ rejection: the bytes did not match the re-admitted
      artifact's digest (tamper), or the publish URL was unformable
      (misconfiguration). Redelivery cannot help, so the job is acked to retire it
      after alarming. Carries the reason.
      -}
      Dropped Text
    | {- | A __terminal__ fault the backend dead-letters. An artifact past the
      plan-sized byte cap can never succeed and re-fetches identical over-cap bytes
      on every redelivery. It is therefore not acked, since a plain delete would
      silently discard it on a durable queue, but handed to the queue's
      'Ecluse.Core.Queue.deadLetter' terminus. The in-memory backend drops it, and a
      durable queue rides it to the dead-letter queue for forensic retention. Carries
      the reason.
      -}
      DeadLettered Text
    | {- | A __transient__ fault: a fetch failure, or a registry rejection worth
      retrying. The message is left un-acked so it redelivers. Carries the reason.
      -}
      Retried Text
    deriving stock (Eq, Show)

{- | Process one mirror job end to end. __Probe the mirror target__ for the job's
version. The worker acks a confirmed-present version outright, the
duplicate-suppression short-circuit. Then __re-evaluate current policy__. Only on a
current admit, fetch the artifact and verify it against the integrity digests of the
artifact that re-evaluation re-admitted. Then publish it to the mirror target. Returns
the 'JobOutcome' that decides whether to ack the message or let it redeliver.

The presence probe exists for the enqueue-to-availability window. Mirroring is
demand-driven, so every public-leg admit of a still-unmirrored version enqueues its own
job, and a fleet-wide install of a novel version enqueues many. Without the probe each
duplicate pays a full artifact download and an integrity recompute. Only then does the
publish return the idempotent already-present answer. With the probe, a duplicate costs
one metadata round trip. The probe is an __optimisation, never a gate__. It skips only
work whose publish is that no-op. The policy re-evaluation below still guards every
artifact that actually publishes.

The policy re-evaluation is the ingest-time gate. The serve path gated the version once.
The enqueue-to-process window is asynchronous and unbounded, so policy may have
tightened toward deny since: a new denylist entry, a freshly-published advisory, a
rule-config change. The worker re-runs the __same__ rules the serve path gates with,
over the version resolved through the __same__ single-version fetch-and-project. A
now-denied version is therefore dropped, acked and never published, rather than frozen
into the rule-exempt trusted mirror store. The worker likewise drops a version the
upstream has since withdrawn. Metadata it cannot re-fetch, or a rule it cannot compute,
leaves the job for redelivery. A current admit carries the re-admitted artifact's
integrity digests to the tamper gate. The gate verifies the fetched bytes against the
exact set the integrity floor cleared. The queue payload carries no digest at all. A
tampered or corrupt artifact fails the job with no publish, because the mirror is later
served without the rules.

The receipt handle is taken so a long publish can 'Ecluse.Core.Queue.extendVisibility'
to hold the message before its window lapses.

The per-job domain span (the worker tracing port) wraps the whole probe → re-evaluate →
fetch → verify → publish. It projects the terminal outcome onto the span, so a refused
or dropped job is explainable from the trace. It also __links__ back to the request
that enqueued the job, through the trace context the job carries ('jobTraceContext').
The unlift discharges the span body to 'IO', so the loop's structured log lines still
compose through the ambient @katip@ context.
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
    -- Project a terminal job outcome onto the worker-job span. The bounded outcome
    -- label goes on always. The failure detail, which marks the span errored, goes on
    -- when the job did not publish.
    jobSpanOutcome :: JobOutcome -> JobSpanOutcome
    jobSpanOutcome = \case
        Succeeded -> JobSpanOutcome "succeeded" Nothing
        Dropped reason -> JobSpanOutcome "dropped" (Just reason)
        DeadLettered reason -> JobSpanOutcome "dead-lettered" (Just reason)
        Retried reason -> JobSpanOutcome "retried" (Just reason)

-- The terminal decision of re-evaluating current policy for a job, before any artifact
-- fetch: admit, drop, or retry. An admit mirrors the job and carries the re-admitted
-- artifact's descriptor. That descriptor is the floor-checked digest set the tamper
-- gate verifies against, plus the filename and declared size that make up the publish
-- document. A drop is a current deny or a withdrawn version, acked and never
-- published. A retry is unobtainable metadata or an uncomputable rule, left for
-- redelivery. This value does not carry the admitting ecosystem's bundle: the
-- dispatcher resolved it before the probe and threads it forward.
data ReevalOutcome
    = ReevalAdmit MirrorArtifact
    | ReevalDrop Text
    | ReevalRetry Text

-- Resolve the job ecosystem's bundle first, so a job whose ecosystem carries none is
-- fail-closed before any network step. Then probe the mirror target, since a
-- confirmed-present version is a no-op job, acked without another byte moved. Then
-- re-evaluate current policy, and mirror on a current admit. The cheap steps run
-- before the (potentially large) artifact fetch. A duplicate therefore retires for one
-- metadata round trip, and a now-denied job drops without downloading its bytes.
-- Every step past the lookup rides the resolved bundle, so no job can consult a
-- foreign ecosystem's probe, rules, request formation, or publish.
reevaluateThenMirror :: ReceiptHandle -> MirrorJob -> WorkerM JobOutcome
reevaluateThenMirror receipt job = do
    policies <- asks wrPolicies
    case Map.lookup (pkgEcosystem (jobPackage job)) policies of
        Nothing ->
            -- Structurally unreachable when every mounted ecosystem declares its
            -- mirror target: activation implies a bundle, and only an activated
            -- ecosystem enqueues jobs. Kept as the fail-closed
            -- defence-in-depth drop for the impossible case.
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

{- Ask the mirror target whether the job's version is already present, through the
bundle's married publish capability. The probe confirms presence __positively only__:
'True' needs the mirror's own metadata to parse and to list the version. A fetch fault
or an unparseable body answers 'False', so the job falls through to the full gated
pipeline. That covers a mirror @404@ for a package not yet mirrored, an auth refusal,
and an outage. A false 'False' costs one redundant download and an idempotent
re-publish, exactly the behaviour without a probe. The probe can therefore only ever
save work, never lose a publish or admit one unvetted. The fetch reports its failures
as 'Ecluse.Core.Registry.FetchFault' values, so the fall-through is a total match,
nothing caught. -}
alreadyMirrored :: WorkerPolicy -> MirrorJob -> WorkerM Bool
alreadyMirrored policy job = do
    probed <- liftIO (mpProbeMetadata (wpPublish policy) (jobPackage job))
    case probed of
        Left fault -> do
            -- A probe that returns no usable body forfeits only duplicate suppression,
            -- never correctness: the job falls through to the full gated pipeline.
            -- Logged at DebugS so a persistently-failing probe (a mirror packument over
            -- the response bound, an auth refusal, an outage) is diagnosable, not silent.
            logFM DebugS (ls ("mirror presence probe did not confirm " <> renderJob job <> "; falling through to full re-evaluation: " <> show fault))
            pure False
        Right response -> case mpParseVersionList (wpPublish policy) response of
            Left _ -> pure False
            Right versions -> pure (jobVersion job `elem` versions)

{- Re-check the job's fetch URL against the mount's tarball-host gate, because the
queue payload is a trust boundary. Then re-run current policy for the job's single
version through the shared admission gate
('Ecluse.Core.Package.Admission.admitArtifact': rules, the job's filename, the
integrity floor).

The outcomes mirror the serve path's degrade. A withdrawn or absent version, or a
filename its current metadata no longer carries, is a non-retryable drop, and
unobtainable metadata is a transient retry. A rule block, deny-by-default, a refused
host, or an integrity-policy refusal drops. An uncomputable rule retries rather than
dropping a serviceable job or publishing it unvetted. -}
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

-- The worker's projection of the shared 'ArtifactAdmission', where the serve gate
-- renders the same verdicts as HTTP statuses. An admit mirrors, carrying the admission
-- gate's own floor-checked digest set forward as the tamper gate's verification set.
-- Every deliberate refusal drops, never frozen into the rule-exempt mirror store. An
-- undecidable verdict retries, so a transient advisory-source outage neither drops a
-- serviceable job nor publishes it unvetted. Total over 'ArtifactAdmission', so a new
-- admission outcome cannot be silently ignored here while the serve path handles it.
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

-- The re-admitted artifact's descriptor, derived entirely from current metadata. It
-- holds the floor-checked digest set the tamper gate verifies the fetched bytes
-- against. It also holds the filename and registry-declared size that make up the
-- publish document. The queue payload contributes nothing here, since it carries no
-- digest or size, so payload text can never reach the trusted-tier publish document.
-- The filename equals the payload's by construction, because admission selected the
-- artifact by exactly that name.
readmittedDescriptor :: Artifact -> NonEmpty Hash -> MirrorArtifact
readmittedDescriptor artifact digests =
    MirrorArtifact
        { maFilename = artFilename artifact
        , maHashes = digests
        , maSize = artSize artifact
        }

-- Fetch the artifact bytes through the admitting ecosystem's own request formation.
-- Verify them against the re-admitted artifact's digests, the floor-checked
-- current-metadata set, since the queue payload carries no digest at all. Publish to
-- the mirror target only on a match. Reached only on a current policy admit, so the
-- bundle carrying the formation always exists here. The integrity gate is the security
-- crux. A tampered or corrupt artifact must never reach the private upstream, which
-- later serves it without the rules. A mismatch therefore fails the job with no
-- publish, and alarms.

{- | Classify a mirror-artifact fetch fault into a terminal job outcome. An artifact
over the plan-sized byte cap is a __terminal, dead-lettered__ fault. It is
deterministic in the artifact's own size, so a redelivery re-fetches the same over-cap
bytes and fails identically, and it must not silently vanish. It goes to the backend's
dead-letter terminus (see 'DeadLettered'). Any other fetch fault, an unformable URL or
a transport failure, is a transient retry, since a redelivery may succeed.
-}
outcomeOfFetchFault :: ArtifactFetchFault -> JobOutcome
outcomeOfFetchFault = \case
    ArtifactOverCap reason -> DeadLettered reason
    ArtifactUnavailable reason -> Retried reason

mirrorArtifact :: WorkerPolicy -> ReceiptHandle -> MirrorJob -> MirrorArtifact -> WorkerM JobOutcome
mirrorArtifact policy receipt job admitted = do
    logFM DebugS (ls ("fetching artifact bytes from " <> jobArtifactAuthority job))
    fetched <- fetchArtifactBytes (wpArtifactLimits policy) (wpBuildArtifactRequest policy) (jobArtifactUrl job)
    case fetched of
        -- 'outcomeOfFetchFault' makes the terminal-versus-transient split, and
        -- 'processMessage' logs the reason at the queue-realisation site.
        Left fault -> pure (outcomeOfFetchFault fault)
        Right bytes ->
            case verifyIntegrity (maHashes admitted) bytes of
                IntegrityMismatch detail -> do
                    logFM ErrorS (ls ("artifact integrity mismatch, refusing to publish: " <> detail))
                    pure (Dropped ("integrity mismatch: " <> detail))
                IntegrityVerified -> publishVerified policy receipt job admitted bytes

-- Publish already-verified bytes to the mirror target. Hold the message past the
-- visibility window first, because a large-artifact publish may run long. Then publish
-- through the bundle's married capability, and classify the registry outcome into a
-- 'JobOutcome'. That capability's codec assembles the ecosystem-specific document from
-- the re-admitted artifact's descriptor. The queue payload carries no digest or size,
-- so payload text cannot reach the trusted-tier packument.
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
            -- Transient: undo the long success-path hold, so the job redelivers at
            -- once rather than waiting it out. The hold only exists to protect a slow
            -- success. The message is left un-acked, so it redelivers either way.
            releaseForRetry receipt
            pure (Retried ("registry rejected publish: " <> show err))
        Left (PublishTransport fault) -> do
            -- Transient: the write never reached the registry (a connection failure,
            -- a timeout). Release the hold and let the un-acked message redeliver,
            -- exactly as a registry rejection does. The classified fault carries its
            -- own bounded detail, and this renders the prefix exactly once.
            releaseForRetry receipt
            pure (Retried ("publish transport failure: " <> show fault))
        Left (PublishUrlUnformable urlErr) ->
            -- Non-retryable: 'processMessage' acks this to retire it, so there is no
            -- redelivery to hasten. Leave the hold be.
            pure (Dropped ("unformable publish URL: " <> show urlErr))

-- Hold a received message past the visibility window before a publish that may run
-- long. A slow write then cannot let the message redeliver mid-publish. A mid-publish
-- redelivery would waste a full re-fetch and re-publish of a potentially large
-- artifact. The hold is an optimisation, since idempotency makes a redelivery
-- harmless. This swallows a failure to extend rather than failing the job.
holdForLongPublish :: ReceiptHandle -> WorkerM ()
holdForLongPublish receipt = do
    queue <- asks wrQueue
    -- The fault channel is a value, and a failed extend is the swallowed 'Left'.
    _ <- liftIO (extendVisibility queue receipt workerPublishVisibilityBudget)
    pass

{- | The visibility window one publish is given before its message could redeliver
mid-write. It covers a publish of the largest artifact the memory plan's fetch cap
admits. That is the mirror-artifact tenant, at most 512 MiB at its ceiling. Even over
a slow mirror-target link (a conservative ~2 MiB/s floor), that artifact uploads in
well under this budget, so a successful publish never redelivers mid-flight. A
__failed__ publish does not wait this out, because the failure path resets the message
to visible at once (see @releaseForRetry@). The generous hold therefore costs nothing
on the retry path. That is the background worker's correct trade: never interrupt a
slow success, and retry latency on failure does not matter.

The liveness staleness bound ('Ecluse.Core.Worker.Liveness.workerHeartbeatStaleAfter')
exceeds a fetch and a publish of this budget. Nothing therefore reads a healthy worker
mid-publish as stalled. The @Ecluse.Worker.LivenessSpec@ module pins that relationship,
so the two constants cannot drift apart.
-}
workerPublishVisibilityBudget :: Seconds
workerPublishVisibilityBudget = Seconds 300

-- Reset a received message to immediately visible, so a failed publish redelivers at
-- once rather than waiting out the long success-path hold ('holdForLongPublish'). A
-- best-effort optimisation: a missed reset only means the message redelivers after
-- the hold instead. This swallows a failure to reset.
releaseForRetry :: ReceiptHandle -> WorkerM ()
releaseForRetry receipt = do
    queue <- asks wrQueue
    -- The fault channel is a value, and a failed reset is the swallowed 'Left'.
    _ <- liftIO (extendVisibility queue receipt (Seconds 0))
    pass

-- A one-line identifier for a job, for log lines.
renderJob :: MirrorJob -> Text
renderJob job = renderPackageName (jobPackage job) <> "@" <> renderVersion (jobVersion job)

{- The job's artifact location as a log-safe authority ('authorityLabel'). The queue
payload's URL can carry userinfo or a pre-signed query. A log line therefore names the
host and port the worker dials, never the URL. -}
jobArtifactAuthority :: MirrorJob -> Text
jobArtifactAuthority = authorityLabel . registryUrlText . jobArtifactUrl
