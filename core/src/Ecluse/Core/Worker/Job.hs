-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Ack within the visibility budget during job processing.

A received message is hidden only for the queue's visibility window. The worker
acks on success; before a publish that may run long it calls
'Ecluse.Core.Queue.extendVisibility' to hold the message before the window lapses; on a
transient failure it does __not__ ack, so the message redelivers. A batch is
processed __sequentially__, so each job has the full visibility budget rather than
competing with its batch-mates for it.
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
import Ecluse.Core.Queue (MirrorJob (jobArtifactFilename, jobArtifactUrl, jobPackage, jobTraceContext, jobVersion), MirrorQueue (ack, deadLetter, extendVisibility), QueueMessage (msgJob, msgReceipt), ReceiptHandle, Seconds (Seconds), qfDetail)
import Ecluse.Core.Registry (MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize), PublishFault (PublishRejected, PublishTransport, PublishUrlUnformable))
import Ecluse.Core.Registry.Metadata (VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent))
import Ecluse.Core.Registry.Publish (MirrorPublish (mpParseVersionList, mpProbeMetadata, mpPublishArtifact))
import Ecluse.Core.Rules.Types (Decision (Blocked, Undecidable), mkEvalContext)
import Ecluse.Core.Security (hostPortAddress)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (JobSpanOutcome (JobSpanOutcome), WorkerTracingPort (..))
import Ecluse.Core.Version (renderVersion)
import Ecluse.Core.Worker.Fetch (ArtifactFetchFault (ArtifactOverCap, ArtifactUnavailable), fetchArtifactBytes)
import Ecluse.Core.Worker.Integrity (IntegrityResult (..), verifyIntegrity)
import Ecluse.Core.Worker.Types

{- | Process one received batch __sequentially__, so each job gets the full
visibility budget rather than competing with its batch-mates for it. A batch is at
most the queue's configured batch size (≤ 10), so sequential processing is a
deliberate throughput-vs-budget choice, not a scaling bottleneck.

The liveness heartbeat advances after __each__ completed job, not once for the whole
batch, so the @\/livez@ staleness bound
('Ecluse.Core.Worker.Liveness.workerHeartbeatStaleAfter') need only cover one job's
worst case rather than a whole sequential batch of large-artifact publishes: a
healthy worker mid-batch is not mistaken for a stalled one.
-}
processBatch :: [QueueMessage] -> WorkerM ()
processBatch = traverse_ $ \message -> do
    processMessage message
    recordWorkerProgress

-- Process one message: run the job, then realise its terminal outcome -- ack a
-- success or a clean non-retryable drop, dead-letter a terminal fault (the backend
-- routes it to its own terminus), or leave a transient failure un-acked so the queue
-- redelivers it ("retry is don't ack").
processMessage :: QueueMessage -> WorkerM ()
processMessage message = do
    metrics <- asks wrMetrics
    outcome <- processJob (msgReceipt message) (msgJob message)
    liftIO (wmpMirrorJobProcessed metrics (jobResultMetric outcome))
    case outcome of
        Succeeded -> ackMessage (msgReceipt message)
        Dropped reason -> do
            -- A clean non-retryable rejection (a tampered artifact, an unformable
            -- publish URL): the job can never succeed and is not worth a dead-letter
            -- forensic trail, so ack it to retire it, having already alarmed.
            logFM ErrorS (ls ("dropping unrecoverable mirror job: " <> reason))
            ackMessage (msgReceipt message)
        DeadLettered reason -> do
            -- A terminal fault the backend routes to its own dead-letter terminus: the
            -- in-memory backend drops it (its only terminus), a durable queue returns
            -- it to ride the operator's redrive policy to the dead-letter queue. The
            -- alarm goes first, since on the memory backend the log and metric are the
            -- only observability there is.
            logFM ErrorS (ls ("dead-lettering unmirrorable mirror job (rides the backend's dead-letter terminus): " <> reason))
            deadLetterMessage (msgReceipt message)
        Retried reason ->
            -- A transient fault: leave the message un-acked. How it is retried is
            -- backend-dependent -- a durable queue redelivers it once the visibility
            -- window lapses, while the in-memory backend (no redelivery) simply
            -- re-mirrors it on the next demand. Either way it is not lost.
            logFM WarningS (ls ("leaving mirror job un-acked for retry (redelivered by a durable queue, re-mirrored on next demand by the in-memory one): " <> reason))

-- Classify a terminal job outcome into the bounded @ecluse.mirror.jobs.processed@
-- result: a successful publish (the idempotent already-present 409 surfaces here too)
-- is published; a dropped, dead-lettered, or retried job is a failure.
jobResultMetric :: JobOutcome -> Metric.MirrorResult
jobResultMetric = \case
    Succeeded -> Metric.Published
    Dropped _ -> Metric.Failed
    DeadLettered _ -> Metric.Failed
    Retried _ -> Metric.Failed

-- Acknowledge a terminally-processed message. A failed ack is absorbed after a
-- warning: the message simply stays un-acked and redelivers, and idempotent
-- publishing makes the repeat harmless -- exactly the retry-is-don't-ack shape,
-- arrived at by accident rather than decision.
ackMessage :: ReceiptHandle -> WorkerM ()
ackMessage receipt = do
    queue <- asks wrQueue
    acked <- liftIO (ack queue receipt)
    whenLeft_ acked $ \fault ->
        logFM WarningS (ls ("ack failed; the processed message will redeliver (harmless, publishing is idempotent): " <> qfDetail fault))

-- Realise a terminal fault through the queue's dead-letter capability: the in-memory
-- backend drops it, a durable backend returns it to ride the operator's redrive policy
-- to the dead-letter queue (never a plain delete, which would silently discard it). A
-- 'Left' is absorbed after a warning -- the message redelivers and re-fails terminally
-- either way, so the fault is not lost.
deadLetterMessage :: ReceiptHandle -> WorkerM ()
deadLetterMessage receipt = do
    queue <- asks wrQueue
    outcome <- liftIO (deadLetter queue receipt)
    whenLeft_ outcome $ \fault ->
        logFM WarningS (ls ("dead-letter realisation failed; the message redelivers and re-fails terminally (harmless): " <> qfDetail fault))

{- | The terminal outcome of processing one mirror job, deciding whether the
message is acked or left to redeliver.
-}
data JobOutcome
    = {- | The publish succeeded, so the job is acked. This covers an idempotent
      redelivery too: a version already present at the mirror target answers a
      status the ecosystem's codec classifies as success (npm's @409@), so it
      surfaces here as 'Succeeded' rather than a distinct case -- as does the same
      presence confirmed by the pre-fetch probe, before any bytes moved.
      -}
      Succeeded
    | {- | A __non-retryable__ rejection: the bytes did not match the re-admitted
      artifact's digest (tamper), or the publish URL was unformable
      (misconfiguration). Redelivery cannot help, so the job is acked to retire it
      after alarming. Carries the reason.
      -}
      Dropped Text
    | {- | A __terminal__ fault the backend dead-letters: an artifact past the
      plan-sized byte cap can never succeed and re-fetches identical over-cap bytes on
      every redelivery, so it is not acked (a plain delete would silently discard it on
      a durable queue) but handed to the queue's 'Ecluse.Core.Queue.deadLetter'
      terminus -- the in-memory backend drops it, a durable queue rides it to the
      dead-letter queue for forensic retention. Carries the reason.
      -}
      DeadLettered Text
    | {- | A __transient__ fault: a fetch failure, or a registry rejection worth
      retrying. The message is left un-acked so it redelivers. Carries the reason.
      -}
      Retried Text
    deriving stock (Eq, Show)

{- | Process one mirror job end to end: __probe the mirror target__ for the job's version
(a confirmed-present version is acked outright, the duplicate-suppression short-circuit),
then __re-evaluate current policy__, and only on a current admit fetch the artifact,
verify it against the integrity digests of the artifact that re-evaluation re-admitted,
and publish it to the mirror target. Returns the 'JobOutcome' that decides ack vs.
redeliver.

The presence probe exists for the enqueue-to-availability window: mirroring is
demand-driven, so every public-leg admit of a still-unmirrored version enqueues its own
job, and a fleet-wide install of a novel version enqueues many. Without the probe each
duplicate pays a full artifact download and an integrity recompute before the publish
discovers the version is already present (the idempotent already-present answer); with
it, a duplicate costs one metadata round trip. The probe is an __optimisation, never a gate__: it skips
only work whose publish would have been that no-op, so the policy re-evaluation below
still guards every artifact that actually publishes.

The policy re-evaluation is the ingest-time gate. The version was gated at serve time, but
the enqueue-to-process window is asynchronous and unbounded, so policy may have tightened
toward deny since (a new denylist entry, a freshly-published advisory, a rule-config
change). The worker re-runs the __same__ rules the serve path gates with, over the version
resolved through the __same__ single-version fetch-and-project, so a now-denied version is
dropped (acked, never published) rather than frozen into the rule-exempt trusted mirror
store; a version the upstream has since withdrawn is likewise dropped, while metadata that
cannot be re-fetched (or a rule that cannot be computed) leaves the job for redelivery. A
current admit carries the re-admitted artifact's integrity digests to the tamper gate, so
the fetched bytes are verified against the exact set the integrity floor cleared (the
queue payload carries no digest at all): a tampered or corrupt artifact fails
the job with no publish, since the mirror is later served without the rules.

The receipt handle is taken so a long publish can 'Ecluse.Core.Queue.extendVisibility'
to hold the message before its window lapses.

The per-job domain span (the worker tracing port) wraps the whole probe → re-evaluate →
fetch → verify → publish, projecting the terminal outcome onto the span so a refused or dropped job
is explainable from the trace, and __linking__ back to the request that enqueued the job
through the trace context the job carries ('jobTraceContext'). The span body is discharged
to 'IO' through the unlift, so the loop's structured log lines still compose through the
ambient @katip@ context.
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
    -- Project a terminal job outcome onto the worker-job span: the bounded outcome
    -- label always, and the failure detail (which marks the span errored) when the
    -- job did not publish.
    jobSpanOutcome :: JobOutcome -> JobSpanOutcome
    jobSpanOutcome = \case
        Succeeded -> JobSpanOutcome "succeeded" Nothing
        Dropped reason -> JobSpanOutcome "dropped" (Just reason)
        DeadLettered reason -> JobSpanOutcome "dead-lettered" (Just reason)
        Retried reason -> JobSpanOutcome "retried" (Just reason)

-- The terminal decision of re-evaluating current policy for a job, before any artifact
-- fetch: admit (mirror it, carrying the re-admitted artifact's descriptor: the
-- floor-checked digest set the tamper gate verifies against, and the filename and
-- declared size the publish document is assembled from), drop (a current deny or a
-- withdrawn version, acked and never published), or retry (metadata unobtainable, or a
-- rule uncomputable, left for redelivery). The admitting ecosystem's bundle is not
-- carried here: the dispatcher resolved it before the probe and threads it forward.
data ReevalOutcome
    = ReevalAdmit MirrorArtifact
    | ReevalDrop Text
    | ReevalRetry Text

-- Resolve the job ecosystem's bundle first (a job whose ecosystem carries none is
-- fail-closed before any network step), then probe the mirror target (a
-- confirmed-present version is a no-op job, acked without another byte moved),
-- then re-evaluate current policy, then mirror on a current admit. The cheap steps
-- run before the (potentially large) artifact fetch, so a duplicate is retired for
-- one metadata round trip and a now-denied job is dropped without downloading its
-- bytes. Every step past the lookup rides the resolved bundle, so no job can
-- consult a foreign ecosystem's probe, rules, request formation, or publish.
reevaluateThenMirror :: ReceiptHandle -> MirrorJob -> WorkerM JobOutcome
reevaluateThenMirror receipt job = do
    policies <- asks wrPolicies
    case Map.lookup (pkgEcosystem (jobPackage job)) policies of
        Nothing ->
            -- Structurally unreachable when every mounted ecosystem declares its
            -- mirror target (activation implies a bundle; only activated
            -- ecosystems' jobs are enqueued); kept as the fail-closed
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
bundle's married publish capability. __Positive confirmation only__: 'True' needs
the mirror's own metadata to parse and to list the version; a fetch fault or an
unparseable body (a mirror @404@ for a package not yet mirrored, an auth refusal, an
outage) answers 'False', so the job falls through to the full gated pipeline. A false
'False' costs one redundant download and an idempotent re-publish -- exactly the
pre-probe behaviour -- so the probe can only ever save work, never lose a publish or
admit one unvetted. The fetch reports its failures as
'Ecluse.Core.Registry.FetchFault' values, so the fall-through is a total match,
nothing caught. -}
alreadyMirrored :: WorkerPolicy -> MirrorJob -> WorkerM Bool
alreadyMirrored policy job = do
    probed <- liftIO (mpProbeMetadata (wpPublish policy) (jobPackage job))
    pure $ case probed of
        Left _ -> False
        Right response -> case mpParseVersionList (wpPublish policy) response of
            Left _ -> False
            Right versions -> jobVersion job `elem` versions

{- Re-run current policy for the job's single version through the shared admission
gate ('Ecluse.Core.Package.Admission.admitArtifact' -- rules, the job's filename,
the integrity floor), after re-checking the job's fetch URL against the mount's
tarball-host gate (the queue payload is a trust boundary).

The outcomes mirror the serve path's degrade: a withdrawn/absent version (or a
filename its current metadata no longer carries) is a non-retryable drop,
unobtainable metadata a transient retry; a rule block, deny-by-default, refused host,
or integrity-policy refusal drops, and an uncomputable rule retries rather than
dropping a serviceable job or publishing it unvetted. -}
reevaluatePolicy :: WorkerPolicy -> MirrorJob -> WorkerM ReevalOutcome
reevaluatePolicy policy job
    | not (wpArtifactHostHonoured policy (hostPortAddress (registryUrlText (jobArtifactUrl job)))) =
        pure (ReevalDrop ("the tarball-host policy refuses the artifact host of " <> renderJob job <> " (" <> registryUrlText (jobArtifactUrl job) <> "); refusing to fetch or mirror it"))
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

-- The worker's projection of the shared 'ArtifactAdmission' (the serve gate renders
-- the same verdicts as HTTP statuses): an admit mirrors, carrying the admission
-- gate's own floor-checked digest set forward
-- as the tamper gate's verification set; every deliberate refusal drops (never
-- frozen into the rule-exempt mirror store); an undecidable verdict retries, so a
-- transient advisory-source outage neither drops a serviceable job nor publishes it
-- unvetted. Total over 'ArtifactAdmission', so a new admission outcome cannot be
-- silently ignored here while the serve path handles it.
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

-- The re-admitted artifact's descriptor, derived entirely from current metadata: the
-- floor-checked digest set the tamper gate verifies the fetched bytes against, and the
-- filename and registry-declared size the publish document is assembled from. The
-- queue payload contributes nothing here (it carries no digest or size to
-- contribute), so payload text can never reach the trusted-tier publish document.
-- The filename equals the payload's by construction (admission selected the
-- artifact by exactly that name).
readmittedDescriptor :: Artifact -> NonEmpty Hash -> MirrorArtifact
readmittedDescriptor artifact digests =
    MirrorArtifact
        { maFilename = artFilename artifact
        , maHashes = digests
        , maSize = artSize artifact
        }

-- Fetch the artifact bytes (through the admitting ecosystem's own request formation),
-- verify them against the re-admitted artifact's digests (the floor-checked,
-- current-metadata set; the queue payload carries no digest at all), and (only on a
-- match) publish to the mirror target. Reached only on a current policy admit, so
-- the bundle carrying the formation always exists here. The integrity gate is the
-- security crux: a tampered or corrupt artifact must never reach the private
-- upstream, which is served without the rules, so a mismatch fails the job with no
-- publish and alarms.

{- | Classify a mirror-artifact fetch fault into a terminal job outcome. An artifact
over the plan-sized byte cap is a __terminal, dead-lettered__ fault: it is
deterministic in the artifact's own size, so a redelivery re-fetches the same over-cap
bytes and fails identically, and it must not silently vanish -- it is handed to the
backend's dead-letter terminus (see 'DeadLettered'). Any other fetch fault (an
unformable URL, a transport failure) is a transient retry, since a redelivery may
succeed. This is issue #846's classification, reworked so an over-cap artifact rides
the durable dead-letter path rather than being flat-dropped.
-}
outcomeOfFetchFault :: ArtifactFetchFault -> JobOutcome
outcomeOfFetchFault = \case
    ArtifactOverCap reason -> DeadLettered reason
    ArtifactUnavailable reason -> Retried reason

mirrorArtifact :: WorkerPolicy -> ReceiptHandle -> MirrorJob -> MirrorArtifact -> WorkerM JobOutcome
mirrorArtifact policy receipt job admitted = do
    logFM DebugS (ls ("fetching artifact bytes from " <> registryUrlText (jobArtifactUrl job)))
    fetched <- fetchArtifactBytes (wpArtifactLimits policy) (wpBuildArtifactRequest policy) (jobArtifactUrl job)
    case fetched of
        -- The terminal-vs-transient split is 'outcomeOfFetchFault'; the reason is
        -- logged at the queue-realisation site in 'processMessage'.
        Left fault -> pure (outcomeOfFetchFault fault)
        Right bytes ->
            case verifyIntegrity (maHashes admitted) bytes of
                IntegrityMismatch detail -> do
                    logFM ErrorS (ls ("artifact integrity mismatch, refusing to publish: " <> detail))
                    pure (Dropped ("integrity mismatch: " <> detail))
                IntegrityVerified -> publishVerified policy receipt job admitted bytes

-- Publish already-verified bytes to the mirror target: hold the message past the
-- visibility window (a large-artifact publish may run long), publish through the
-- bundle's married capability, whose codec assembles the ecosystem-specific document
-- from the re-admitted artifact's descriptor (the queue payload carries no digest
-- or size, so payload text cannot reach the trusted-tier packument), and classify
-- the registry outcome into a 'JobOutcome'.
publishVerified :: WorkerPolicy -> ReceiptHandle -> MirrorJob -> MirrorArtifact -> ByteString -> WorkerM JobOutcome
publishVerified policy receipt job admitted bytes = do
    holdForLongPublish receipt
    metrics <- asks wrMetrics
    -- The publish is the long, network-bound step; time it for the publish-latency
    -- histogram whichever way the registry responds.
    (result, seconds) <- timedSeconds (liftIO (mpPublishArtifact (wpPublish policy) (jobPackage job) (jobVersion job) admitted bytes))
    liftIO (wmpMirrorPublishDuration metrics seconds)
    case result of
        Right () -> do
            logFM InfoS (ls ("mirrored artifact published: " <> renderJob job))
            pure Succeeded
        Left (PublishRejected err) -> do
            -- Transient: undo the long success-path hold so the job redelivers at once
            -- rather than waiting it out (the hold only exists to protect a slow
            -- success). The message is left un-acked, so it redelivers either way.
            releaseForRetry receipt
            pure (Retried ("registry rejected publish: " <> show err))
        Left (PublishTransport fault) -> do
            -- Transient: the write never reached the registry (a connection failure, a
            -- timeout), so release the hold and let the un-acked message redeliver,
            -- exactly as a registry rejection. The classified fault carries its own
            -- bounded detail; the prefix is rendered here exactly once.
            releaseForRetry receipt
            pure (Retried ("publish transport failure: " <> show fault))
        Left (PublishUrlUnformable urlErr) ->
            -- Non-retryable: 'processMessage' acks this to retire it, so there is no
            -- redelivery to hasten -- leave the hold be.
            pure (Dropped ("unformable publish URL: " <> show urlErr))

-- Hold a received message past the visibility window before a publish that may run
-- long, so a slow write does not let the message redeliver mid-publish -- which would
-- waste a full re-fetch and re-publish of a (potentially large) artifact. The hold is
-- an optimization (idempotency makes a redelivery harmless), so a failure to extend
-- is swallowed rather than failing the job.
holdForLongPublish :: ReceiptHandle -> WorkerM ()
holdForLongPublish receipt = do
    queue <- asks wrQueue
    -- The fault channel is a value; a failed extend is the swallowed 'Left'.
    _ <- liftIO (extendVisibility queue receipt workerPublishVisibilityBudget)
    pass

{- | The visibility window one publish is given before its message could redeliver
mid-write. Sized to comfortably cover a publish of the largest artifact the memory
plan's fetch cap admits (the mirror-artifact tenant, at most 512 MiB at its ceiling):
even over a slow mirror-target link (a conservative ~2 MiB/s floor) that uploads in
well under this, so a successful publish never redelivers mid-flight. A __failed__
publish does not wait this out -- the failure path
resets the message to visible at once (see @releaseForRetry@) -- so the generous hold
costs nothing on the retry path; this is the background worker's correct trade (never
interrupt a slow success; retry latency on failure does not matter).

The liveness staleness bound ('Ecluse.Core.Worker.Liveness.workerHeartbeatStaleAfter')
is sized to exceed a fetch and a publish of this budget, so a healthy worker
mid-publish is never read as stalled; @Ecluse.Worker.LivenessSpec@ pins that
relationship so the two constants cannot drift apart.
-}
workerPublishVisibilityBudget :: Seconds
workerPublishVisibilityBudget = Seconds 300

-- Reset a received message to immediately visible, so a failed publish redelivers at
-- once rather than waiting out the long success-path hold ('holdForLongPublish'). A
-- best-effort optimization (a missed reset just means the message redelivers after the
-- hold instead), so a failure to reset is swallowed.
releaseForRetry :: ReceiptHandle -> WorkerM ()
releaseForRetry receipt = do
    queue <- asks wrQueue
    -- The fault channel is a value; a failed reset is the swallowed 'Left'.
    _ <- liftIO (extendVisibility queue receipt (Seconds 0))
    pass

-- A one-line identifier for a job, for log lines.
renderJob :: MirrorJob -> Text
renderJob job = renderPackageName (jobPackage job) <> "@" <> renderVersion (jobVersion job)
