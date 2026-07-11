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
    processJob,
    processBatch,
) where

import Data.Map.Strict qualified as Map
import Katip (Severity (DebugS, ErrorS, InfoS, WarningS), katipAddNamespace, logFM, ls)
import UnliftIO (tryAny, withRunInIO)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Package (pkgEcosystem, renderPackageName)
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
import Ecluse.Core.Queue (MirrorArtifact (maFilename, maHashes), MirrorJob (jobArtifact, jobArtifactUrl, jobPackage, jobTraceContext, jobVersion), MirrorQueue (ack, extendVisibility), QueueMessage (msgJob, msgReceipt), ReceiptHandle, Seconds (Seconds))
import Ecluse.Core.Registry (PublishFault (PublishRejected, PublishTransport, PublishUrlUnformable), RegistryClient (fetchMetadata, parseVersionList, publishArtifact))
import Ecluse.Core.Registry.Metadata (VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent))
import Ecluse.Core.Rules.Types (Decision (Blocked, Undecidable), mkEvalContext)
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (JobSpanOutcome (JobSpanOutcome), WorkerTracingPort (..))
import Ecluse.Core.Version (renderVersion)
import Ecluse.Core.Worker.Fetch (fetchArtifactBytes)
import Ecluse.Core.Worker.Integrity (IntegrityResult (..), verifyIntegrity)
import Ecluse.Core.Worker.Types

{- | Process one received batch __sequentially__, so each job gets the full
visibility budget rather than competing with its batch-mates for it. A batch is at
most the queue's configured batch size (≤ 10), so sequential processing is a
deliberate throughput-vs-budget choice, not a scaling bottleneck.
-}
processBatch :: [QueueMessage] -> WorkerM ()
processBatch = traverse_ processMessage

-- Process one message: run the job, and ack on any terminal outcome (success, or a
-- non-retryable drop). A transient failure leaves the message un-acked so the queue
-- redelivers it ("retry is don't ack").
processMessage :: QueueMessage -> WorkerM ()
processMessage message = do
    metrics <- asks wrMetrics
    outcome <- processJob (msgReceipt message) (msgJob message)
    liftIO (wmpMirrorJobProcessed metrics (jobResultMetric outcome))
    case outcome of
        Succeeded -> ackMessage (msgReceipt message)
        Dropped reason -> do
            -- A non-retryable fault (a tampered artifact, an unformable URL): the
            -- job can never succeed, so it must not redeliver forever. Ack it to
            -- retire it from the queue, having already alarmed at the fault site.
            logFM ErrorS (ls ("dropping unrecoverable mirror job: " <> reason))
            ackMessage (msgReceipt message)
        Retried reason ->
            -- A transient fault: leave the message un-acked. How it is retried is
            -- backend-dependent -- a durable queue redelivers it once the visibility
            -- window lapses, while the in-memory backend (no redelivery) simply
            -- re-mirrors it on the next demand. Either way it is not lost.
            logFM WarningS (ls ("leaving mirror job un-acked for retry (redelivered by a durable queue, re-mirrored on next demand by the in-memory one): " <> reason))

-- Classify a terminal job outcome into the bounded @ecluse.mirror.jobs.processed@
-- result: a successful publish (the idempotent already-present 409 surfaces here too)
-- is published, a dropped or retried job is a failure.
jobResultMetric :: JobOutcome -> Metric.MirrorResult
jobResultMetric = \case
    Succeeded -> Metric.Published
    Dropped _ -> Metric.Failed
    Retried _ -> Metric.Failed

ackMessage :: ReceiptHandle -> WorkerM ()
ackMessage receipt = do
    queue <- asks wrQueue
    liftIO (ack queue receipt)

{- | The terminal outcome of processing one mirror job, deciding whether the
message is acked or left to redeliver.
-}
data JobOutcome
    = {- | The publish succeeded, so the job is acked. This covers an idempotent
      redelivery too: a version already present at the mirror target is a @409@ the
      registry handle treats as success ('Ecluse.Core.Registry.publishArtifact'), so it
      surfaces here as 'Succeeded' rather than a distinct case -- as does the same
      presence confirmed by the pre-fetch probe, before any bytes moved.
      -}
      Succeeded
    | {- | A __non-retryable__ fault: the bytes did not match the serve-time digest
      (tamper), or the publish URL was unformable (misconfiguration). Redelivery
      cannot help, so the job is dropped after alarming. Carries the reason.
      -}
      Dropped Text
    | {- | A __transient__ fault: a fetch failure, or a registry rejection worth
      retrying. The message is left un-acked so it redelivers. Carries the reason.
      -}
      Retried Text
    deriving stock (Eq, Show)

{- | Process one mirror job end to end: __probe the mirror target__ for the job's version
(a confirmed-present version is acked outright, the duplicate-suppression short-circuit),
then __re-evaluate current policy__, and only on a current admit fetch the artifact,
verify it against the job's serve-time-admitted integrity digest, and publish it to the
mirror target. Returns the 'JobOutcome' that decides ack vs. redeliver.

The presence probe exists for the enqueue-to-availability window: mirroring is
demand-driven, so every public-leg admit of a still-unmirrored version enqueues its own
job, and a fleet-wide install of a novel version enqueues many. Without the probe each
duplicate pays a full artifact download and an integrity recompute before the publish
discovers the version is already present (the idempotent @409@); with it, a duplicate
costs one metadata round trip. The probe is an __optimisation, never a gate__: it skips
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
current admit proceeds to the integrity gate: a tampered or corrupt artifact fails the job
with no publish, since the mirror is later served without the rules.

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
        Retried reason -> JobSpanOutcome "retried" (Just reason)

-- The terminal decision of re-evaluating current policy for a job, before any artifact
-- fetch: admit (mirror it), drop (a current deny or a withdrawn version, acked and never
-- published), or retry (metadata unobtainable, or a rule uncomputable, left for redelivery).
data ReevalOutcome
    = ReevalAdmit
    | ReevalDrop Text
    | ReevalRetry Text

-- Probe the mirror target first (a confirmed-present version is a no-op job, acked
-- without another byte moved), then re-evaluate current policy, then mirror on a current
-- admit. Both cheap steps run before the (potentially large) artifact fetch, so a
-- duplicate is retired for one metadata round trip and a now-denied job is dropped
-- without downloading its bytes.
reevaluateThenMirror :: ReceiptHandle -> MirrorJob -> WorkerM JobOutcome
reevaluateThenMirror receipt job =
    alreadyMirrored job >>= \case
        True -> do
            logFM InfoS (ls ("already present at the mirror target, acking without re-publish: " <> renderJob job))
            pure Succeeded
        False ->
            reevaluatePolicy job >>= \case
                ReevalAdmit -> mirrorArtifact receipt job
                ReevalDrop reason -> pure (Dropped reason)
                ReevalRetry reason -> pure (Retried reason)

{- Ask the mirror target whether the job's version is already present, through the
publish-side registry handle's read fields. __Positive confirmation only__: 'True' needs
the mirror's own metadata to parse and to list the version; a fetch fault or an
unparseable body (a mirror @404@ for a package not yet mirrored, an auth refusal, an
outage) answers 'False', so the job falls through to the full gated pipeline. A false
'False' costs one redundant download and an idempotent @409@ -- exactly the pre-probe
behaviour -- so the probe can only ever save work, never lose a publish or admit one
unvetted. The fetch reports its failures as 'Ecluse.Core.Registry.FetchFault' values,
so the fall-through is a total match, nothing caught. -}
alreadyMirrored :: MirrorJob -> WorkerM Bool
alreadyMirrored job = do
    client <- asks wrRegistry
    probed <- liftIO (fetchMetadata client (jobPackage job))
    pure $ case probed of
        Left _ -> False
        Right response -> case parseVersionList client response of
            Left _ -> False
            Right versions -> jobVersion job `elem` versions

{- Re-run current policy for the job's single version through the shared admission
gate ('Ecluse.Core.Package.Admission.admitArtifact' -- rules, the job's filename,
the integrity floor), after re-checking the job's fetch URL against the mount's
tarball-host gate (the queue payload is a trust boundary). A job for an ecosystem
with no configured bundle is fail-closed (dropped) rather than mirrored unvetted.

The outcomes mirror the serve path's degrade: a withdrawn/absent version (or a
filename its current metadata no longer carries) is a non-retryable drop,
unobtainable metadata a transient retry; a rule block, deny-by-default, refused host,
or integrity-policy refusal drops, and an uncomputable rule retries rather than
dropping a serviceable job or publishing it unvetted. -}
reevaluatePolicy :: MirrorJob -> WorkerM ReevalOutcome
reevaluatePolicy job = do
    policies <- asks wrPolicies
    case Map.lookup ecosystem policies of
        Nothing ->
            pure (ReevalDrop ("no rule policy is configured for the " <> ecosystemName ecosystem <> " ecosystem; refusing to mirror " <> renderJob job))
        Just policy
            | not (wpArtifactHostHonoured policy (hostAddress (registryUrlText (jobArtifactUrl job)))) ->
                pure (ReevalDrop ("the tarball-host policy refuses the artifact host of " <> renderJob job <> " (" <> registryUrlText (jobArtifactUrl job) <> "); refusing to fetch or mirror it"))
            | otherwise -> do
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
                                    (maFilename (jobArtifact job))
                                    details
                                )
                        pure (outcomeOfAdmission job admission)
  where
    ecosystem = pkgEcosystem (jobPackage job)

-- The worker's projection of the shared 'ArtifactAdmission' (the serve gate renders
-- the same verdicts as HTTP statuses): an admit mirrors; every deliberate refusal
-- drops (never frozen into the rule-exempt mirror store); an undecidable verdict
-- retries, so a transient advisory-source outage neither drops a serviceable job nor
-- publishes it unvetted. Total over 'ArtifactAdmission', so a new admission outcome
-- cannot be silently ignored here while the serve path handles it.
outcomeOfAdmission :: MirrorJob -> ArtifactAdmission -> ReevalOutcome
outcomeOfAdmission job = \case
    AdmissionAdmit _ -> ReevalAdmit
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

-- Fetch the artifact bytes, verify them against the job's serve-time-admitted integrity
-- digest, and (only on a match) publish to the mirror target. Reached only on a current
-- policy admit. The integrity gate is the security crux: a tampered or corrupt artifact
-- must never reach the private upstream, which is served without the rules, so a mismatch
-- fails the job with no publish and alarms.
mirrorArtifact :: ReceiptHandle -> MirrorJob -> WorkerM JobOutcome
mirrorArtifact receipt job = do
    logFM DebugS (ls ("fetching artifact bytes from " <> registryUrlText (jobArtifactUrl job)))
    fetched <- fetchArtifactBytes (jobArtifactUrl job)
    case fetched of
        Left reason -> pure (Retried reason)
        Right bytes ->
            case verifyIntegrity (maHashes artifact) bytes of
                IntegrityMismatch detail -> do
                    logFM ErrorS (ls ("artifact integrity mismatch, refusing to publish: " <> detail))
                    pure (Dropped ("integrity mismatch: " <> detail))
                IntegrityVerified -> publishVerified receipt job bytes
  where
    artifact = jobArtifact job

-- Publish already-verified bytes to the mirror target: hold the message past the
-- visibility window (a large-artifact publish may run long), publish through the
-- composition-root publish client (which assembles the ecosystem-specific document),
-- and classify the registry outcome into a 'JobOutcome'.
publishVerified :: ReceiptHandle -> MirrorJob -> ByteString -> WorkerM JobOutcome
publishVerified receipt job bytes = do
    holdForLongPublish receipt
    client <- asks wrRegistry
    metrics <- asks wrMetrics
    -- The publish is the long, network-bound step; time it for the publish-latency
    -- histogram whichever way the registry responds.
    (result, seconds) <- timedSeconds (liftIO (publishArtifact client (jobPackage job) (jobVersion job) artifact bytes))
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
        Left (PublishTransport detail) -> do
            -- Transient: the write never reached the registry (a connection failure, a
            -- timeout), so release the hold and let the un-acked message redeliver,
            -- exactly as a registry rejection.
            releaseForRetry receipt
            pure (Retried ("publish transport failure: " <> detail))
        Left (PublishUrlUnformable urlErr) ->
            -- Non-retryable: 'processMessage' acks this to retire it, so there is no
            -- redelivery to hasten -- leave the hold be.
            pure (Dropped ("unformable publish URL: " <> show urlErr))
  where
    artifact = jobArtifact job

-- Hold a received message past the visibility window before a publish that may run
-- long, so a slow write does not let the message redeliver mid-publish -- which would
-- waste a full re-fetch and re-publish of a (potentially large) artifact. The hold is
-- an optimization (idempotency makes a redelivery harmless), so a failure to extend
-- is swallowed rather than failing the job.
holdForLongPublish :: ReceiptHandle -> WorkerM ()
holdForLongPublish receipt = do
    queue <- asks wrQueue
    _ <- tryAny (liftIO (extendVisibility queue receipt extendBy))
    pass
  where
    -- The window one publish is given before the message could redeliver mid-write.
    -- Sized to comfortably cover a publish of the maximum artifact ('workerArtifactLimits',
    -- 512 MiB): even over a slow mirror-target link (a conservative ~2 MiB/s floor)
    -- that uploads in well under 300s, so a successful publish never redelivers
    -- mid-flight. A *failed* publish does not wait this out -- the failure path resets
    -- the message to visible at once (see 'releaseForRetry') -- so the generous hold
    -- costs nothing on the retry path; this is the background worker's correct trade
    -- (never interrupt a slow success; retry latency on failure does not matter).
    extendBy :: Seconds
    extendBy = Seconds 300

-- Reset a received message to immediately visible, so a failed publish redelivers at
-- once rather than waiting out the long success-path hold ('holdForLongPublish'). A
-- best-effort optimization (a missed reset just means the message redelivers after the
-- hold instead), so a failure to reset is swallowed.
releaseForRetry :: ReceiptHandle -> WorkerM ()
releaseForRetry receipt = do
    queue <- asks wrQueue
    _ <- tryAny (liftIO (extendVisibility queue receipt (Seconds 0)))
    pass

-- A one-line identifier for a job, for log lines.
renderJob :: MirrorJob -> Text
renderJob job = renderPackageName (jobPackage job) <> "@" <> renderVersion (jobVersion job)
