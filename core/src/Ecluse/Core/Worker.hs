{- | The mirror worker: the supervised consume loop that turns enqueued jobs into
mirrored packages.

The worker is the consumer end of the demand-driven mirror queue (see
"Ecluse.Core.Queue"). The consume loop long-polls the queue, and for each received job:

1. fetches the artifact bytes from the public upstream named on the job,
2. __verifies__ those bytes against the integrity digest the job carries — the
   digest the rules admitted at serve time, not a fresh re-fetch,
3. assembles the npm publish document and publishes it to the mirror target (the
   publish-side registry handle on the 'WorkerRuntime', resolved at the composition
   root with the bearer from the "Ecluse.Core.Credential" provider), and
4. acknowledges the job.

== The integrity gate is the security crux

A mirrored artifact is later served from the private upstream __without re-running
the rules__, so a corrupt or tampered artifact must never enter it. Verification is
therefore the gate: a hash __mismatch fails the job with no publish__ and is logged
loudly. Because the digest is the __serve-time-admitted__ one carried on the job,
the worker mirrors exactly the bytes the rules cleared — an upstream packument
mutated in the enqueue → process window cannot substitute a different artifact.

== Loop robustness and supervision

The loop is wrapped so a single bad iteration cannot kill the worker thread: a
transient @receive@ / fetch / publish error, or an undecodable body, is caught,
logged, and the loop backs off and continues. (Job-level "retry is don't ack" is a
separate concern — it governs whether one message redelivers; it does not protect
the loop, since an escaping exception would still tear the thread down.) The
composition root holds the worker under @concurrently_@ alongside the server, so a
genuinely fatal error propagates and takes the process down (fail-stop), while
transient faults self-recover here. A successful poll advances the 'WorkerHeartbeat',
so a stalled loop is visible to the liveness probe.

Shutdown tears the loop down cleanly: the composition root runs it under
@concurrently_@ within its resource bracket, so process teardown cancels the loop
thread and an in-flight, un-acked message simply redelivers — safe, because
publishing is idempotent (a version already present is success).

== Ack within the visibility budget

A received message is hidden only for the queue's visibility window. The worker
acks on success; before a publish that may run long it
'Ecluse.Core.Queue.extendVisibility' to hold the message before the window lapses; on a
transient failure it does __not__ ack, so the message redelivers. A batch is
processed __sequentially__, so each job has the full visibility budget rather than
competing with its batch-mates for it.

See @docs\/architecture\/cloud-backends.md@ → "Mirror Queue" and "Process model".
-}
module Ecluse.Core.Worker (
    -- * Worker runtime
    WorkerRuntime (..),

    -- * Per-ecosystem ingest re-evaluation
    WorkerPolicy (..),
    WorkerPolicies,

    -- * The worker monad
    WorkerM,
    runWorkerM,

    -- * Loop and job processing (exposed for direct testing)
    workerLoop,
    processBatch,
    processJob,
    JobOutcome (..),

    -- * Liveness
    WorkerHeartbeat,
    newWorkerHeartbeat,
    recordPoll,
    lastPoll,
    workerHeartbeatStaleAfter,
    heartbeatHealthy,
    heartbeatHealthyNow,

    -- * Integrity verification
    IntegrityResult (..),
    verifyIntegrity,
) where

import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import Data.Foldable (maximumBy)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Katip (Katip, KatipContext, LogEnv, Severity (ErrorS, InfoS, WarningS), SimpleLogPayload, katipAddNamespace, logFM, ls)
import Katip.Monadic (KatipContextT, runKatipContextT)
import Network.HTTP.Client (HttpException, Manager, Request, brRead, responseBody, withResponse)
import UnliftIO (MonadUnliftIO, tryAny, withRunInIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (try)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Package (Hash (hashAlg, hashValue), HashAlg (SHA1, SHA256, SRI), PackageName, computeDigest, pkgEcosystem, renderPackageName)
import Ecluse.Core.Package.Integrity (Strength, assertedAlg, integrityStrength, sriBody, sriPrefix)
import Ecluse.Core.Queue (
    MirrorArtifact (maFilename, maHashes),
    MirrorJob (jobArtifact, jobArtifactUrl, jobPackage, jobTraceContext, jobVersion),
    MirrorQueue (ack, extendVisibility, receive),
    QueueMessage (msgJob, msgReceipt),
    ReceiptHandle,
    Seconds (Seconds),
 )
import Ecluse.Core.Registry (
    PublishFault (PublishRejected, PublishUrlUnformable),
    RegistryClient (publishArtifact),
 )
import Ecluse.Core.Registry.Metadata (
    VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent),
 )
import Ecluse.Core.Registry.Npm (
    NpmClientConfig (NpmClientConfig, npmBaseUrl, npmLimits, npmManager, npmToken),
    ResponseBoundExceeded (ResponseBoundExceeded),
    artifactRequestByUrl,
    npmPublishDocument,
 )
import Ecluse.Core.Rules (PreparedRule, evalRules)
import Ecluse.Core.Rules.Types (Decision (Admitted, Blocked, BlockedByDefault, Undecidable), EvalContext (EvalContext))
import Ecluse.Core.Security (Limits (maxBodyBytes), boundedRead, defaultLimits)
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (WorkerMetricsPort (..), timedSeconds)
import Ecluse.Core.Telemetry.Span (JobSpanOutcome (JobSpanOutcome), WorkerTracingPort (..))
import Ecluse.Core.Version (Version, renderVersion)

-- ── worker runtime ────────────────────────────────────────────────────────────

{- | The runtime backends the mirror worker is closed over: exactly the effectful
capabilities the consume loop needs to poll, fetch, verify, publish, and record. A
record of concrete handles and abstract ports (the Handle pattern), assembled by the
composition root ('Ecluse.Env.workerRuntimeOf') and read by the loop through the
'WorkerM' reader.

The mirror queue is the demand-driven hand-off the loop consumes; the publish-side
registry client writes approved artifacts to the mirror target; the untrusted
data-plane manager fetches the artifact bytes (the validating TLS manager, over an
https-only @dist.tarball@); the heartbeat is the loop's liveness surface. The metric and
tracing ports are the abstract recording interfaces ("Ecluse.Core.Telemetry.Record",
"Ecluse.Core.Telemetry.Span"); the application supplies their OpenTelemetry-backed
implementations, so the loop records without naming a telemetry backend. There is no log
field: the loop logs through the ambient @katip@ context the entry point establishes.
-}
data WorkerRuntime = WorkerRuntime
    { wrQueue :: MirrorQueue
    -- ^ The mirror-queue handle the consume loop long-polls and acks against.
    , wrRegistry :: RegistryClient
    {- ^ The publish-side registry handle approved artifacts are written to the mirror
    target through.
    -}
    , wrManager :: Manager
    {- ^ The validating-TLS data-plane manager for the __untrusted__ artifact fetch (over
    an https-only @dist.tarball@).
    -}
    , wrHeartbeat :: WorkerHeartbeat
    {- ^ The consume-loop heartbeat, advanced on every successful poll and read by the
    liveness probe.
    -}
    , wrMetrics :: WorkerMetricsPort
    -- ^ The metric-recording port the worker emits its @ecluse.mirror.*@ job signals through.
    , wrTracing :: WorkerTracingPort
    -- ^ The tracing port the worker opens its per-job span through.
    , wrInjectTraceContext :: forall m a. (KatipContext m, MonadIO m) => m a -> m a
    {- ^ Evaluate and inject the current OpenTelemetry correlation payload into the
    @katip@ context for the inner action.
    -}
    , wrPolicies :: WorkerPolicies
    {- ^ The per-ecosystem re-evaluation bundles, keyed by a job's ecosystem. The worker
    re-runs current policy against a job's version before it mirrors it, so a policy that
    has tightened toward deny since the job was enqueued drops the job rather than freezing
    a now-disallowed version into the trusted mirror store.
    -}
    }

-- ── per-ecosystem ingest re-evaluation ────────────────────────────────────────

{- | The per-ecosystem re-evaluation bundle the worker re-runs current policy through
before it mirrors a job: a resolver that fetches and projects the single version's
metadata, the prepared rule set, and the wall-clock the age rules read.

The resolver is the __shared__ single-version fetch-and-project
('Ecluse.Core.Registry.Metadata.fetchVersionDetails' over the guarded public origin,
wired by the composition root), and the rules are the __same__ prepared rules the serve
path gates with, so the worker's ingest decision and the serve-time decision run one
codepath and any per-source breaker state is shared, never forked.
-}
data WorkerPolicy = WorkerPolicy
    { wpResolveVersion :: PackageName -> Version -> IO VersionEvaluation
    {- ^ Resolve and project one version's metadata through the guarded public origin,
    classifying the outcome ('Ecluse.Core.Registry.Metadata.fetchVersionDetails'). Total:
    a fetch failure is a 'VersionMetadataUnavailable' value, never an escaping exception.
    -}
    , wpRules :: [PreparedRule]
    {- ^ The prepared rule set evaluated against the resolved version under current policy
    (the same rules the serve path gates the public version set with).
    -}
    , wpNow :: IO UTCTime
    {- ^ The wall-clock "now" for the rules' 'EvalContext'; injected so the time-sensitive
    age gate is deterministic under test.
    -}
    }

{- | The worker's per-ecosystem re-evaluation bundles, keyed by the ecosystem a job's
package belongs to ('Ecluse.Core.Package.pkgEcosystem'). Built once at boot and shared
with the serve mounts; a job whose ecosystem is absent here is fail-closed (dropped), never
mirrored unvetted.
-}
type WorkerPolicies = Map Ecosystem WorkerPolicy

-- ── the worker monad ──────────────────────────────────────────────────────────

{- | The mirror worker's monad: a reader over the 'WorkerRuntime' layered on @katip@'s
logging context.

A @newtype@ over @'ReaderT' 'WorkerRuntime' ('KatipContextT' 'IO')@ so its instances are
this module's to control and call sites name one concrete monad. The derived instances
give reader access to the runtime ('MonadReader' 'WorkerRuntime'), arbitrary effects
('MonadIO'), the unlift capability ('MonadUnliftIO') the loop's @tryAny@ and the per-job
span bracket need, and the @katip@ classes ('Katip', 'KatipContext') so a structured log
call composes through the ambient context the entry point establishes.

The @katip@ base is a reader, never a 'StateT', so the logging context behaves correctly
across the loop (see @docs\/architecture\/technology-stack.md@ → "Key Decisions").
-}
newtype WorkerM a = WorkerM
    { unWorkerM :: ReaderT WorkerRuntime (KatipContextT IO) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader WorkerRuntime
        , MonadUnliftIO
        , Katip
        , KatipContext
        )

{- | Run a 'WorkerM' against the 'WorkerRuntime' and the @katip@ logging environment and
initial context the entry point supplies, yielding the underlying 'IO' action. This is
the boundary where the worker's 'WorkerM' code is discharged to 'IO'.

The 'LogEnv' (the structured-log scribes) and the initial context payload are passed in
rather than read from the runtime, so the application owns the log stream and the
trace-correlation @dd@ enrichment: it resolves the @dd@ identity and hands it here as the
initial context, so every line the loop emits carries @dd@. The loop narrows the
namespace with @katip@'s combinators on top as it logs.
-}
runWorkerM :: LogEnv -> SimpleLogPayload -> WorkerRuntime -> WorkerM a -> IO a
runWorkerM logEnv initialContext runtime action =
    runKatipContextT logEnv initialContext mempty (runReaderT (unWorkerM action) runtime)

-- ── the consume loop ──────────────────────────────────────────────────────────

{- | The continuous consume loop: long-poll for a batch, process it, repeat.

Each iteration is wrapped so a single failure — a @receive@ that throws, a fetch or
publish error, an undecodable body — is caught and logged, then the loop backs off
briefly and continues, so one bad iteration cannot kill the worker thread. A
successful poll advances the heartbeat (whether or not the batch was empty), so a
liveness probe sees the loop is alive; an idle queue is a healthy empty poll, not a
stall.
-}
workerLoop :: WorkerM ()
workerLoop = forever $ do
    outcome <- tryAny pollAndProcess
    whenLeft_ outcome $ \err -> do
        logFM ErrorS (ls ("worker iteration failed, backing off: " <> displayExceptionT err))
        backoff
  where
    pollAndProcess :: WorkerM ()
    pollAndProcess = do
        queue <- asks wrQueue
        messages <- liftIO (receive queue)
        -- Heartbeat on every successful poll — an empty long-poll is a healthy idle.
        heartbeat <- asks wrHeartbeat
        now <- liftIO getCurrentTime
        liftIO (recordPoll heartbeat now)
        processBatch messages

-- The fixed pause after a failed iteration, so a persistently failing dependency
-- (queue, upstream) is retried at a bounded rate rather than hot-looping.
backoff :: WorkerM ()
backoff = threadDelay 1_000_000

-- ── liveness ──────────────────────────────────────────────────────────────────

{- | The mirror worker's consume-loop heartbeat: the wall-clock time of the
worker's __last successful poll__ of the queue.

It is the worker's own liveness signal, kept apart from the server's HTTP
readiness so single-process health reflects a stalled worker today and a future
standalone worker binary keeps the same probe. The worker 'recordPoll's after each
successful @receive@ (whether or not the batch was empty — an empty long-poll is a
healthy idle, not a stall); a liveness probe reads 'lastPoll' and compares it
against the wall clock to decide whether the loop has gone quiet for too long.
-}
newtype WorkerHeartbeat = WorkerHeartbeat (TVar (Maybe UTCTime))

{- | Build a fresh 'WorkerHeartbeat' with no poll yet recorded ('lastPoll' is
'Nothing' until the worker's first successful @receive@).
-}
newWorkerHeartbeat :: IO WorkerHeartbeat
newWorkerHeartbeat = WorkerHeartbeat <$> newTVarIO Nothing

{- | Record the time of a successful queue poll, advancing the heartbeat. Called
by the worker after each @receive@ returns (the loop is alive even on an empty
batch).
-}
recordPoll :: WorkerHeartbeat -> UTCTime -> IO ()
recordPoll (WorkerHeartbeat var) now = atomically (writeTVar var (Just now))

{- | The time of the worker's last successful poll, or 'Nothing' before its first.
A liveness probe reads this and compares it against the wall clock.
-}
lastPoll :: WorkerHeartbeat -> IO (Maybe UTCTime)
lastPoll (WorkerHeartbeat var) = readTVarIO var

{- | How long the worker's last successful poll may be stale before the loop is
considered stalled — the staleness threshold the liveness probe applies.

It is a generous multiple of the long-poll cadence: a healthy idle worker still
completes a poll at least every 'Ecluse.Core.Queue.Sqs.sqsWaitSeconds' (≤ 20s by
default), so a gap several times that is a genuine stall, not an idle queue. Set
well above one poll window so liveness never flaps on normal scheduling jitter.
-}
workerHeartbeatStaleAfter :: NominalDiffTime
workerHeartbeatStaleAfter = 120

{- | Whether the worker's consume loop is healthy as of @now@, given its last
successful poll. This is the liveness signal the single-process @\/livez@ probe
folds in (see "Ecluse.Server"), distinct from HTTP readiness.

* 'Nothing' (no poll yet) is __healthy__: the worker is still starting, not stalled.
* A poll within 'workerHeartbeatStaleAfter' is healthy.
* A poll older than that is __unhealthy__: the loop has gone quiet for too long.

>>> import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
>>> let t0 = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)
>>> heartbeatHealthy t0 Nothing
True

>>> let now = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 10)
>>> heartbeatHealthy now (Just t0)
True

>>> let later = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 300)
>>> heartbeatHealthy later (Just t0)
False
-}
heartbeatHealthy :: UTCTime -> Maybe UTCTime -> Bool
heartbeatHealthy _ Nothing = True
heartbeatHealthy now (Just polledAt) = diffUTCTime now polledAt <= workerHeartbeatStaleAfter

{- | Read the worker heartbeat and decide liveness against the current wall clock —
the @IO@ wrapper the liveness probe calls. 'True' while the consume loop is alive
(or still starting); 'False' once the last successful poll is staler than
'workerHeartbeatStaleAfter'.
-}
heartbeatHealthyNow :: WorkerHeartbeat -> IO Bool
heartbeatHealthyNow heartbeat = heartbeatHealthy <$> getCurrentTime <*> lastPoll heartbeat

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
            -- backend-dependent — a durable queue redelivers it once the visibility
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

-- ── per-job processing ──────────────────────────────────────────────────────────

{- | The terminal outcome of processing one mirror job, deciding whether the
message is acked or left to redeliver.
-}
data JobOutcome
    = {- | The publish succeeded, so the job is acked. This covers an idempotent
      redelivery too: a version already present at the mirror target is a @409@ the
      registry handle treats as success ('Ecluse.Core.Registry.publishArtifact'), so it
      surfaces here as 'Succeeded' rather than a distinct case.
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

{- | Process one mirror job end to end: __re-evaluate current policy__ for the job's
version, and only on a current admit fetch the artifact, verify it against the job's
serve-time-admitted integrity digest, and publish it to the mirror target. Returns the
'JobOutcome' that decides ack vs. redeliver.

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

The per-job domain span (the worker tracing port) wraps the whole re-evaluate → fetch →
verify → publish, projecting the terminal outcome onto the span so a refused or dropped job
is explainable from the trace, and __linking__ back to the request that enqueued the job
through the trace context the job carries ('jobTraceContext'). The span body is discharged
to 'IO' through the unlift, so the loop's structured log lines still compose through the
ambient @katip@ context.
-}
processJob :: ReceiptHandle -> MirrorJob -> WorkerM JobOutcome
processJob receipt job = katipAddNamespace "job" $ do
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

-- ── ingest-time policy re-evaluation ──────────────────────────────────────────

-- The terminal decision of re-evaluating current policy for a job, before any artifact
-- fetch: admit (mirror it), drop (a current deny or a withdrawn version, acked and never
-- published), or retry (metadata unobtainable, or a rule uncomputable, left for redelivery).
data ReevalOutcome
    = ReevalAdmit
    | ReevalDrop Text
    | ReevalRetry Text

-- Re-evaluate current policy for the job's version, then mirror it on a current admit. The
-- gate runs before the (potentially large) artifact fetch, so a now-denied job is dropped
-- without downloading its bytes.
reevaluateThenMirror :: ReceiptHandle -> MirrorJob -> WorkerM JobOutcome
reevaluateThenMirror receipt job =
    reevaluatePolicy job >>= \case
        ReevalAdmit -> mirrorArtifact receipt job
        ReevalDrop reason -> pure (Dropped reason)
        ReevalRetry reason -> pure (Retried reason)

{- Re-run current policy for the job's single version: look up the job's ecosystem bundle,
resolve and project the version's metadata through the shared single-version fetch, and
evaluate the prepared rules over it. A job for an ecosystem with no configured bundle is
fail-closed (dropped) rather than mirrored unvetted. The outcomes mirror the serve path's
degrade: a withdrawn/absent version is a non-retryable drop, unobtainable metadata a
transient retry; a rule block (or deny-by-default) drops, and an uncomputable rule retries
rather than dropping a serviceable job or publishing it unvetted. -}
reevaluatePolicy :: MirrorJob -> WorkerM ReevalOutcome
reevaluatePolicy job = do
    policies <- asks wrPolicies
    case Map.lookup ecosystem policies of
        Nothing ->
            pure (ReevalDrop ("no rule policy is configured for the " <> ecosystemName ecosystem <> " ecosystem; refusing to mirror " <> renderJob job))
        Just policy -> do
            evaluation <- liftIO (wpResolveVersion policy (jobPackage job) (jobVersion job))
            case evaluation of
                VersionMetadataUnavailable ->
                    pure (ReevalRetry ("could not re-fetch metadata to re-evaluate current policy for " <> renderJob job))
                VersionMissing ->
                    pure (ReevalDrop ("the public upstream no longer offers " <> renderJob job <> "; refusing to mirror a withdrawn version"))
                VersionPresent details -> do
                    ctx <- liftIO (EvalContext <$> wpNow policy)
                    decision <- liftIO (evalRules ctx (wpRules policy) details)
                    pure (outcomeOfDecision job decision)
  where
    ecosystem = pkgEcosystem (jobPackage job)

-- Map a re-evaluation 'Decision' to a job outcome. An admit mirrors; a rule block or
-- deny-by-default drops (current policy denies the version, so it must not be frozen into
-- the trusted mirror store); an undecidable verdict (a fail-closed rule that could not be
-- computed) retries, so a transient advisory-source outage neither drops a serviceable job
-- nor publishes it unvetted (the serve path renders the same cause a transient 503).
outcomeOfDecision :: MirrorJob -> Decision -> ReevalOutcome
outcomeOfDecision job = \case
    Admitted{} -> ReevalAdmit
    Blocked ruleName reason ->
        ReevalDrop ("current policy denies " <> renderJob job <> ": blocked by " <> ruleName <> " (" <> reason <> ")")
    BlockedByDefault _ ->
        ReevalDrop ("current policy denies " <> renderJob job <> ": no rule admits it")
    Undecidable _ reason ->
        ReevalRetry ("current policy could not be evaluated for " <> renderJob job <> ": " <> reason)

-- Fetch the artifact bytes, verify them against the job's serve-time-admitted integrity
-- digest, and (only on a match) publish to the mirror target. Reached only on a current
-- policy admit. The integrity gate is the security crux: a tampered or corrupt artifact
-- must never reach the private upstream, which is served without the rules, so a mismatch
-- fails the job with no publish and alarms.
mirrorArtifact :: ReceiptHandle -> MirrorJob -> WorkerM JobOutcome
mirrorArtifact receipt job = do
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
-- visibility window (a large-artifact publish may run long), assemble the npm
-- publish document, publish through the composition-root publish client, and
-- classify the registry outcome into a 'JobOutcome'.
publishVerified :: ReceiptHandle -> MirrorJob -> ByteString -> WorkerM JobOutcome
publishVerified receipt job bytes = do
    holdForLongPublish receipt
    client <- asks wrRegistry
    metrics <- asks wrMetrics
    let document =
            npmPublishDocument
                (jobPackage job)
                (jobVersion job)
                (maFilename artifact)
                (sriOf artifact)
                (sha1Of artifact)
                bytes
    -- The publish is the long, network-bound step; time it for the publish-latency
    -- histogram whichever way the registry responds.
    (result, seconds) <- timedSeconds (liftIO (publishArtifact client (jobPackage job) (jobVersion job) document))
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
        Left (PublishUrlUnformable urlErr) ->
            -- Non-retryable: 'processMessage' acks this to retire it, so there is no
            -- redelivery to hasten — leave the hold be.
            pure (Dropped ("unformable publish URL: " <> show urlErr))
  where
    artifact = jobArtifact job

-- ── artifact fetch ────────────────────────────────────────────────────────────

{- Fetch the artifact bytes from the public upstream at the job's authoritative
URL into memory. Publishing is __publish-by-document__: the npm @PUT \/{pkg}@ carries
the tarball base64-encoded under @_attachments@, so the whole artifact must be in
hand to verify it and assemble the document. This path is therefore
__bounded-buffered__, not streamed — the bytes are necessarily held — but the read
is capped (see 'workerArtifactLimits'), so an upstream returning an unbounded body
is refused fail-closed rather than exhausting memory. A network failure is returned
as a transient reason ('Retried' at the call site), not thrown, so a flaky upstream
redelivers rather than killing the iteration. -}
fetchArtifactBytes :: Text -> WorkerM (Either Text ByteString)
fetchArtifactBytes url = do
    manager <- asks wrManager
    case artifactRequestByUrl (fetchConfig manager) url of
        Left urlErr -> pure (Left ("unformable artifact URL: " <> show urlErr))
        Right request ->
            try (liftIO (boundedFetch manager request)) <&> \case
                Left (e :: HttpException) -> Left ("artifact fetch failed: " <> show e)
                Right (Left (ResponseBoundExceeded limitErr)) ->
                    Left ("artifact exceeded the response bound: " <> show limitErr)
                Right (Right bytes) -> Right bytes
  where
    -- The public artifact fetch is anonymous (the client credential is never sent
    -- upstream) and uses the untrusted data-plane manager, the validating TLS manager
    -- over an https-only dist.tarball. The base URL is unused for the by-URL request form
    -- (the URL is absolute); the manager and anonymous posture are what matter.
    fetchConfig :: Manager -> NpmClientConfig
    fetchConfig manager =
        NpmClientConfig
            { npmBaseUrl = url
            , npmManager = manager
            , npmToken = Nothing
            , npmLimits = workerArtifactLimits
            }

{- Open the artifact request and read its body chunk-by-chunk through the bounded
read, returning the whole bytes when within the artifact cap or a typed
'ResponseBoundExceeded' otherwise. A network failure throws (caught by the caller
as a transient reason). The cap bounds the necessarily-buffered tarball so an
unbounded body is refused fail-closed. -}
boundedFetch :: Manager -> Request -> IO (Either ResponseBoundExceeded ByteString)
boundedFetch manager request =
    withResponse request manager $ \response ->
        boundedRead workerArtifactLimits (brRead (responseBody response)) >>= \case
            Right body -> pure (Right body)
            Left limitErr -> pure (Left (ResponseBoundExceeded limitErr))

{- The response-bound budget for an __artifact__ fetch. The metadata-path
'Ecluse.Core.Security.defaultLimits' caps bodies at 12 MiB, which is fine for a packument
but far too small for a real tarball, so the artifact cap is raised to a realistic
ceiling while the other limits (version count, nesting depth) stay at their defaults
(they do not apply to an opaque tarball). A body past this is refused fail-closed
rather than buffered, bounding the worker's memory per in-flight job. -}
workerArtifactLimits :: Limits
workerArtifactLimits = defaultLimits{maxBodyBytes = 512 * 1024 * 1024}

-- ── visibility helpers ──────────────────────────────────────────────────────────

-- Hold a received message past the visibility window before a publish that may run
-- long, so a slow write does not let the message redeliver mid-publish — which would
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
    -- mid-flight. A *failed* publish does not wait this out — the failure path resets
    -- the message to visible at once (see 'releaseForRetry') — so the generous hold
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

-- ── integrity verification ──────────────────────────────────────────────────────

{- | The result of verifying fetched bytes against the admitted integrity digests.
A sum type, not a 'Bool', so the mismatch carries the detail an operator needs to
explain why a publish was refused.
-}
data IntegrityResult
    = -- | The bytes matched the most authoritative admitted digest.
      IntegrityVerified
    | {- | The bytes failed the integrity gate. Carries a human-readable detail (the
      digest they were checked against, or that the strongest one was uncomputable).
      -}
      IntegrityMismatch Text
    deriving stock (Eq, Show)

{- | Verify fetched artifact bytes against the __most authoritative__ integrity
digest the version carries — never against a weaker one while a stronger is present.

A real npm version carries both a modern SRI @sha512@ digest and the legacy SHA-1
@shasum@. Passing on /any/ match would let an artifact that matches the weak SHA-1
but fails the strong @sha512@ through — and SHA-1 collision resistance is broken, so
that is exploitable. So the gate ranks the admitted digests by algorithm authority
(strongest first: @sha512@ \/ @blake2b@ > @sha384@ > @sha256@ > @sha1@ > @md5@), and
checks the bytes against the strongest one present: the bytes pass __iff__ that digest
matches.
A weaker digest can neither override nor rescue a failed strong one.

The bytes are recomputed in the strongest digest's own algorithm through the shared
'Ecluse.Core.Package.computeDigest', the one definition of which algorithms Écluse can
verify. That computable set covers every algorithm the public integrity floor admits, so an
admitted artifact is always verifiable here. If the strongest digest is nonetheless in an
algorithm 'computeDigest' declines (MD5, a forgeable hash) or an SRI whose inner algorithm
does not resolve, the gate __fails closed__ rather than falling back to a weaker digest: a
tampered artifact must never be admitted on the strength of a hash an attacker could forge.

This is the tamper gate before a publish: a mismatch fails the job and never
publishes a corrupt or substituted artifact into the private upstream.

>>> import Ecluse.Core.Package (mkHash, HashAlg (SHA1))
>>> fmap (\h -> verifyIntegrity (h :| []) "Hello World") (mkHash SHA1 "0a4d55a8d778e5022fab701977c5d840bbc486d0")
Right IntegrityVerified

>>> fmap (\h -> verifyIntegrity (h :| []) "Hello World") (mkHash SHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709")
Right (IntegrityMismatch "the SHA1 digest did not match the fetched bytes")
-}
verifyIntegrity :: NonEmpty Hash -> ByteString -> IntegrityResult
verifyIntegrity hashes bytes =
    let strongest = maximumBy (comparing authority) hashes
     in case matchStrongest strongest of
            Nothing ->
                -- Fail closed: the strongest present digest is in an algorithm we
                -- cannot recompute, so we cannot prove the bytes — never drop to a
                -- weaker digest an attacker could forge.
                IntegrityMismatch
                    ( "the strongest admitted digest ("
                        <> describe strongest
                        <> ") is in an algorithm the worker cannot verify"
                    )
            Just True -> IntegrityVerified
            Just False ->
                IntegrityMismatch ("the " <> describe strongest <> " digest did not match the fetched bytes")
  where
    lazyBytes = toLazy bytes

    -- Algorithm authority, strongest first, so 'maximumBy' selects the digest a match
    -- must be proven against. It reuses the shared 'integrityStrength' ranking so the
    -- tamper gate and the serve-admission floor agree on which algorithms are strong.
    -- An SRI is ranked by the algorithm it asserts ('assertedAlg' — npm's @sha512-…@
    -- ranks as 'SHA512'); an SRI whose inner alg is unrecognised asserts nothing and ranks
    -- at the SHA-256 floor tier (above the legacy SHA-1/MD5). It therefore WINS the
    -- 'maximumBy' and, unresolvable, the gate fails closed in 'matchStrongest' rather than
    -- downgrading to a weaker computable digest an attacker who also controls it could
    -- forge; it stays below a computable sha512, so a real sha512, when co-present, is
    -- still preferred and verified.
    authority :: Hash -> Strength
    authority = maybe (integrityStrength SHA256) integrityStrength . assertedAlg

    -- Whether the fetched bytes match the chosen digest: resolve its algorithm
    -- ('assertedAlg', 'Nothing' for an unresolvable SRI), recompute the bytes in that
    -- algorithm ('computeDigest', 'Nothing' for one the worker will not verify against),
    -- and compare in the digest's own wire encoding. A hex tag compares case-insensitively
    -- (hex is); an SRI's base64 body compares case-sensitively (base64 is; folding its case
    -- would admit a digest that matches the bytes only after a case change). Either 'Nothing'
    -- is the fail-closed case above.
    matchStrongest :: Hash -> Maybe Bool
    matchStrongest h = do
        alg <- assertedAlg h
        digestOf <- computeDigest alg
        let digest = digestOf lazyBytes
        pure $ case hashAlg h of
            SRI -> base64 digest == sriBody (hashValue h)
            _ -> hexLower digest == T.toLower (hashValue h)

    describe :: Hash -> Text
    describe h = case hashAlg h of
        SRI -> "SRI " <> sriPrefix (hashValue h)
        alg -> show alg

-- The lower-cased hex encoding of raw digest bytes (matching npm's hex shasum form).
hexLower :: ByteString -> Text
hexLower d = T.toLower (decodeUtf8 (convertToBase Base16 d :: ByteString))

-- The standard-base64 encoding of raw digest bytes (matching the SRI @<base64>@ body).
base64 :: ByteString -> Text
base64 d = decodeUtf8 (convertToBase Base64 d :: ByteString)

-- ── small helpers ───────────────────────────────────────────────────────────────

-- A one-line identifier for a job, for log lines.
renderJob :: MirrorJob -> Text
renderJob job = renderPackageName (jobPackage job) <> "@" <> renderVersion (jobVersion job)

-- Pick the SRI (@dist.integrity@) string from the admitted digests, if present.
sriOf :: MirrorArtifact -> Maybe Text
sriOf = firstHashValue SRI

-- Pick the SHA-1 shasum from the admitted digests, if present.
sha1Of :: MirrorArtifact -> Maybe Text
sha1Of = firstHashValue SHA1

firstHashValue :: HashAlg -> MirrorArtifact -> Maybe Text
firstHashValue alg artifact =
    fmap hashValue (find ((== alg) . hashAlg) (NE.toList (maHashes artifact)))

-- Render an exception as 'Text' for a log line (relude's 'displayException' is over
-- 'String').
displayExceptionT :: (Exception e) => e -> Text
displayExceptionT = toText . displayException
