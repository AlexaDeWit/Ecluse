{- | The mirror worker: the supervised consume loop that turns enqueued jobs into
mirrored packages.

The worker is the consumer end of the demand-driven mirror queue (see
"Ecluse.Queue"). 'runWorker' long-polls the queue, and for each received job:

1. fetches the artifact bytes from the public upstream named on the job,
2. __verifies__ those bytes against the integrity digest the job carries — the
   digest the rules admitted at serve time, not a fresh re-fetch,
3. assembles the npm publish document and publishes it to the mirror target
   ('Ecluse.Env.envRegistry', resolved at the composition root with the bearer from
   the "Ecluse.Credential" provider), and
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
transient faults self-recover here. A successful poll advances the
'Ecluse.Env.WorkerHeartbeat', so a stalled loop is visible to the liveness probe.

Shutdown tears the loop down cleanly: the composition root runs it under
@concurrently_@ within the @withEnv@ resource bracket, so process teardown cancels
the loop thread and an in-flight, un-acked message simply redelivers — safe, because
publishing is idempotent (a version already present is success).

== Ack within the visibility budget

A received message is hidden only for the queue's visibility window. The worker
acks on success; before a publish that may run long it
'Ecluse.Queue.extendVisibility' to hold the message before the window lapses; on a
transient failure it does __not__ ack, so the message redelivers. A batch is
processed __sequentially__, so each job has the full visibility budget rather than
competing with its batch-mates for it.

See @docs\/architecture\/cloud-backends.md@ → "Mirror Queue" and "Process model".
-}
module Ecluse.Worker (
    -- * Entry point
    runWorker,

    -- * Loop and job processing (exposed for direct testing)
    workerLoop,
    processBatch,
    processJob,
    JobOutcome (..),

    -- * Liveness
    workerHeartbeatStaleAfter,
    heartbeatHealthy,
    heartbeatHealthyNow,

    -- * Integrity verification
    IntegrityResult (..),
    verifyIntegrity,
) where

import Crypto.Hash (Digest, SHA1, SHA512, hashlazy)
import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import Data.Foldable (maximumBy)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Katip (Severity (ErrorS, InfoS, WarningS), katipAddContext, katipAddNamespace, logFM, ls)
import Network.HTTP.Client (HttpException, Manager, Request, brRead, responseBody, withResponse)
import UnliftIO (tryAny)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (try)

import Ecluse.App (App, runApp)
import Ecluse.Env (
    Env (envDdContext, envManager, envMetrics, envQueue, envRegistry, envTelemetry, envWorkerHeartbeat),
    WorkerHeartbeat,
    lastPoll,
    recordPoll,
 )
import Ecluse.Package (Hash (hashAlg, hashValue), HashAlg (Blake2b, MD5, SHA1, SHA256, SHA512, SRI), renderPackageName)
import Ecluse.Package.Integrity (Strength, assertedAlg, integrityStrength, sriAlgorithm, sriBody, sriPrefix)
import Ecluse.Queue (
    MirrorArtifact (maFilename, maHashes),
    MirrorJob (jobArtifact, jobArtifactUrl, jobPackage, jobVersion),
    MirrorQueue (ack, extendVisibility, receive),
    QueueMessage (msgJob, msgReceipt),
    ReceiptHandle,
    Seconds (Seconds),
 )
import Ecluse.Registry (
    PublishFault (PublishRejected, PublishUrlUnformable),
    RegistryClient (publishArtifact),
 )
import Ecluse.Registry.Npm (
    NpmClientConfig (NpmClientConfig, npmBaseUrl, npmLimits, npmManager, npmToken),
    ResponseBoundExceeded (ResponseBoundExceeded),
    artifactRequestByUrl,
    npmPublishDocument,
 )
import Ecluse.Security (Limits (maxBodyBytes), boundedRead, defaultLimits)
import Ecluse.Telemetry.Correlation (ddPayloadNow)
import Ecluse.Telemetry.Instruments (recordMirrorJobProcessed, recordMirrorPublishDuration, timedSeconds)
import Ecluse.Telemetry.Metrics qualified as Metric
import Ecluse.Telemetry.Tracing (JobSpanOutcome (JobSpanOutcome), withMirrorJobSpan)
import Ecluse.Version (renderVersion)

-- ── entry point ───────────────────────────────────────────────────────────────

{- | Run the supervised mirror worker over the composition-root 'Env': the
consume → fetch → verify → publish → ack loop, in the @App@ orchestration monad.

This is a self-contained service entry over the shared 'Env' (the split-ready
shape the single-process program runs alongside the server). It does not return
under normal operation; its caller brackets it for shutdown.
-}
runWorker :: Env -> IO ()
runWorker env = runApp env (katipAddNamespace "worker" workerLoop)

-- ── the consume loop ──────────────────────────────────────────────────────────

{- | The continuous consume loop: long-poll for a batch, process it, repeat.

Each iteration is wrapped so a single failure — a @receive@ that throws, a fetch or
publish error, an undecodable body — is caught and logged, then the loop backs off
briefly and continues, so one bad iteration cannot kill the worker thread. A
successful poll advances the heartbeat (whether or not the batch was empty), so a
liveness probe sees the loop is alive; an idle queue is a healthy empty poll, not a
stall.
-}
workerLoop :: App ()
workerLoop = forever $ do
    outcome <- tryAny pollAndProcess
    whenLeft_ outcome $ \err -> do
        logFM ErrorS (ls ("worker iteration failed, backing off: " <> displayExceptionT err))
        backoff
  where
    pollAndProcess :: App ()
    pollAndProcess = do
        queue <- asks envQueue
        messages <- liftIO (receive queue)
        -- Heartbeat on every successful poll — an empty long-poll is a healthy idle.
        heartbeat <- asks envWorkerHeartbeat
        now <- liftIO getCurrentTime
        liftIO (recordPoll heartbeat now)
        processBatch messages

-- The fixed pause after a failed iteration, so a persistently failing dependency
-- (queue, upstream) is retried at a bounded rate rather than hot-looping.
backoff :: App ()
backoff = threadDelay 1_000_000

-- ── liveness ──────────────────────────────────────────────────────────────────

{- | How long the worker's last successful poll may be stale before the loop is
considered stalled — the staleness threshold the liveness probe applies.

It is a generous multiple of the long-poll cadence: a healthy idle worker still
completes a poll at least every 'Ecluse.Queue.Sqs.sqsWaitSeconds' (≤ 20s by
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
processBatch :: [QueueMessage] -> App ()
processBatch = traverse_ processMessage

-- Process one message: run the job, and ack on any terminal outcome (success, or a
-- non-retryable drop). A transient failure leaves the message un-acked so the queue
-- redelivers it ("retry is don't ack").
processMessage :: QueueMessage -> App ()
processMessage message = do
    metrics <- asks envMetrics
    outcome <- processJob (msgReceipt message) (msgJob message)
    recordMirrorJobProcessed metrics (jobResultMetric outcome)
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

ackMessage :: ReceiptHandle -> App ()
ackMessage receipt = do
    queue <- asks envQueue
    liftIO (ack queue receipt)

-- ── per-job processing ──────────────────────────────────────────────────────────

{- | The terminal outcome of processing one mirror job, deciding whether the
message is acked or left to redeliver.
-}
data JobOutcome
    = {- | The publish succeeded, so the job is acked. This covers an idempotent
      redelivery too: a version already present at the mirror target is a @409@ the
      registry handle treats as success ('Ecluse.Registry.publishArtifact'), so it
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

{- | Process one mirror job end to end: fetch the artifact, verify it against the
job's serve-time-admitted integrity digest, and — only on a match — publish it to
the mirror target. Returns the 'JobOutcome' that decides ack vs. redeliver.

The receipt handle is taken so a long publish can 'Ecluse.Queue.extendVisibility'
to hold the message before its window lapses. The rules are __not__ re-run: the
job was gated at serve time.
-}
processJob :: ReceiptHandle -> MirrorJob -> App JobOutcome
processJob receipt job = katipAddNamespace "job" $ do
    telemetry <- asks envTelemetry
    withMirrorJobSpan telemetry (jobPackage job) (jobVersion job) jobSpanOutcome $ stampJobDd $ do
        fetched <- fetchArtifactBytes (jobArtifactUrl job)
        case fetched of
            Left reason -> pure (Retried reason)
            Right bytes ->
                case verifyIntegrity (maHashes artifact) bytes of
                    IntegrityMismatch detail -> do
                        -- The security crux: a tampered or corrupt artifact must never
                        -- reach the private upstream, which is served without rules. Fail
                        -- the job with no publish and alarm.
                        logFM ErrorS (ls ("artifact integrity mismatch, refusing to publish: " <> detail))
                        pure (Dropped ("integrity mismatch: " <> detail))
                    IntegrityVerified -> publishVerified receipt job bytes
  where
    artifact = jobArtifact job

    -- Stamp the worker-job span's trace/span ids onto the dd object for this job's log
    -- lines: read inside the span, so a job log correlates to its own span (the
    -- service identity is already on every line via 'runApp'; this tightens the ids to
    -- the active job span). Inert when telemetry is off (no span -> no ids).
    stampJobDd :: App a -> App a
    stampJobDd body = do
        dd <- ddPayloadNow =<< asks envDdContext
        katipAddContext dd body

    -- Project a terminal job outcome onto the worker-job span: the bounded outcome
    -- label always, and the failure detail (which marks the span errored) when the
    -- job did not publish.
    jobSpanOutcome :: JobOutcome -> JobSpanOutcome
    jobSpanOutcome = \case
        Succeeded -> JobSpanOutcome "succeeded" Nothing
        Dropped reason -> JobSpanOutcome "dropped" (Just reason)
        Retried reason -> JobSpanOutcome "retried" (Just reason)

-- Publish already-verified bytes to the mirror target: hold the message past the
-- visibility window (a large-artifact publish may run long), assemble the npm
-- publish document, publish through the composition-root publish client, and
-- classify the registry outcome into a 'JobOutcome'.
publishVerified :: ReceiptHandle -> MirrorJob -> ByteString -> App JobOutcome
publishVerified receipt job bytes = do
    holdForLongPublish receipt
    client <- asks envRegistry
    metrics <- asks envMetrics
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
    recordMirrorPublishDuration metrics seconds
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
fetchArtifactBytes :: Text -> App (Either Text ByteString)
fetchArtifactBytes url = do
    manager <- asks envManager
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
    -- upstream) and uses the guarded data-plane manager, which carries the
    -- resolved-IP SSRF recheck. The base URL is unused for the by-URL request form
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
'Ecluse.Security.defaultLimits' caps bodies at 16 MiB, which is fine for a packument
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
holdForLongPublish :: ReceiptHandle -> App ()
holdForLongPublish receipt = do
    queue <- asks envQueue
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
releaseForRetry :: ReceiptHandle -> App ()
releaseForRetry receipt = do
    queue <- asks envQueue
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
(strongest first: @sha512@ \/ @blake2b@ > @sha256@ > @sha1@ > @md5@), and checks the
bytes against the strongest one present: the bytes pass __iff__ that digest matches.
A weaker digest can neither override nor rescue a failed strong one.

If the strongest digest present is in an algorithm the worker cannot compute, the
gate __fails closed__ rather than falling back to a weaker digest — a tampered
artifact must never be admitted on the strength of a hash an attacker could forge.

This is the tamper gate before a publish: a mismatch fails the job and never
publishes a corrupt or substituted artifact into the private upstream.

>>> import Ecluse.Package (mkHash, HashAlg (SHA1))
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
    -- ranks as 'SHA512'); an SRI whose inner alg is unrecognised is a strong digest the
    -- worker cannot recompute, so it asserts nothing and ranks at the SHA-256 strong
    -- tier (above the legacy SHA-1/MD5). It therefore WINS the 'maximumBy' and the gate
    -- fails closed in 'matchStrongest', rather than downgrading to a weaker computable
    -- digest an attacker who also controls it could forge; it stays below a computable
    -- sha512, so a real sha512, when co-present, is still preferred and verified.
    authority :: Hash -> Strength
    authority = maybe (integrityStrength SHA256) integrityStrength . assertedAlg

    -- Whether the fetched bytes match the chosen digest, compared in that digest's
    -- own wire encoding. A hex digest (SHA-1, hex SHA-512) compares
    -- case-insensitively, since hex is; an SRI's base64 body compares
    -- case-sensitively, since base64 is — folding its case would admit a digest that
    -- matches the bytes only after a case change, silently weakening the gate.
    -- 'Nothing' for an algorithm the worker cannot compute (the fail-closed case
    -- above).
    matchStrongest :: Hash -> Maybe Bool
    matchStrongest h = case hashAlg h of
        SHA1 -> Just (hexLower (hashlazy lazyBytes :: Digest SHA1) == T.toLower (hashValue h))
        SHA512 -> Just (hexLower (hashlazy lazyBytes :: Digest SHA512) == T.toLower (hashValue h))
        SRI -> matchSri (hashValue h)
        SHA256 -> Nothing
        MD5 -> Nothing
        Blake2b -> Nothing

    -- A Subresource-Integrity string is @"<alg>-<base64>"@; only @sha512@ (npm's
    -- @dist.integrity@) is computable here. Recompute SHA-512, base64-encode it, and
    -- compare against the SRI's base64 body __exactly__ — base64 is case-sensitive.
    -- Any other SRI algorithm is uncomputable, so it fails closed rather than passing.
    matchSri :: Text -> Maybe Bool
    matchSri sri = case sriAlgorithm sri of
        Just SHA512 -> Just (base64 (hashlazy lazyBytes :: Digest SHA512) == sriBody sri)
        _ -> Nothing

    describe :: Hash -> Text
    describe h = case hashAlg h of
        SRI -> "SRI " <> sriPrefix (hashValue h)
        alg -> show alg

-- The lower-cased hex encoding of a digest (matching npm's hex shasum form).
hexLower :: Digest a -> Text
hexLower d = T.toLower (decodeUtf8 (convertToBase Base16 d :: ByteString))

-- The standard-base64 encoding of a digest (matching the SRI @<base64>@ body).
base64 :: Digest a -> Text
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
