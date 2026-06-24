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

The loop is bracketed, so process shutdown tears it down cleanly; an in-flight,
un-acked message simply redelivers — safe, because publishing is idempotent (a
version already present is success).

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

    -- * Integrity verification
    IntegrityResult (..),
    verifyIntegrity,
) where

import Crypto.Hash (Digest, SHA1, SHA512, hashlazy)
import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Katip (Severity (ErrorS, InfoS, WarningS), katipAddNamespace, logFM, ls)
import Network.HTTP.Client (HttpException, Manager, Request, brRead, responseBody, withResponse)
import UnliftIO (tryAny)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (try)

import Ecluse.App (App, runApp)
import Ecluse.Env (
    Env (envManager, envQueue, envRegistry, envWorkerHeartbeat),
    recordPoll,
 )
import Ecluse.Package (Hash (hashAlg, hashValue), HashAlg (Blake2b, MD5, SHA1, SHA256, SHA512, SRI), renderPackageName)
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
import Ecluse.Security (boundedRead, defaultLimits)
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
processMessage message =
    processJob (msgReceipt message) (msgJob message) >>= \case
        Succeeded -> ackMessage (msgReceipt message)
        AlreadyPresent -> ackMessage (msgReceipt message)
        Dropped reason -> do
            -- A non-retryable fault (a tampered artifact, an unformable URL): the
            -- job can never succeed, so it must not redeliver forever. Ack it to
            -- retire it from the queue, having already alarmed at the fault site.
            logFM ErrorS (ls ("dropping unrecoverable mirror job: " <> reason))
            ackMessage (msgReceipt message)
        Retried reason ->
            -- A transient fault: leave the message un-acked so it redelivers.
            logFM WarningS (ls ("leaving mirror job for redelivery: " <> reason))

ackMessage :: ReceiptHandle -> App ()
ackMessage receipt = do
    queue <- asks envQueue
    liftIO (ack queue receipt)

-- ── per-job processing ──────────────────────────────────────────────────────────

{- | The terminal outcome of processing one mirror job, deciding whether the
message is acked or left to redeliver.
-}
data JobOutcome
    = -- | Published (a fresh write). Ack.
      Succeeded
    | {- | The version was already present at the mirror target (an idempotent
      redelivery — a @409@-equivalent the registry handle treats as success). Ack.
      -}
      AlreadyPresent
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

-- Publish already-verified bytes to the mirror target: hold the message past the
-- visibility window (a large-artifact publish may run long), assemble the npm
-- publish document, publish through the composition-root publish client, and
-- classify the registry outcome into a 'JobOutcome'.
publishVerified :: ReceiptHandle -> MirrorJob -> ByteString -> App JobOutcome
publishVerified receipt job bytes = do
    holdForLongPublish receipt
    client <- asks envRegistry
    let document =
            npmPublishDocument
                (jobPackage job)
                (jobVersion job)
                (maFilename artifact)
                (sriOf artifact)
                (sha1Of artifact)
                bytes
    result <- liftIO (publishArtifact client (jobPackage job) (jobVersion job) document)
    case result of
        Right () -> do
            logFM InfoS (ls ("mirrored artifact published: " <> renderJob job))
            pure Succeeded
        Left (PublishRejected err) ->
            pure (Retried ("registry rejected publish: " <> show err))
        Left (PublishUrlUnformable urlErr) ->
            pure (Dropped ("unformable publish URL: " <> show urlErr))
  where
    artifact = jobArtifact job

-- ── artifact fetch ────────────────────────────────────────────────────────────

{- Fetch the artifact bytes from the public upstream at the job's authoritative
URL, buffering them so they can be verified and attached to the publish document.
A network failure is returned as a transient reason ('Retried' at the call site),
not thrown, so a flaky upstream redelivers rather than killing the iteration.

The read is bounded by 'Ecluse.Security.maxBodyBytes': a publish-by-document needs
the whole tarball in hand to base64-encode it, so the bytes are necessarily held,
but the bound caps that at a configured ceiling — an upstream returning an
unbounded body is refused fail-closed rather than exhausting memory. -}
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
            , npmLimits = defaultLimits
            }

{- Open the artifact request and read its body chunk-by-chunk through the bounded
read, returning the whole bytes when within the response bound or a typed
'ResponseBoundExceeded' otherwise. A network failure throws (caught by the caller
as a transient reason). The bound caps the necessarily-buffered tarball at a
configured ceiling so an unbounded body is refused fail-closed. -}
boundedFetch :: Manager -> Request -> IO (Either ResponseBoundExceeded ByteString)
boundedFetch manager request =
    withResponse request manager $ \response ->
        boundedRead defaultLimits (brRead (responseBody response)) >>= \case
            Right body -> pure (Right body)
            Left limitErr -> pure (Left (ResponseBoundExceeded limitErr))

-- ── visibility helpers ──────────────────────────────────────────────────────────

-- Hold a received message past the visibility window before a publish that may run
-- long, so a slow write does not let the message redeliver mid-publish. The hold is
-- an optimization (idempotency makes a redelivery harmless), so a failure to extend
-- is swallowed rather than failing the job.
holdForLongPublish :: ReceiptHandle -> App ()
holdForLongPublish receipt = do
    queue <- asks envQueue
    _ <- tryAny (liftIO (extendVisibility queue receipt extendBy))
    pass
  where
    -- A generous window relative to the default 30s visibility timeout, so even a
    -- large-artifact publish completes inside one extension.
    extendBy :: Seconds
    extendBy = Seconds 120

-- ── integrity verification ──────────────────────────────────────────────────────

{- | The result of verifying fetched bytes against the admitted integrity digests.
A sum type, not a 'Bool', so the mismatch carries the detail an operator needs to
explain why a publish was refused.
-}
data IntegrityResult
    = -- | The bytes matched at least one admitted digest.
      IntegrityVerified
    | -- | No admitted digest matched. Carries a human-readable detail.
      IntegrityMismatch Text
    deriving stock (Eq, Show)

{- | Verify fetched artifact bytes against the serve-time-admitted integrity
digests. The bytes pass when they match __any__ of the digests (a version may carry
both a modern SRI digest and the legacy SHA-1 shasum); a digest in an algorithm the
worker cannot compute is skipped, not treated as a pass.

This is the tamper gate before a publish: a mismatch must fail the job, never
publish a corrupt or substituted artifact into the private upstream.

>>> import Ecluse.Package (Hash (Hash), HashAlg (SHA1))
>>> verifyIntegrity (Hash SHA1 "0a4d55a8d778e5022fab701977c5d840bbc486d0" :| []) "Hello World"
IntegrityVerified

>>> import Ecluse.Package (Hash (Hash), HashAlg (SHA1))
>>> verifyIntegrity (Hash SHA1 "deadbeef" :| []) "Hello World"
IntegrityMismatch "no admitted digest (SHA1) matched the fetched bytes"
-}
verifyIntegrity :: NonEmpty Hash -> ByteString -> IntegrityResult
verifyIntegrity hashes bytes
    | any matches hashes = IntegrityVerified
    | otherwise =
        IntegrityMismatch
            ( "no admitted digest ("
                <> T.intercalate ", " (map (show . hashAlg) (NE.toList hashes))
                <> ") matched the fetched bytes"
            )
  where
    matches :: Hash -> Bool
    matches h = case computeLike h of
        Nothing -> False
        Just computed -> computed == T.toLower (hashValue h)

    -- Compute the digest of the fetched bytes in the same wire encoding the given
    -- hash uses, so the comparison is like-for-like. 'Nothing' for an algorithm the
    -- worker does not verify (its presence alone never passes the bytes).
    computeLike :: Hash -> Maybe Text
    computeLike h = case hashAlg h of
        SHA1 -> Just (hexLower (hashlazy lazyBytes :: Digest SHA1))
        SRI -> sriDigest (hashValue h)
        SHA256 -> Nothing
        SHA512 -> Nothing
        MD5 -> Nothing
        Blake2b -> Nothing

    lazyBytes = toLazy bytes

    -- A Subresource-Integrity string is @"<alg>-<base64>"@. We support sha512 SRI
    -- (npm's @dist.integrity@): recompute SHA-512 over the bytes, base64-encode it,
    -- and render the same @"sha512-<base64>"@ string for a like-for-like compare.
    sriDigest :: Text -> Maybe Text
    sriDigest sri = case T.breakOn "-" sri of
        ("sha512", _) -> Just (T.toLower ("sha512-" <> base64 (hashlazy lazyBytes :: Digest SHA512)))
        _ -> Nothing

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
