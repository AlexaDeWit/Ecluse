-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror-queue handle: the durable hand-off from the request path to the
mirror worker.

Mirroring is __demand-driven__: when a client fetches an artifact whose version
passes the rules, the proxy 'enqueue's a 'MirrorJob' and serves the artifact
immediately, never blocking on the mirror. A separate worker 'receive's jobs,
fetches and verifies the artifact, publishes it to the mirror target, and 'ack's
the job (see @docs\/architecture\/cloud-backends.md@ → "Mirror Queue").

The queue is the one cloud surface with materially different APIs per provider
(AWS SQS @SendMessage@\/@ReceiveMessage@+visibility-timeout\/@DeleteMessage@; GCP
Pub\/Sub @Publish@\/@Pull@+ack-deadline\/@Acknowledge@), so it is its own handle --
a __record of functions__ (the Handle pattern). Both providers fit the same
receive → process → ack shape; their differences (visibility timeout vs ack
deadline, batch limits, dead-letter wiring) stay behind the handle, and
'ReceiptHandle' is opaque so neither leaks.

Like the other handles, the effectful fields return __'IO', not @App@__, so an
adapter stays decoupled from the proxy's @Env@\/@App@ (see
@docs\/architecture\/technology-stack.md@ → "Key Decisions").

== Conventions

The two cloud backends both give __at-least-once delivery__, which is safe here
because publishing is idempotent (a registry treats versions as immutable). The
handle's contract reflects that:

* __'enqueue' is best-effort.__ It runs on the request hot path (enqueue, then
  serve immediately), so a failure must be logged\/metered and __never fail the
  client response__ -- the artifact is already served, and a later pull
  re-enqueues.
* __Retry is "don't 'ack'".__ A job that fails processing is simply not acked;
  the visibility timeout \/ ack deadline redelivers it, and the backend's native
  dead-letter path catches the persistently failing ones. There is deliberately
  __no @nack@__.
* __'extendVisibility'__ lets the worker hold a long publish (a large artifact)
  past the visibility window. It is an /optimization/, not correctness-critical,
  since idempotency already makes redelivery harmless.

This module provides the handle, its payload types, and the building blocks a
backend implementation reaches for; the two STM-backed in-memory implementations
(the visibility-timeout __test double__ and the bounded, best-effort __production
backend__ behind @ECLUSE_QUEUE_BACKEND=memory@) live in "Ecluse.Core.Queue.Memory".

It also provides 'newEnqueueBuffer', a __bounded producer-side hand-off buffer__
wrapped in front of any backend so the serve path's 'enqueue' completes in
microseconds while a composition-root drain loop delivers to the (possibly slow)
backend off the request path.
-}
module Ecluse.Core.Queue (
    -- * Queue handle
    MirrorQueue (..),

    -- * Faults
    QueueFault (..),
    queueFault,
    queueTransportFault,

    -- * Payloads
    MirrorJob (..),
    RemoteSpanContext (..),
    QueueMessage (..),

    -- * Opaque receipt
    ReceiptHandle,
    mkReceiptHandle,
    unReceiptHandle,

    -- * Durations
    Seconds (..),

    -- * Backend building blocks
    writeOrDrop,
    reportWorthy,

    -- * Buffered producer hand-off
    newEnqueueBuffer,
) where

import Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Fault (TransportCause, TransportFault (TransportFault), transportFault)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Supervision (BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros), backoffMicros)
import Ecluse.Core.Version (Version)

{- | A mirror job: everything the worker needs to back-fill one artifact into the
mirror target. The version was gated by the rules at serve time (when the job was
enqueued); the worker __re-evaluates current policy__ through the same shared
admission oracle before mirroring (see "Ecluse.Core.Worker.Job"), then fetches the
bytes, verifies them against the digests of the artifact that re-evaluation
re-admitted, and publishes.

The queue payload is a trust boundary, so it carries __selection keys, never
authority__: the filename ('jobArtifactFilename') names the artifact the worker's
ingest re-evaluation selects and gates under current policy, and the payload
carries no digest or size at all -- the descriptor the tamper gate and the publish
document consume ('Ecluse.Core.Registry.MirrorArtifact') is derived entirely from
the artifact that re-evaluation re-admits.
-}
data MirrorJob = MirrorJob
    { jobPackage :: PackageName
    -- ^ The package whose artifact is being mirrored.
    , jobVersion :: Version
    -- ^ The specific version to mirror.
    , jobArtifactUrl :: RegistryUrl
    {- ^ Where to fetch the artifact bytes from (the public upstream), carried as
    the validated https egress witness rather than bare text; the SQS wire decode
    re-forms it, since the queue payload is a trust boundary.
    -}
    , jobArtifactFilename :: Text
    {- ^ The serve-time-admitted artifact's filename: the selection key the
    worker's ingest re-evaluation gates by, cross-checked against current metadata
    by the shared admission gate rather than trusted.
    -}
    , jobTraceContext :: Maybe RemoteSpanContext
    {- ^ The trace context of the serve-time span that enqueued the job, captured
    at enqueue time so the worker's per-job span can __link__ back to the request
    that produced the work across the asynchronous hop. 'Nothing' when tracing was
    off at enqueue time (or for a job from a producer that carried none). The queue
    treats it as opaque transport; only the tracing port reads it.
    -}
    }
    deriving stock (Eq, Show)

{- | A serialised W3C trace-context carrier riding on a 'MirrorJob': the
@traceparent@ (and any @tracestate@) of the span that enqueued the job, in the
standard wire encoding. It is captured at enqueue time and read back by the worker's
tracing port to re-establish a span __link__ from the per-job span to the enqueueing
request, so the asynchronous mirror hand-off is navigable in a trace.

The two fields are the W3C header values verbatim; the queue carries them opaquely
(it neither parses nor validates them -- an unparseable carrier simply yields no link),
so this type names what is carried without coupling the queue to any tracing backend.
-}
data RemoteSpanContext = RemoteSpanContext
    { rscTraceparent :: Text
    -- ^ The W3C @traceparent@ header value of the enqueueing span.
    , rscTracestate :: Text
    {- ^ The W3C @tracestate@ header value (possibly empty) carried alongside, so
    vendor trace state survives the hop.
    -}
    }
    deriving stock (Eq, Show)

{- | An __opaque__ handle identifying a received message for 'ack' \/
'extendVisibility'. It carries the backend's own delivery token -- an SQS receipt
handle or a Pub\/Sub @ackId@ -- as text; the constructor is hidden so neither
provider's representation leaks into worker code, and a handle is only ever
obtained from a 'QueueMessage' returned by 'receive'. Build one (in a backend)
with 'mkReceiptHandle' and read the token back with 'unReceiptHandle'.
-}
newtype ReceiptHandle = ReceiptHandle Text
    deriving stock (Eq, Ord, Show)

{- | Wrap a backend's delivery token (an SQS receipt handle, a Pub\/Sub @ackId@)
as an opaque 'ReceiptHandle'. For backend implementations only -- worker code
obtains handles from 'receive', never builds them.
-}
mkReceiptHandle :: Text -> ReceiptHandle
mkReceiptHandle = ReceiptHandle

{- | Recover the backend's delivery token from a 'ReceiptHandle', to pass back to
the backend on 'ack' \/ 'extendVisibility'. For backend implementations only.
-}
unReceiptHandle :: ReceiptHandle -> Text
unReceiptHandle (ReceiptHandle t) = t

{- | A received message: the 'MirrorJob' to process together with the
'ReceiptHandle' used to 'ack' it (or 'extendVisibility' on it) once processed.
-}
data QueueMessage = QueueMessage
    { msgJob :: MirrorJob
    -- ^ The job to process.
    , msgReceipt :: ReceiptHandle
    -- ^ The handle identifying this delivery, for 'ack' \/ 'extendVisibility'.
    }
    deriving stock (Eq, Show)

{- | A duration in whole seconds, for 'extendVisibility'. A 'newtype' so a raw
@Int@ of seconds is never confused with some other count.
-}
newtype Seconds = Seconds Int
    deriving stock (Eq, Ord, Show)

{- | Why a queue operation could not be delivered to the backend, reported as a
__value__ on every handle field: the closed transport cause a consumer branches
on, and the backend's rendered detail for its log line. The cause vocabulary is
"Ecluse.Core.Fault"'s ('TransportCause'); a cloud backend's service-level
refusal (a throttle, an access denial) classifies as 'TransportProtocol' with
the service detail carried. Build one with 'queueFault' (or adopt an
already-classified transport fault with 'queueTransportFault') so the detail
stays bounded.

Every fault is __safe to absorb__ under the handle's contract: an enqueue fault
is the documented best-effort loss (re-enqueued on the next demand), a receive
fault is a failed poll (retried after backoff), and an ack or visibility fault
just means the message redelivers (idempotent). The typed channel exists so each
caller makes that absorption decision explicitly, with the cause in hand.
-}
data QueueFault = QueueFault
    { qfCause :: TransportCause
    -- ^ The closed classification a consumer or an operator reads.
    , qfDetail :: Text
    {- ^ The backend's rendered detail, bounded to a log-line-sized budget.
    Diagnostic text only: it is never parsed, and no decision may branch on it.
    -}
    }
    deriving stock (Eq, Show)

{- | Build a 'QueueFault' with the detail truncated to the shared log-line budget
(delegated to 'Ecluse.Core.Fault.transportFault', so the two vocabularies cannot
drift on what "bounded" means).
-}
queueFault :: TransportCause -> Text -> QueueFault
queueFault cause detail = queueTransportFault (transportFault cause detail)

{- | Adopt an already-classified 'TransportFault' (an adapter edge's
classification of its client library's exception) as a 'QueueFault'.
-}
queueTransportFault :: TransportFault -> QueueFault
queueTransportFault (TransportFault cause detail) = QueueFault cause detail

{- | The mirror-queue handle -- a record of functions over a backend whose private
state the closures capture. See the module header for the @enqueue@ /
don't-@ack@-to-retry / no-@nack@ conventions; all fields are 'IO', and each
reports its backend failures as a 'QueueFault' __value__, so no queue outage
ever rides the exception channel through a caller.
-}
data MirrorQueue = MirrorQueue
    { enqueue :: MirrorJob -> IO (Either QueueFault ())
    {- ^ Producer. __Best-effort__: runs on the request hot path, so a 'Left' is
    counted\/logged by the caller and never fails the client response (see the
    header); the lost job is re-enqueued on the next demand for its artifact.
    -}
    , receive :: IO (Either QueueFault [QueueMessage])
    {- ^ Consumer. One long-poll for a batch of messages; @Right []@ on timeout
    (an empty, healthy poll), so the worker loop simply polls again. A 'Left' is
    a failed poll: the worker logs it and backs off, and -- unlike an empty
    poll -- it does __not__ advance the liveness heartbeat, so a persistently
    failing backend still surfaces through @\/livez@.
    -}
    , ack :: ReceiptHandle -> IO (Either QueueFault ())
    {- ^ Acknowledge a processed message so it is not redelivered. __Not__ acking
    is how a failed job is retried (the header's "retry is don't ack"), so a
    'Left' here is absorbed after logging: the processed message redelivers, and
    idempotent publishing makes the repeat harmless.
    -}
    , extendVisibility :: ReceiptHandle -> Seconds -> IO (Either QueueFault ())
    {- ^ Extend a received message's visibility window to hold a long publish. An
    optimization, not correctness-critical (redelivery is harmless), so a 'Left'
    is absorbed silently by the caller.
    -}
    }

{- | Hand a job to a bounded queue within the caller's transaction: write it when
there is room, or drop it at the cap (drop-newest) and return the incremented
running drop total for the caller's report policy. Dropping rather than blocking
keeps the producer non-blocking, and the loss is safe: a dropped job is
re-enqueued on the next demand for its artifact. A backend building block, shared
by the bounded in-memory backend ("Ecluse.Core.Queue.Memory") and
'newEnqueueBuffer''s hand-off so the two cannot drift on the drop policy.
-}
writeOrDrop :: TBQueue MirrorJob -> TVar Int -> MirrorJob -> STM (Maybe Int)
writeOrDrop queue dropCount job = do
    full <- isFullTBQueue queue
    if full
        then Just <$> bumpCount dropCount
        else writeTBQueue queue job $> Nothing

-- Increment a running counter and return the new total.
bumpCount :: TVar Int -> STM Int
bumpCount counter = do
    n <- (+ 1) <$> readTVar counter
    writeTVar counter n
    pure n

{- | Whether the @n@-th event in a rate-limited series should be reported: the first
(@n == 1@), then every @interval@-th. Shared by the bounded queue's drop reporting and
the composition root's enqueue-buffer reporting so the two cannot drift.
-}
reportWorthy :: Int -> Int -> Bool
reportWorthy n interval = n == 1 || n `mod` interval == 0

{- | Wrap a bounded __producer-side hand-off buffer__ in front of a queue, so the
serve path's 'enqueue' is an in-process STM write (microseconds) no matter how slow
the backend's own producer call is.

The motivating case is the SQS backend: its 'enqueue' is an HTTP round trip
(@SendMessage@), and the serve path runs the mirror enqueue after the response body
has been sent but before the handler returns -- so on a keep-alive connection those
milliseconds hold the connection's turn and tax the next request on it. Buffered,
the handler hands the job off and returns; the returned __drain loop__ -- which the
composition root runs alongside the server -- delivers buffered jobs to the backend
at the backend's own pace. The consumer fields ('receive', 'ack',
'extendVisibility') pass through untouched.

Loss stays safe, so the buffer keeps the handle's best-effort producer contract
(mirroring is demand-driven: a lost job is re-enqueued on the next demand for its
artifact -- the same argument "Ecluse.Core.Queue.Memory"'s bounded backend makes):

* __Drop-newest on overflow.__ A hand-off finding the buffer full drops the job and
  invokes @onDrop@ with the running drop total. The callback fires on __every__
  drop (metric-grade); the caller owns any log rate-limiting.
* __A backend failure inside the drain loop__ invokes @onDeliveryFailure@ with the
  running failure total and the failure's detail, then the loop __backs off__ (bounded,
  growing with consecutive failures) before the next job so a persistently-unreachable
  backend is retried at a bounded rate rather than hot-looping; the failed job is not
  redelivered here, and the monotonic failure count is the operator's degraded-hand-off
  surface.
* __Cancellation loses the buffer.__ The drain loop never returns, so the
  composition root races it against the services; shutdown cancels it and any
  still-buffered jobs are dropped -- the same safe loss.

The wrapped 'enqueue' never fails: it is always @Right ()@ (a drop is the
documented safe loss, reported through @onDrop@, not a fault), so the never-fails
producer contract is visible in the type.
-}
newEnqueueBuffer ::
    {- | Buffer depth: how many undelivered jobs the hand-off retains before
    dropping the newest.
    -}
    Int ->
    -- | Invoked on every hand-off drop, with the running drop total.
    (Int -> IO ()) ->
    {- | Invoked on every backend delivery failure, with the running failure total
    and the failure's detail.
    -}
    (Int -> Text -> IO ()) ->
    -- | The backend whose 'enqueue' is being decoupled from its callers.
    MirrorQueue ->
    IO (MirrorQueue, IO ())
newEnqueueBuffer depth onDrop onDeliveryFailure backend = do
    -- A capacity of at least one, so a degenerate depth can never make the
    -- hand-off an always-full drop (the same guard the bounded backend applies).
    buffer <- newTBQueueIO (fromIntegral (max 1 depth))
    dropCount <- newTVarIO (0 :: Int)
    failureCount <- newTVarIO (0 :: Int)
    let
        -- Unlike the bounded backend, every hand-off drop reports (metric-grade);
        -- the caller owns any log rate-limiting.
        handOff job = do
            dropped <- atomically (writeOrDrop buffer dropCount job)
            -- 'onDrop' is a best-effort observer (log/metric) and runs on the serve
            -- hot path, so a throwing observer must never turn a safe drop into an
            -- exception on the client response: guard it (async-safe 'tryAny').
            whenJust dropped (void . tryAny . onDrop)
            -- The hand-off is an in-process STM write with a drop policy: it has
            -- no fault to report (the header's never-fails producer contract,
            -- visible in the type as an always-'Right').
            pure (Right ())
    pure (backend{enqueue = handOff}, drainLoop buffer failureCount onDeliveryFailure backend)

{- The drain loop: deliver buffered jobs to the backend's own 'enqueue', forever. Each
iteration blocks until a job is buffered, then delivers it. A delivery failure is
reported through the best-effort failure callback (guarded, so a throwing observer
cannot tear the loop down), then the loop __backs off__ before the next delivery so a
persistently-unreachable backend is retried at a bounded rate rather than hot-looping
through the buffer and shedding every job at once. The backoff grows with consecutive
failures to a cap and resets on the next success; the failed job is not redelivered here
(the safe loss 'newEnqueueBuffer' documents: it is re-enqueued on the next demand for its
artifact, and the running failure count the callback carries is the operator's surface
for a persistently-degraded hand-off). -}
drainLoop :: TBQueue MirrorJob -> TVar Int -> (Int -> Text -> IO ()) -> MirrorQueue -> IO ()
drainLoop buffer failureCount onDeliveryFailure backend = go 0
  where
    go consecutiveFailures = do
        job <- atomically (readTBQueue buffer)
        -- The backend reports its delivery failures as 'QueueFault' values, so the
        -- branch is a total match; an exception escaping here is an invariant
        -- break, left to the loop's supervisor.
        enqueue backend job >>= \case
            Right () -> go 0
            Left fault -> do
                n <- atomically (bumpCount failureCount)
                -- 'onDeliveryFailure' is a best-effort observer; guard it so a throwing
                -- observer can never escape the loop and tear down the composition root.
                void (tryAny (onDeliveryFailure n (qfDetail fault)))
                threadDelay (backoffMicros drainBackoff consecutiveFailures)
                go (consecutiveFailures + 1)

{- The bounded backoff between failed deliveries (the shared
'Ecluse.Core.Supervision.BackoffSchedule' shape): from 200ms towards a 30s cap as
consecutive failures mount, so a persistently-dead backend is retried at most
once per the cap interval. This is the loop's own per-delivery pacing over the
typed fault channel; the supervision combinator wrapping the whole loop paces
only residue. -}
drainBackoff :: BackoffSchedule
drainBackoff = BackoffSchedule{bsBaseMicros = 200_000, bsCapMicros = 30_000_000}
