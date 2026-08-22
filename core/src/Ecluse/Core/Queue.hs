-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror-queue handle: the durable hand-off from the request path to the
mirror worker.

Mirroring is demand-driven. When a client fetches an artifact whose version passes
the rules, the proxy 'enqueue's a 'MirrorJob'. It then serves the artifact at once,
never blocking on the mirror. A separate worker 'receive's jobs, fetches and verifies
the artifact, publishes it to the mirror target, and 'ack's the job (see
@docs\/architecture\/cloud-backends.md@ → "Mirror Queue").

The queue is the one cloud surface whose API differs materially per provider. It is
therefore its own handle: a record of functions (the Handle pattern). AWS SQS speaks
@SendMessage@\/@ReceiveMessage@+visibility-timeout\/@DeleteMessage@, and GCP Pub\/Sub
speaks @Publish@\/@Pull@+ack-deadline\/@Acknowledge@. Both providers fit the same
receive → process → ack shape. Their differences (visibility timeout vs ack deadline,
batch limits, dead-letter wiring) stay behind the handle, and 'ReceiptHandle' is opaque
so neither leaks.

Like the other handles, the effectful fields return 'IO' rather than @App@. An
adapter therefore stays decoupled from the proxy's @Env@\/@App@ (see
@docs\/architecture\/technology-stack.md@ → "Key Decisions").

== Conventions

The two cloud backends both give at-least-once delivery, which is safe here because
publishing is idempotent (a registry treats versions as immutable). The handle's
contract reflects that:

* __'enqueue' is best-effort__. It runs on the request hot path (enqueue, then serve
  immediately), so the caller logs and meters a failure and never fails the client
  response. The artifact is already served, and a later pull re-enqueues.
* __Retry is "don't 'ack'"__. A job that fails processing transiently (a flaky fetch,
  a registry blip) is simply not acked. The visibility timeout \/ ack deadline
  redelivers it, and it may succeed next time. There is deliberately no @nack@ for
  the transient case.
* __'deadLetter' is the terminal terminus__. A job that can never succeed (an artifact
  past the plan-sized byte cap) is a terminal verdict, and each backend realises it
  its own way. The in-memory backend drops the delivery, its only terminus. The SQS
  backend returns the message with a backoff visibility timeout without deleting it.
  It then rides the operator's redrive policy to the dead-letter queue for forensic
  retention rather than being silently discarded. This is not a @nack@ (a retry) and
  not an 'ack' (a clean retire). It is the third, terminal outcome.
* __The 'deliveryBudget' is the backstop terminus__. Returning a message only has a
  terminus if something captures it. A queue with no dead-letter terminus would
  otherwise cycle a poison message until the retention window discarded it unseen,
  re-fetching on every delivery. So every delivery carries its 'msgReceiveCount'. The
  worker retires a delivery that spends the budget ('deliveryBudgetSpent'), killing
  the job with an alarm rather than letting it churn. 'effectiveDeliveryBudget' raises
  the budget past an attached terminus's own capture count. A dead-letter queue
  therefore always captures first.
* __'extendVisibility'__ lets the worker hold a long publish (a large artifact) past
  the visibility window. It is an /optimisation/, not correctness-critical, since
  idempotency already makes redelivery harmless.

This module provides the handle, its payload types, and the building blocks a backend
implementation reaches for. "Ecluse.Core.Queue.Memory" holds the STM-backed bounded,
best-effort production backend that mirroring rolls over to when no
@ECLUSE_QUEUE__URL@ is set.

It also provides 'newEnqueueBuffer', a bounded producer-side hand-off buffer that
wraps any backend. The serve path's 'enqueue' then completes in microseconds, while a
composition-root drain loop delivers to the (possibly slow) backend off the request
path.
-}
module Ecluse.Core.Queue (
    -- * Queue handle
    MirrorQueue (..),
    noMirrorQueue,

    -- * Faults
    QueueFault (..),
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

    -- * Dead-letter terminus and the redelivery budget
    DeadLetterTerminus (..),
    DeliveryBudget (..),
    defaultDeliveryBudget,
    effectiveDeliveryBudget,
    retiringDelivery,
    deliveryBudgetSpent,

    -- * Backend building blocks
    writeOrDrop,
    reportWorthy,

    -- * Buffered producer hand-off
    newEnqueueBuffer,
) where

import Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Fault (TransportCause (TransportProtocol), TransportFault (TransportFault), transportFault)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Supervision (BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros), backoffMicros)
import Ecluse.Core.Version (Version)

{- | A mirror job: everything the worker needs to back-fill one artifact into the
mirror target. The rules gated the version at serve time, when the job was enqueued.
Before mirroring, the worker re-evaluates current policy through the same shared
admission oracle (see "Ecluse.Core.Worker.Job"). It then fetches the bytes, verifies
them against the digests of the artifact that re-evaluation re-admitted, and
publishes.

The queue payload is a trust boundary, so it carries selection keys and never
authority. The filename ('jobArtifactFilename') names the artifact the worker's ingest
re-evaluation selects and gates under current policy. The payload carries no digest or
size at all. The tamper gate and the publish document consume a descriptor
('Ecluse.Core.Registry.MirrorArtifact') derived entirely from the artifact that
re-evaluation re-admits.
-}
data MirrorJob = MirrorJob
    { jobPackage :: PackageName
    -- ^ The package whose artifact the worker mirrors.
    , jobVersion :: Version
    -- ^ The specific version to mirror.
    , jobArtifactUrl :: RegistryUrl
    {- ^ Where to fetch the artifact bytes from (the public upstream), carried as
    the validated https egress witness rather than bare text. The SQS wire decode
    re-forms it, since the queue payload is a trust boundary.
    -}
    , jobArtifactFilename :: Text
    {- ^ The serve-time-admitted artifact's filename: the selection key the
    worker's ingest re-evaluation gates by. The shared admission gate cross-checks it
    against current metadata rather than trusting it.
    -}
    , jobTraceContext :: Maybe RemoteSpanContext
    {- ^ The trace context of the serve-time span that enqueued the job, captured at
    enqueue time. The worker's per-job span can then link back to the request that
    produced the work across the asynchronous hop. 'Nothing' when tracing was off at
    enqueue time, or for a job from a producer that carried none. The queue treats it
    as opaque transport, and only the tracing port reads it.
    -}
    }
    deriving stock (Eq, Show)

{- | A serialised W3C trace-context carrier riding on a 'MirrorJob': the
@traceparent@ (and any @tracestate@) of the enqueueing span, in the standard wire
encoding. That span captures it. The worker's tracing port reads it back to
re-establish a span link from the per-job span to the enqueueing request. The
asynchronous mirror hand-off is then navigable in a trace.

The two fields are the W3C header values verbatim. The queue carries them opaquely and
neither parses nor validates them, so an unparseable carrier simply yields no link.
This type names what the job carries without coupling the queue to any tracing
backend.
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

{- | An opaque handle identifying a received message for 'ack' \/
'extendVisibility'. It carries the backend's own delivery token as text: an SQS
receipt handle, or a Pub\/Sub @ackId@. The constructor is hidden, so neither
provider's representation leaks into worker code. Worker code only ever takes a handle
from a 'QueueMessage' that 'receive' returned. Build one (in a backend) with
'mkReceiptHandle' and read the token back with 'unReceiptHandle'.
-}
newtype ReceiptHandle = ReceiptHandle Text
    deriving stock (Eq, Ord, Show)

{- | Wrap a backend's delivery token (an SQS receipt handle, a Pub\/Sub @ackId@)
as an opaque 'ReceiptHandle'. For backend implementations only: worker code obtains
handles from 'receive' and never builds them.
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
    , msgReceiveCount :: Int
    {- ^ The number of deliveries of this message, counting this one: @1@ on a first
    delivery. A backend that cannot report a count reports @1@, so only evidence puts a
    delivery past the 'deliveryBudget'. Backends supply the count. The verdict belongs
    to 'deliveryBudgetSpent' alone.
    -}
    }
    deriving stock (Eq, Show)

{- | A duration in whole seconds, for 'extendVisibility'. A 'newtype', so a raw
@Int@ of seconds cannot pass for some other count.
-}
newtype Seconds = Seconds Int
    deriving stock (Eq, Ord, Show)

{- | Why the handle could not deliver a queue operation to the backend, reported as
a value on every handle field. It carries the closed transport cause a consumer branches
on, and the backend's rendered detail for its log line. The cause vocabulary is
"Ecluse.Core.Fault"'s ('TransportCause'). A cloud backend's service-level refusal (a
throttle, an access denial) classifies as 'TransportProtocol' and carries the service
detail. Build one with 'queueTransportFault', which adopts an already-classified
transport fault ('Ecluse.Core.Fault.transportFault') so the detail stays bounded.

Every fault is safe to absorb under the handle's contract. An enqueue fault is the
documented best-effort loss, re-enqueued on the next demand. A receive fault is a
failed poll, retried after backoff. An ack or visibility fault just means the message
redelivers, which is idempotent. The typed channel exists so each caller makes that
absorption decision explicitly, with the cause in hand.
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

{- | Adopt an already-classified 'TransportFault' (an adapter edge's
classification of its client library's exception) as a 'QueueFault'. The
'TransportFault' side ('Ecluse.Core.Fault.transportFault') truncates the detail
to the shared log-line budget, so the two vocabularies cannot drift on what
"bounded" means.
-}
queueTransportFault :: TransportFault -> QueueFault
queueTransportFault (TransportFault cause detail) = QueueFault cause detail

{- | How many deliveries of one message a queue grants before the worker itself
retires it. A 'newtype', so no caller confuses a count of receives with some other
'Int'.
-}
newtype DeliveryBudget = DeliveryBudget Int
    deriving stock (Eq, Ord, Show)

{- | The redelivery budget a backend holds when the operator configures none: five
deliveries, SQS's own redrive convention. A configured deployment runs on
@ECLUSE_QUEUE__MAX_RECEIVE_COUNT@ instead, pinned to this same value in
@config\/default.yaml@.
-}
defaultDeliveryBudget :: DeliveryBudget
defaultDeliveryBudget = DeliveryBudget 5

{- | Whether a queue has a __dead-letter terminus__: somewhere that captures a message
the worker can never mirror, for inspection. Without one the message cycles until the
queue's retention window discards it unseen. The backend probes this once when it is
built. The SQS backend reads its @RedrivePolicy@. The in-memory backend has none by
construction.

An attached terminus carries its own __capture count__ when the backend can read one:
the delivery at which the terminus takes the message. The worker then holds its own
budget above that count and never pre-empts the capture.
-}
data DeadLetterTerminus
    = -- | A terminus captures poison messages, at this capture count when it is known.
      TerminusAttached (Maybe DeliveryBudget)
    | -- | Nothing captures poison messages: the worker's budget is the only terminus.
      TerminusAbsent
    deriving stock (Eq, Show)

{- | The budget the worker enforces: the configured floor, raised past an attached
terminus's own capture count. The dead-letter queue then always captures a poison
message first, and the worker only retires what the terminus did not take. With no
terminus, or one whose capture count the backend could not read, the configured value
stands alone.
-}
effectiveDeliveryBudget :: DeliveryBudget -> DeadLetterTerminus -> DeliveryBudget
effectiveDeliveryBudget configured = \case
    TerminusAttached (Just (DeliveryBudget captureAt)) -> max configured (DeliveryBudget (captureAt + 1))
    TerminusAttached Nothing -> configured
    TerminusAbsent -> configured

{- | The delivery a budget retires on: the configured value, floored at two, so a
first delivery never spends the budget. Retiring a job before the worker tries it once
would be a bug, not a policy. Both the verdict ('deliveryBudgetSpent') and the worker's
alarm read this number. The number an operator is told is the number that fired.
-}
retiringDelivery :: DeliveryBudget -> Int
retiringDelivery (DeliveryBudget budget) = max 2 budget

{- | Whether this delivery spends the queue's redelivery budget. The worker then
retires the message through its terminal path rather than letting it cycle. Every
backend's deliveries meet this one decision: a backend supplies the count
('msgReceiveCount'), never the verdict.
-}
deliveryBudgetSpent :: DeliveryBudget -> QueueMessage -> Bool
deliveryBudgetSpent budget message = msgReceiveCount message >= retiringDelivery budget

{- | The mirror-queue handle: a record of functions over a backend whose private
state the closures capture. See the module header for the @enqueue@ /
don't-@ack@-to-retry / no-@nack@ conventions. Every operation is 'IO'. Each reports
its backend failures as a 'QueueFault' value, so no queue outage ever rides the
exception channel through a caller. The two remaining fields are what the backend
settled once at construction: the redelivery budget it grants, and what its
dead-letter probe found.
-}
data MirrorQueue = MirrorQueue
    { enqueue :: MirrorJob -> IO (Either QueueFault ())
    {- ^ Producer. Best-effort: it runs on the request hot path, so the caller counts
    and logs a 'Left' and never fails the client response (see the header). The next
    demand for the artifact re-enqueues the lost job.
    -}
    , receive :: IO (Either QueueFault [QueueMessage])
    {- ^ Consumer. One long-poll for a batch of messages, with @Right []@ on timeout
    (an empty, healthy poll), so the worker loop simply polls again. A 'Left' is a
    failed poll: the worker logs it and backs off. Unlike an empty poll it does not
    advance the liveness heartbeat, so a persistently failing backend still surfaces
    through @\/livez@.
    -}
    , ack :: ReceiptHandle -> IO (Either QueueFault ())
    {- ^ Acknowledge a processed message so the backend does not redeliver it. Not
    acking is how a failed job retries (the header's "retry is don't ack"), so the
    caller logs a 'Left' here and absorbs it. The processed message redelivers, and
    idempotent publishing makes the repeat harmless.
    -}
    , extendVisibility :: ReceiptHandle -> Seconds -> IO (Either QueueFault ())
    {- ^ Extend a received message's visibility window to hold a long publish. An
    optimisation, not correctness-critical (redelivery is harmless), so the caller
    absorbs a 'Left' silently.
    -}
    , deadLetter :: ReceiptHandle -> IO (Either QueueFault ())
    {- ^ Realise a terminal fault: a job that can never succeed (an artifact past the
    plan-sized byte cap), decided as a verdict at the read site. Each backend routes it
    to its own dead-letter terminus. The in-memory backend drops the delivery, its only
    terminus, and its observability is the worker's log and metric. The SQS backend
    returns the message with a backoff visibility timeout without deleting it. It then
    rides the operator's redrive policy to the dead-letter queue rather than being
    silently discarded. This is distinct from 'ack' (a clean retire) and from
    not-acking (a transient redelivery). See the header's terminus convention. The
    caller logs a 'Left' and absorbs it, like 'ack'.
    -}
    , deliveryBudget :: DeliveryBudget
    {- ^ How many deliveries of one message this queue grants before the worker
    retires it ('deliveryBudgetSpent'). The backend settles this once when it is built:
    the configured floor, raised past an attached terminus's capture count
    ('effectiveDeliveryBudget'). Judging a delivery therefore costs no per-message work.
    -}
    , deadLetterTerminus :: Either QueueFault DeadLetterTerminus
    {- ^ What this backend's dead-letter probe found at construction: whether anything
    captures a message the worker can never mirror, or the fault that stopped the probe.
    The composition root reads it once to decide its boot warning. A 'Left' leaves the
    configured budget standing and never blocks boot.
    -}
    }

{- | The inert queue a deployment with zero mirroring mounts carries, so the
composition-root 'Env' keeps its total shape without a backend. It is unreachable by
construction: no serve path enqueues on a mount that never mirrors, and no worker runs
to poll it. Reached anyway, 'enqueue' is a typed, counted refusal rather than a crash,
and 'receive' is the empty healthy poll.
-}
noMirrorQueue :: MirrorQueue
noMirrorQueue =
    MirrorQueue
        { enqueue = \_ -> pure (Left inertFault)
        , receive = pure (Right [])
        , ack = \_ -> pure (Right ())
        , extendVisibility = \_ _ -> pure (Right ())
        , deadLetter = \_ -> pure (Right ())
        , deliveryBudget = defaultDeliveryBudget
        , deadLetterTerminus = Right TerminusAbsent
        }
  where
    inertFault =
        queueTransportFault
            (transportFault TransportProtocol "no mount mirrors, so no mirror queue is built")

{- | Hand a job to a bounded queue within the caller's transaction. With room, it
writes the job. At the cap it drops the newest job and returns the incremented running
drop total for the caller's report policy. Dropping rather than blocking keeps the
producer non-blocking, and the loss is safe: the next demand for the artifact
re-enqueues a dropped job. A backend building block, shared by the bounded in-memory
backend ("Ecluse.Core.Queue.Memory") and 'newEnqueueBuffer''s hand-off, so the two
cannot drift on the drop policy.
-}
writeOrDrop :: TBQueue MirrorJob -> TVar Int -> MirrorJob -> STM (Maybe Int)
writeOrDrop queue dropCount job = do
    full <- isFullTBQueue queue
    if full
        then Just <$> bumpCount dropCount
        else writeTBQueue queue job $> Nothing

bumpCount :: TVar Int -> STM Int
bumpCount counter = do
    n <- (+ 1) <$> readTVar counter
    writeTVar counter n
    pure n

{- | Whether the caller should report the @n@-th event in a rate-limited series: the
first (@n == 1@), then every @interval@-th. The bounded queue's drop reporting and the
composition root's enqueue-buffer reporting share it, so the two cannot drift.
-}
reportWorthy :: Int -> Int -> Bool
reportWorthy n interval = n == 1 || n `mod` interval == 0

{- | Wrap a bounded producer-side hand-off buffer in front of a queue. The serve
path's 'enqueue' is then an in-process STM write of microseconds, however slow the
backend's own producer call is.

The motivating case is the SQS backend. Its 'enqueue' is an HTTP round trip
(@SendMessage@). The serve path runs the mirror enqueue after it sends the response
body but before the handler returns. On a keep-alive connection those milliseconds hold
the connection's turn and tax the next request on it. Buffered, the handler hands the
job off and returns. The returned drain loop, which the composition root runs alongside
the server, delivers buffered jobs to the backend at the backend's own pace. The
consumer fields ('receive', 'ack', 'extendVisibility') pass through untouched.

Loss stays safe, so the buffer keeps the handle's best-effort producer contract.
Mirroring is demand-driven: the next demand for the artifact re-enqueues a lost job,
the same argument "Ecluse.Core.Queue.Memory"'s bounded backend makes.

* Drop-newest on overflow. A hand-off that finds the buffer full drops the job and
  invokes @onDrop@ with the running drop total. The callback fires on every drop
  (metric-grade), and the caller owns any log rate-limiting.
* A backend failure inside the drain loop invokes @onDeliveryFailure@ with the
  running failure total and the failure's detail. The loop then backs off before the
  next job (bounded, growing with consecutive failures). It therefore retries a
  persistently-unreachable backend at a bounded rate rather than hot-looping. The
  failed job does not redeliver here, and the monotonic failure count is the
  operator's degraded-hand-off surface.
* Cancellation loses the buffer. The drain loop never returns, so the composition
  root races it against the services. Shutdown cancels it and drops any still-buffered
  jobs, the same safe loss.

The wrapped 'enqueue' never fails. It is always @Right ()@, because a drop is the
documented safe loss reported through @onDrop@ rather than a fault. The type therefore
shows the never-fails producer contract.
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
    -- | The backend whose 'enqueue' the buffer decouples from its callers.
    MirrorQueue ->
    IO (MirrorQueue, IO ())
newEnqueueBuffer depth onDrop onDeliveryFailure backend = do
    -- A capacity of at least one, so a degenerate depth can never make the
    -- hand-off an always-full drop (the same guard the bounded backend applies).
    buffer <- newTBQueueIO (fromIntegral (max 1 depth))
    dropCount <- newTVarIO (0 :: Int)
    failureCount <- newTVarIO (0 :: Int)
    let
        -- Unlike the bounded backend, every hand-off drop reports (metric-grade).
        -- The caller owns any log rate-limiting.
        handOff job = do
            dropped <- atomically (writeOrDrop buffer dropCount job)
            -- 'onDrop' is a best-effort observer (log/metric) and runs on the serve
            -- hot path. A throwing observer must never turn a safe drop into an
            -- exception on the client response, so guard it with async-safe 'tryAny'.
            whenJust dropped (void . tryAny . onDrop)
            -- The hand-off is an in-process STM write with a drop policy, so it has
            -- no fault to report. That is the header's never-fails producer contract,
            -- visible in the type as an always-'Right'.
            pure (Right ())
    pure (backend{enqueue = handOff}, drainLoop buffer failureCount onDeliveryFailure backend)

{- The drain loop: deliver buffered jobs to the backend's own 'enqueue', forever. Each
iteration blocks until a job arrives, then delivers it. A delivery failure goes to the
best-effort failure callback, guarded so a throwing observer cannot tear the loop down.
The loop then backs off before the next delivery. It therefore retries a
persistently-unreachable backend at a bounded rate, rather than hot-looping through the
buffer and shedding every job at once. The backoff grows with consecutive failures to a
cap and resets on the next success. The failed job does not redeliver here, the safe
loss 'newEnqueueBuffer' documents. The next demand for the artifact re-enqueues it, and
the running failure count the callback carries is the operator's surface for a
persistently-degraded hand-off. -}
drainLoop :: TBQueue MirrorJob -> TVar Int -> (Int -> Text -> IO ()) -> MirrorQueue -> IO ()
drainLoop buffer failureCount onDeliveryFailure backend = go 0
  where
    go consecutiveFailures = do
        job <- atomically (readTBQueue buffer)
        -- The backend reports its delivery failures as 'QueueFault' values, so the
        -- branch is a total match. An exception escaping here is an invariant break,
        -- left to the loop's supervisor.
        enqueue backend job >>= \case
            Right () -> go 0
            Left fault -> do
                n <- atomically (bumpCount failureCount)
                -- 'onDeliveryFailure' is a best-effort observer. Guard it so a throwing
                -- observer can never escape the loop and tear down the composition root.
                void (tryAny (onDeliveryFailure n (qfDetail fault)))
                threadDelay (backoffMicros drainBackoff consecutiveFailures)
                go (consecutiveFailures + 1)

{- The bounded backoff between failed deliveries, in the shared
'Ecluse.Core.Supervision.BackoffSchedule' shape: from 200ms towards a 30s cap as
consecutive failures mount. The loop therefore retries a persistently-dead backend at
most once per cap interval. This is the loop's own per-delivery pacing over the typed
fault channel. The supervision combinator wrapping the whole loop paces only
residue. -}
drainBackoff :: BackoffSchedule
drainBackoff = BackoffSchedule{bsBaseMicros = 200_000, bsCapMicros = 30_000_000}
