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

This module provides the handle and its payload types, plus two STM-backed
in-memory implementations:

* 'newInMemoryQueue' -- the __test double__ that models the cloud backends'
  visibility-timeout semantics (receive → ack \/ redeliver-on-no-ack), used to
  exercise the worker's retry path without a cloud queue.
* 'newBoundedInMemoryQueue' -- the __bounded, best-effort production backend__
  selected by @ECLUSE_QUEUE_BACKEND=memory@. See its own Haddock for why it is
  correctness-safe (a dropped job is re-enqueued on the next demand) and why it
  deliberately does __not__ redeliver.

It also provides 'newEnqueueBuffer', a __bounded producer-side hand-off buffer__
wrapped in front of either backend so the serve path's 'enqueue' completes in
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
    MirrorArtifact (..),
    RemoteSpanContext (..),
    QueueMessage (..),

    -- * Opaque receipt
    ReceiptHandle,
    mkReceiptHandle,
    unReceiptHandle,

    -- * Durations
    Seconds (..),

    -- * In-memory double
    newInMemoryQueue,

    -- * Bounded in-memory production backend
    MemoryQueueConfig (..),
    defaultMemoryQueueConfig,
    newBoundedInMemoryQueue,
    memoryQueueBatchSize,
    memoryQueueDropReportInterval,
    reportWorthy,

    -- * Buffered producer hand-off
    newEnqueueBuffer,
) where

import Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, newTBQueueIO, readTBQueue, tryReadTBQueue, writeTBQueue)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import System.Timeout (timeout)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Fault (TransportCause, TransportFault (TransportFault), transportFault)
import Ecluse.Core.Package (Hash, PackageName)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Supervision (BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros), backoffMicros)
import Ecluse.Core.Version (Version)

{- | A mirror job: everything the worker needs to back-fill one artifact into the
mirror target. The version was gated by the rules at serve time (when the job was
enqueued); the worker __re-evaluates current policy__ through the same shared
admission oracle before mirroring (see "Ecluse.Core.Worker.Job"), then fetches the
bytes, verifies them against the __serve-time-admitted__ integrity digest the job
carries, and publishes.

The integrity digest and the artifact descriptor are captured __at enqueue time__
('jobArtifact'), not re-fetched: the worker mirrors exactly the bytes the rules
admitted, so an upstream packument mutated in the enqueue → process window cannot
substitute a different artifact for the one that was gated. The descriptor also
carries the filename and declared size the worker needs to assemble the publish
document.
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
    , jobMirrorTarget :: Text
    -- ^ The mirror-target endpoint the artifact is published to.
    , jobArtifact :: MirrorArtifact
    {- ^ The serve-time-admitted artifact descriptor: the integrity digest the
    fetched bytes are verified against, plus the filename and declared size the
    publish document is assembled from.
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

{- | The serve-time-admitted artifact descriptor carried on a 'MirrorJob': exactly
the fields the worker needs to verify the fetched bytes and assemble the publish
document, captured when the version was gated.

'maHashes' is a 'NonEmpty' because the serve path admits a public version only when
it carries at least one integrity digest (the integrity-presence admission policy),
so a job with __no__ digest to verify against is unrepresentable -- the worker always
has a fingerprint to check the bytes against before they reach the private upstream.
-}
data MirrorArtifact = MirrorArtifact
    { maFilename :: Text
    {- ^ The artifact's on-the-wire filename, the @_attachments@ key in the publish
    document.
    -}
    , maHashes :: NonEmpty Hash
    {- ^ The serve-time-admitted integrity digests (at least one). The worker
    verifies the fetched bytes against these before publishing; a mismatch fails
    the job with no publish.
    -}
    , maSize :: Maybe Int
    {- ^ The registry-declared size, if reported. Not guaranteed to be the tarball byte
    count: for npm it is the unpacked-tree size (@dist.unpackedSize@).
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

{- The mutable state of the in-memory queue.

Modelled as visible (waiting) jobs plus in-flight (received-but-unacked) ones,
exactly mirroring the visibility-timeout model the cloud backends use: a 'receive'
makes visible jobs in-flight, an 'ack' drops an in-flight job, and an unacked
in-flight job becomes visible again -- redelivered -- on a subsequent 'receive'.
-}
data QueueState = QueueState
    { -- A monotonic counter giving each delivery a unique 'ReceiptHandle'.
      qsNextReceipt :: Word64
    , -- Jobs waiting to be delivered, oldest first (FIFO). 'Seq' gives
      -- O(1) amortised snoc so enqueue cost does not grow with queue depth.
      qsVisible :: Seq MirrorJob
    , -- Delivered-but-unacked jobs, keyed by the numeric receipt counter (not the
      -- rendered 'ReceiptHandle' text) so iteration stays in delivery -- hence
      -- FIFO-reclaim -- order rather than the lexicographic order text keys give.
      qsInFlight :: Map Word64 InFlight
    }

{- One in-flight job and whether its visibility has been extended.

A held ('inFlightHeld' = 'True') job survives one reclaim pass (the effect of
'extendVisibility'); otherwise an in-flight job is reclaimed -- made visible again
for redelivery -- on the next 'receive', modelling expiry of the visibility
window.
-}
data InFlight = InFlight
    { -- The job awaiting acknowledgement.
      inFlightJob :: MirrorJob
    , -- Whether 'extendVisibility' has held it past the next reclaim.
      inFlightHeld :: Bool
    }

{- | Build a fresh STM-backed in-memory 'MirrorQueue'.

Honours the handle's contract: 'enqueue' appends (FIFO), 'receive' delivers all
currently-visible jobs and moves them in-flight, 'ack' removes an in-flight job,
and an in-flight job that is never acked is __redelivered__ on the next 'receive'
("retry is don't ack"). 'extendVisibility' holds a job in-flight across one such
redelivery pass. This is a test double -- there is no long-poll blocking; an empty
'receive' returns @[]@ at once.
-}
newInMemoryQueue :: IO MirrorQueue
newInMemoryQueue = do
    stateVar <- newTVarIO (QueueState 0 Seq.empty mempty)
    -- Every operation is one STM transaction over process-local state, so this
    -- backend has no fault to report: each field is always 'Right'.
    let modifyState :: (QueueState -> QueueState) -> IO (Either QueueFault ())
        modifyState = fmap Right . atomically . modifyTVar' stateVar
    pure
        MirrorQueue
            { enqueue = modifyState . enqueueJob
            , receive = fmap Right . atomically $ do
                qs <- readTVar stateVar
                let (messages, qs') = deliverAll qs
                writeTVar stateVar qs'
                pure messages
            , ack = modifyState . ackJob
            , extendVisibility = \handle _seconds -> modifyState (holdJob handle)
            }

-- Append a job to the back of the visible queue (FIFO). O(1) amortised.
enqueueJob :: MirrorJob -> QueueState -> QueueState
enqueueJob job qs = qs{qsVisible = qsVisible qs <> Seq.singleton job}

-- Drop an acked in-flight job; a handle that is unknown (already acked, never
-- issued, or not one of ours) is a harmless no-op.
ackJob :: ReceiptHandle -> QueueState -> QueueState
ackJob handle qs =
    case receiptKey handle of
        Just key -> qs{qsInFlight = Map.delete key (qsInFlight qs)}
        Nothing -> qs

-- Hold an in-flight job past the next reclaim pass. Unknown handle: no-op.
holdJob :: ReceiptHandle -> QueueState -> QueueState
holdJob handle qs =
    case receiptKey handle of
        Just key ->
            qs{qsInFlight = Map.adjust (\f -> f{inFlightHeld = True}) key (qsInFlight qs)}
        Nothing -> qs

-- Recover the numeric counter a handle was minted from (the inverse of the
-- 'show' in 'assignReceipts'); 'Nothing' for a handle this queue never issued.
receiptKey :: ReceiptHandle -> Maybe Word64
receiptKey = readMaybe . toString . unReceiptHandle

{- A single receive over the in-memory queue state. First reclaim any un-held
in-flight jobs (their visibility window has lapsed) back to the front of the
visible queue, and clear the held flag on the rest so they are reclaimed next
time unless held again. Then deliver every visible job: assign each a fresh
receipt, move it in-flight, and return it as a 'QueueMessage'. -}
deliverAll :: QueueState -> ([QueueMessage], QueueState)
deliverAll qs =
    let (reclaimed, stillHeld) = reclaim (Map.toList (qsInFlight qs))
        toDeliver = Seq.fromList reclaimed <> qsVisible qs
        (messages, nextReceipt, delivered) =
            assignReceipts (qsNextReceipt qs) (toList toDeliver)
     in ( messages
        , QueueState
            { qsNextReceipt = nextReceipt
            , qsVisible = Seq.empty
            , qsInFlight = Map.fromList stillHeld <> delivered
            }
        )

{- Partition in-flight entries into (jobs reclaimed to visible, entries that
stay in flight). A held entry stays in flight but has its hold cleared, so a
later un-held receive reclaims it. -}
reclaim ::
    [(Word64, InFlight)] ->
    ([MirrorJob], [(Word64, InFlight)])
reclaim = foldr step ([], [])
  where
    step (key, f) (jobs, held)
        | inFlightHeld f = (jobs, (key, f{inFlightHeld = False}) : held)
        | otherwise = (inFlightJob f : jobs, held)

{- Give each job a fresh receipt, threading the monotonic counter. Returns
the messages, the next free counter value, and the new in-flight entries. The
in-flight map is keyed by the numeric counter; the 'ReceiptHandle' the message
carries is that counter rendered as text. -}
assignReceipts ::
    Word64 ->
    [MirrorJob] ->
    ([QueueMessage], Word64, Map Word64 InFlight)
assignReceipts next [] = ([], next, mempty)
assignReceipts next (job : rest) =
    let message = QueueMessage{msgJob = job, msgReceipt = mkReceiptHandle (show next)}
        (messages, next', inFlight) = assignReceipts (next + 1) rest
     in ( message : messages
        , next'
        , Map.insert next (InFlight{inFlightJob = job, inFlightHeld = False}) inFlight
        )

{- | What the bounded in-memory backend needs: its depth cap and its idle-poll
window. A record (like the SQS backend's @SqsConfig@) so each knob is named rather
than a bare 'Int'; build it with 'defaultMemoryQueueConfig' for the production poll
window.
-}
data MemoryQueueConfig = MemoryQueueConfig
    { memQueueMaxDepth :: Int
    {- ^ The maximum number of jobs the queue holds. A fresh 'enqueue' past this cap
    is __dropped-newest__ (the enqueue is rejected); a dropped job is safe, as it is
    re-enqueued on the next demand. Must be positive (the config layer enforces it).
    -}
    , memQueuePollWaitMicros :: Int
    {- ^ The idle long-poll window in microseconds: how long a 'receive' waits for a
    job before returning @[]@ (an empty, healthy poll). Bounds the idle wait so the
    worker's liveness heartbeat keeps advancing -- see 'newBoundedInMemoryQueue'.
    -}
    }
    deriving stock (Eq, Show)

{- | A 'MemoryQueueConfig' for a given depth cap with the idle-poll window at its
production default -- @20s@, mirroring the SQS long-poll cadence
(the SQS backend's @defaultSqsConfig@) and comfortably under the worker's @120s@
heartbeat-staleness budget ('Ecluse.Core.Worker.workerHeartbeatStaleAfter'), so an idle
'receive' returns a healthy empty poll long before @\/livez@ would flag the loop
stalled. The depth cap stays the operator-tunable knob; the poll window is a fixed
cadence, exposed on the record only so a test can shorten it.
-}
defaultMemoryQueueConfig :: Int -> MemoryQueueConfig
defaultMemoryQueueConfig maxDepth =
    MemoryQueueConfig
        { memQueueMaxDepth = maxDepth
        , memQueuePollWaitMicros = 20_000_000
        }

{- | The most jobs one 'receive' delivers from the bounded in-memory backend. Held
at the SQS batch cap so the worker -- which processes a batch __sequentially__ and
advances its liveness heartbeat once per poll -- sees the same bounded batch shape
regardless of backend, rather than one poll returning a whole cold-cache burst and
starving the heartbeat past its staleness window.
-}
memoryQueueBatchSize :: Int
memoryQueueBatchSize = 10

{- | How many cap-overflow drops the bounded in-memory backend absorbs between
warning reports. The first drop is always reported, then every multiple of this, so
a sustained flood logs at most about one line per this many drops rather than one
per dropped job.
-}
memoryQueueDropReportInterval :: Int
memoryQueueDropReportInterval = 1000

{- | Build a bounded, best-effort in-memory 'MirrorQueue' -- the production backend
behind @ECLUSE_QUEUE_BACKEND=memory@, a 'TBQueue' shared between the serve path's
'enqueue' and the worker's 'receive'.

It is __correctness-safe despite being lossy__: mirroring is a demand-driven
optimization over the always-available public upstream, so a job lost to the cap or
to process teardown just means the package is served from public again and
re-enqueued on the next pull -- a deferred performance win, never a correctness loss.
That admits two deliberate departures from the cloud backends' contract:

* __Bounded, drop-newest on overflow.__ The queue holds at most 'memQueueMaxDepth'
  jobs; an 'enqueue' that would exceed the cap is rejected (the newest job is
  dropped) rather than growing memory without bound -- the load-bearing constraint,
  since a cold-cache @npm ci@ enqueues thousands of jobs at once. 'enqueue' never
  throws (it runs on the serve hot path), and each report-worthy drop invokes the
  injected drop callback with the running drop count, rate-limited by
  'memoryQueueDropReportInterval' so a flood does not spam.
* __No redelivery; 'ack' \/ 'extendVisibility' are no-ops.__ Unlike the cloud
  backends (and 'newInMemoryQueue'), there is no visibility-timeout in-flight
  tracking: a 'receive' removes a job for good. A job whose processing fails is
  therefore __not__ redelivered -- it is simply re-enqueued on the next demand. This
  bounds memory hardest (nothing is retained after delivery) and is admissible
  precisely because a lost job is safe.

'receive' is a __bounded long-poll__: it waits up to 'memQueuePollWaitMicros' for a
job, then drains up to 'memoryQueueBatchSize' without blocking, or returns @[]@ when
the window lapses -- the in-process analogue of the cloud long-poll. The bound is
load-bearing: the worker advances its liveness heartbeat only when 'receive' returns
(an empty poll is a healthy idle), so an idle 'receive' that blocked forever would
let the heartbeat go stale and @\/livez@ flag the loop stalled. The wait is the
@timeout@-over-@atomically@ idiom rather than @registerDelay@ so it works on the
non-threaded RTS too; an interrupted poll aborts the STM transaction, consuming
nothing.
-}
newBoundedInMemoryQueue ::
    -- | The depth cap (and any future knobs).
    MemoryQueueConfig ->
    {- | Invoked on each report-worthy cap-overflow drop with the running total drops,
    so the composition root can log it (and, once the @ecluse.mirror.*@ metric
    catalogue lands, increment a drop counter alongside).
    -}
    (Int -> IO ()) ->
    IO MirrorQueue
newBoundedInMemoryQueue cfg onDrop = do
    -- A capacity of at least one: the config layer enforces a positive cap, but guard
    -- so a directly-constructed queue can never be the degenerate always-full zero.
    queue <- newTBQueueIO (fromIntegral (max 1 (memQueueMaxDepth cfg)))
    dropCount <- newTVarIO (0 :: Int)
    nextReceipt <- newTVarIO (0 :: Word64)
    pure
        MirrorQueue
            { enqueue = \job -> do
                dropped <- atomically (writeOrDrop queue dropCount job)
                whenJust dropped (\n -> when (shouldReportDrop n) (onDrop n))
                -- A cap overflow is the documented drop-newest shed (reported through
                -- the callback), not a backend fault: the enqueue itself worked.
                pure (Right ())
            , -- A bounded long-poll: wait up to the poll window for a batch, else return
              -- [] so the worker's heartbeat keeps advancing on an idle queue. The
              -- timeout aborts the blocked STM transaction, so no job is consumed.
              receive = Right . fromMaybe [] <$> timeout (memQueuePollWaitMicros cfg) (atomically (receiveBatch queue nextReceipt))
            , -- A delivered job is already gone from the queue, so there is nothing to
              -- retire and a failed job redelivers via the next demand, not here.
              ack = const (pure (Right ()))
            , extendVisibility = \_ _ -> pure (Right ())
            }

-- Report the first drop, then every interval-th, so the first shed is always
-- visible while a sustained flood is rate-limited.
shouldReportDrop :: Int -> Bool
shouldReportDrop n = reportWorthy n memoryQueueDropReportInterval

{- | Whether the @n@-th event in a rate-limited series should be reported: the first
(@n == 1@), then every @interval@-th. Shared by the bounded queue's drop reporting and
the composition root's enqueue-buffer reporting so the two cannot drift.
-}
reportWorthy :: Int -> Int -> Bool
reportWorthy n interval = n == 1 || n `mod` interval == 0

{- Take a bounded batch within one STM transaction: block (retry) until at least one
job is available, then drain up to 'memoryQueueBatchSize' total without blocking. The
caller bounds the initial block with a timeout (so an idle queue yields @[]@ rather
than hanging the worker); if that timeout fires, this transaction is aborted and
consumes nothing. Each delivery is assigned a fresh receipt from a monotonic counter
so messages stay distinct, even though 'ack' on this backend is a no-op. -}
receiveBatch :: TBQueue MirrorJob -> TVar Word64 -> STM [QueueMessage]
receiveBatch queue nextReceipt = do
    headJob <- readTBQueue queue
    rest <- drainUpTo (memoryQueueBatchSize - 1)
    traverse assignReceipt (headJob : rest)
  where
    drainUpTo :: Int -> STM [MirrorJob]
    drainUpTo budget
        | budget <= 0 = pure []
        | otherwise =
            tryReadTBQueue queue >>= \case
                Nothing -> pure []
                Just job -> (job :) <$> drainUpTo (budget - 1)

    assignReceipt :: MirrorJob -> STM QueueMessage
    assignReceipt job = do
        n <- readTVar nextReceipt
        writeTVar nextReceipt (n + 1)
        pure QueueMessage{msgJob = job, msgReceipt = mkReceiptHandle (show n)}

{- Hand a job to a bounded queue within the caller's transaction: write it when
there is room, or drop it at the cap (drop-newest) and return the incremented
running drop total for the caller's report policy. Dropping rather than blocking
keeps the producer non-blocking, and the loss is safe: a dropped job is
re-enqueued on the next demand for its artifact. -}
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
artifact -- the same argument 'newBoundedInMemoryQueue' makes):

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
