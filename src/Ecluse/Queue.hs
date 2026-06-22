{- | The mirror-queue seam: the durable hand-off from the request path to the
mirror worker.

Mirroring is __demand-driven__: when a client fetches an artifact whose version
passes the rules, the proxy 'enqueue's a 'MirrorJob' and serves the artifact
immediately, never blocking on the mirror. A separate worker 'receive's jobs,
fetches and verifies the artifact, publishes it to the mirror target, and 'ack's
the job (see @docs\/architecture\/cloud-backends.md@ → "Mirror Queue").

The queue is the one cloud surface with materially different APIs per provider
(AWS SQS @SendMessage@\/@ReceiveMessage@+visibility-timeout\/@DeleteMessage@; GCP
Pub\/Sub @Publish@\/@Pull@+ack-deadline\/@Acknowledge@), so it is its own seam —
a __record of functions__ (the Handle pattern). Both providers fit the same
receive → process → ack shape; their differences (visibility timeout vs ack
deadline, batch limits, dead-letter wiring) stay behind the seam, and
'ReceiptHandle' is opaque so neither leaks.

Like the other seams, the effectful fields return __'IO', not @App@__, so an
adapter stays decoupled from the proxy's @Env@\/@App@ (see
@docs\/architecture\/technology-stack.md@ → "Key Decisions").

== Conventions

The two cloud backends both give __at-least-once delivery__, which is safe here
because publishing is idempotent (a registry treats versions as immutable). The
seam's contract reflects that:

* __'enqueue' is best-effort.__ It runs on the request hot path (enqueue, then
  serve immediately), so a failure must be logged\/metered and __never fail the
  client response__ — the artifact is already served, and a later pull
  re-enqueues.
* __Retry is "don't 'ack'".__ A job that fails processing is simply not acked;
  the visibility timeout \/ ack deadline redelivers it, and the backend's native
  dead-letter path catches the persistently failing ones. There is deliberately
  __no @nack@__.
* __'extendVisibility'__ lets the worker hold a long publish (a large artifact)
  past the visibility window. It is an /optimization/, not correctness-critical,
  since idempotency already makes redelivery harmless.

This module provides the seam and its payload types. 'newInMemoryQueue' is an
STM-backed in-memory implementation honouring the receive → ack \/
redeliver-on-no-ack semantics above.
-}
module Ecluse.Queue (
    -- * Queue seam
    MirrorQueue (..),

    -- * Payloads
    MirrorJob (..),
    QueueMessage (..),

    -- * Opaque receipt
    ReceiptHandle,

    -- * Durations
    Seconds (..),

    -- * In-memory double
    newInMemoryQueue,
) where

import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Ecluse.Package (PackageName)
import Ecluse.Version (Version)

{- | A mirror job: everything the worker needs to back-fill one artifact into the
mirror target. The version was already gated by the rules at serve time (when
the job was enqueued), so the worker does not re-run the rules; it fetches,
verifies the bytes against the artifact's integrity hash, and publishes.
-}
data MirrorJob = MirrorJob
    { jobPackage :: PackageName
    -- ^ The package whose artifact is being mirrored.
    , jobVersion :: Version
    -- ^ The specific version to mirror.
    , jobArtifactUrl :: Text
    -- ^ Where to fetch the artifact bytes from (the public upstream).
    , jobMirrorTarget :: Text
    -- ^ The mirror-target endpoint the artifact is published to.
    }
    deriving stock (Eq, Show)

{- | An __opaque__ handle identifying a received message for 'ack' \/
'extendVisibility'. It abstracts an SQS receipt handle or a Pub\/Sub @ackId@; the
constructor is hidden so neither provider's representation leaks into worker
code, and a handle is only ever obtained from a 'QueueMessage' returned by
'receive'.
-}
newtype ReceiptHandle = ReceiptHandle Word64
    deriving stock (Eq, Ord, Show)

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

{- | The mirror-queue seam — a record of functions over a backend whose private
state the closures capture. See the module header for the @enqueue@ /
don't-@ack@-to-retry / no-@nack@ conventions; all fields are 'IO'.
-}
data MirrorQueue = MirrorQueue
    { enqueue :: MirrorJob -> IO ()
    -- ^ Producer. __Best-effort__: runs on the request hot path, so a failure is
    -- logged\/metered and never fails the client response (see the header).
    , receive :: IO [QueueMessage]
    -- ^ Consumer. One long-poll for a batch of messages; returns @[]@ on timeout
    -- (an empty poll), so the worker loop simply polls again.
    , ack :: ReceiptHandle -> IO ()
    -- ^ Acknowledge a processed message so it is not redelivered. __Not__ acking
    -- is how a failed job is retried (the header's "retry is don't ack").
    , extendVisibility :: ReceiptHandle -> Seconds -> IO ()
    -- ^ Extend a received message's visibility window to hold a long publish. An
    -- optimization, not correctness-critical (redelivery is harmless).
    }

-- ── in-memory double ─────────────────────────────────────────────────────────

{- | The mutable state of the in-memory queue.

Modelled as visible (waiting) jobs plus in-flight (received-but-unacked) ones,
exactly mirroring the visibility-timeout model the cloud backends use: a 'receive'
makes visible jobs in-flight, an 'ack' drops an in-flight job, and an unacked
in-flight job becomes visible again — redelivered — on a subsequent 'receive'.
-}
data QueueState = QueueState
    { qsNextReceipt :: Word64
    -- ^ A monotonic counter giving each delivery a unique 'ReceiptHandle'.
    , qsVisible :: Seq MirrorJob
    -- ^ Jobs waiting to be delivered, oldest first (FIFO). 'Seq' gives
    -- O(1) amortised snoc so enqueue cost does not grow with queue depth.
    , qsInFlight :: Map ReceiptHandle InFlight
    -- ^ Delivered-but-unacked jobs, keyed by the handle used to 'ack' them.
    }

{- | One in-flight job and whether its visibility has been extended.

A held ('inFlightHeld' = 'True') job survives one reclaim pass (the effect of
'extendVisibility'); otherwise an in-flight job is reclaimed — made visible again
for redelivery — on the next 'receive', modelling expiry of the visibility
window.
-}
data InFlight = InFlight
    { inFlightJob :: MirrorJob
    -- ^ The job awaiting acknowledgement.
    , inFlightHeld :: Bool
    -- ^ Whether 'extendVisibility' has held it past the next reclaim.
    }

{- | Build a fresh STM-backed in-memory 'MirrorQueue'.

Honours the seam's contract: 'enqueue' appends (FIFO), 'receive' delivers all
currently-visible jobs and moves them in-flight, 'ack' removes an in-flight job,
and an in-flight job that is never acked is __redelivered__ on the next 'receive'
("retry is don't ack"). 'extendVisibility' holds a job in-flight across one such
redelivery pass. This is a test double — there is no long-poll blocking; an empty
'receive' returns @[]@ at once.
-}
newInMemoryQueue :: IO MirrorQueue
newInMemoryQueue = do
    stateVar <- newTVarIO (QueueState 0 Seq.empty mempty)
    let modifyState :: (QueueState -> QueueState) -> IO ()
        modifyState = atomically . modifyTVar' stateVar
    pure
        MirrorQueue
            { enqueue = modifyState . enqueueJob
            , receive = atomically $ do
                qs <- readTVar stateVar
                let (messages, qs') = deliver qs
                writeTVar stateVar qs'
                pure messages
            , ack = modifyState . ackJob
            , extendVisibility = \handle _seconds -> modifyState (holdJob handle)
            }
  where
    -- Append a job to the back of the visible queue (FIFO). O(1) amortised.
    enqueueJob :: MirrorJob -> QueueState -> QueueState
    enqueueJob job qs = qs{qsVisible = qsVisible qs <> Seq.singleton job}

    -- Drop an acked in-flight job; a handle that is unknown (already acked, or
    -- never issued) is a harmless no-op.
    ackJob :: ReceiptHandle -> QueueState -> QueueState
    ackJob handle qs = qs{qsInFlight = Map.delete handle (qsInFlight qs)}

    -- Hold an in-flight job past the next reclaim pass. Unknown handle: no-op.
    holdJob :: ReceiptHandle -> QueueState -> QueueState
    holdJob handle qs =
        qs{qsInFlight = Map.adjust (\f -> f{inFlightHeld = True}) handle (qsInFlight qs)}

    {- A single receive. First reclaim any un-held in-flight jobs (their
    visibility window has lapsed) back to the front of the visible queue, and
    clear the held flag on the rest so they are reclaimed next time unless held
    again. Then deliver every visible job: assign each a fresh receipt, move it
    in-flight, and return it as a 'QueueMessage'. -}
    deliver :: QueueState -> ([QueueMessage], QueueState)
    deliver qs =
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
        [(ReceiptHandle, InFlight)] ->
        ([MirrorJob], [(ReceiptHandle, InFlight)])
    reclaim = foldr step ([], [])
      where
        step (handle, f) (jobs, held)
            | inFlightHeld f = (jobs, (handle, f{inFlightHeld = False}) : held)
            | otherwise = (inFlightJob f : jobs, held)

    {- Give each job a fresh receipt, threading the monotonic counter. Returns
    the messages, the next free counter value, and the new in-flight entries. -}
    assignReceipts ::
        Word64 ->
        [MirrorJob] ->
        ([QueueMessage], Word64, Map ReceiptHandle InFlight)
    assignReceipts next [] = ([], next, mempty)
    assignReceipts next (job : rest) =
        let handle = ReceiptHandle next
            message = QueueMessage{msgJob = job, msgReceipt = handle}
            (messages, next', inFlight) = assignReceipts (next + 1) rest
         in ( message : messages
            , next'
            , Map.insert handle (InFlight{inFlightJob = job, inFlightHeld = False}) inFlight
            )
