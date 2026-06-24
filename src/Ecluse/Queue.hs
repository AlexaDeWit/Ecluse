{- | The mirror-queue handle: the durable hand-off from the request path to the
mirror worker.

Mirroring is __demand-driven__: when a client fetches an artifact whose version
passes the rules, the proxy 'enqueue's a 'MirrorJob' and serves the artifact
immediately, never blocking on the mirror. A separate worker 'receive's jobs,
fetches and verifies the artifact, publishes it to the mirror target, and 'ack's
the job (see @docs\/architecture\/cloud-backends.md@ → "Mirror Queue").

The queue is the one cloud surface with materially different APIs per provider
(AWS SQS @SendMessage@\/@ReceiveMessage@+visibility-timeout\/@DeleteMessage@; GCP
Pub\/Sub @Publish@\/@Pull@+ack-deadline\/@Acknowledge@), so it is its own handle —
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
  client response__ — the artifact is already served, and a later pull
  re-enqueues.
* __Retry is "don't 'ack'".__ A job that fails processing is simply not acked;
  the visibility timeout \/ ack deadline redelivers it, and the backend's native
  dead-letter path catches the persistently failing ones. There is deliberately
  __no @nack@__.
* __'extendVisibility'__ lets the worker hold a long publish (a large artifact)
  past the visibility window. It is an /optimization/, not correctness-critical,
  since idempotency already makes redelivery harmless.

This module provides the handle and its payload types. 'newInMemoryQueue' is an
STM-backed in-memory implementation honouring the receive → ack \/
redeliver-on-no-ack semantics above.
-}
module Ecluse.Queue (
    -- * Queue handle
    MirrorQueue (..),

    -- * Payloads
    MirrorJob (..),
    MirrorArtifact (..),
    QueueMessage (..),

    -- * Opaque receipt
    ReceiptHandle,
    mkReceiptHandle,
    unReceiptHandle,

    -- * Durations
    Seconds (..),

    -- * In-memory double
    newInMemoryQueue,
) where

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq

import Ecluse.Package (Hash, PackageName)
import Ecluse.Version (Version)

{- | A mirror job: everything the worker needs to back-fill one artifact into the
mirror target. The version was already gated by the rules at serve time (when
the job was enqueued), so the worker does not re-run the rules; it fetches the
bytes, verifies them against the __serve-time-admitted__ integrity digest the job
carries, and publishes.

The integrity digest and the artifact descriptor are captured __at enqueue time__
('jobArtifact'), not re-fetched: the worker mirrors exactly what the rules
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
    , jobArtifactUrl :: Text
    -- ^ Where to fetch the artifact bytes from (the public upstream).
    , jobMirrorTarget :: Text
    -- ^ The mirror-target endpoint the artifact is published to.
    , jobArtifact :: MirrorArtifact
    {- ^ The serve-time-admitted artifact descriptor: the integrity digest the
    fetched bytes are verified against, plus the filename and declared size the
    publish document is assembled from.
    -}
    }
    deriving stock (Eq, Show)

{- | The serve-time-admitted artifact descriptor carried on a 'MirrorJob': exactly
the fields the worker needs to verify the fetched bytes and assemble the publish
document, captured when the version was gated.

'maHashes' is a 'NonEmpty' because the serve path admits a public version only when
it carries at least one integrity digest (the integrity-presence admission policy),
so a job with __no__ digest to verify against is unrepresentable — the worker always
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
    -- ^ The declared artifact size in bytes, if the registry reported it.
    }
    deriving stock (Eq, Show)

{- | An __opaque__ handle identifying a received message for 'ack' \/
'extendVisibility'. It carries the backend's own delivery token — an SQS receipt
handle or a Pub\/Sub @ackId@ — as text; the constructor is hidden so neither
provider's representation leaks into worker code, and a handle is only ever
obtained from a 'QueueMessage' returned by 'receive'. Build one (in a backend)
with 'mkReceiptHandle' and read the token back with 'unReceiptHandle'.
-}
newtype ReceiptHandle = ReceiptHandle Text
    deriving stock (Eq, Ord, Show)

{- | Wrap a backend's delivery token (an SQS receipt handle, a Pub\/Sub @ackId@)
as an opaque 'ReceiptHandle'. For backend implementations only — worker code
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

{- | The mirror-queue handle — a record of functions over a backend whose private
state the closures capture. See the module header for the @enqueue@ /
don't-@ack@-to-retry / no-@nack@ conventions; all fields are 'IO'.
-}
data MirrorQueue = MirrorQueue
    { enqueue :: MirrorJob -> IO ()
    {- ^ Producer. __Best-effort__: runs on the request hot path, so a failure is
    logged\/metered and never fails the client response (see the header).
    -}
    , receive :: IO [QueueMessage]
    {- ^ Consumer. One long-poll for a batch of messages; returns @[]@ on timeout
    (an empty poll), so the worker loop simply polls again.
    -}
    , ack :: ReceiptHandle -> IO ()
    {- ^ Acknowledge a processed message so it is not redelivered. __Not__ acking
    is how a failed job is retried (the header's "retry is don't ack").
    -}
    , extendVisibility :: ReceiptHandle -> Seconds -> IO ()
    {- ^ Extend a received message's visibility window to hold a long publish. An
    optimization, not correctness-critical (redelivery is harmless).
    -}
    }

-- ── in-memory double ─────────────────────────────────────────────────────────

{- The mutable state of the in-memory queue.

Modelled as visible (waiting) jobs plus in-flight (received-but-unacked) ones,
exactly mirroring the visibility-timeout model the cloud backends use: a 'receive'
makes visible jobs in-flight, an 'ack' drops an in-flight job, and an unacked
in-flight job becomes visible again — redelivered — on a subsequent 'receive'.
-}
data QueueState = QueueState
    { -- A monotonic counter giving each delivery a unique 'ReceiptHandle'.
      qsNextReceipt :: Word64
    , -- Jobs waiting to be delivered, oldest first (FIFO). 'Seq' gives
      -- O(1) amortised snoc so enqueue cost does not grow with queue depth.
      qsVisible :: Seq MirrorJob
    , -- Delivered-but-unacked jobs, keyed by the numeric receipt counter (not the
      -- rendered 'ReceiptHandle' text) so iteration stays in delivery — hence
      -- FIFO-reclaim — order rather than the lexicographic order text keys give.
      qsInFlight :: Map Word64 InFlight
    }

{- One in-flight job and whether its visibility has been extended.

A held ('inFlightHeld' = 'True') job survives one reclaim pass (the effect of
'extendVisibility'); otherwise an in-flight job is reclaimed — made visible again
for redelivery — on the next 'receive', modelling expiry of the visibility
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
