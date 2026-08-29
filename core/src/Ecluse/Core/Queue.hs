-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror-queue handle: the durable hand-off from the request path to the mirror worker.
Mirroring is demand-driven, so the serve path 'enqueue's a 'MirrorJob' and answers at once while
a worker 'receive's it, verifies the artifact, publishes it, and 'ack's. The queue is the one
cloud surface whose API differs materially per provider, so it is a record of functions (the
Handle pattern) and 'ReceiptHandle' is opaque. At-least-once delivery is safe because publishing
is idempotent, which is why retry is "do not 'ack'" and there is no @nack@. 'deadLetter' is the
third, terminal outcome, and 'deliveryBudget' is the backstop when nothing else captures a poison
message. See @docs\/architecture\/cloud-backends.md@ → "Mirror Queue".
-}
module Ecluse.Core.Queue (
    -- * Queue handle
    MirrorQueue (..),
    noMirrorQueue,

    -- * Payloads
    MirrorJob (..),
    RemoteSpanContext (..),
    QueueMessage (..),

    -- * The payload's wire mapping
    encodeJob,
    decodeJob,

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
import Data.Aeson (eitherDecodeStrict', object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName, parseEcosystem)
import Ecluse.Core.Fault (TransportCause (TransportProtocol), TransportFault, tfDetail, transportFault)
import Ecluse.Core.Package (PackageName, pkgEcosystem, renderPackageName)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Server.Path (Filename, mkFilename, unFilename)
import Ecluse.Core.Supervision (BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros), backoffMicros)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)

{- | A mirror job: everything the worker needs to back-fill one artifact into the mirror
target.

The queue payload is a trust boundary, so it carries selection keys and never authority: no
digest, no size. The worker re-evaluates current policy through the shared admission oracle
(see "Ecluse.Core.Worker.Job") and derives the tamper gate's descriptor
('Ecluse.Core.Registry.MirrorArtifact') from the artifact that re-evaluation re-admits.
-}
data MirrorJob = MirrorJob
    { jobPackage :: PackageName
    -- ^ The package whose artifact the worker mirrors.
    , jobVersion :: Version
    -- ^ The specific version to mirror.
    , jobArtifactUrl :: RegistryUrl
    {- ^ Where the worker fetches the artifact bytes from (the public upstream). The SQS wire
    decode re-forms the validated https egress witness, since the queue payload is a trust
    boundary.
    -}
    , jobArtifactFilename :: Filename
    {- ^ The serve-time-admitted artifact's filename: a selection key, not authority. The shared
    admission gate cross-checks it against current metadata rather than trusting it.
    -}
    , jobTraceContext :: Maybe RemoteSpanContext
    {- ^ The trace context of the serve-time span that enqueued the job, so the worker's
    per-job span links back across the asynchronous hop. 'Nothing' when tracing was off at
    enqueue time, or when the producer carried none.
    -}
    }
    deriving stock (Eq, Show)

{- | A serialised W3C trace-context carrier riding on a 'MirrorJob': the @traceparent@ and
@tracestate@ of the enqueueing span, verbatim. The worker's tracing port reads it back to
link the per-job span to the enqueueing request. The queue never parses the values, so an
unparseable carrier yields no link.
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

{- | Encode a 'MirrorJob' as the JSON text of a queue message body, the inverse of 'decodeJob'.
The package name rides in its own ecosystem's wire form, so no backend re-encodes a namespace
split of its own.
-}
encodeJob :: MirrorJob -> Text
encodeJob job =
    decodeUtf8 . Aeson.encode $
        object
            [ "ecosystem" .= ecosystemName (pkgEcosystem (jobPackage job))
            , "name" .= renderPackageName (jobPackage job)
            , "version" .= renderVersion (jobVersion job)
            , "artifactUrl" .= registryUrlText (jobArtifactUrl job)
            , "filename" .= unFilename (jobArtifactFilename job)
            , "traceContext" .= (encodeTraceContext <$> jobTraceContext job)
            ]

-- The W3C traceparent and tracestate verbatim, so the worker can re-establish the
-- cross-async span link. A 'Nothing' carrier round-trips through a JSON null.
encodeTraceContext :: RemoteSpanContext -> Aeson.Value
encodeTraceContext rsc =
    object
        [ "traceparent" .= rscTraceparent rsc
        , "tracestate" .= rscTracestate rsc
        ]

{- | Decode a queue message body back into a 'MirrorJob'. The payload is a __trust boundary__, so
the name, the filename, and the artifact URL each go back through their own gate rather than
being taken as given.
-}
decodeJob ::
    -- | Read a wire name through its ecosystem's own grammar.
    (Ecosystem -> Text -> Either Text PackageName) ->
    -- | Re-form the artifact URL's validated https egress witness.
    (Text -> Either Text RegistryUrl) ->
    Text ->
    Either Text MirrorJob
decodeJob packageName egressUrl body =
    first toText (eitherDecodeStrict' (encodeUtf8 body))
        >>= first toText . parseEither (parseMirrorJob packageName egressUrl)

-- Parse the top-level job object 'encodeJob' writes, delegating the nested trace-context
-- carrier to 'parseTraceContext'.
parseMirrorJob ::
    (Ecosystem -> Text -> Either Text PackageName) ->
    (Text -> Either Text RegistryUrl) ->
    Aeson.Value ->
    Parser MirrorJob
parseMirrorJob packageName egressUrl = withObject "MirrorJob" $ \o -> do
    ecoName <- o .: "ecosystem"
    eco <- maybe (fail (unusable "ecosystem" ecoName)) pure (parseEcosystem ecoName)
    rawName <- o .: "name"
    package <- either (fail . toString) pure (packageName eco rawName)
    rawVersion <- o .: "version"
    rawArtifactUrl <- o .: "artifactUrl"
    -- The type the worker's fetch requires cannot be fabricated from an unvalidated string.
    artifactUrl <- either (fail . toString) pure (egressUrl rawArtifactUrl)
    rawFilename <- o .: "filename"
    -- The filename is interpolated into an upstream path, so it is refused here unless it is a
    -- safe path component.
    filename <- maybe (fail (unusable "artifact filename" rawFilename)) pure (mkFilename rawFilename)
    -- A job enqueued with tracing off carries no "traceContext". It yields no span link
    -- rather than a decode failure.
    traceContext <- o .:? "traceContext" >>= traverse parseTraceContext
    pure
        MirrorJob
            { jobPackage = package
            , jobVersion = mkVersion eco rawVersion
            , jobArtifactUrl = artifactUrl
            , jobArtifactFilename = filename
            , jobTraceContext = traceContext
            }
  where
    unusable field value = "unusable " <> field <> " " <> show (value :: Text)

-- The carrier is untrusted opaque transport, so both fields are taken as-is. An unparseable
-- W3C value yields no link in the tracing port rather than failing the decode.
parseTraceContext :: Aeson.Value -> Parser RemoteSpanContext
parseTraceContext = withObject "RemoteSpanContext" $ \t ->
    RemoteSpanContext <$> t .: "traceparent" <*> t .: "tracestate"

{- | An opaque handle identifying a received message for 'ack' \/ 'extendVisibility': the
backend's own delivery token (an SQS receipt handle, a Pub\/Sub @ackId@) as text. The
constructor is hidden, so worker code only ever takes a handle from a 'QueueMessage' that
'receive' returned.
-}
newtype ReceiptHandle = ReceiptHandle Text
    deriving stock (Eq, Ord, Show)

{- | Wrap a backend's delivery token as an opaque 'ReceiptHandle'. For backend
implementations only: worker code takes handles from 'receive' and never builds them.
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
    {- ^ The number of deliveries of this message, counting this one: @1@ on a first delivery.
    A backend that cannot report a count reports @1@, so only evidence puts a delivery past the
    'deliveryBudget'.
    -}
    }
    deriving stock (Eq, Show)

{- | A duration in whole seconds, for 'extendVisibility'. A 'newtype', so a raw
@Int@ of seconds cannot pass for some other count.
-}
newtype Seconds = Seconds Int
    deriving stock (Eq, Ord, Show)

{- | How many deliveries of one message a queue grants before the worker itself
retires it. A 'newtype', so no caller confuses a count of receives with some other
'Int'.
-}
newtype DeliveryBudget = DeliveryBudget Int
    deriving stock (Eq, Ord, Show)

{- | The redelivery budget a backend holds when the operator configures none: five
deliveries, SQS's own redrive convention. @ECLUSE_QUEUE__MAX_RECEIVE_COUNT@ overrides it,
pinned to this same value in @config\/default.yaml@.
-}
defaultDeliveryBudget :: DeliveryBudget
defaultDeliveryBudget = DeliveryBudget 5

{- | Whether a queue has a __dead-letter terminus__: somewhere that captures a message the
worker can never mirror. Without one the message cycles until the queue's retention window
discards it unseen.

An attached terminus carries its own __capture count__ when the backend can read one: the
delivery at which it takes the message, which the worker's budget stays above so it never
pre-empts the capture.
-}
data DeadLetterTerminus
    = -- | A terminus captures poison messages, at this capture count when it is known.
      TerminusAttached (Maybe DeliveryBudget)
    | -- | Nothing captures poison messages: the worker's budget is the only terminus.
      TerminusAbsent
    deriving stock (Eq, Show)

{- | The budget the worker enforces: the configured floor, raised past an attached
terminus's capture count, so the dead-letter queue always captures a poison message first.
With no terminus, or one whose capture count the backend could not read, the configured
value stands alone.
-}
effectiveDeliveryBudget :: DeliveryBudget -> DeadLetterTerminus -> DeliveryBudget
effectiveDeliveryBudget configured = \case
    TerminusAttached (Just (DeliveryBudget captureAt)) -> max configured (DeliveryBudget (captureAt + 1))
    TerminusAttached Nothing -> configured
    TerminusAbsent -> configured

{- | The delivery a budget retires on: the configured value, floored at two, so a first
delivery never spends the budget. Both the verdict ('deliveryBudgetSpent') and the worker's
alarm read this number, so the number an operator is told is the number that fired.
-}
retiringDelivery :: DeliveryBudget -> Int
retiringDelivery (DeliveryBudget budget) = max 2 budget

{- | Whether this delivery spends the queue's redelivery budget, so the worker retires the
message through its terminal path rather than letting it cycle. A backend supplies the count
('msgReceiveCount'), never the verdict.
-}
deliveryBudgetSpent :: DeliveryBudget -> QueueMessage -> Bool
deliveryBudgetSpent budget message = msgReceiveCount message >= retiringDelivery budget

{- | The mirror-queue handle: a record of functions over a backend whose state the closures
capture. Every operation reports failure as an 'Ecluse.Core.Fault.TransportFault' value.
-}
data MirrorQueue = MirrorQueue
    { enqueue :: MirrorJob -> IO (Either TransportFault ())
    {- ^ Producer. Best-effort: the caller counts and logs a 'Left' and never fails the client
    response, since a later pull re-enqueues.
    -}
    , receive :: IO (Either TransportFault [QueueMessage])
    {- ^ Consumer. One long-poll for a batch, with @Right []@ on timeout: an empty, healthy poll
    the worker simply repeats. A 'Left' is a failed poll and does not advance the liveness
    heartbeat, so a persistently failing backend surfaces through @\/livez@.
    -}
    , ack :: ReceiptHandle -> IO (Either TransportFault ())
    {- ^ Acknowledge a processed message so the backend does not redeliver it. The caller logs
    a 'Left' and absorbs it, since idempotent publishing makes the repeat harmless.
    -}
    , extendVisibility :: ReceiptHandle -> Seconds -> IO (Either TransportFault ())
    {- ^ Extend a received message's visibility window to hold a long publish. An optimisation,
    not correctness-critical, so the caller absorbs a 'Left' silently.
    -}
    , deadLetter :: ReceiptHandle -> IO (Either TransportFault ())
    {- ^ Realise a terminal fault: a job that can never succeed, decided as a verdict at the
    read site. Each backend routes it to its own dead-letter terminus (see the header). The
    caller logs a 'Left' and absorbs it, like 'ack'.
    -}
    , deliveryBudget :: DeliveryBudget
    {- ^ How many deliveries of one message this queue grants before the worker retires it
    ('deliveryBudgetSpent'). The backend settles it once at construction with
    'effectiveDeliveryBudget', so judging a delivery costs no per-message work.
    -}
    , deadLetterTerminus :: Either TransportFault DeadLetterTerminus
    {- ^ What this backend's dead-letter probe found at construction, or the fault that stopped
    the probe. The composition root reads it once to decide its boot warning. A 'Left' leaves
    the configured budget standing and never blocks boot.
    -}
    }

{- | The inert queue a deployment with zero mirroring mounts carries, so the
composition-root 'Env' keeps its total shape without a backend. It is unreachable by
construction: no serve path enqueues on a non-mirroring mount, and no worker runs to poll
it. Reached anyway, 'enqueue' refuses with a typed fault rather than crashing.
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
    inertFault = transportFault TransportProtocol "no mount mirrors, so no mirror queue is built"

{- | Hand a job to a bounded queue inside the caller's transaction. At the cap it drops the newest
job and returns the running drop total, a safe loss because the next demand re-enqueues it.
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

{- | Whether the caller should report the @n@-th event in a rate-limited series: the first, then
every @interval@-th.
-}
reportWorthy :: Int -> Int -> Bool
reportWorthy n interval = n == 1 || n `mod` interval == 0

{- | Wrap a bounded producer-side hand-off buffer in front of a queue, so the serve path's
'enqueue' is an in-process STM write rather than the backend's own producer call. On the SQS
backend that call is an HTTP round trip, which the handler otherwise pays before it returns,
taxing the next request on a keep-alive connection. The returned drain loop delivers buffered
jobs at the backend's own pace, and the consumer operations pass through untouched.

* Drop-newest on overflow, invoking @onDrop@ on every drop with the running drop total. The
caller owns any log rate-limiting.
* A delivery failure invokes @onDeliveryFailure@ with the running failure total and detail,
then backs off before the next job. The failed job does not redeliver here.
* The drain loop never returns, so the caller races it against the services rather than
sequencing it. Cancelling it at shutdown drops any still-buffered jobs.

Every loss is safe, because the next demand for the artifact re-enqueues the job. The wrapped
'enqueue' therefore never fails and is always @Right ()@.
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
            -- 'onDrop' is a best-effort observer on the serve hot path. Guard it so a throwing
            -- observer cannot turn a safe drop into an exception on the client response.
            whenJust dropped (void . tryAny . onDrop)
            -- A drop is the documented safe loss, not a fault, so there is nothing to report.
            pure (Right ())
    pure (backend{enqueue = handOff}, drainLoop buffer failureCount onDeliveryFailure backend)

{- Deliver buffered jobs to the backend, forever. A failed delivery reports through the
best-effort callback and then backs off, so a dead backend is retried at a bounded rate rather
than hot-looped through the whole buffer. That job redelivers on the next demand, not here. -}
drainLoop :: TBQueue MirrorJob -> TVar Int -> (Int -> Text -> IO ()) -> MirrorQueue -> IO ()
drainLoop buffer failureCount onDeliveryFailure backend = go 0
  where
    go consecutiveFailures = do
        job <- atomically (readTBQueue buffer)
        -- Delivery failures arrive as 'TransportFault' values, so this match is total. An
        -- exception escaping here is an invariant break, left to the loop's supervisor.
        enqueue backend job >>= \case
            Right () -> go 0
            Left fault -> do
                n <- atomically (bumpCount failureCount)
                -- 'onDeliveryFailure' is a best-effort observer. Guard it so a throwing
                -- observer can never escape the loop and tear down the composition root.
                void (tryAny (onDeliveryFailure n (tfDetail fault)))
                threadDelay (backoffMicros drainBackoff consecutiveFailures)
                go (consecutiveFailures + 1)

{- The pacing between failed deliveries: from 200ms towards a 30s cap as consecutive failures
mount, so the loop retries a dead backend at most once per cap interval. The supervision
combinator wrapping the loop paces only residue. -}
drainBackoff :: BackoffSchedule
drainBackoff = BackoffSchedule{bsBaseMicros = 200_000, bsCapMicros = 30_000_000}
