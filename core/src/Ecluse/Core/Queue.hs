-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror-queue handle: the durable hand-off from the request path to the mirror worker.

Mirroring is demand-driven, so the serve path 'enqueue's a 'MirrorJob' and answers at once
while a worker 'receive's it, verifies the artifact, publishes it and 'ack's. At-least-once
delivery is safe because publishing is idempotent, which is why retry is "do not 'ack'" and
there is no @nack@. This cloud surface is the one whose API differs materially per provider,
so it is a record of functions and 'ReceiptHandle' is opaque. See
@docs\/architecture\/cloud-backends.md@, "Mirror Queue".
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

    -- * Durations and the receipt lease
    Seconds (..),
    ReceiptLease (..),

    -- * Dead-letter terminus and the redelivery budget
    DeadLetterTerminus (..),
    DeliveryBudget (..),
    defaultDeliveryBudget,
    effectiveDeliveryBudget,
    retiringDelivery,
    deliveryBudgetSpent,
) where

import Data.Aeson (eitherDecodeStrict', object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName, parseEcosystem)
import Ecluse.Core.Fault (TransportCause (TransportProtocol), TransportFault, transportFault)
import Ecluse.Core.Package (PackageName, pkgEcosystem, pkgNamespace, unScope, unscopedName)
import Ecluse.Core.Queue.Lease (ReceiptLease (..), Seconds (..))
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Server.Path (Filename, mkFilename, unFilename)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)

{- | Everything the worker needs to back-fill one artifact into the mirror target. The payload
is a __trust boundary__: it carries selection keys and never authority, so no digest, no size.
-}
data MirrorJob = MirrorJob
    { jobPackage :: PackageName
    -- ^ The package whose artifact the worker mirrors.
    , jobVersion :: Version
    -- ^ The specific version to mirror.
    , jobArtifactUrl :: RegistryUrl
    {- ^ Where the worker fetches the artifact bytes from. A wire decode re-forms the validated
    https egress witness rather than trusting the payload's text.
    -}
    , jobArtifactFilename :: Filename
    {- ^ The serve-time-admitted artifact's filename: a selection key the admission gate
    cross-checks against current metadata, not authority.
    -}
    , jobTraceContext :: Maybe RemoteSpanContext
    {- ^ The trace context of the span that enqueued the job, so the worker's per-job span links
    back across the asynchronous hop. 'Nothing' when the producer carried none.
    -}
    }
    deriving stock (Eq, Show)

{- | The @traceparent@ and @tracestate@ of the enqueueing span, verbatim. The queue never parses
them, so an unparseable carrier yields no span link rather than a decode failure.
-}
data RemoteSpanContext = RemoteSpanContext
    { rscTraceparent :: Text
    -- ^ The W3C @traceparent@ header value of the enqueueing span.
    , rscTracestate :: Text
    -- ^ The W3C @tracestate@ value (possibly empty), so vendor trace state survives the hop.
    }
    deriving stock (Eq, Show)

{- | Encode a 'MirrorJob' as the JSON text of a queue message body, the inverse of 'decodeJob'. The
identity rides as a namespace and a base name, so a namespaced name round-trips on any ecosystem.
-}
encodeJob :: MirrorJob -> Text
encodeJob job =
    decodeUtf8 . Aeson.encode $
        object
            [ "ecosystem" .= ecosystemName (pkgEcosystem (jobPackage job))
            , "namespace" .= (unScope <$> pkgNamespace (jobPackage job))
            , "name" .= unscopedName (jobPackage job)
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
the name, the filename, and the artifact URL each go back through their own gate.
-}
decodeJob ::
    -- | Read a wire namespace and base name through the ecosystem's own grammar.
    (Ecosystem -> Maybe Text -> Text -> Either Text PackageName) ->
    -- | Re-form the artifact URL's validated https egress witness.
    (Text -> Either Text RegistryUrl) ->
    Text ->
    Either Text MirrorJob
decodeJob packageName egressUrl body =
    first toText (eitherDecodeStrict' (encodeUtf8 body))
        >>= first toText . parseEither (parseMirrorJob packageName egressUrl)

parseMirrorJob ::
    (Ecosystem -> Maybe Text -> Text -> Either Text PackageName) ->
    (Text -> Either Text RegistryUrl) ->
    Aeson.Value ->
    Parser MirrorJob
parseMirrorJob packageName egressUrl = withObject "MirrorJob" $ \o -> do
    ecoName <- o .: "ecosystem"
    eco <- maybe (fail (unusable "ecosystem" ecoName)) pure (parseEcosystem ecoName)
    rawNamespace <- o .:? "namespace"
    rawName <- o .: "name"
    -- The payload states the namespace and the base name separately, so the ecosystem re-joins
    -- and re-reads them rather than either field being trusted as given.
    package <- either (fail . toString) pure (packageName eco rawNamespace rawName)
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

-- The refusal a trust-boundary field reports when its own gate rejects the payload's text.
-- 'String' because that is what aeson's 'fail' takes.
unusable :: String -> Text -> String
unusable field value = "unusable " <> field <> " " <> show value

-- The carrier is untrusted opaque transport, so both fields are taken as-is. An unparseable
-- W3C value yields no link in the tracing port rather than failing the decode.
parseTraceContext :: Aeson.Value -> Parser RemoteSpanContext
parseTraceContext = withObject "RemoteSpanContext" $ \t ->
    RemoteSpanContext <$> t .: "traceparent" <*> t .: "tracestate"

{- | The backend's own delivery token (an SQS receipt handle, a Pub\/Sub @ackId@). The
constructor is hidden, so worker code only takes one from a 'QueueMessage' that 'receive' returned.
-}
newtype ReceiptHandle = ReceiptHandle Text
    deriving stock (Eq, Ord, Show)

-- | Wrap a backend's delivery token. For backend implementations only.
mkReceiptHandle :: Text -> ReceiptHandle
mkReceiptHandle = ReceiptHandle

-- | Recover a backend's delivery token to pass back to it. For backend implementations only.
unReceiptHandle :: ReceiptHandle -> Text
unReceiptHandle (ReceiptHandle t) = t

-- | A received message: the job to process and the handle that settles this delivery.
data QueueMessage = QueueMessage
    { msgJob :: MirrorJob
    -- ^ The job to process.
    , msgReceipt :: ReceiptHandle
    -- ^ The handle identifying this delivery, for 'ack' \/ 'extendVisibility'.
    , msgReceiveCount :: Int
    {- ^ Deliveries of this message including this one, @1@ on a first delivery. A backend that
    cannot report a count reports @1@, so only evidence puts a delivery past the 'deliveryBudget'.
    -}
    , msgLease :: Maybe ReceiptLease
    {- ^ How long this delivery stays hidden, for the worker's renewal controller.
    'Nothing' from a backend that never expires a delivery.
    -}
    }
    deriving stock (Eq, Show)

{- | How many deliveries of one message a queue grants before the worker itself retires it.
A 'newtype', so no caller confuses a count of receives with some other 'Int'.
-}
newtype DeliveryBudget = DeliveryBudget Int
    deriving stock (Eq, Ord, Show)

{- | The redelivery budget a backend holds when the operator configures none: five deliveries,
SQS's own redrive convention, pinned to this same value in @config\/default.yaml@.
-}
defaultDeliveryBudget :: DeliveryBudget
defaultDeliveryBudget = DeliveryBudget 5

{- | Whether a queue has somewhere that captures a message the worker can never mirror. Without
one the message cycles until the queue's retention window discards it unseen.
-}
data DeadLetterTerminus
    = -- | A terminus captures poison messages, at this capture count when the backend reports one.
      TerminusAttached (Maybe DeliveryBudget)
    | -- | Nothing captures poison messages: the worker's budget is the only terminus.
      TerminusAbsent
    deriving stock (Eq, Show)

{- | The budget the worker enforces: the configured floor, raised past an attached terminus's
capture count, so the dead-letter queue always captures a poison message first.
-}
effectiveDeliveryBudget :: DeliveryBudget -> DeadLetterTerminus -> DeliveryBudget
effectiveDeliveryBudget configured = \case
    TerminusAttached (Just (DeliveryBudget captureAt)) -> max configured (DeliveryBudget (captureAt + 1))
    TerminusAttached Nothing -> configured
    TerminusAbsent -> configured

{- | The delivery a budget retires on: the configured value, floored at two, so a first delivery
never spends it. The verdict and the worker's alarm read this one number, so they cannot disagree.
-}
retiringDelivery :: DeliveryBudget -> Int
retiringDelivery (DeliveryBudget budget) = max 2 budget

{- | Whether this delivery spends the queue's redelivery budget. A backend supplies the count
('msgReceiveCount'), never the verdict.
-}
deliveryBudgetSpent :: DeliveryBudget -> QueueMessage -> Bool
deliveryBudgetSpent budget message = msgReceiveCount message >= retiringDelivery budget

{- | The mirror-queue handle: a record of functions over a backend whose state the closures
capture. Every operation reports failure as an 'Ecluse.Core.Fault.TransportFault' value.
-}
data MirrorQueue = MirrorQueue
    { enqueue :: MirrorJob -> IO (Either TransportFault ())
    {- ^ Producer. Best-effort: the caller logs a 'Left' and never fails the client response,
    since a later pull re-enqueues.
    -}
    , receive :: IO (Either TransportFault [QueueMessage])
    {- ^ Consumer. One long-poll for a batch, @Right []@ on a healthy empty poll. A 'Left' does
    not advance the liveness heartbeat, so a persistently failing backend surfaces at @\/livez@.
    -}
    , ack :: ReceiptHandle -> IO (Either TransportFault ())
    {- ^ Acknowledge a processed message. The caller logs a 'Left' and absorbs it, since
    idempotent publishing makes the repeat harmless.
    -}
    , extendVisibility :: ReceiptHandle -> Seconds -> IO (Either TransportFault ())
    {- ^ Reset a received message's visibility window: renew the worker's lease on it, or at
    zero release it for an immediate redelivery.
    -}
    , deadLetter :: ReceiptHandle -> IO (Either TransportFault ())
    {- ^ Realise a terminal fault, routed to the backend's own dead-letter terminus. The caller
    logs a 'Left' and absorbs it, like 'ack'.
    -}
    , deliveryBudget :: DeliveryBudget
    {- ^ The budget 'deliveryBudgetSpent' judges against, settled once at construction with
    'effectiveDeliveryBudget' so judging a delivery costs no per-message work.
    -}
    , deadLetterTerminus :: Either TransportFault DeadLetterTerminus
    {- ^ What the backend's dead-letter probe found at construction, or the fault that stopped
    it. A 'Left' leaves the configured budget standing and never blocks boot.
    -}
    }

{- | The inert queue a deployment with zero mirroring mounts carries, so the composition-root
'Env' keeps its shape. Reached anyway, 'enqueue' refuses with a typed fault rather than crashing.
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
