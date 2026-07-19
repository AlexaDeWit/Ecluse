-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The AWS SQS backend behind the 'MirrorQueue' handle.

Maps the handle's receive → process → ack shape onto SQS:

* 'enqueue' → @SendMessage@ (the 'MirrorJob' encoded as the message body),
* 'receive' → one long-poll @ReceiveMessage@ (a batch, @[]@ on an empty poll),
* 'ack' → @DeleteMessage@ (the message is gone, never redelivered),
* 'extendVisibility' → @ChangeMessageVisibility@ (hold a long publish),
* 'deadLetter' → @ChangeMessageVisibility@ with the 'sqsTerminalBackoff' window and
  __no @DeleteMessage@__ (a terminal fault rides the redrive policy to the DLQ).

The provider differences SQS embodies -- the visibility timeout, the long-poll
window, the batch limit -- are 'SqsConfig' knobs with sane defaults, and the SQS
receipt handle is carried opaquely in a 'ReceiptHandle' (via 'mkReceiptHandle'),
so none of it leaks past the handle. __Retry is "don't ack"__: a job whose
processing fails transiently is simply not 'ack'ed, and SQS redelivers it once the
visibility timeout lapses; persistent failures fall to the queue's native dead-letter
(max-receive-count), so there is no @nack@ (see "Ecluse.Core.Queue"). A __terminal__
fault ('deadLetter') is returned with a backoff window and never deleted, so it too
falls to the operator's dead-letter queue rather than being discarded -- #933 assumes
that redrive policy exists (the no-DLQ case is issue #935). Every
operation reports its AWS failure as the handle's typed
'Ecluse.Core.Queue.QueueFault' value, classified into the core transport
vocabulary at this edge ("Ecluse.Runtime.Aws.Fault"), so a queue outage never
rides the exception channel through a caller.

The @amazonka@ 'AWS.Env' is built once at 'newSqsQueue' and captured by the
handle's closures, so the backend's state never reaches the proxy's @Env@\/@App@
(see @docs\/architecture\/technology-stack.md@ → "Key Decisions"). The
'MirrorJob' wire mapping is a plain JSON object, decoded on 'receive'; a body that
fails to parse is dropped rather than yielded as a partial, so -- like any message
left unprocessed -- it is not 'ack'ed and SQS redelivers it, ultimately to the
dead-letter queue. Each drop (a missing body or receipt, or an undecodable body) is
logged at 'DebugS' with its reason and the SQS message id when present, so a poison
message is visible rather than cycling silently; the untrusted body is never logged.

The SQS queue is a __trusted, operator-declared destination__ (the configured queue
URL, or an endpoint override): like the OTLP telemetry endpoint (see
"Ecluse.Runtime.Telemetry.Resolve"), it is reached through @amazonka@'s own client and is
__not__ subject to the data-plane egress controls (the host allowlist and the https-only
egress posture of "Ecluse.Core.Security.Egress"), which guard only untrusted package
downloads, never a destination the operator configured.
-}
module Ecluse.Runtime.Queue.Sqs (
    -- * Configuration
    SqsConfig (..),
    SqsEndpoint (..),
    defaultSqsConfig,

    -- * The backend
    newSqsQueue,

    -- * Received-message lifting
    ReceivedMessage (..),
    liftReceivedMessages,

    -- * Job wire mapping
    encodeJob,
    decodeJob,
) where

import Amazonka qualified as AWS

import Amazonka.SQS.ChangeMessageVisibility qualified as SQS
import Amazonka.SQS.DeleteMessage qualified as SQS
import Amazonka.SQS.ReceiveMessage qualified as SQS
import Amazonka.SQS.SendMessage qualified as SQS
import Amazonka.SQS.Types qualified as SQS
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (
    eitherDecodeStrict',
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither)
import Katip (LogEnv, Severity (DebugS), logFM, ls, sl)
import Katip.Monadic (runKatipContextT)
import Lens.Micro ((?~), (^.))

import Ecluse.Core.Ecosystem (ecosystemName, parseEcosystem)
import Ecluse.Core.Package (
    mkPackageName,
    mkScope,
    pkgEcosystem,
    pkgNamespace,
    unScope,
    unscopedName,
 )
import Ecluse.Core.Queue (
    MirrorJob (..),
    MirrorQueue (..),
    QueueFault,
    QueueMessage (..),
    RemoteSpanContext (RemoteSpanContext, rscTraceparent, rscTracestate),
    Seconds (..),
    mkReceiptHandle,
    queueTransportFault,
    unReceiptHandle,
 )
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.Runtime.Aws.Fault (classifyAwsTransport)
import Ecluse.Runtime.Log (moduleField)

{- | Where an SQS-compatible endpoint lives, for pointing the backend at a
non-default host: a local emulator (@ministack@) in tests, or a VPC endpoint. A
non-default host: a local emulator (@ministack@) in tests, or a VPC endpoint.
-}
data SqsEndpoint = SqsEndpoint
    { endpointSecure :: Bool
    -- ^ Whether to connect over HTTPS (an emulator is usually plain HTTP).
    , endpointHost :: Text
    -- ^ The host to connect to (e.g. @"localhost"@).
    , endpointPort :: Int
    -- ^ The port to connect to (e.g. @4566@ for ministack).
    }
    deriving stock (Eq, Show)

{- | What the SQS backend needs. The batch size, long-poll window, and visibility
timeout are provider knobs (see "Ecluse.Core.Queue") with defaults in
'defaultSqsConfig'.
-}
data SqsConfig = SqsConfig
    { sqsQueueUrl :: Text
    -- ^ The fully-qualified SQS queue URL mirror jobs are sent to and received from.
    , sqsRegion :: Text
    -- ^ The AWS region the queue lives in (e.g. @"us-east-1"@).
    , sqsEndpoint :: Maybe SqsEndpoint
    {- ^ An endpoint override for an emulator or VPC endpoint; 'Nothing' uses
    @amazonka@'s default resolution and the ambient credential chain.
    -}
    , sqsBatchSize :: Int
    {- ^ Maximum messages to pull per 'receive' (SQS caps this at 10). A larger
    batch amortises the round-trip when the queue is busy.
    -}
    , sqsWaitSeconds :: Int
    {- ^ The long-poll window in seconds (SQS caps this at 20): how long a
    'receive' waits for a message before returning @[]@, so an idle worker does
    not hot-loop on empty polls.
    -}
    , sqsVisibilityTimeout :: Seconds
    {- ^ How long a received message stays hidden from other 'receive's before SQS
    redelivers it -- the budget for processing-then-'ack', extendable per message
    via 'extendVisibility'.
    -}
    , sqsTerminalBackoff :: Seconds
    {- ^ The visibility timeout 'deadLetter' returns a __terminal__ message with
    (@ChangeMessageVisibility@, never @DeleteMessage@): larger than the normal
    processing window so a permanently-unmirrorable artifact is not re-fetched in a
    hot loop, while it rides the operator's redrive policy to the dead-letter queue.
    A per-attempt incremental backoff would need the @ApproximateReceiveCount@
    attribute (deferred with the receive-count work in issue #935); this fixed
    backoff is the conservative default.
    -}
    }
    deriving stock (Eq, Show)

{- | A 'SqsConfig' for a queue URL and region with the provider knobs at sane
defaults: a full batch of 10, the maximum 20-second long poll, and a 30-second
visibility timeout. Override the record fields to tune them, or set 'sqsEndpoint'
to target an emulator.
-}
defaultSqsConfig :: Text -> Text -> SqsConfig
defaultSqsConfig queueUrl region =
    SqsConfig
        { sqsQueueUrl = queueUrl
        , sqsRegion = region
        , sqsEndpoint = Nothing
        , sqsBatchSize = 10
        , sqsWaitSeconds = 20
        , sqsVisibilityTimeout = Seconds 30
        , sqsTerminalBackoff = Seconds 300
        }

{- | Build an SQS-backed 'MirrorQueue'. The @amazonka@ 'AWS.Env' is constructed
once here -- region-scoped, and pointed at 'sqsEndpoint' with its throwaway
credentials when one is given, otherwise discovering the ambient AWS credential
chain -- and captured by the returned handle's closures.
-}
newSqsQueue :: LogEnv -> (Text -> Either Text RegistryUrl) -> SqsConfig -> IO MirrorQueue
newSqsQueue logEnv egressUrl cfg = do
    env <- mkEnv cfg
    -- Every operation reports its AWS failure as the handle's 'QueueFault' value:
    -- 'AWS.sendEither' keeps the error sum out of the exception channel, and the
    -- shared classifier folds it into the core transport vocabulary at this edge.
    let run :: (AWS.AWSRequest a) => a -> IO (Either QueueFault (AWS.AWSResponse a))
        run = fmap (first (queueTransportFault . classifyAwsTransport)) . runResourceT . AWS.sendEither env
        queueUrl = sqsQueueUrl cfg
        Seconds terminalBackoffSecs = sqsTerminalBackoff cfg
    pure
        MirrorQueue
            { enqueue = fmap void . run . SQS.newSendMessage queueUrl . encodeJob
            , receive = do
                outcome <- run (receiveRequest cfg)
                traverse (liftReceivedMessages logEnv egressUrl . receivedMessages) outcome
            , ack = fmap void . run . SQS.newDeleteMessage queueUrl . unReceiptHandle
            , extendVisibility = \receipt (Seconds secs) ->
                fmap void . run $
                    SQS.newChangeMessageVisibility queueUrl (unReceiptHandle receipt) secs
            , -- A terminal fault: return the message with the backoff visibility timeout
              -- (@ChangeMessageVisibility@), __never__ @DeleteMessage@, so it is not
              -- silently discarded but rides the operator's redrive policy to the
              -- dead-letter queue -- the well-monitored terminus with forensic retention.
              deadLetter = \receipt ->
                fmap void . run $
                    SQS.newChangeMessageVisibility queueUrl (unReceiptHandle receipt) terminalBackoffSecs
            }

-- Build the region-scoped, optionally endpoint-overridden amazonka environment.
mkEnv :: SqsConfig -> IO AWS.Env
mkEnv cfg = case sqsEndpoint cfg of
    Just ep -> do
        base <- regioned <$> AWS.newEnv AWS.discover
        pure (configured ep base)
    Nothing -> regioned <$> AWS.newEnv AWS.discover
  where
    regioned :: AWS.Env -> AWS.Env
    regioned env = env{AWS.region = AWS.Region' (sqsRegion cfg)}

    configured :: SqsEndpoint -> AWS.Env -> AWS.Env
    configured ep =
        AWS.configureService
            ( AWS.setEndpoint
                (endpointSecure ep)
                (encodeUtf8 (endpointHost ep))
                (endpointPort ep)
                SQS.defaultService
            )

-- One long-poll ReceiveMessage with the configured batch / wait / visibility.
-- SQS caps the long-poll ('sqsWaitSeconds') at 20s, which stays within amazonka's
-- default per-service request timeout, so the client never cuts a long-poll short
-- and no explicit response-timeout override is needed; a configured wait above the
-- SQS cap is clamped by SQS, so the relationship cannot be broken from config.
receiveRequest :: SqsConfig -> SQS.ReceiveMessage
receiveRequest cfg =
    SQS.newReceiveMessage (sqsQueueUrl cfg)
        & SQS.receiveMessage_maxNumberOfMessages
        ?~ sqsBatchSize cfg
            & SQS.receiveMessage_waitTimeSeconds
        ?~ sqsWaitSeconds cfg
            & SQS.receiveMessage_visibilityTimeout
        ?~ visibilitySeconds
  where
    Seconds visibilitySeconds = sqsVisibilityTimeout cfg

{- | The fields of a received SQS message the backend reads. Lifting them out of
the @amazonka@ 'SQS.Message' keeps the 'QueueMessage' mapping (and its drop
decision) free of the AWS type, so the receive path's drop behaviour is exercised
directly in tests.
-}
data ReceivedMessage = ReceivedMessage
    { rmBody :: Maybe Text
    -- ^ The message body carrying the encoded 'MirrorJob' (SQS always supplies one).
    , rmReceipt :: Maybe Text
    -- ^ The receipt handle a later 'ack' deletes the message by (SQS always supplies one).
    , rmMessageId :: Maybe Text
    -- ^ The SQS-assigned message id, for the drop log; not part of the untrusted body.
    }
    deriving stock (Eq, Show)

-- The read fields of an amazonka Message, lifted at the effectful edge ('receive').
receivedFields :: SQS.Message -> ReceivedMessage
receivedFields message =
    ReceivedMessage
        { rmBody = message ^. SQS.message_body
        , rmReceipt = message ^. SQS.message_receiptHandle
        , rmMessageId = message ^. SQS.message_messageId
        }

-- The received batch's messages, each reduced to the fields the backend reads.
receivedMessages :: SQS.ReceiveMessageResponse -> [ReceivedMessage]
receivedMessages response =
    maybe [] (map receivedFields) (response ^. SQS.receiveMessageResponse_messages)

-- Why a received message could not become a QueueMessage. A closed set with no
-- payload, so a drop log never echoes any of the (untrusted) message contents.
data SqsDropReason = MissingBody | MissingReceipt | UndecodableBody
    deriving stock (Eq, Show)

{- Lift one received message into a QueueMessage, or report why it cannot be. A
message missing its body or receipt (which SQS always supplies), or one whose body
does not decode, is dropped rather than crashing the poll: it is left un-acked, so
the visibility timeout redelivers it and a persistently bad message falls to the
dead-letter queue. -}
toQueueMessage :: (Text -> Either Text RegistryUrl) -> ReceivedMessage -> Either SqsDropReason QueueMessage
toQueueMessage egressUrl received = do
    body <- maybeToRight MissingBody (rmBody received)
    receipt <- maybeToRight MissingReceipt (rmReceipt received)
    job <- first (const UndecodableBody) (decodeJob egressUrl body)
    pure QueueMessage{msgJob = job, msgReceipt = mkReceiptHandle receipt}

{- | Lift a received batch into deliverable 'QueueMessage's, logging each dropped
message (a missing body or receipt, or an undecodable body) at 'DebugS' so a poison
message is visible rather than cycling silently until the queue's max-receive count.
A dropped message is omitted from the result and left un-'ack'ed, so redelivery and
dead-letter behaviour are unchanged.
-}
liftReceivedMessages :: LogEnv -> (Text -> Either Text RegistryUrl) -> [ReceivedMessage] -> IO [QueueMessage]
liftReceivedMessages logEnv egressUrl =
    fmap catMaybes . traverse (liftReceivedMessage logEnv egressUrl)

-- Deliver a received message, or log the drop at DebugS and yield Nothing.
liftReceivedMessage :: LogEnv -> (Text -> Either Text RegistryUrl) -> ReceivedMessage -> IO (Maybe QueueMessage)
liftReceivedMessage logEnv egressUrl received =
    case toQueueMessage egressUrl received of
        Right queueMessage -> pure (Just queueMessage)
        Left reason -> Nothing <$ logSqsDrop logEnv reason (rmMessageId received)

-- One DebugS line naming why a received message was dropped, and its SQS message id
-- when present. The message body is untrusted payload and is never logged.
logSqsDrop :: LogEnv -> SqsDropReason -> Maybe Text -> IO ()
logSqsDrop logEnv reason messageId =
    runKatipContextT logEnv payload mempty (logFM DebugS (ls message))
  where
    payload =
        moduleField "Ecluse.Runtime.Queue.Sqs"
            <> sl "reason" (dropReasonLabel reason)
            <> maybe mempty (sl "messageId") messageId
    message = "dropped an unusable SQS message: " <> dropReasonLabel reason

-- The operator-facing phrase for each drop reason.
dropReasonLabel :: SqsDropReason -> Text
dropReasonLabel = \case
    MissingBody -> "missing body"
    MissingReceipt -> "missing receipt"
    UndecodableBody -> "undecodable body"

{- | Encode a 'MirrorJob' as the JSON text of an SQS message body. The inverse of
'decodeJob': the package identity is split into its ecosystem, optional scope, and
bare name so it round-trips through 'mkPackageName', and the version keeps its raw
string. The serve-time-admitted artifact's filename rides as a plain field: it is
the selection key the worker's ingest re-evaluation gates by, and the only thing
of the artifact the wire carries -- the digests and size the worker verifies and
publishes with are derived from current metadata, never the payload.
-}
encodeJob :: MirrorJob -> Text
encodeJob job =
    decodeUtf8 . Aeson.encode $
        object
            [ "ecosystem" .= ecosystemName (pkgEcosystem name)
            , "scope" .= (unScope <$> pkgNamespace name)
            , "name" .= unscopedName name
            , "version" .= renderVersion (jobVersion job)
            , "artifactUrl" .= registryUrlText (jobArtifactUrl job)
            , "filename" .= jobArtifactFilename job
            , "traceContext" .= (encodeTraceContext <$> jobTraceContext job)
            ]
  where
    name = jobPackage job

-- Encode the optional enqueue-span trace-context carrier: the W3C traceparent and
-- tracestate verbatim, so the worker can re-establish the cross-async span link. A
-- 'Nothing' carrier (tracing was off at enqueue) serialises to a JSON null and
-- round-trips back to 'Nothing'.
encodeTraceContext :: RemoteSpanContext -> Aeson.Value
encodeTraceContext rsc =
    object
        [ "traceparent" .= rscTraceparent rsc
        , "tracestate" .= rscTracestate rsc
        ]

{- | Decode an SQS message body back into a 'MirrorJob', or a human-readable error
if the body is not the JSON object 'encodeJob' produces (a missing field, an
unknown ecosystem, an artifact URL the egress former refuses, malformed JSON).

The queue payload is a __trust boundary__, so the artifact URL is re-formed into
its 'RegistryUrl' egress witness on decode through the given former -- the
composition root passes the https-only 'Ecluse.Core.Security.Egress.mkRegistryUrl';
the loopback test harnesses pass their flag-gated dev former. A URL the former
refuses fails the decode, so a tampered or misproduced message can never hand the
worker's fetch an unwitnessed URL (it redelivers and falls to the dead-letter
queue, like any undecodable body).
-}
decodeJob :: (Text -> Either Text RegistryUrl) -> Text -> Either Text MirrorJob
decodeJob egressUrl body =
    first toText (eitherDecodeStrict' (encodeUtf8 body))
        >>= first toText . parseEither (parseMirrorJob egressUrl)

-- Parse the top-level job object 'encodeJob' writes, delegating the nested
-- trace-context carrier to 'parseTraceContext'.
parseMirrorJob :: (Text -> Either Text RegistryUrl) -> Aeson.Value -> Parser MirrorJob
parseMirrorJob egressUrl = withObject "MirrorJob" $ \o -> do
    ecoName <- o .: "ecosystem"
    eco <- maybe (fail (unknownEcosystem ecoName)) pure (parseEcosystem ecoName)
    scope <- o .:? "scope"
    rawName <- o .: "name"
    rawVersion <- o .: "version"
    rawArtifactUrl <- o .: "artifactUrl"
    -- Re-form the egress witness at the wire boundary: the type the worker's fetch
    -- requires cannot be fabricated from an unvalidated payload string.
    artifactUrl <- either (fail . toString) pure (egressUrl rawArtifactUrl)
    filename <- o .: "filename"
    -- The trace-context carrier is optional: a job from an older producer (or one
    -- enqueued with tracing off) carries no "traceContext", which decodes to
    -- 'Nothing' and simply yields no span link in the worker.
    traceContext <- o .:? "traceContext" >>= traverse parseTraceContext
    pure
        MirrorJob
            { jobPackage = mkPackageName eco (mkScope <$> scope) rawName
            , jobVersion = mkVersion eco rawVersion
            , jobArtifactUrl = artifactUrl
            , jobArtifactFilename = filename
            , jobTraceContext = traceContext
            }
  where
    unknownEcosystem n = "unknown ecosystem " <> show (n :: Text)

-- Parse the optional trace-context carrier back into a 'RemoteSpanContext': the W3C
-- traceparent and tracestate verbatim. The carrier is untrusted opaque transport, so
-- both fields are taken as-is -- an unparseable W3C value is the tracing port's concern
-- (it yields no link), never a decode failure that would strand a serviceable job.
parseTraceContext :: Aeson.Value -> Parser RemoteSpanContext
parseTraceContext = withObject "RemoteSpanContext" $ \t ->
    RemoteSpanContext <$> t .: "traceparent" <*> t .: "tracestate"
