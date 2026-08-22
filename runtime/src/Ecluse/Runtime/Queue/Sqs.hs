-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The AWS SQS backend behind the 'MirrorQueue' handle.

Maps the handle's receive → process → ack shape onto SQS:

* 'enqueue' → @SendMessage@ (the 'MirrorJob' encoded as the message body).
* 'receive' → one long-poll @ReceiveMessage@ (a batch, @[]@ on an empty poll).
* 'ack' → @DeleteMessage@ (the message is gone, never redelivered).
* 'extendVisibility' → @ChangeMessageVisibility@ (hold a long publish).
* 'deadLetter' → @ChangeMessageVisibility@ with the 'sqsTerminalBackoff' window and
  __no @DeleteMessage@__ (a terminal fault rides the redrive policy to the
  dead-letter queue).

'newSqsQueue' __probes the queue's redrive configuration once__ (@GetQueueAttributes@
for @RedrivePolicy@, which SQS never delivers with a message). The composition root can
then warn when nothing captures a poison message. 'newSqsQueue' also holds the handle's
redelivery budget one delivery above an attached policy's own @maxReceiveCount@. The
dead-letter queue therefore always captures first. @ReceiveMessage@ likewise asks for
@ApproximateReceiveCount@ explicitly, since SQS omits it by default. That is the
delivery count every 'Ecluse.Core.Queue.QueueMessage' carries.

The provider differences SQS embodies are 'SqsConfig' knobs with sane defaults: the
visibility timeout, the long-poll window, and the batch limit. The SQS receipt handle
rides opaquely in a 'ReceiptHandle' (via 'mkReceiptHandle'), so none of it leaks past
the handle. Retry is __"don't ack"__: a job whose processing fails transiently is
simply not 'ack'ed, and SQS redelivers it once the visibility timeout lapses.
Persistent failures fall to the queue's native dead-letter (max-receive-count), so
there is no @nack@ (see "Ecluse.Core.Queue"). A __terminal__ fault ('deadLetter')
returns with a backoff window and is never deleted. It too falls to the operator's
dead-letter queue rather than being discarded. A deployment with no dead-letter queue
has nothing to fall to. The worker's redelivery budget retires such a message instead
(see "Ecluse.Core.Queue"), rather than letting it cycle until the retention window
discards it unseen. Every operation reports its AWS failure as the handle's typed
'Ecluse.Core.Queue.QueueFault' value, classified into the core transport vocabulary at
this edge ("Ecluse.Runtime.Aws.Fault"). A queue outage never rides the exception channel
through a caller.

'newSqsQueue' builds the @amazonka@ 'AWS.Env' once, and the handle's closures capture
it. The backend's state therefore never reaches the proxy's @Env@\/@App@ (see
@docs\/architecture\/technology-stack.md@ → "Key Decisions"). The 'MirrorJob' wire
mapping is a plain JSON object, decoded on 'receive'. A body that fails to parse is
dropped rather than yielded as a partial. Like any message left unprocessed, it is not
'ack'ed, so SQS redelivers it and it eventually reaches the dead-letter queue. Each
drop is logged at 'DebugS' with its reason and the SQS message id when present: a
missing body or receipt, or an undecodable body. A poison message is then visible
rather than cycling silently. The untrusted body is never logged.

The SQS queue is a __trusted, operator-declared destination__ (the configured queue
URL, or an endpoint override). Like the OTLP telemetry endpoint (see
"Ecluse.Runtime.Telemetry.Resolve"), @amazonka@'s own client reaches it. It is __not__
subject to the data-plane egress controls: the host allowlist and the https-only
egress posture of "Ecluse.Core.Security.Egress". Those controls guard only untrusted
package downloads, never a destination the operator configured.
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

    -- * Dead-letter probe
    deadLetterTerminusOf,

    -- * Job wire mapping
    encodeJob,
    decodeJob,
) where

import Amazonka qualified as AWS

import Amazonka.SQS.ChangeMessageVisibility qualified as SQS
import Amazonka.SQS.DeleteMessage qualified as SQS
import Amazonka.SQS.GetQueueAttributes qualified as SQS
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
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Katip (LogEnv, Severity (DebugS), logFM, ls, sl)
import Katip.Monadic (runKatipContextT)
import Lens.Micro ((?~), (^.))

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems), ecosystemName, parseEcosystem)
import Ecluse.Core.Package (
    PackageName,
    mkPackageName,
    mkScope,
    pkgEcosystem,
    pkgNamespace,
    unScope,
    unscopedName,
 )
import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent, TerminusAttached),
    DeliveryBudget (DeliveryBudget),
    MirrorJob (..),
    MirrorQueue (..),
    QueueFault,
    QueueMessage (..),
    RemoteSpanContext (RemoteSpanContext, rscTraceparent, rscTracestate),
    Seconds (..),
    defaultDeliveryBudget,
    effectiveDeliveryBudget,
    mkReceiptHandle,
    queueTransportFault,
    unReceiptHandle,
 )
import Ecluse.Core.Registry (parseErrorMessage)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Text (nonBlank)
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.Runtime.Aws.Fault (classifyAwsTransport)
import Ecluse.Runtime.Log (moduleField)

{- | Where an SQS-compatible endpoint lives, for pointing the backend at a
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

{- | What the SQS backend needs. The provider knobs take their defaults from
'defaultSqsConfig' (see "Ecluse.Core.Queue").
-}
data SqsConfig = SqsConfig
    { sqsQueueUrl :: Text
    -- ^ The fully-qualified SQS queue URL mirror jobs are sent to and received from.
    , sqsRegion :: Text
    -- ^ The AWS region the queue lives in (e.g. @"us-east-1"@).
    , sqsEndpoint :: Maybe SqsEndpoint
    {- ^ An endpoint override for an emulator or VPC endpoint. 'Nothing' uses
    @amazonka@'s default resolution and the ambient credential chain.
    -}
    , sqsBatchSize :: Int
    -- ^ Maximum messages to pull per 'receive' (SQS caps this at 10).
    , sqsWaitSeconds :: Int
    {- ^ The long-poll window in seconds (SQS caps this at 20). A 'receive' waits this long
    for a message before returning @[]@, so an idle worker does not hot-loop on empty polls.
    -}
    , sqsVisibilityTimeout :: Seconds
    {- ^ How long a received message stays hidden from other 'receive's before SQS
    redelivers it: the budget for processing-then-'ack', extendable per message via
    'extendVisibility'.
    -}
    , sqsTerminalBackoff :: Seconds
    {- ^ The visibility timeout 'deadLetter' applies when it returns a __terminal__ message.
    It exceeds the normal processing window, so the worker does not re-fetch a permanently
    unmirrorable artifact in a hot loop.
    -}
    , sqsMaxReceiveCount :: DeliveryBudget
    {- ^ The configured __floor__ on how many deliveries one message gets before the
    worker retires it (@ECLUSE_QUEUE__MAX_RECEIVE_COUNT@). 'newSqsQueue' raises the
    handle's effective budget past an attached redrive policy's own @maxReceiveCount@,
    so this floor never pre-empts a dead-letter queue's capture.
    -}
    }
    deriving stock (Eq, Show)

-- | A 'SqsConfig' for a queue URL and region, with the provider knobs at their defaults.
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
        , sqsMaxReceiveCount = defaultDeliveryBudget
        }

{- | Build an SQS-backed 'MirrorQueue'. The @amazonka@ 'AWS.Env' is built once here and
the returned handle's closures capture it.
-}
newSqsQueue :: LogEnv -> (Text -> Either Text RegistryUrl) -> SqsConfig -> IO MirrorQueue
newSqsQueue logEnv egressUrl cfg = do
    env <- mkEnv cfg
    -- 'AWS.sendEither' keeps the AWS error out of the exception channel, so every operation
    -- reports its failure as the handle's 'QueueFault' value.
    let run :: (AWS.AWSRequest a) => a -> IO (Either QueueFault (AWS.AWSResponse a))
        run = fmap (first (queueTransportFault . classifyAwsTransport)) . runResourceT . AWS.sendEither env
        queueUrl = sqsQueueUrl cfg
        Seconds terminalBackoffSecs = sqsTerminalBackoff cfg
    -- SQS keeps the redrive configuration on the queue, not on a message, so the backend
    -- probes it once at boot and falls back to the configured floor when the probe faults.
    terminus <- fmap terminusOfResponse <$> run (terminusRequest queueUrl)
    let budget = either (const (sqsMaxReceiveCount cfg)) (effectiveDeliveryBudget (sqsMaxReceiveCount cfg)) terminus
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
              -- (@ChangeMessageVisibility@), __never__ @DeleteMessage@, so the message is not
              -- discarded but rides the operator's redrive policy to the dead-letter queue.
              deadLetter = \receipt ->
                fmap void . run $
                    SQS.newChangeMessageVisibility queueUrl (unReceiptHandle receipt) terminalBackoffSecs
            , deliveryBudget = budget
            , deadLetterTerminus = terminus
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

-- SQS caps the long poll at 20s and clamps a larger configured wait. That stays inside
-- @amazonka@'s default request timeout, so the client needs no response-timeout override.
receiveRequest :: SqsConfig -> SQS.ReceiveMessage
receiveRequest cfg =
    SQS.newReceiveMessage (sqsQueueUrl cfg)
        & SQS.receiveMessage_maxNumberOfMessages
        ?~ sqsBatchSize cfg
            & SQS.receiveMessage_waitTimeSeconds
        ?~ sqsWaitSeconds cfg
            & SQS.receiveMessage_visibilityTimeout
        ?~ visibilitySeconds
            & SQS.receiveMessage_attributeNames
        -- Asked for explicitly: SQS omits the delivery count unless a request names
        -- it. The redelivery budget judges a delivery by that count.
        ?~ [SQS.MessageAttribute_ApproximateReceiveCount]
  where
    Seconds visibilitySeconds = sqsVisibilityTimeout cfg

-- The boot-time redrive probe.
terminusRequest :: Text -> SQS.GetQueueAttributes
terminusRequest queueUrl =
    SQS.newGetQueueAttributes queueUrl
        & SQS.getQueueAttributes_attributeNames
        ?~ [SQS.QueueAttributeName_RedrivePolicy]

terminusOfResponse :: SQS.GetQueueAttributesResponse -> DeadLetterTerminus
terminusOfResponse response =
    deadLetterTerminusOf (soleValue =<< (response ^. SQS.getQueueAttributesResponse_attributes))

-- Every request here names exactly one attribute and SQS returns only the named
-- attributes that are set, so an empty map means unset, not an ambiguous pick.
soleValue :: (Foldable t) => t Text -> Maybe Text
soleValue = listToMaybe . toList

{- | Classify a queue's raw @RedrivePolicy@ attribute into its dead-letter terminus. No
policy or a blank attribute gives 'TerminusAbsent'. Any other policy is __attached__,
with @maxReceiveCount@ when that is readable. An unreadable count still counts as
attached, so the boot warning stays silent and the configured floor stands alone.
-}
deadLetterTerminusOf :: Maybe Text -> DeadLetterTerminus
deadLetterTerminusOf raw =
    maybe TerminusAbsent (TerminusAttached . captureCountOf) (nonBlank =<< raw)

-- The @maxReceiveCount@ an attached redrive policy declares. AWS embeds the policy as
-- JSON and renders the count as either a string or a number, so both parse.
captureCountOf :: Text -> Maybe DeliveryBudget
captureCountOf policy = do
    value <- rightToMaybe (eitherDecodeStrict' (encodeUtf8 policy))
    raw <- parseMaybe (withObject "RedrivePolicy" (.: "maxReceiveCount")) value
    DeliveryBudget <$> countOf raw

countOf :: Aeson.Value -> Maybe Int
countOf = \case
    Aeson.String text -> readMaybe (toString text)
    other -> case Aeson.fromJSON other of
        Aeson.Success count -> Just count
        Aeson.Error _ -> Nothing

{- | The fields of a received SQS message the backend reads. Lifting them out of the
@amazonka@ 'SQS.Message' keeps the 'QueueMessage' mapping free of the AWS type.
-}
data ReceivedMessage = ReceivedMessage
    { rmBody :: Maybe Text
    -- ^ The message body carrying the encoded 'MirrorJob' (SQS always supplies one).
    , rmReceipt :: Maybe Text
    -- ^ The receipt handle a later 'ack' deletes the message by (SQS always supplies one).
    , rmMessageId :: Maybe Text
    -- ^ The SQS-assigned message id, for the drop log. Not part of the untrusted body.
    , rmReceiveCount :: Maybe Text
    {- ^ The raw @ApproximateReceiveCount@ system attribute, which the poll asks for
    explicitly. 'Nothing' when SQS supplied none, which reads as a first delivery.
    -}
    }
    deriving stock (Eq, Show)

-- The read fields of an amazonka Message, lifted at the effectful edge ('receive').
receivedFields :: SQS.Message -> ReceivedMessage
receivedFields message =
    ReceivedMessage
        { rmBody = message ^. SQS.message_body
        , rmReceipt = message ^. SQS.message_receiptHandle
        , rmMessageId = message ^. SQS.message_messageId
        , rmReceiveCount = soleValue =<< (message ^. SQS.message_attributes)
        }

-- The received batch's messages, each reduced to the fields the backend reads.
receivedMessages :: SQS.ReceiveMessageResponse -> [ReceivedMessage]
receivedMessages response =
    maybe [] (map receivedFields) (response ^. SQS.receiveMessageResponse_messages)

-- Why a received message could not become a QueueMessage. A closed set with no
-- payload, so a drop log never echoes any of the (untrusted) message contents.
data SqsDropReason = MissingBody | MissingReceipt | UndecodableBody
    deriving stock (Eq, Show)

{- A message missing its body or receipt, which SQS always supplies, or one whose body
does not decode, is dropped rather than crashing the poll. -}
toQueueMessage :: (Text -> Either Text RegistryUrl) -> ReceivedMessage -> Either SqsDropReason QueueMessage
toQueueMessage egressUrl received = do
    body <- maybeToRight MissingBody (rmBody received)
    receipt <- maybeToRight MissingReceipt (rmReceipt received)
    job <- first (const UndecodableBody) (decodeJob egressUrl body)
    pure
        QueueMessage
            { msgJob = job
            , msgReceipt = mkReceiptHandle receipt
            , msgReceiveCount = receiveCountOf (rmReceiveCount received)
            }

-- The delivery count SQS reported, or a first delivery when it reported none, an
-- unreadable one, or a count below one. Only evidence ever puts a message past its budget.
receiveCountOf :: Maybe Text -> Int
receiveCountOf raw = max 1 (fromMaybe 1 (readMaybe . toString =<< raw))

{- | Lift a received batch into deliverable 'QueueMessage's, logging each drop at 'DebugS'
so a poison message is visible instead of cycling silently. A dropped message is omitted
and left un-'ack'ed, so redelivery and dead-letter behaviour are unchanged.
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

{- | Encode a 'MirrorJob' as the JSON text of an SQS message body, the inverse of
'decodeJob'. The filename is the only part of the artifact the wire carries. The worker
takes the digests and size it verifies from current metadata, never from the payload.
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

-- The W3C traceparent and tracestate verbatim, so the worker can re-establish the
-- cross-async span link. A 'Nothing' carrier round-trips through a JSON null.
encodeTraceContext :: RemoteSpanContext -> Aeson.Value
encodeTraceContext rsc =
    object
        [ "traceparent" .= rscTraceparent rsc
        , "tracestate" .= rscTracestate rsc
        ]

{- | Decode an SQS message body back into a 'MirrorJob'. A missing field, an unknown
ecosystem, malformed JSON, an unusable package name, or a refused artifact URL yields a
human-readable error.

The queue payload is a __trust boundary__, so the decode re-forms the artifact URL into
its 'RegistryUrl' egress witness through the given former, and re-reads the package name
through its ecosystem's grammar. A tampered message can never hand the worker's fetch an
unwitnessed URL or a name the front door would have refused.
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
    -- The payload states the scope and the bare name separately, so re-join them and read the
    -- result through the ecosystem's own grammar rather than trusting the two fields.
    package <- either (fail . toString) pure (mirrorJobPackage eco scope rawName)
    rawVersion <- o .: "version"
    rawArtifactUrl <- o .: "artifactUrl"
    -- Re-form the egress witness at the wire boundary: the type the worker's fetch
    -- requires cannot be fabricated from an unvalidated payload string.
    artifactUrl <- either (fail . toString) pure (egressUrl rawArtifactUrl)
    filename <- o .: "filename"
    -- A job from an older producer, or one enqueued with tracing off, carries no
    -- "traceContext". It yields no span link rather than a decode failure.
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
    unknownEcosystem n = "unknown ecosystem " <> show (n :: Text)

{- Rebuild the payload's package identity, reading npm's wire name through 'projectName'. npm is
the only ecosystem with a name grammar today, so the others take the two fields as given. -}
mirrorJobPackage :: Ecosystem -> Maybe Text -> Text -> Either Text PackageName
mirrorJobPackage eco scope rawName = case eco of
    Npm -> first parseErrorMessage (projectName npmWireName)
    PyPI -> Right asGiven
    RubyGems -> Right asGiven
  where
    asGiven = mkPackageName eco (mkScope <$> scope) rawName
    npmWireName = maybe rawName (\s -> "@" <> s <> "/" <> rawName) scope

-- The carrier is untrusted opaque transport, so both fields are taken as-is. An
-- unparseable W3C value yields no link in the tracing port rather than failing the decode.
parseTraceContext :: Aeson.Value -> Parser RemoteSpanContext
parseTraceContext = withObject "RemoteSpanContext" $ \t ->
    RemoteSpanContext <$> t .: "traceparent" <*> t .: "tracestate"
