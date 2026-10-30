-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | SQS queue operations with grammar-checked npm, Cargo and RubyGems identities and bounded receipt leases.
module Ecluse.Runtime.Queue.Sqs.Internal (
    -- * Configuration
    SqsConfig (..),
    defaultSqsConfig,

    -- * The backend
    newSqsQueue,
    sqsQueueOver,

    -- * Received-message lifting
    ReceivedMessage (..),
    liftReceivedMessages,

    -- * Dead-letter probe
    deadLetterTerminusOf,

    -- * The queue payload's wire-name gate
    mirrorJobPackage,
) where

import Amazonka qualified as AWS

import Amazonka.SQS.ChangeMessageVisibility qualified as SQS
import Amazonka.SQS.DeleteMessage qualified as SQS
import Amazonka.SQS.GetQueueAttributes qualified as SQS
import Amazonka.SQS.ReceiveMessage qualified as SQS
import Amazonka.SQS.SendMessage qualified as SQS
import Amazonka.SQS.Types qualified as SQS
import Data.Aeson (eitherDecodeStrict', withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseMaybe)
import Katip (LogEnv, Severity (DebugS), sl)
import Lens.Micro ((?~), (^.))

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Fault (TransportFault)
import Ecluse.Core.Package (PackageName, WireNameRefusal (..), mkPackageName, mkScope, unnamespacedWireName)
import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent, TerminusAttached),
    DeliveryBudget (DeliveryBudget),
    MirrorQueue (..),
    QueueMessage (..),
    Seconds (..),
    decodeJob,
    defaultDeliveryBudget,
    effectiveDeliveryBudget,
    encodeJob,
    mkReceiptHandle,
    unReceiptHandle,
 )
import Ecluse.Core.Queue.Lease (ReceiptLease, monotonicNow, receiptLease)
import Ecluse.Core.Registry (parseErrorMessage)
import Ecluse.Core.Registry.Cargo.Project qualified as Cargo
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.RubyGems.Project qualified as RubyGems
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Text (nonBlank, readDecimalText)
import Ecluse.Runtime.Aws.Env (AwsEndpoint, newAwsEnv)
import Ecluse.Runtime.Aws.Fault (classifyAwsTransport, sendClassified)
import Ecluse.Runtime.Log (logLine, moduleField)

-- | What the SQS backend needs. The provider knobs take their defaults from 'defaultSqsConfig' (see "Ecluse.Core.Queue").
data SqsConfig = SqsConfig
    { sqsQueueUrl :: Text
    -- ^ The fully-qualified SQS queue URL mirror jobs are sent to and received from.
    , sqsRegion :: Text
    -- ^ The AWS region the queue lives in (e.g. @"us-east-1"@).
    , sqsEndpoint :: Maybe AwsEndpoint
    -- ^ An endpoint override for an emulator or VPC endpoint. 'Nothing' keeps @amazonka@'s own resolution.
    , sqsBatchSize :: Int
    -- ^ Maximum messages to pull per 'receive' (SQS caps this at 10).
    , sqsWaitSeconds :: Int
    -- ^ The long-poll window in seconds (SQS caps it at 20), so an idle worker does not hot-loop on empty polls.
    , sqsVisibilityTimeout :: Seconds
    -- ^ How long a received message stays hidden before SQS redelivers it. 'extendVisibility' renews it per message.
    , sqsTerminalBackoff :: Seconds
    -- ^ The visibility 'deadLetter' gives a terminal message, longer than processing so an unmirrorable artifact is not re-fetched in a hot loop.
    , sqsMaxReceiveCount :: DeliveryBudget
    -- ^ The floor on deliveries before the worker retires a message. A redrive policy's higher count raises it, so a dead-letter capture is never pre-empted.
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

-- | Build an SQS-backed 'MirrorQueue' over one @amazonka@ env, which the handle's closures capture.
newSqsQueue :: LogEnv -> (Text -> Either Text RegistryUrl) -> SqsConfig -> IO MirrorQueue
newSqsQueue logEnv egressUrl cfg = mkEnv cfg >>= \env -> sqsQueueOver logEnv egressUrl env cfg

-- | Use an explicit AWS environment, including an endpoint supplied by a test.
sqsQueueOver :: LogEnv -> (Text -> Either Text RegistryUrl) -> AWS.Env -> SqsConfig -> IO MirrorQueue
sqsQueueOver logEnv egressUrl env cfg = do
    -- Every operation reports its failure as the handle's 'TransportFault' value, never
    -- through the exception channel.
    let run :: (AWS.AWSRequest a) => a -> IO (Either TransportFault (AWS.AWSResponse a))
        run = sendClassified classifyAwsTransport env
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
                -- Stamped before the request, so a lease never outlives the window SQS granted.
                receivedAt <- monotonicNow
                let lease = receiptLease receivedAt (sqsVisibilityTimeout cfg) sqsInFlightMaximum
                outcome <- run (receiveRequest cfg)
                traverse (liftReceivedMessages logEnv egressUrl lease . receivedMessages) outcome
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
mkEnv cfg = newAwsEnv (Just (sqsRegion cfg)) (sqsEndpoint cfg) SQS.defaultService

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

{- The longest SQS keeps one receipt in flight, however often its visibility is renewed. A
renewal never asks past it, so a lease that reaches it is dropped rather than silently lapsing. -}
sqsInFlightMaximum :: Seconds
sqsInFlightMaximum = Seconds 43_200

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

-- | Classify redrive attachment and retain its readable delivery budget.
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
    Aeson.String text -> readDecimalText text
    other -> case Aeson.fromJSON other of
        Aeson.Success count -> Just count
        Aeson.Error _ -> Nothing

-- | Received fields preserve SQS receipt authority separately from the untrusted body.
data ReceivedMessage = ReceivedMessage
    { rmBody :: Maybe Text
    -- ^ The message body carrying the encoded job (SQS always supplies one).
    , rmReceipt :: Maybe Text
    -- ^ The receipt handle a later 'ack' deletes the message by (SQS always supplies one).
    , rmMessageId :: Maybe Text
    -- ^ The SQS-assigned message id, for the drop log. Not part of the untrusted body.
    , rmReceiveCount :: Maybe Text
    -- ^ The raw @ApproximateReceiveCount@ the poll asks for. 'Nothing' reads as a first delivery.
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
toQueueMessage :: (Text -> Either Text RegistryUrl) -> ReceiptLease -> ReceivedMessage -> Either SqsDropReason QueueMessage
toQueueMessage egressUrl lease received = do
    body <- maybeToRight MissingBody (rmBody received)
    receipt <- maybeToRight MissingReceipt (rmReceipt received)
    job <- first (const UndecodableBody) (decodeJob mirrorJobPackage egressUrl body)
    pure
        QueueMessage
            { msgJob = job
            , msgReceipt = mkReceiptHandle receipt
            , msgReceiveCount = receiveCountOf (rmReceiveCount received)
            , msgLease = Just lease
            }

-- The delivery count SQS reported, or a first delivery when it reported none, an
-- unreadable one, or a count below one. Only evidence ever puts a message past its budget.
receiveCountOf :: Maybe Text -> Int
receiveCountOf raw = max 1 (fromMaybe 1 (readDecimalText =<< raw))

-- | Apply the poll's lease. Unusable messages remain unacknowledged for redelivery.
liftReceivedMessages :: LogEnv -> (Text -> Either Text RegistryUrl) -> ReceiptLease -> [ReceivedMessage] -> IO [QueueMessage]
liftReceivedMessages logEnv egressUrl lease =
    fmap catMaybes . traverse (liftReceivedMessage logEnv egressUrl lease)

-- Deliver a received message, or log the drop at DebugS and yield Nothing.
liftReceivedMessage :: LogEnv -> (Text -> Either Text RegistryUrl) -> ReceiptLease -> ReceivedMessage -> IO (Maybe QueueMessage)
liftReceivedMessage logEnv egressUrl lease received =
    case toQueueMessage egressUrl lease received of
        Right queueMessage -> pure (Just queueMessage)
        Left reason -> Nothing <$ logSqsDrop logEnv reason (rmMessageId received)

-- One DebugS line naming why a received message was dropped, and its SQS message id when
-- present. The body is untrusted payload, and @module@ names the public module operators filter on.
logSqsDrop :: LogEnv -> SqsDropReason -> Maybe Text -> IO ()
logSqsDrop logEnv reason messageId =
    logLine logEnv payload DebugS message
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

{- | Read npm, Cargo and RubyGems identities through their grammars before the worker accepts them.
PyPI names pass as given.
-}
mirrorJobPackage :: Ecosystem -> Maybe Text -> Text -> Either Text PackageName
mirrorJobPackage eco namespace rawName = case eco of
    Npm -> first parseErrorMessage (projectName npmWireName)
    Cargo -> unnamespaced "Cargo" Cargo.projectName
    RubyGems -> unnamespaced "RubyGems" RubyGems.projectName
    PyPI -> Right (mkPackageName eco (mkScope <$> namespace) rawName)
  where
    npmWireName = maybe rawName (\ns -> "@" <> ns <> "/" <> rawName) namespace
    unnamespaced label parse = first (refusal label) (unnamespacedWireName parse namespace rawName)
    refusal label = \case
        WireNamespaced -> label <> " packages cannot have a namespace"
        WireNameUnusable fault -> parseErrorMessage fault
