{- | The AWS SQS backend behind the 'MirrorQueue' handle.

Maps the handle's receive → process → ack shape onto SQS:

* 'enqueue' → @SendMessage@ (the 'MirrorJob' encoded as the message body),
* 'receive' → one long-poll @ReceiveMessage@ (a batch, @[]@ on an empty poll),
* 'ack' → @DeleteMessage@ (the message is gone, never redelivered),
* 'extendVisibility' → @ChangeMessageVisibility@ (hold a long publish).

The provider differences SQS embodies — the visibility timeout, the long-poll
window, the batch limit — are 'SqsConfig' knobs with sane defaults, and the SQS
receipt handle is carried opaquely in a 'ReceiptHandle' (via 'mkReceiptHandle'),
so none of it leaks past the handle. __Retry is "don't ack"__: a job whose
processing fails is simply not 'ack'ed, and SQS redelivers it once the visibility
timeout lapses; persistent failures fall to the queue's native dead-letter
(max-receive-count), so there is no @nack@ (see "Ecluse.Queue").

The @amazonka@ 'AWS.Env' is built once at 'newSqsQueue' and captured by the
handle's closures, so the backend's state never reaches the proxy's @Env@\/@App@
(see @docs\/architecture\/technology-stack.md@ → "Key Decisions"). The
'MirrorJob' wire mapping is a plain JSON object, decoded strictly on 'receive' so
a malformed body surfaces as an error rather than a silently-dropped field.
-}
module Ecluse.Queue.Sqs (
    -- * Configuration
    SqsConfig (..),
    SqsEndpoint (..),
    defaultSqsConfig,

    -- * The backend
    newSqsQueue,

    -- * Job wire mapping
    encodeJob,
    decodeJob,
) where

import Amazonka qualified as AWS
import Amazonka.Auth qualified as AWS.Auth
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
import Data.Aeson.Types (parseEither)
import Data.Text qualified as T
import Lens.Micro ((?~), (^.))

import Ecluse.Ecosystem (ecosystemName, parseEcosystem)
import Ecluse.Package (PackageName, mkPackageName, mkScope, pkgEcosystem, pkgNamespace, renderPackageName, renderScope, unScope)
import Ecluse.Queue (
    MirrorJob (..),
    MirrorQueue (..),
    QueueMessage (..),
    Seconds (..),
    mkReceiptHandle,
    unReceiptHandle,
 )
import Ecluse.Version (mkVersion, renderVersion)

{- | Where an SQS-compatible endpoint lives, for pointing the backend at a
non-default host: a local emulator (@ministack@) in tests, or a VPC endpoint. A
'Nothing' 'sqsEndpoint' uses @amazonka@'s default resolution and the ambient AWS
credential chain; an override needs explicit credentials because an emulator is
off that chain.
-}
data SqsEndpoint = SqsEndpoint
    { endpointSecure :: Bool
    -- ^ Whether to connect over HTTPS (an emulator is usually plain HTTP).
    , endpointHost :: Text
    -- ^ The host to connect to (e.g. @"localhost"@).
    , endpointPort :: Int
    -- ^ The port to connect to (e.g. @4566@ for ministack).
    , endpointAccessKey :: Text
    -- ^ A throwaway access key id the emulator accepts (real SQS uses the chain).
    , endpointSecretKey :: Text
    -- ^ The matching throwaway secret key.
    }
    deriving stock (Eq, Show)

{- | What the SQS backend needs. The batch size, long-poll window, and visibility
timeout are provider knobs (see "Ecluse.Queue") with defaults in
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
    redelivers it — the budget for processing-then-'ack', extendable per message
    via 'extendVisibility'.
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
        }

{- | Build an SQS-backed 'MirrorQueue'. The @amazonka@ 'AWS.Env' is constructed
once here — region-scoped, and pointed at 'sqsEndpoint' with its throwaway
credentials when one is given, otherwise discovering the ambient AWS credential
chain — and captured by the returned handle's closures.
-}
newSqsQueue :: SqsConfig -> IO MirrorQueue
newSqsQueue cfg = do
    env <- mkEnv cfg
    let run :: (AWS.AWSRequest a) => a -> IO (AWS.AWSResponse a)
        run = runResourceT . AWS.send env
        queueUrl = sqsQueueUrl cfg
    pure
        MirrorQueue
            { enqueue = void . run . SQS.newSendMessage queueUrl . encodeJob
            , receive = do
                response <- run (receiveRequest cfg)
                let messages = fromMaybe [] (response ^. SQS.receiveMessageResponse_messages)
                pure (mapMaybe toQueueMessage messages)
            , ack = void . run . SQS.newDeleteMessage queueUrl . unReceiptHandle
            , extendVisibility = \receipt (Seconds secs) ->
                void . run $
                    SQS.newChangeMessageVisibility queueUrl (unReceiptHandle receipt) secs
            }

-- Build the region-scoped, optionally endpoint-overridden amazonka environment.
mkEnv :: SqsConfig -> IO AWS.Env
mkEnv cfg = case sqsEndpoint cfg of
    -- Real AWS: discover credentials the standard way (env, instance/container
    -- role, SSO, STS). Off the emulator path, so unit/integration cannot exercise
    -- it; the live smoke tier is its only end-to-end check.
    Nothing -> regioned <$> AWS.newEnv AWS.discover
    -- An emulator is off the AWS credential chain, so seed throwaway keys and
    -- point the SQS service at the override host.
    Just ep -> do
        base <- AWS.Auth.fromKeys (accessKey ep) (secretKey ep) <$> AWS.newEnvNoAuth
        pure (configured ep (regioned base))
  where
    regioned :: AWS.Env -> AWS.Env
    regioned env = env{AWS.region = AWS.Region' (sqsRegion cfg)}

    accessKey ep = AWS.AccessKey (encodeUtf8 (endpointAccessKey ep))
    secretKey ep = AWS.SecretKey (encodeUtf8 (endpointSecretKey ep))

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

{- Lift one SQS Message into a QueueMessage. A message missing its body or
receipt handle (which SQS always supplies) is dropped rather than crashing the
poll; likewise an undecodable body — the visibility timeout then redelivers it,
and a persistently bad message falls to the dead-letter queue. -}
toQueueMessage :: SQS.Message -> Maybe QueueMessage
toQueueMessage message = do
    body <- message ^. SQS.message_body
    receipt <- message ^. SQS.message_receiptHandle
    job <- rightToMaybe (decodeJob body)
    pure QueueMessage{msgJob = job, msgReceipt = mkReceiptHandle receipt}

-- ── job wire mapping ─────────────────────────────────────────────────────────

{- | Encode a 'MirrorJob' as the JSON text of an SQS message body. The inverse of
'decodeJob': the package identity is split into its ecosystem, optional scope, and
bare name so it round-trips through 'mkPackageName', and the version keeps its raw
string.
-}
encodeJob :: MirrorJob -> Text
encodeJob job =
    decodeUtf8 . Aeson.encode $
        object
            [ "ecosystem" .= ecosystemName (pkgEcosystem name)
            , "scope" .= (unScope <$> pkgNamespace name)
            , "name" .= bareName name
            , "version" .= renderVersion (jobVersion job)
            , "artifactUrl" .= jobArtifactUrl job
            , "mirrorTarget" .= jobMirrorTarget job
            ]
  where
    name = jobPackage job

{- | Decode an SQS message body back into a 'MirrorJob', or a human-readable error
if the body is not the JSON object 'encodeJob' produces (a missing field, an
unknown ecosystem, malformed JSON).
-}
decodeJob :: Text -> Either Text MirrorJob
decodeJob body =
    first toText (eitherDecodeStrict' (encodeUtf8 body))
        >>= first toText . parseEither parser
  where
    parser = withObject "MirrorJob" $ \o -> do
        ecoName <- o .: "ecosystem"
        eco <- maybe (fail (unknownEcosystem ecoName)) pure (parseEcosystem ecoName)
        scope <- o .:? "scope"
        rawName <- o .: "name"
        rawVersion <- o .: "version"
        artifactUrl <- o .: "artifactUrl"
        mirrorTarget <- o .: "mirrorTarget"
        pure
            MirrorJob
                { jobPackage = mkPackageName eco (mkScope <$> scope) rawName
                , jobVersion = mkVersion eco rawVersion
                , jobArtifactUrl = artifactUrl
                , jobMirrorTarget = mirrorTarget
                }
    unknownEcosystem n = "unknown ecosystem " <> show (n :: Text)

{- The bare (scope-stripped) package name: the third argument 'mkPackageName'
took. When scoped, 'mkPackageName' builds the display form as @scope/name@, so
dropping that exact @scope/@ prefix length recovers the bare name and keeps
encode/decode an exact round-trip. -}
bareName :: PackageName -> Text
bareName name =
    case pkgNamespace name of
        Just scope -> T.drop (T.length (renderScope scope) + 1) display
        Nothing -> display
  where
    display = renderPackageName name
