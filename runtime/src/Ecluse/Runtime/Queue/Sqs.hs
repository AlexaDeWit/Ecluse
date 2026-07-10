{- | The AWS SQS backend behind the 'MirrorQueue' handle.

Maps the handle's receive → process → ack shape onto SQS:

* 'enqueue' → @SendMessage@ (the 'MirrorJob' encoded as the message body),
* 'receive' → one long-poll @ReceiveMessage@ (a batch, @[]@ on an empty poll),
* 'ack' → @DeleteMessage@ (the message is gone, never redelivered),
* 'extendVisibility' → @ChangeMessageVisibility@ (hold a long publish).

The provider differences SQS embodies -- the visibility timeout, the long-poll
window, the batch limit -- are 'SqsConfig' knobs with sane defaults, and the SQS
receipt handle is carried opaquely in a 'ReceiptHandle' (via 'mkReceiptHandle'),
so none of it leaks past the handle. __Retry is "don't ack"__: a job whose
processing fails is simply not 'ack'ed, and SQS redelivers it once the visibility
timeout lapses; persistent failures fall to the queue's native dead-letter
(max-receive-count), so there is no @nack@ (see "Ecluse.Core.Queue").

The @amazonka@ 'AWS.Env' is built once at 'newSqsQueue' and captured by the
handle's closures, so the backend's state never reaches the proxy's @Env@\/@App@
(see @docs\/architecture\/technology-stack.md@ → "Key Decisions"). The
'MirrorJob' wire mapping is a plain JSON object, decoded on 'receive'; a body that
fails to parse is dropped rather than yielded as a partial, so -- like any message
left unprocessed -- it is not 'ack'ed and SQS redelivers it, ultimately to the
dead-letter queue.

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

    -- * Job wire mapping
    encodeJob,
    decodeJob,
    parseHashAlg,
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
import Lens.Micro ((?~), (^.))

import Ecluse.Core.Ecosystem (ecosystemName, parseEcosystem)
import Ecluse.Core.Package (
    Hash,
    HashAlg (Blake2b, MD5, SHA1, SHA256, SHA384, SHA512, SRI),
    hashAlg,
    hashValue,
    mkHash,
    mkPackageName,
    mkScope,
    mkSriHashes,
    pkgEcosystem,
    pkgNamespace,
    unScope,
    unscopedName,
 )
import Ecluse.Core.Package.Integrity (renderHashAlg)
import Ecluse.Core.Queue (
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    MirrorJob (..),
    MirrorQueue (..),
    QueueMessage (..),
    RemoteSpanContext (RemoteSpanContext, rscTraceparent, rscTracestate),
    Seconds (..),
    mkReceiptHandle,
    unReceiptHandle,
 )
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Version (mkVersion, renderVersion)

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
once here -- region-scoped, and pointed at 'sqsEndpoint' with its throwaway
credentials when one is given, otherwise discovering the ambient AWS credential
chain -- and captured by the returned handle's closures.
-}
newSqsQueue :: (Text -> Either Text RegistryUrl) -> SqsConfig -> IO MirrorQueue
newSqsQueue egressUrl cfg = do
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
                pure (mapMaybe (toQueueMessage egressUrl) messages)
            , ack = void . run . SQS.newDeleteMessage queueUrl . unReceiptHandle
            , extendVisibility = \receipt (Seconds secs) ->
                void . run $
                    SQS.newChangeMessageVisibility queueUrl (unReceiptHandle receipt) secs
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

{- Lift one SQS Message into a QueueMessage. A message missing its body or
receipt handle (which SQS always supplies) is dropped rather than crashing the
poll; likewise an undecodable body -- the visibility timeout then redelivers it,
and a persistently bad message falls to the dead-letter queue. -}
toQueueMessage :: (Text -> Either Text RegistryUrl) -> SQS.Message -> Maybe QueueMessage
toQueueMessage egressUrl message = do
    body <- message ^. SQS.message_body
    receipt <- message ^. SQS.message_receiptHandle
    job <- rightToMaybe (decodeJob egressUrl body)
    pure QueueMessage{msgJob = job, msgReceipt = mkReceiptHandle receipt}

{- | Encode a 'MirrorJob' as the JSON text of an SQS message body. The inverse of
'decodeJob': the package identity is split into its ecosystem, optional scope, and
bare name so it round-trips through 'mkPackageName', and the version keeps its raw
string. The serve-time-admitted artifact descriptor ('jobArtifact') -- the filename,
the integrity digests, and the declared size -- round-trips as a nested object so the
worker has the digest to verify the fetched bytes against and the inputs to assemble
the publish document.
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
            , "mirrorTarget" .= jobMirrorTarget job
            , "artifact" .= encodeArtifact (jobArtifact job)
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

-- Encode the serve-time-admitted artifact descriptor: filename, the integrity
-- digests (each an algorithm-tagged value), and the declared size when known.
encodeArtifact :: MirrorArtifact -> Aeson.Value
encodeArtifact artifact =
    object
        [ "filename" .= maFilename artifact
        , "hashes" .= map encodeHash (toList (maHashes artifact))
        , "size" .= maSize artifact
        ]
  where
    encodeHash :: Hash -> Aeson.Value
    encodeHash h = object ["alg" .= renderHashAlg (hashAlg h), "value" .= hashValue h]

{- | Decode an SQS message body back into a 'MirrorJob', or a human-readable error
if the body is not the JSON object 'encodeJob' produces (a missing field, an
unknown ecosystem, an empty hash list, an artifact URL the egress former refuses,
malformed JSON).

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
-- carriers to 'parseArtifact' and 'parseTraceContext'.
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
    mirrorTarget <- o .: "mirrorTarget"
    artifact <- o .: "artifact" >>= parseArtifact
    -- The trace-context carrier is optional: a job from an older producer (or one
    -- enqueued with tracing off) carries no "traceContext", which decodes to
    -- 'Nothing' and simply yields no span link in the worker.
    traceContext <- o .:? "traceContext" >>= traverse parseTraceContext
    pure
        MirrorJob
            { jobPackage = mkPackageName eco (mkScope <$> scope) rawName
            , jobVersion = mkVersion eco rawVersion
            , jobArtifactUrl = artifactUrl
            , jobMirrorTarget = mirrorTarget
            , jobArtifact = artifact
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

-- Parse the nested artifact descriptor, failing on an empty hash list (the
-- 'NonEmpty' invariant the serve path upholds -- a job must carry a digest to verify
-- against).
parseArtifact :: Aeson.Value -> Parser MirrorArtifact
parseArtifact = withObject "MirrorArtifact" $ \o -> do
    filename <- o .: "filename"
    rawHashes <- (o .: "hashes" :: Parser [Aeson.Value]) >>= traverse parseHashes
    size <- o .:? "size"
    case nonEmpty (concatMap toList rawHashes) of
        Nothing -> fail "MirrorArtifact carries no integrity digest"
        Just hashes ->
            pure MirrorArtifact{maFilename = filename, maHashes = hashes, maSize = size}

-- Parse one algorithm-tagged digest entry from the artifact descriptor's hash list.
-- An entry usually yields one 'Hash'; an @sri@ entry whose value joins several
-- components (a job enqueued by an older producer, which carried the wire string
-- whole) is split into one single-component 'Hash' each ('mkSriHashes'), so an
-- in-flight job survives the upgrade and the worker still verifies against exact
-- components rather than a joined string.
parseHashes :: Aeson.Value -> Parser (NonEmpty Hash)
parseHashes = withObject "Hash" $ \h -> do
    algName <- h .: "alg"
    alg <- maybe (fail (unknownAlg algName)) pure (parseHashAlg algName)
    value <- h .: "value"
    -- The queue is a trust boundary: validate the digest on decode through the same
    -- constructors the serve path uses ('mkHash' / 'mkSriHashes'), so the worker can
    -- never ingest a malformed digest to verify the fetched bytes against. A malformed
    -- value fails the decode (the job is left un-acked and redelivers, ultimately to
    -- the dead-letter queue).
    case alg of
        SRI -> either (fail . toString) pure (mkSriHashes value)
        _ -> either (fail . toString) (pure . one) (mkHash alg value)
  where
    unknownAlg n = "unknown hash algorithm " <> show (n :: Text)

-- Decode a wire algorithm name back to its 'HashAlg' -- the inverse of 'renderHashAlg'
-- over the SQS message vocabulary, including the @sri@ wrapper an npm @dist.integrity@
-- digest rides under. An exact match on a name a digest is serialized under, so a
-- well-formed message round-trips and an unrecognised name yields 'Nothing' (the job
-- is then rejected with a digest the worker could never have verified against).
parseHashAlg :: Text -> Maybe HashAlg
parseHashAlg = \case
    "sha1" -> Just SHA1
    "sha256" -> Just SHA256
    "sha384" -> Just SHA384
    "sha512" -> Just SHA512
    "md5" -> Just MD5
    "blake2b" -> Just Blake2b
    "sri" -> Just SRI
    _ -> Nothing
