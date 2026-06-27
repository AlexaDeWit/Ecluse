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
(max-receive-count), so there is no @nack@ (see "Ecluse.Core.Queue").

The @amazonka@ 'AWS.Env' is built once at 'newSqsQueue' and captured by the
handle's closures, so the backend's state never reaches the proxy's @Env@\/@App@
(see @docs\/architecture\/technology-stack.md@ → "Key Decisions"). The
'MirrorJob' wire mapping is a plain JSON object, decoded on 'receive'; a body that
fails to parse is dropped rather than yielded as a partial, so — like any message
left unprocessed — it is not 'ack'ed and SQS redelivers it, ultimately to the
dead-letter queue.

The SQS queue is a __trusted, operator-declared destination__ (the configured queue
URL, or an endpoint override): like the OTLP telemetry endpoint (see
"Ecluse.Telemetry.Resolve"), it is reached through @amazonka@'s own client and is
__not__ subject to the data-plane egress controls (the host allowlist, the
internal-range block, or the resolved-IP recheck of "Ecluse.Core.Security.Egress"), which
guard only untrusted package downloads — never a destination the operator configured.
-}
module Ecluse.Core.Queue.Sqs (
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
import Data.Aeson.Types (Parser, parseEither)
import Lens.Micro ((?~), (^.))

import Ecluse.Core.Credential (Secret, unSecret)
import Ecluse.Core.Ecosystem (ecosystemName, parseEcosystem)
import Ecluse.Core.Package (
    Hash,
    HashAlg (Blake2b, MD5, SHA1, SHA256, SHA384, SHA512, SRI),
    hashAlg,
    hashValue,
    mkHash,
    mkPackageName,
    mkScope,
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
    Seconds (..),
    mkReceiptHandle,
    unReceiptHandle,
 )
import Ecluse.Core.Version (mkVersion, renderVersion)

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
    , endpointSecretKey :: Secret
    {- ^ The matching secret key, held as a redacted 'Secret' so it never reaches the
    derived 'Show' of this record (the secret-redaction guarantee must survive even on
    an off-path log\/error).
    -}
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
    -- The secret is recovered from its redacted 'Secret' only here, at the point of
    -- use (building the signer), never rendered.
    secretKey ep = AWS.SecretKey (encodeUtf8 (unSecret (endpointSecretKey ep)))

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
string. The serve-time-admitted artifact descriptor ('jobArtifact') — the filename,
the integrity digests, and the declared size — round-trips as a nested object so the
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
            , "artifactUrl" .= jobArtifactUrl job
            , "mirrorTarget" .= jobMirrorTarget job
            , "artifact" .= encodeArtifact (jobArtifact job)
            ]
  where
    name = jobPackage job

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
unknown ecosystem, an empty hash list, malformed JSON).
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
        artifact <- o .: "artifact" >>= parseArtifact
        pure
            MirrorJob
                { jobPackage = mkPackageName eco (mkScope <$> scope) rawName
                , jobVersion = mkVersion eco rawVersion
                , jobArtifactUrl = artifactUrl
                , jobMirrorTarget = mirrorTarget
                , jobArtifact = artifact
                }
    unknownEcosystem n = "unknown ecosystem " <> show (n :: Text)

-- Parse the nested artifact descriptor, failing on an empty hash list (the
-- 'NonEmpty' invariant the serve path upholds — a job must carry a digest to verify
-- against).
parseArtifact :: Aeson.Value -> Parser MirrorArtifact
parseArtifact = withObject "MirrorArtifact" $ \o -> do
    filename <- o .: "filename"
    rawHashes <- o .: "hashes" >>= traverse parseHash
    size <- o .:? "size"
    case nonEmpty rawHashes of
        Nothing -> fail "MirrorArtifact carries no integrity digest"
        Just hashes ->
            pure MirrorArtifact{maFilename = filename, maHashes = hashes, maSize = size}
  where
    parseHash :: Aeson.Value -> Parser Hash
    parseHash = withObject "Hash" $ \h -> do
        algName <- h .: "alg"
        alg <- maybe (fail (unknownAlg algName)) pure (parseHashAlg algName)
        value <- h .: "value"
        -- The queue is a trust boundary: validate the digest on decode through the same
        -- 'mkHash' the serve path uses, so the worker can never ingest a malformed digest
        -- to verify the fetched bytes against. A malformed value fails the decode (the job
        -- is left un-acked and redelivers, ultimately to the dead-letter queue).
        either (fail . toString) pure (mkHash alg value)
    unknownAlg n = "unknown hash algorithm " <> show (n :: Text)

-- Decode a wire algorithm name back to its 'HashAlg' — the inverse of 'renderHashAlg'
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
