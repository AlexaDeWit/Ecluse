-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared @ministack@ bootstrapping for the integration suite.

The specs that drive the real AWS SQS and S3 backends launch a @ministack@ container
through @testcontainers@ and point at it with throwaway credentials. This module owns the
container, its ASCII-relabelled image, the endpoint-overridden @amazonka@ environment, and
a fresh per-test queue. It carries no 'Test.Hspec.Spec' of its own, and its name avoids the
@Spec@ suffix so @hspec-discover@ does not collect it.
-}
module Ecluse.Integration.Ministack (
    -- * Container lifecycle
    withMinistack,

    -- * Per-test queue
    freshQueue,
    freshQueueUrl,
    QueueOptions (..),
    defaultQueueOptions,
    receiveUntil,
    receiveUntilWithin,

    -- * Endpoint
    endpointFor,

    -- * Stored objects
    objectMetadata,

    -- * Logging
    quietLogEnv,
) where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.Lens qualified as S3L
import Amazonka.SQS.CreateQueue qualified as SQS
import Amazonka.SQS.GetQueueAttributes qualified as SQS
import Amazonka.SQS.SetQueueAttributes qualified as SQS
import Amazonka.SQS.Types qualified as SQS
import Control.Monad.Trans.Resource (runResourceT)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Ecluse.Test.Log (newTestLogEnv)
import Katip (LogEnv)
import Lens.Micro ((.~), (?~), (^.))
import TestContainers (Container, containerAddress)
import TestContainers qualified as TC
import TestContainers.Docker (fromDockerfile, withLabels)
import TestContainers.Hspec (withContainers)
import UnliftIO.Exception (try)

import Ecluse.Core.Queue (MirrorQueue (receive), QueueMessage, Seconds (Seconds))
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Runtime.Aws.Env (AwsEndpoint (..), newAwsEnv)
import Ecluse.Runtime.Queue.Sqs.Internal (SqsConfig (..), defaultSqsConfig, newSqsQueue)
import Ecluse.Test.Container.Image (PinnedImageRef, ministackImage, renderPinnedImageRef)
import Ecluse.Test.Containers (testContainerLabels)
import Ecluse.Test.Env (withEnvOverrides)
import Ecluse.Test.Poll (pollUntil)

-- | The SQS gateway port @ministack@ serves on.
ministackPort :: TC.Port
ministackPort = 4566

-- | One emulator per spec. A derived ASCII image label avoids a testcontainers decoding fault.
withMinistack :: (Container -> IO ()) -> IO ()
withMinistack body = withEnvOverrides [("AWS_ACCESS_KEY_ID", "test"), ("AWS_SECRET_ACCESS_KEY", "test")] $ do
    labels <- testContainerLabels "integration"
    -- Fail the suite if the pin does not validate, so no mutable tag reaches @FROM@.
    image <- either (fail . toString) pure ministackImage
    withContainers (ministack labels image) body

-- The reaping labels are threaded in rather than baked into the image, so the container
-- carries this worktree's scope and `task test-clean` reaps it after a hard kill.
ministack :: [(Text, Text)] -> PinnedImageRef -> TC.TestContainer Container
ministack labels image =
    TC.run $
        TC.containerRequest (fromDockerfile (ministackDockerfile image))
            & TC.setExpose [ministackPort]
            & TC.setWaitingFor (TC.waitUntilTimeout 120 (TC.waitUntilMappedPortReachable ministackPort))
            & TC.setRm True
            & withLabels labels

-- ASCII description label (see 'withMinistack' for why), plus the coarse test marker so
-- `task test-clean-all` can prune a stale build image.
ministackDockerfile :: PinnedImageRef -> Text
ministackDockerfile image =
    "FROM "
        <> renderPinnedImageRef image
        <> "\n\
           \LABEL description=\"Local AWS Service Emulator\"\n\
           \LABEL com.ecluse.test=integration\n"

-- | The SQS endpoint override pointing @amazonka@ at the running @ministack@ container.
endpointFor :: Container -> AwsEndpoint
endpointFor container =
    let (host, mappedPort) = containerAddress container ministackPort
     in AwsEndpoint
            { endpointSecure = False
            , endpointHost = host
            , endpointPort = mappedPort
            }

-- | The queue tunables a spec may vary per case.
data QueueOptions = QueueOptions
    { qoVisibilityTimeout :: Seconds
    -- ^ How long a received message stays hidden before SQS redelivers it.
    , qoWaitSeconds :: Int
    -- ^ The long-poll window for a @receive@.
    , qoTerminalBackoff :: Seconds
    -- ^ The backoff @deadLetter@ returns a terminal message with. A short one shows the redelivery within a test's patience.
    , qoDeadLetterAfter :: Maybe Int
    -- ^ Attach a fresh dead-letter queue at this @maxReceiveCount@. 'Nothing' leaves the queue without one.
    }
    deriving stock (Eq, Show)

-- | Defaults for a queue roundtrip. The short long poll keeps an empty poll from stalling a test.
defaultQueueOptions :: QueueOptions
defaultQueueOptions =
    QueueOptions
        { qoVisibilityTimeout = Seconds 30
        , qoWaitSeconds = 2
        , qoTerminalBackoff = Seconds 300
        , qoDeadLetterAfter = Nothing
        }

-- | A scribe-free 'LogEnv' for a layer that needs a logger the spec does not read.
quietLogEnv :: IO LogEnv
quietLogEnv = newTestLogEnv

-- | Create the named queue, retrying while the emulator finishes startup.
freshQueue :: Container -> Text -> QueueOptions -> IO MirrorQueue
freshQueue container queueName options = do
    queueUrl <- freshQueueUrl container queueName
    -- The backend probes the redrive policy once at construction and holds what it
    -- found, so attach the policy first.
    whenJust (qoDeadLetterAfter options) (attachDeadLetterQueue container queueName queueUrl)
    logEnv <- quietLogEnv
    -- The wire decode's egress former: the loopback dev former, since these
    -- suites' artifact URLs point at in-process http servers.
    newSqsQueue
        logEnv
        (Right . loopbackRegistryUrl)
        (defaultSqsConfig queueUrl "us-east-1")
            { sqsEndpoint = Just (endpointFor container)
            , sqsWaitSeconds = qoWaitSeconds options
            , sqsVisibilityTimeout = qoVisibilityTimeout options
            , sqsTerminalBackoff = qoTerminalBackoff options
            }

-- | Attach a sibling dead-letter queue by ARN at the given capture count.
attachDeadLetterQueue :: Container -> Text -> Text -> Int -> IO ()
attachDeadLetterQueue container queueName queueUrl captureAt = do
    env <- envFor (endpointFor container)
    deadLetterUrl <- createQueueWithRetry env (queueName <> "-dlq") 30
    arn <- queueAttribute env deadLetterUrl SQS.QueueAttributeName_QueueArn
    void . runResourceT . AWS.send env $
        SQS.newSetQueueAttributes queueUrl
            & SQS.setQueueAttributes_attributes
            .~ fromList [(SQS.QueueAttributeName_RedrivePolicy, redrivePolicy arn)]
  where
    redrivePolicy arn =
        "{\"deadLetterTargetArn\":\"" <> arn <> "\",\"maxReceiveCount\":\"" <> show captureAt <> "\"}"

-- Read one queue attribute back. The request names only that attribute, so the
-- response carries the one value.
queueAttribute :: AWS.Env -> Text -> SQS.QueueAttributeName -> IO Text
queueAttribute env queueUrl attribute = do
    response <-
        runResourceT . AWS.send env $
            SQS.newGetQueueAttributes queueUrl & SQS.getQueueAttributes_attributeNames ?~ [attribute]
    case listToMaybe (toList (fromMaybe mempty (response ^. SQS.getQueueAttributesResponse_attributes))) of
        Just value -> pure value
        Nothing -> fail ("ministack GetQueueAttributes returned no " <> show attribute)

-- | Return a fresh SQS queue URL, retrying @CreateQueue@ while the service warms up.
freshQueueUrl :: Container -> Text -> IO Text
freshQueueUrl container queueName = do
    env <- envFor (endpointFor container)
    createQueueWithRetry env queueName 30

-- The same env the SQS backend builds, so a fixture request and a production one reach
-- ministack identically. 'withMinistack' put the throwaway keys where discovery finds them.
envFor :: AwsEndpoint -> IO AWS.Env
envFor endpoint = newAwsEnv (Just "us-east-1") (Just endpoint) SQS.defaultService

-- Create the queue, retrying while ministack's SQS service warms up. The last attempt's
-- diagnostic is the one that reaches the test.
createQueueWithRetry :: AWS.Env -> Text -> Int -> IO Text
createQueueWithRetry env queueName attempts =
    pollUntil attempts 500_000 isRight attempt >>= either (fail . toString) pure
  where
    attempt :: IO (Either Text Text)
    attempt =
        try (runResourceT (AWS.send env (SQS.newCreateQueue queueName))) >>= \case
            Left (e :: SomeException) -> pure (Left ("ministack CreateQueue never succeeded: " <> show e))
            Right response ->
                pure $
                    maybe
                        (Left "ministack CreateQueue returned no queue URL")
                        Right
                        (response ^. SQS.createQueueResponse_queueUrl)

-- | Poll until a non-empty batch arrives, for about 10s, so an empty queue fails the test rather than hanging.
receiveUntil :: MirrorQueue -> IO [QueueMessage]
receiveUntil = receiveUntilWithin 20

-- | 'receiveUntil' with its own attempt budget, each the queue's long poll plus about 500ms, for a longer visibility window.
receiveUntilWithin :: Int -> MirrorQueue -> IO [QueueMessage]
receiveUntilWithin attempts queue =
    pollUntil attempts 500_000 arrived (receive queue) >>= \case
        Right messages@(_ : _) -> pure messages
        -- A transient transport fault retries like an empty poll, so a fault that survives
        -- the budget is the last attempt's and names the cause.
        Left fault -> fail ("receive faulted against ministack: " <> show fault)
        Right [] -> fail "receiveUntilWithin: no message arrived within the retry budget"
  where
    arrived = either (const False) (not . null)

-- | The user metadata the object at @key@ carries, keyed in lower case as S3 stores the keys.
objectMetadata :: AWS.Env -> Text -> Text -> IO (HashMap.HashMap Text Text)
objectMetadata aws bucket key = do
    response <- runResourceT (AWS.send aws (S3.newHeadObject (S3.BucketName bucket) (S3.ObjectKey key)))
    pure (HashMap.fromList (map (first T.toLower) (HashMap.toList (response ^. S3L.headObjectResponse_metadata))))
