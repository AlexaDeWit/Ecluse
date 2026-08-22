-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared @ministack@ bootstrapping for the integration suite.

The mirror-queue and mirror-worker specs both drive the real AWS SQS
'Ecluse.Core.Queue.MirrorQueue' backend against a @ministack@ container, launched via
@testcontainers@. They point at the emulator with throwaway credentials. Both are
hermetic and gating, but require a Docker daemon and no real AWS. This module stands
the bootstrapping up __once__: the container, its ASCII-relabelled image, the
endpoint-overridden @amazonka@ environment, and a fresh per-test queue. Both specs
share it rather than each re-deriving it.

This is test support, not a spec. It carries no @hspec@ 'Test.Hspec.Spec' of its own,
and its name avoids the @Spec@ suffix so @hspec-discover@ does not collect it.
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
    unwrapQ,

    -- * Endpoint
    endpointFor,

    -- * Logging
    quietLogEnv,
) where

import Amazonka qualified as AWS
import Amazonka.Auth qualified as AWS.Auth
import Amazonka.SQS.CreateQueue qualified as SQS
import Amazonka.SQS.GetQueueAttributes qualified as SQS
import Amazonka.SQS.SetQueueAttributes qualified as SQS
import Amazonka.SQS.Types qualified as SQS
import Control.Monad.Trans.Resource (runResourceT)
import Ecluse.Test.Support (newTestLogEnv)
import Katip (LogEnv)
import Lens.Micro ((.~), (?~), (^.))
import TestContainers (Container, containerAddress)
import TestContainers qualified as TC
import TestContainers.Docker (fromDockerfile, withLabels)
import TestContainers.Hspec (withContainers)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (try)

import System.Environment (setEnv)

import Ecluse.Core.Queue (MirrorQueue (receive), QueueMessage, Seconds (Seconds))
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Runtime.Queue.Sqs (SqsConfig (..), SqsEndpoint (..), defaultSqsConfig, newSqsQueue)
import Ecluse.Test.Container.Image (PinnedImageRef, mkPinnedImageRef, renderPinnedImageRef)
import Ecluse.Test.Containers (testContainerLabels)

-- | The SQS gateway port @ministack@ serves on.
ministackPort :: TC.Port
ministackPort = 4566

{- | An @hspec@ resource hook for a @ministack@ container, used with @aroundAll@ so one
container serves a whole spec while each case isolates on its own queue ('freshQueue').

A derived build re-labels the image with ASCII, because the upstream
@ministackorg/ministack@ @description@ label is non-ASCII and testcontainers 0.5.3.0
corrupts multi-byte bytes when it parses @docker inspect@ output.
-}
withMinistack :: (Container -> IO ()) -> IO ()
withMinistack body = do
    setEnv "AWS_ACCESS_KEY_ID" "test"
    setEnv "AWS_SECRET_ACCESS_KEY" "test"
    labels <- testContainerLabels "integration"
    -- Fail the suite if the image literal is not digest-pinned, so no mutable tag reaches @FROM@.
    image <- either (fail . toString) pure (mkPinnedImageRef ministackImage)
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

-- ministack, tag 1.3-full, pinned by digest.
ministackImage :: Text
ministackImage = "ministackorg/ministack@sha256:5164592def36af01b8ac76364028e27c5ecd8f1494c8a53d5fcd811cc7dfb594"

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
endpointFor :: Container -> SqsEndpoint
endpointFor container =
    let (host, mappedPort) = containerAddress container ministackPort
     in SqsEndpoint
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
    {- ^ The backoff window @deadLetter@ returns a terminal message with (a short one
    lets a test observe the not-deleted redelivery within its patience).
    -}
    , qoDeadLetterAfter :: Maybe Int
    {- ^ Attach a redrive policy to a fresh dead-letter queue, capturing at this
    @maxReceiveCount@. 'Nothing' leaves the queue with no dead-letter terminus.
    -}
    }
    deriving stock (Eq, Show)

{- | Defaults for a queue roundtrip. The short long poll keeps an empty poll from stalling
a test, and the terminal backoff matches the production default.
-}
defaultQueueOptions :: QueueOptions
defaultQueueOptions =
    QueueOptions
        { qoVisibilityTimeout = Seconds 30
        , qoWaitSeconds = 2
        , qoTerminalBackoff = Seconds 300
        , qoDeadLetterAfter = Nothing
        }

{- | A scribe-free 'LogEnv' for a layer that needs a logger where the spec does not assert
on the log.
-}
quietLogEnv :: IO LogEnv
quietLogEnv = newTestLogEnv

{- | Create a fresh SQS queue in the @ministack@ container and bind a 'MirrorQueue' to it.
The SQS service may not be up the instant the port opens, so this retries @CreateQueue@.
-}
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

{- | Attach a redrive policy to an existing queue, capturing at the given
@maxReceiveCount@. A policy names its target by ARN, so this creates the sibling queue and
reads its ARN first.
-}
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

{- | Create a fresh SQS queue and return its URL, for a test that drives the queue through
'Ecluse.Composition.planMirrorQueue' rather than the backend constructor. Retries
@CreateQueue@ while the SQS service warms up.
-}
freshQueueUrl :: Container -> Text -> IO Text
freshQueueUrl container queueName = do
    env <- envFor (endpointFor container)
    createQueueWithRetry env queueName 30

-- A region-scoped, endpoint-overridden amazonka Env with the throwaway keys.
envFor :: SqsEndpoint -> IO AWS.Env
envFor endpoint = do
    base <- AWS.Auth.fromKeys "test" "test" <$> AWS.newEnvNoAuth
    let regioned = base{AWS.region = AWS.Region' "us-east-1"}
    pure $
        AWS.configureService
            ( AWS.setEndpoint
                (endpointSecure endpoint)
                (encodeUtf8 (endpointHost endpoint))
                (endpointPort endpoint)
                SQS.defaultService
            )
            regioned

-- Create the queue, retrying while ministack's SQS service warms up.
createQueueWithRetry :: AWS.Env -> Text -> Int -> IO Text
createQueueWithRetry env queueName attemptsLeft = do
    outcome <- try (runResourceT (AWS.send env (SQS.newCreateQueue queueName)))
    case outcome of
        Right response
            | Just url <- response ^. SQS.createQueueResponse_queueUrl -> pure url
        _
            | attemptsLeft > 1 -> do
                threadDelay 500_000
                createQueueWithRetry env queueName (attemptsLeft - 1)
        Left (e :: SomeException) ->
            fail ("ministack CreateQueue never succeeded: " <> show e)
        Right _ ->
            fail "ministack CreateQueue returned no queue URL"

{- | Poll until a non-empty batch arrives, bounded at about 10s, so an empty long poll does
not flake and a genuinely empty queue fails the test rather than hanging.
-}
receiveUntil :: MirrorQueue -> IO [QueueMessage]
receiveUntil = receiveUntilWithin 20

{- | 'receiveUntil' with an explicit attempt budget, for a case that must wait out a longer
visibility window. Each attempt waits the queue's long poll plus about 500ms.
-}
receiveUntilWithin :: Int -> MirrorQueue -> IO [QueueMessage]
receiveUntilWithin = go
  where
    go 0 _ = fail "receiveUntilWithin: no message arrived within the retry budget"
    go n queue =
        receive queue >>= \case
            -- A transient transport fault retries like an empty poll. The last attempt
            -- fails loudly with the classified fault.
            Left fault
                | n > 1 -> threadDelay 500_000 >> go (n - 1) queue
                | otherwise -> fail ("receive faulted against ministack: " <> show fault)
            Right [] -> threadDelay 500_000 >> go (n - 1) queue
            Right messages -> pure messages

{- | Unwrap a queue outcome from a backend the test expects to be healthy. A 'Left' fails
the test with the classified fault.
-}
unwrapQ :: (Show e) => IO (Either e a) -> IO a
unwrapQ act = act >>= either (\fault -> fail ("queue operation faulted: " <> show fault)) pure
