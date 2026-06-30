{- | Shared @ministack@ bootstrapping for the integration suite.

The mirror-queue and mirror-worker specs both exercise the real AWS SQS
'Ecluse.Core.Queue.MirrorQueue' backend against a @ministack@ container (launched via
@testcontainers@), pointed at the emulator with throwaway credentials — hermetic
and gating, but requiring a Docker daemon and no real AWS. This module stands the
bootstrapping up __once__ (the container, its ASCII-relabelled image, the
endpoint-overridden @amazonka@ environment, and creating a fresh per-test queue) so
both specs share it rather than each re-deriving it.

This is test support, not a spec — it carries no @hspec@ 'Test.Hspec.Spec' of its
own and is named off the @Spec@ suffix so @hspec-discover@ does not collect it.
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
) where

import Amazonka qualified as AWS
import Amazonka.Auth qualified as AWS.Auth
import Amazonka.SQS.CreateQueue qualified as SQS
import Amazonka.SQS.Types qualified as SQS
import Control.Monad.Trans.Resource (runResourceT)
import Lens.Micro ((^.))
import TestContainers (Container, containerAddress)
import TestContainers qualified as TC
import TestContainers.Docker (fromDockerfile)
import TestContainers.Hspec (withContainers)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (try)


import Ecluse.Core.Queue (MirrorQueue (receive), QueueMessage, Seconds (Seconds))
import Ecluse.Core.Queue.Sqs (SqsConfig (..), SqsEndpoint (..), defaultSqsConfig, newSqsQueue)

-- ── container lifecycle ───────────────────────────────────────────────────────

-- | The SQS gateway port @ministack@ serves on.
ministackPort :: TC.Port
ministackPort = 4566

{- | An @hspec@ @around@ hook that starts a @ministack@ container exposing the SQS
gateway port, waits until that port accepts connections, and tears it down after
the action.

The image is wrapped in a trivial derived build that re-labels it with ASCII: the
upstream @ministackorg/ministack@ carries a non-ASCII @description@ label (an em
dash), and testcontainers 0.5.3.0 corrupts multi-byte bytes when it parses
@docker inspect@ output (@ByteString.Char8.pack@ over a 'String'), so inspecting the
raw image fails. The override keeps the test on the real emulator while sidestepping
that parser bug.
-}
withMinistack :: (Container -> IO ()) -> IO ()
withMinistack = withContainers ministack

ministack :: TC.TestContainer Container
ministack =
    TC.run $
        TC.containerRequest (fromDockerfile ministackDockerfile)
            & TC.setExpose [ministackPort]
            & TC.setWaitingFor (TC.waitUntilTimeout 120 (TC.waitUntilMappedPortReachable ministackPort))
            & TC.setRm True

-- ministack (pinned by digest; tag 1.3-full) with an ASCII description label
-- (see 'withMinistack' for why).
ministackDockerfile :: Text
ministackDockerfile =
    "FROM ministackorg/ministack@sha256:5164592def36af01b8ac76364028e27c5ecd8f1494c8a53d5fcd811cc7dfb594\n\
    \LABEL description=\"Local AWS Service Emulator\"\n"

-- ── endpoint ──────────────────────────────────────────────────────────────────

{- | The SQS endpoint override pointing @amazonka@ at the running @ministack@
container with throwaway credentials. ministack ignores credentials, so any
non-empty pair signs successfully.
-}
endpointFor :: Container -> SqsEndpoint
endpointFor container =
    let (host, mappedPort) = containerAddress container ministackPort
     in SqsEndpoint
            { endpointSecure = False
            , endpointHost = host
            , endpointPort = mappedPort
            }

-- ── per-test queue ────────────────────────────────────────────────────────────

{- | The tunables a spec may want to vary per case: the visibility timeout (short,
to observe redelivery within the test's patience) and the long-poll window (short,
so a @receive@ does not stall the test).
-}
data QueueOptions = QueueOptions
    { qoVisibilityTimeout :: Seconds
    -- ^ How long a received message stays hidden before SQS redelivers it.
    , qoWaitSeconds :: Int
    -- ^ The long-poll window for a @receive@.
    }
    deriving stock (Eq, Show)

{- | A 30-second visibility timeout and a 2-second long poll: the queue-roundtrip
default that does not stall a test on an empty poll.
-}
defaultQueueOptions :: QueueOptions
defaultQueueOptions = QueueOptions{qoVisibilityTimeout = Seconds 30, qoWaitSeconds = 2}

{- | Create a fresh SQS queue in the @ministack@ container and bind a
'MirrorQueue' to it with the given options. ministack may not have the SQS service
up the instant the port opens, so the @CreateQueue@ call is retried.
-}
freshQueue :: Container -> Text -> QueueOptions -> IO MirrorQueue
freshQueue container queueName options = do
    queueUrl <- freshQueueUrl container queueName
    newSqsQueue
        (defaultSqsConfig queueUrl "us-east-1")
            { sqsEndpoint = Just (endpointFor container)
            , sqsWaitSeconds = qoWaitSeconds options
            , sqsVisibilityTimeout = qoVisibilityTimeout options
            }

{- | Create a fresh SQS queue in the @ministack@ container and return its queue URL
(without binding a 'MirrorQueue' to it), so a test can drive the queue through the
config-driven composition root ('Ecluse.Composition.planMirrorQueue') and the
endpoint-override key rather than the direct backend constructor. ministack may not
have the SQS service up the instant the port opens, so @CreateQueue@ is retried.
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

{- | Poll a queue until a non-empty batch arrives, so a test does not flake on an
empty long-poll while a message becomes (or becomes again) visible. Bounded (~10s)
so a genuinely-empty queue fails the test rather than hanging.
-}
receiveUntil :: MirrorQueue -> IO [QueueMessage]
receiveUntil = receiveUntilWithin 20

{- | 'receiveUntil' with an explicit attempt budget (each attempt waits up to the
queue's long-poll window, plus a ~500ms pause), for a case that must wait out a
longer visibility \/ extension window before a message reappears.
-}
receiveUntilWithin :: Int -> MirrorQueue -> IO [QueueMessage]
receiveUntilWithin = go
  where
    go 0 _ = fail "receiveUntilWithin: no message arrived within the retry budget"
    go n queue = do
        messages <- receive queue
        if null messages
            then threadDelay 500_000 >> go (n - 1) queue
            else pure messages
