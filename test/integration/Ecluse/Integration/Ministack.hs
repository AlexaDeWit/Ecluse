-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared @ministack@ bootstrapping for the integration suite.

The mirror-queue and mirror-worker specs both exercise the real AWS SQS
'Ecluse.Core.Queue.MirrorQueue' backend against a @ministack@ container (launched via
@testcontainers@), pointed at the emulator with throwaway credentials -- hermetic
and gating, but requiring a Docker daemon and no real AWS. This module stands the
bootstrapping up __once__ (the container, its ASCII-relabelled image, the
endpoint-overridden @amazonka@ environment, and creating a fresh per-test queue) so
both specs share it rather than each re-deriving it.

This is test support, not a spec -- it carries no @hspec@ 'Test.Hspec.Spec' of its
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
    unwrapQ,

    -- * Endpoint
    endpointFor,

    -- * Logging
    quietLogEnv,
) where

import Amazonka qualified as AWS
import Amazonka.Auth qualified as AWS.Auth
import Amazonka.SQS.CreateQueue qualified as SQS
import Amazonka.SQS.Types qualified as SQS
import Control.Monad.Trans.Resource (runResourceT)
import Katip (Environment (Environment), LogEnv, Namespace (Namespace), initLogEnv)
import Lens.Micro ((^.))
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

{- | An @hspec@ resource hook that starts a @ministack@ container exposing the SQS
gateway port, waits until that port accepts connections, and tears it down after
the action. Used with @aroundAll@ so a whole spec shares __one__ container while each
case still isolates on its own queue ('freshQueue' \/ 'freshQueueUrl'); it never needs
a fresh container per case.

The image is wrapped in a trivial derived build that re-labels it with ASCII: the
upstream @ministackorg/ministack@ carries a non-ASCII @description@ label (an em
dash), and testcontainers 0.5.3.0 corrupts multi-byte bytes when it parses
@docker inspect@ output (@ByteString.Char8.pack@ over a 'String'), so inspecting the
raw image fails. The override keeps the test on the real emulator while sidestepping
that parser bug.
-}
withMinistack :: (Container -> IO ()) -> IO ()
withMinistack body = do
    setEnv "AWS_ACCESS_KEY_ID" "test"
    setEnv "AWS_SECRET_ACCESS_KEY" "test"
    labels <- testContainerLabels "integration"
    -- Resolve the pinned base image at startup, failing the suite loudly (the harness's
    -- IO idiom, 'fail') if the literal is not digest-pinned; the @FROM@ line is then built
    -- only from a validated 'PinnedImageRef', so a mutable tag can never reach it.
    image <- either (fail . toString) pure (mkPinnedImageRef ministackImage)
    withContainers (ministack labels image) body

-- The reaping labels ('testContainerLabels') are threaded in rather than baked into
-- the image so the container carries this worktree's scope; 'withContainers' already
-- tears the container down on a normal exit, but the label lets `task test-clean` reap
-- it after a hard kill. See "Ecluse.Test.Containers".
ministack :: [(Text, Text)] -> PinnedImageRef -> TC.TestContainer Container
ministack labels image =
    TC.run $
        TC.containerRequest (fromDockerfile (ministackDockerfile image))
            & TC.setExpose [ministackPort]
            & TC.setWaitingFor (TC.waitUntilTimeout 120 (TC.waitUntilMappedPortReachable ministackPort))
            & TC.setRm True
            & withLabels labels

-- ministack, tag 1.3-full, pinned by digest. Resolved to a 'PinnedImageRef' at startup
-- (see 'withMinistack'); the @FROM@ line is built from the validated reference, so a
-- mutable tag can never reach it.
ministackImage :: Text
ministackImage = "ministackorg/ministack@sha256:5164592def36af01b8ac76364028e27c5ecd8f1494c8a53d5fcd811cc7dfb594"

-- The derived build 'FROM' the pinned base, with an ASCII description label (see
-- 'withMinistack' for why) and the coarse test marker so a stale build image is prunable
-- by `task test-clean-all`.
ministackDockerfile :: PinnedImageRef -> Text
ministackDockerfile image =
    "FROM "
        <> renderPinnedImageRef image
        <> "\n\
           \LABEL description=\"Local AWS Service Emulator\"\n\
           \LABEL com.ecluse.test=integration\n"

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

{- | A scribe-free 'LogEnv' for wiring an SQS 'MirrorQueue' in the integration
suite: the backend now takes a logger for its poison-message drop line, and these
specs do not assert on it, so a no-output environment satisfies the dependency
without cluttering the run.
-}
quietLogEnv :: IO LogEnv
quietLogEnv = initLogEnv (Namespace ["ecluse"]) (Environment "test")

{- | Create a fresh SQS queue in the @ministack@ container and bind a
'MirrorQueue' to it with the given options. ministack may not have the SQS service
up the instant the port opens, so the @CreateQueue@ call is retried.
-}
freshQueue :: Container -> Text -> QueueOptions -> IO MirrorQueue
freshQueue container queueName options = do
    queueUrl <- freshQueueUrl container queueName
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
    go n queue =
        receive queue >>= \case
            -- A transient transport fault against the emulator retries like an
            -- empty poll (production backs off and re-polls the same way); the
            -- last attempt's fault fails loudly with its classified detail.
            Left fault
                | n > 1 -> threadDelay 500_000 >> go (n - 1) queue
                | otherwise -> fail ("receive faulted against ministack: " <> show fault)
            Right [] -> threadDelay 500_000 >> go (n - 1) queue
            Right messages -> pure messages

{- | Unwrap a typed queue outcome from a backend the test expects to be healthy:
a 'Left' is a loud test failure carrying the classified fault. Shared by the
queue and worker specs, which drive the real SQS backend directly.
-}
unwrapQ :: (Show e) => IO (Either e a) -> IO a
unwrapQ act = act >>= either (\fault -> fail ("queue operation faulted: " <> show fault)) pure
