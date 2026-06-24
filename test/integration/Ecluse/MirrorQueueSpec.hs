module Ecluse.MirrorQueueSpec (spec) where

import Amazonka qualified as AWS
import Amazonka.Auth qualified as AWS.Auth
import Amazonka.SQS.CreateQueue qualified as SQS
import Amazonka.SQS.Types qualified as SQS
import Control.Monad.Trans.Resource (runResourceT)
import Lens.Micro ((^.))
import Test.Hspec
import TestContainers (Container, containerAddress)
import TestContainers qualified as TC
import TestContainers.Docker (fromDockerfile)
import TestContainers.Hspec (withContainers)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (try)

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (Hash (Hash), HashAlg (SHA1), mkPackageName)
import Ecluse.Queue (
    MirrorArtifact (..),
    MirrorJob (..),
    MirrorQueue (..),
    QueueMessage (..),
    Seconds (..),
 )
import Ecluse.Queue.Sqs (
    SqsConfig (..),
    SqsEndpoint (..),
    defaultSqsConfig,
    newSqsQueue,
 )
import Ecluse.Version (mkVersion)

{- | Integration tests exercise the SQS 'MirrorQueue' backend against a real
endpoint provided by a @ministack@ container (launched via @testcontainers@).
@amazonka@ is pointed at the container with throwaway credentials, so they are
hermetic and __gating__ — but they require a running Docker daemon and no real
AWS.
-}
spec :: Spec
spec =
    around (withContainers ministack) $
        describe "mirror queue (ministack)" $ do
            it "round-trips a job: enqueue, receive, ack, then no redelivery" $ \container -> do
                queue <- newQueue container "mirror-roundtrip" (Seconds 30)
                enqueue queue sampleJob
                [message] <- receiveUntil queue
                msgJob message `shouldBe` sampleJob
                ack queue (msgReceipt message)
                -- After the ack the job is gone: a poll past the (short) visibility
                -- window yields nothing.
                afterAck <- receive queue
                map msgJob afterAck `shouldBe` []

            it "redelivers a job that was received but never acked" $ \container -> do
                -- A very short visibility timeout so the un-acked job becomes
                -- visible again within the test's patience.
                queue <- newQueue container "mirror-redeliver" (Seconds 1)
                enqueue queue sampleJob
                _firstDelivery <- receiveUntil queue
                -- Deliberately do not ack: retry-is-don't-ack means the job must
                -- reappear once its visibility window lapses.
                redelivered <- receiveUntil queue
                map msgJob redelivered `shouldBe` [sampleJob]

            it "extendVisibility holds an un-acked job past its original window" $ \container -> do
                -- Start with a 1s visibility timeout, then extend the in-flight
                -- message's window well past it. The job must NOT reappear in the
                -- gap the original timeout would have redelivered in, proving the
                -- ChangeMessageVisibility call held it.
                queue <- newQueue container "mirror-extend" (Seconds 1)
                enqueue queue sampleJob
                [message] <- receiveUntil queue
                extendVisibility queue (msgReceipt message) (Seconds 30)
                -- Past the original 1s window (poll twice over ~2s); still hidden.
                stillHidden1 <- receive queue
                stillHidden2 <- receive queue
                map msgJob (stillHidden1 <> stillHidden2) `shouldBe` []

-- | A sample mirror job carried end-to-end through SQS.
sampleJob :: MirrorJob
sampleJob =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "left-pad"
        , jobVersion = mkVersion Npm "1.3.0"
        , jobArtifactUrl = "https://registry.npmjs.org/left-pad/-/left-pad-1.3.0.tgz"
        , jobMirrorTarget = "https://mirror.example/left-pad/-/left-pad-1.3.0.tgz"
        , jobArtifact =
            MirrorArtifact
                { maFilename = "left-pad-1.3.0.tgz"
                , maHashes = Hash SHA1 "abc123" :| []
                , maSize = Just 256
                }
        }

-- ── ministack container + queue setup ────────────────────────────────────────

-- The SQS port ministack serves on.
ministackPort :: TC.Port
ministackPort = 4566

{- Start a ministack container exposing the SQS gateway port and wait until that
port accepts connections. The per-test queue is created afterwards (the service
takes a moment to initialise), so readiness here is just the port being up.

The image is wrapped in a trivial derived build that re-labels it with ASCII:
the upstream @ministackorg/ministack@ carries a non-ASCII @description@ label (an
em dash), and testcontainers 0.5.3.0 corrupts multi-byte bytes when it parses
@docker inspect@ output (@ByteString.Char8.pack@ over a 'String'), so inspecting
the raw image fails. The override keeps the test on the real emulator while
sidestepping that parser bug. -}
ministack :: TC.TestContainer Container
ministack =
    TC.run $
        TC.containerRequest (fromDockerfile ministackDockerfile)
            & TC.setExpose [ministackPort]
            & TC.setWaitingFor (TC.waitUntilTimeout 120 (TC.waitUntilMappedPortReachable ministackPort))
            & TC.setRm True

-- ministack with an ASCII description label (see 'ministack' for why).
ministackDockerfile :: Text
ministackDockerfile =
    "FROM ministackorg/ministack:latest\n\
    \LABEL description=\"Local AWS Service Emulator\"\n"

{- The amazonka Env pointed at the ministack container with throwaway keys. SQS at
ministack ignores credentials, so any non-empty pair signs successfully. -}
endpointFor :: Container -> SqsEndpoint
endpointFor container =
    let (host, mappedPort) = containerAddress container ministackPort
     in SqsEndpoint
            { endpointSecure = False
            , endpointHost = host
            , endpointPort = mappedPort
            , endpointAccessKey = "test"
            , endpointSecretKey = "test"
            }

{- Create a fresh SQS queue in ministack and return a 'MirrorQueue' bound to it
with the given visibility timeout and a short long-poll window (so tests do not
stall). ministack may not have the SQS service up the instant the port opens, so
the CreateQueue call is retried. -}
newQueue :: Container -> Text -> Seconds -> IO MirrorQueue
newQueue container queueName visibility = do
    let endpoint = endpointFor container
    env <- envFor endpoint
    queueUrl <- createQueueWithRetry env queueName 30
    newSqsQueue
        (defaultSqsConfig queueUrl "us-east-1")
            { sqsEndpoint = Just endpoint
            , sqsWaitSeconds = 2
            , sqsVisibilityTimeout = visibility
            }

-- A region-scoped, endpoint-overridden amazonka Env with the throwaway keys.
envFor :: SqsEndpoint -> IO AWS.Env
envFor endpoint = do
    base <-
        AWS.Auth.fromKeys
            (AWS.AccessKey (encodeUtf8 (endpointAccessKey endpoint)))
            (AWS.SecretKey (encodeUtf8 (endpointSecretKey endpoint)))
            <$> AWS.newEnvNoAuth
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

{- Poll until a non-empty batch arrives, so a test does not flake on an empty
long-poll while the message becomes (or becomes again) visible. Bounded so a
genuinely-empty queue fails the test rather than hanging. -}
receiveUntil :: MirrorQueue -> IO [QueueMessage]
receiveUntil = go (20 :: Int)
  where
    go 0 _ = fail "receiveUntil: no message arrived within the retry budget"
    go n queue = do
        messages <- receive queue
        if null messages
            then threadDelay 500_000 >> go (n - 1) queue
            else pure messages
