module Ecluse.WorkerSpec (spec) where

import Amazonka qualified as AWS
import Amazonka.Auth qualified as AWS.Auth
import Amazonka.SQS.CreateQueue qualified as SQS
import Amazonka.SQS.Types qualified as SQS
import Control.Monad.Trans.Resource (runResourceT)
import Crypto.Hash (Digest, SHA1, hashlazy)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Katip (Environment (Environment), LogEnv, Namespace (Namespace), initLogEnv)
import Lens.Micro ((^.))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200, status201)
import Network.Wai (Application, rawPathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import TestContainers (Container, containerAddress)
import TestContainers qualified as TC
import TestContainers.Docker (fromDockerfile)
import TestContainers.Hspec (withContainers)
import UnliftIO (timeout)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (try)

import Ecluse.App (runApp)
import Ecluse.Credential (AuthToken (..), CredentialProvider, mkSecret, staticProvider)
import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Env (Env, newEnv, newWorkerHeartbeat)
import Ecluse.Package (Hash (Hash), HashAlg (SHA1), mkPackageName)
import Ecluse.Queue (
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    MirrorJob (..),
    MirrorQueue (enqueue, receive),
 )
import Ecluse.Queue.Sqs (SqsConfig (..), SqsEndpoint (..), defaultSqsConfig, newSqsQueue)
import Ecluse.Registry.Npm (NpmClientConfig (NpmClientConfig, npmBaseUrl, npmLimits, npmManager, npmToken), newNpmClient)
import Ecluse.Security (defaultLimits)
import Ecluse.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Version (mkVersion)
import Ecluse.Worker (workerLoop)

{- | The mirror worker, end to end: a job enqueued on a real SQS queue (a
@ministack@ container) is consumed by 'workerLoop', its artifact fetched from a WAI
upstream stub, verified against the threaded digest, published to a WAI mirror-target
stub, and acked — so a redelivery pass finds the queue empty. A digest mismatch
publishes nothing and the job is dropped (acked after alarming, never published).

Hermetic and gating, but requires a Docker daemon (for ministack) and no real AWS.
-}
spec :: Spec
spec =
    around (withContainers ministack) $
        describe "mirror worker (ministack + WAI stubs)" $ do
            it "fetches, verifies, publishes, and acks a faithful job" $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget $ \mirrorUrl publishLog -> do
                        queue <- newQueue container "worker-success"
                        env <- envFor queue mirrorUrl
                        enqueue queue (job upstreamUrl trueSha1)
                        -- One bounded run of the consume loop: it should publish and ack.
                        _ <- timeout 5000000 (runApp env workerLoop)
                        published <- readIORef publishLog
                        length published `shouldBe` 1
                        -- The job was acked, so a later poll finds nothing.
                        leftover <- receive queue
                        leftover `shouldBe` []

            it "publishes nothing when the artifact fails its integrity digest" $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget $ \mirrorUrl publishLog -> do
                        queue <- newQueue container "worker-tamper"
                        env <- envFor queue mirrorUrl
                        -- The threaded digest does not match the served bytes: a tampered
                        -- artifact. The worker must refuse to publish.
                        enqueue queue (job upstreamUrl "deadbeef")
                        _ <- timeout 5000000 (runApp env workerLoop)
                        published <- readIORef publishLog
                        published `shouldBe` []

-- ── fixtures ──────────────────────────────────────────────────────────────────

-- The artifact bytes the upstream stub serves.
tarballBytes :: LByteString
tarballBytes = "left-pad-artifact-bytes"

-- The true lower-cased hex SHA-1 of the served bytes.
trueSha1 :: Text
trueSha1 = decodeUtf8 (convertToBase Base16 (hashlazy tarballBytes :: Digest SHA1) :: ByteString)

-- A mirror job pointing at the upstream stub, carrying the given SHA-1 digest.
job :: Text -> Text -> MirrorJob
job upstreamUrl sha1 =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "left-pad"
        , jobVersion = mkVersion Npm "1.3.0"
        , jobArtifactUrl = upstreamUrl <> "/left-pad/-/left-pad-1.3.0.tgz"
        , jobMirrorTarget = "ignored-the-publish-client-base-url-is-used"
        , jobArtifact =
            MirrorArtifact
                { maFilename = "left-pad-1.3.0.tgz"
                , maHashes = Hash SHA1 sha1 :| []
                , maSize = Nothing
                }
        }

-- ── Env over the real queue + a publish client at the mirror stub ──────────────

envFor :: MirrorQueue -> Text -> IO Env
envFor queue mirrorUrl = do
    manager <- newManager defaultManagerSettings
    publishClient <-
        newNpmClient
            NpmClientConfig
                { npmBaseUrl = mirrorUrl
                , npmManager = manager
                , npmToken = Just (mkSecret "test-token")
                , npmLimits = defaultLimits
                }
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- newTestLogEnv
    heartbeat <- newWorkerHeartbeat
    newEnv publishClient queue credentials manager manager metadataCache logEnv telemetryDisabled heartbeat
  where
    credentials :: CredentialProvider
    credentials = staticProvider AuthToken{authSecret = mkSecret "test-token", authExpiresAt = Nothing}

-- A scribe-free LogEnv (no stdout output during the integration run).
newTestLogEnv :: IO LogEnv
newTestLogEnv = initLogEnv (Namespace ["ecluse"]) (Environment "test")

-- ── WAI stubs: the public upstream and the mirror target ───────────────────────

-- A WAI upstream serving the artifact bytes at any path, yielding its base URL.
withUpstream :: (Text -> IO a) -> IO a
withUpstream body =
    testWithApplication (pure app) $ \port -> body ("http://127.0.0.1:" <> show port)
  where
    app :: Application
    app _ respond = respond (responseLBS status200 [] tarballBytes)

{- A WAI mirror-target stub accepting an npm publish @PUT@. It records each publish
PUT into an 'IORef' (so a test can assert a publish happened) and answers @201@. The
base URL and the publish log are yielded to the body. -}
withMirrorTarget :: (Text -> IORef [ByteString] -> IO a) -> IO a
withMirrorTarget body = do
    logRef <- newIORef []
    testWithApplication (pure (app logRef)) $ \port ->
        body ("http://127.0.0.1:" <> show port) logRef
  where
    app :: IORef [ByteString] -> Application
    app logRef request respond = do
        when (requestMethod request == "PUT") $
            atomicModifyIORef' logRef (\xs -> (rawPathInfo request : xs, ()))
        respond (responseLBS status201 [] "{}")

-- ── ministack container + queue setup (mirrors Ecluse.MirrorQueueSpec) ─────────

ministackPort :: TC.Port
ministackPort = 4566

ministack :: TC.TestContainer Container
ministack =
    TC.run $
        TC.containerRequest (fromDockerfile ministackDockerfile)
            & TC.setExpose [ministackPort]
            & TC.setWaitingFor (TC.waitUntilTimeout 120 (TC.waitUntilMappedPortReachable ministackPort))
            & TC.setRm True

-- ministack with an ASCII description label (testcontainers 0.5.3 mishandles the
-- upstream image's non-ASCII label; see Ecluse.MirrorQueueSpec for the detail).
ministackDockerfile :: Text
ministackDockerfile =
    "FROM ministackorg/ministack:latest\n\
    \LABEL description=\"Local AWS Service Emulator\"\n"

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

-- Create a fresh SQS queue and bind a 'MirrorQueue' to it with a short long-poll so
-- the loop's poll returns promptly.
newQueue :: Container -> Text -> IO MirrorQueue
newQueue container queueName = do
    let endpoint = endpointFor container
    env <- envForSqs endpoint
    queueUrl <- createQueueWithRetry env queueName 30
    newSqsQueue
        (defaultSqsConfig queueUrl "us-east-1")
            { sqsEndpoint = Just endpoint
            , sqsWaitSeconds = 1
            }

envForSqs :: SqsEndpoint -> IO AWS.Env
envForSqs endpoint = do
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
