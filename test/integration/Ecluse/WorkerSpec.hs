module Ecluse.WorkerSpec (spec) where

import Crypto.Hash (Digest, SHA1, hashlazy)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Katip (Environment (Environment), LogEnv, Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status, status200, status201, status409, status503)
import Network.Wai (Application, rawPathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO (race_, timeout)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.App (runApp)
import Ecluse.Credential (AuthToken (..), CredentialProvider, mkSecret, staticProvider)
import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Env (Env, envWorkerHeartbeat, lastPoll, newEnv, newWorkerHeartbeat)
import Ecluse.Integration.Ministack (
    QueueOptions (qoVisibilityTimeout, qoWaitSeconds),
    defaultQueueOptions,
    freshQueue,
    receiveUntilWithin,
    withMinistack,
 )
import Ecluse.Package (Hash (Hash), HashAlg (SHA1), mkPackageName)
import Ecluse.Queue (
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    MirrorJob (..),
    MirrorQueue (enqueue, receive),
    QueueMessage (msgJob),
    Seconds (Seconds),
 )
import Ecluse.Registry.Npm (NpmClientConfig (NpmClientConfig, npmBaseUrl, npmLimits, npmManager, npmToken), newNpmClient)
import Ecluse.Security (defaultLimits)
import Ecluse.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Version (mkVersion)
import Ecluse.Worker (workerLoop)

{- | The mirror worker, end to end against real SQS (a @ministack@ container, shared
through "Ecluse.Integration.Ministack") and WAI upstream/mirror stubs. These cases
exercise the queue semantics the in-memory double cannot faithfully reproduce —
real visibility timeouts, redelivery, @extendVisibility@-held messages, and the
supervised 'workerLoop' itself polling a real queue (heartbeat included).

Hermetic and gating, but requires a Docker daemon (for ministack) and no real AWS.
-}
spec :: Spec
spec =
    around withMinistack $
        describe "mirror worker (ministack + WAI stubs)" $ do
            it "fetches, verifies, publishes, and acks a faithful job (via the loop)" $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-success" defaultQueueOptions
                        env <- envFor queue mirrorUrl
                        enqueue queue (job upstreamUrl trueSha1)
                        -- Run the supervised loop against the real queue until it has
                        -- published, then cancel it.
                        runLoopUntil env (publishedAtLeast publishLog 1)
                        published <- readIORef publishLog
                        length published `shouldBe` 1
                        -- The job was acked, so it does not redeliver.
                        leftover <- receive queue
                        leftover `shouldBe` []

            it "publishes nothing when the artifact fails its integrity digest" $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-tamper" defaultQueueOptions
                        env <- envFor queue mirrorUrl
                        -- The threaded digest does not match the served bytes: a
                        -- tampered artifact. The worker must refuse to publish.
                        enqueue queue (job upstreamUrl "deadbeef")
                        runLoopFor env 4_000_000
                        published <- readIORef publishLog
                        published `shouldBe` []

            it "treats a 409 (version already present) as idempotent success and acks" $ \container ->
                withUpstream $ \upstreamUrl ->
                    -- The mirror target answers 409 Conflict (the version is already
                    -- present): S08's 409-is-success means the worker treats this as a
                    -- successful publish and acks, so the job does not redeliver.
                    withMirrorTarget status409 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-idempotent" defaultQueueOptions
                        env <- envFor queue mirrorUrl
                        enqueue queue (job upstreamUrl trueSha1)
                        runLoopUntil env (publishedAtLeast publishLog 1)
                        leftover <- receive queue
                        leftover `shouldBe` []

            it "leaves a transiently-rejected job un-acked, so it redelivers" $ \container ->
                withUpstream $ \upstreamUrl ->
                    -- The mirror target answers 503 (a retryable rejection). The worker
                    -- must not ack, so the message redelivers once its (short)
                    -- visibility window lapses — a real second delivery.
                    withMirrorTarget status503 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-retry" defaultQueueOptions{qoVisibilityTimeout = Seconds 1}
                        env <- envFor queue mirrorUrl
                        enqueue queue (job upstreamUrl trueSha1)
                        -- Run the loop until it has attempted the (failing) publish,
                        -- then stop. The worker extends visibility around the publish,
                        -- so redelivery follows once that hold lapses — never an ack.
                        runLoopUntil env (publishedAtLeast publishLog 1)
                        redelivered <- receiveUntilWithin 30 queue
                        map (jobArtifactUrl . msgJob) redelivered `shouldBe` [upstreamUrl <> artifactPath]

            it "advances the heartbeat as the loop polls a real queue" $ \container ->
                withUpstream $ \_upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl _publishLog -> do
                        queue <- freshQueue container "worker-heartbeat" defaultQueueOptions{qoWaitSeconds = 1}
                        env <- envFor queue mirrorUrl
                        pollBefore <- lastPoll (envWorkerHeartbeat env)
                        pollBefore `shouldBe` Nothing
                        -- No job enqueued: an idle loop still completes real polls, so
                        -- the heartbeat must advance from Nothing.
                        runLoopFor env 3_000_000
                        pollAfter <- lastPoll (envWorkerHeartbeat env)
                        pollAfter `shouldSatisfy` isJust

-- ── fixtures ──────────────────────────────────────────────────────────────────

-- The artifact bytes the upstream stub serves.
tarballBytes :: LByteString
tarballBytes = "left-pad-artifact-bytes"

-- The true lower-cased hex SHA-1 of the served bytes.
trueSha1 :: Text
trueSha1 = decodeUtf8 (convertToBase Base16 (hashlazy tarballBytes :: Digest SHA1) :: ByteString)

-- The path the job's artifact URL appends to the upstream stub base.
artifactPath :: Text
artifactPath = "/left-pad/-/left-pad-1.3.0.tgz"

-- A mirror job pointing at the upstream stub, carrying the given SHA-1 digest.
job :: Text -> Text -> MirrorJob
job upstreamUrl sha1 =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "left-pad"
        , jobVersion = mkVersion Npm "1.3.0"
        , jobArtifactUrl = upstreamUrl <> artifactPath
        , jobMirrorTarget = "the-publish-client-base-url-is-used-instead"
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

-- ── driving the supervised loop ────────────────────────────────────────────────

{- Run the supervised 'workerLoop' against the real queue until a condition holds,
then tear it down. The loop never returns on its own, so it is raced against a
condition-poller ('race_'): when the poller observes the condition, 'race_' cancels
the loop — the same cooperative cancellation process shutdown uses. A hard timeout
bounds the whole thing so a failing test cannot hang. -}
runLoopUntil :: Env -> IO Bool -> IO ()
runLoopUntil env done =
    void $ timeout 12_000_000 $ race_ (runApp env workerLoop) (waitFor done)

{- Run the supervised 'workerLoop' for a fixed wall-clock window, then cancel it —
for the cases that assert a /negative/ (nothing published, a redelivery, an idle
heartbeat) where there is no positive condition to wait on. -}
runLoopFor :: Env -> Int -> IO ()
runLoopFor env micros = void (timeout micros (runApp env workerLoop))

-- Poll a condition until it holds, bounded so a failing test does not hang past the
-- enclosing timeout.
waitFor :: IO Bool -> IO ()
waitFor done = go (60 :: Int)
  where
    go :: Int -> IO ()
    go 0 = pure ()
    go n =
        done >>= \case
            True -> pure ()
            False -> threadDelay 200_000 >> go (n - 1)

publishedAtLeast :: IORef [a] -> Int -> IO Bool
publishedAtLeast logRef n = (>= n) . length <$> readIORef logRef

-- ── WAI stubs: the public upstream and the mirror target ───────────────────────

-- A WAI upstream serving the artifact bytes at any path, yielding its base URL.
withUpstream :: (Text -> IO a) -> IO a
withUpstream body =
    testWithApplication (pure app) $ \port -> body ("http://127.0.0.1:" <> show port)
  where
    app :: Application
    app _ respond = respond (responseLBS status200 [] tarballBytes)

{- A WAI mirror-target stub accepting an npm publish @PUT@ and answering with the
given status (201 success, 409 idempotent-conflict, 503 transient). It records each
publish PUT's path into an 'IORef'; the base URL and that log are yielded to the
body. -}
withMirrorTarget :: Status -> (Text -> IORef [ByteString] -> IO a) -> IO a
withMirrorTarget status body = do
    logRef <- newIORef []
    testWithApplication (pure (app logRef)) $ \port ->
        body ("http://127.0.0.1:" <> show port) logRef
  where
    app :: IORef [ByteString] -> Application
    app logRef request respond = do
        when (requestMethod request == "PUT") $
            atomicModifyIORef' logRef (\xs -> (rawPathInfo request : xs, ()))
        respond (responseLBS status [] "{}")
