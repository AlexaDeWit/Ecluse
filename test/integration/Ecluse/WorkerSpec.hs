-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.WorkerSpec (spec) where

import Crypto.Hash (Digest, SHA512, hashlazy)
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Katip (Environment (Environment), LogEnv, Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status, status200, status201, status409, status503)
import Network.Wai (Application, rawPathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO (race_, timeout)
import UnliftIO.Concurrent (threadDelay)

import Ecluse (runWorker)
import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (HashAlg (SRI), mkPackageName)
import Ecluse.Core.Queue (
    MirrorJob (..),
    MirrorQueue (enqueue, receive),
 )
import Ecluse.Core.Registry.Npm (NpmClientConfig (NpmClientConfig, npmBaseUrl, npmLimits, npmManager, npmToken), newNpmClient)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Core.Worker (WorkerPolicies)
import Ecluse.Integration.Ministack (
    QueueOptions (qoWaitSeconds),
    defaultQueueOptions,
    freshQueue,
    unwrapQ,
    withMinistack,
 )
import Ecluse.Runtime.Env (Env, envWorkerHeartbeat, lastPoll, newEnvWithAdmission, newWorkerHeartbeat)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Test.Package (unsafeHash)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Support (testServeAdmission)
import Ecluse.Test.Worker (admitAllPolicies)

{- | The mirror worker, end to end against real SQS (a @ministack@ container, shared
through "Ecluse.Integration.Ministack") and WAI upstream/mirror stubs. These cases
exercise the queue semantics the in-memory double cannot faithfully reproduce --
real visibility timeouts, redelivery, @extendVisibility@-held messages, and the
supervised worker loop ('Ecluse.runWorker') itself polling a real queue (heartbeat
included).

Hermetic and gating, but requires a Docker daemon (for ministack) and no real AWS.
-}
spec :: Spec
spec =
    aroundAll withMinistack $
        describe "mirror worker (ministack + WAI stubs)" $ do
            it "fetches, verifies, publishes, and acks a faithful job (via the loop)" $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-success" defaultQueueOptions
                        env <- envFor queue mirrorUrl
                        unwrapQ (enqueue queue (job upstreamUrl))
                        -- Run the supervised loop against the real queue until it has
                        -- published, then cancel it.
                        runLoopUntil faithfulPolicies env (publishedAtLeast publishLog 1)
                        published <- readIORef publishLog
                        length published `shouldBe` 1
                        -- The job was acked, so it does not redeliver.
                        leftover <- unwrapQ (receive queue)
                        leftover `shouldBe` []

            it "publishes nothing when the artifact fails its integrity digest" $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-tamper" defaultQueueOptions
                        env <- envFor queue mirrorUrl
                        -- The version's current-metadata digest (the set the worker
                        -- re-admits and verifies against) is well-formed but does not
                        -- match the served bytes: a tampered artifact. The worker must
                        -- refuse to publish.
                        unwrapQ (enqueue queue (job upstreamUrl))
                        runLoopFor (admitAllPolicies (unsafeHash SRI mismatchSri :| [])) env 4_000_000
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
                        unwrapQ (enqueue queue (job upstreamUrl))
                        runLoopUntil faithfulPolicies env (publishedAtLeast publishLog 1)
                        leftover <- unwrapQ (receive queue)
                        leftover `shouldBe` []

            it "leaves a transiently-rejected job un-acked, so it redelivers" $ \container ->
                withUpstream $ \upstreamUrl ->
                    -- The mirror target answers 503 (a retryable rejection). The worker
                    -- must not ack, so the message redelivers -- a real second delivery.
                    withMirrorTarget status503 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-retry" defaultQueueOptions
                        env <- envFor queue mirrorUrl
                        unwrapQ (enqueue queue (job upstreamUrl))
                        -- Observe the redelivery through the worker's /own/ second
                        -- publish attempt: a transient 503 is never acked, so the message
                        -- becomes visible again and the running loop re-consumes it and
                        -- PUTs the same artifact a second time. We wait for two recorded
                        -- publish PUTs -- that second PUT only exists because the un-acked
                        -- message genuinely redelivered -- then tear the loop down.
                        --
                        -- This is robust-by-construction where the old test was a timing
                        -- knife-edge. Redelivery on this path is driven by the worker's
                        -- 'releaseForRetry' (it resets the message to visible the instant
                        -- the 503 comes back), NOT by the visibility timeout lapsing: the
                        -- success-path 'holdForLongPublish' has already extended the
                        -- in-flight window to 300s, so a bare timeout would never
                        -- redeliver within a test's patience (which is why this no longer
                        -- sets a 1s 'qoVisibilityTimeout' -- it never governed redelivery
                        -- here, it only looked like the lever).
                        --
                        -- The old test instead /stole/ the redelivery: it tore the loop
                        -- down the instant the first PUT was logged, then polled the queue
                        -- itself. But the stub records the PUT before it answers 503, so
                        -- the log fills a beat before the worker runs 'releaseForRetry' --
                        -- and under the slower -fhpc-instrumented loop the teardown could
                        -- win that race and cancel the loop before the release ran,
                        -- leaving the message held under the 300s hold so it never
                        -- redelivered within budget. Waiting on the worker's /second/ PUT
                        -- removes that race entirely: the second PUT cannot happen unless
                        -- the release ran and the message redelivered.
                        runLoopUntil faithfulPolicies env (publishedAtLeast publishLog 2)
                        published <- readIORef publishLog
                        -- The one un-acked job was delivered and PUT more than once -- a
                        -- real redelivery -- and every PUT targeted its publish path. We
                        -- assert "at least twice, all to the publish path" rather than an
                        -- exact count: once the condition trips, the loop keeps redriving
                        -- (each redelivery is another 503, another redelivery) until the
                        -- 'race_' teardown lands, so the exact tally is teardown-timing
                        -- dependent while "redelivered at least once" is the invariant.
                        length published `shouldSatisfy` (>= 2)
                        published `shouldSatisfy` all (== npmPublishPath)

            it "advances the heartbeat as the loop polls a real queue" $ \container ->
                withUpstream $ \_upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl _publishLog -> do
                        queue <- freshQueue container "worker-heartbeat" defaultQueueOptions{qoWaitSeconds = 1}
                        env <- envFor queue mirrorUrl
                        pollBefore <- lastPoll (envWorkerHeartbeat env)
                        pollBefore `shouldBe` Nothing
                        -- No job enqueued: an idle loop still completes real polls, so
                        -- the heartbeat must advance from Nothing.
                        runLoopFor faithfulPolicies env 3_000_000
                        pollAfter <- lastPoll (envWorkerHeartbeat env)
                        pollAfter `shouldSatisfy` isJust

-- The artifact bytes the upstream stub serves.
tarballBytes :: LByteString
tarballBytes = "left-pad-artifact-bytes"

-- The true SRI (@sha512-<base64>@) of the served bytes: the digest the worker's
-- re-evaluation re-admits from current metadata and verifies the fetched bytes against.
trueSri :: Text
trueSri = "sha512-" <> decodeUtf8 (convertToBase Base64 (hashlazy tarballBytes :: Digest SHA512) :: ByteString)

{- | A well-formed sha512 SRI of OTHER bytes, the tamper fixture: current metadata
whose digest the served bytes cannot satisfy, distinct from a malformed digest.
-}
mismatchSri :: Text
mismatchSri = "sha512-" <> decodeUtf8 (convertToBase Base64 (hashlazy "completely-different-bytes" :: Digest SHA512) :: ByteString)

{- | The faithful current-metadata policies: the re-admitted artifact carries the
served bytes' true digest, so verification passes and the pipeline publishes.
-}
faithfulPolicies :: WorkerPolicies
faithfulPolicies = admitAllPolicies (unsafeHash SRI trueSri :| [])

-- The path the job's artifact URL appends to the upstream stub base.
artifactPath :: Text
artifactPath = "/left-pad/-/left-pad-1.3.0.tgz"

-- The request path an npm publish PUTs to: @\/{package}@ (the unscoped @left-pad@
-- needs no escaping). This is what the mirror-target stub records in its publish
-- log, so the redelivery case asserts each delivery's PUT landed here.
npmPublishPath :: ByteString
npmPublishPath = "/left-pad"

-- A mirror job pointing at the upstream stub; the payload names the artifact by
-- filename only (the digests the worker verifies against live on the policies).
job :: Text -> MirrorJob
job upstreamUrl =
    MirrorJob
        { jobPackage = mkPackageName Npm Nothing "left-pad"
        , jobVersion = mkVersion Npm "1.3.0"
        , -- The flag-gated loopback former: the job points at an in-process http stub.
          jobArtifactUrl = loopbackRegistryUrl (upstreamUrl <> artifactPath)
        , jobArtifactFilename = "left-pad-1.3.0.tgz"
        , jobTraceContext = Nothing
        }

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
    admission <- testServeAdmission
    newEnvWithAdmission admission publishClient queue manager manager metadataCache logEnv telemetryDisabled heartbeat

-- A scribe-free LogEnv (no stdout output during the integration run).
newTestLogEnv :: IO LogEnv
newTestLogEnv = initLogEnv (Namespace ["ecluse"]) (Environment "test")

{- Run the supervised mirror worker ('runWorker') under the given re-evaluation
policies against the real queue until a condition holds, then tear it down. The loop
never returns on its own, so it is raced against a condition-poller ('race_'): when
the poller observes the condition, 'race_' cancels the loop -- the same cooperative
cancellation process shutdown uses. A hard timeout bounds the whole thing so a
failing test cannot hang. -}
runLoopUntil :: WorkerPolicies -> Env -> IO Bool -> IO ()
runLoopUntil policies env done =
    void $ timeout loopHardTimeout $ race_ (runWorker policies env) (waitFor done)

{- The hard ceiling on a 'runLoopUntil' run, sized so even the slowest positive
condition (the redelivery case waiting on a /second/ publish -- two full
fetch → verify → publish cycles plus a real redelivery in between) lands with
comfortable headroom under @-fhpc@ instrumentation, where the loop runs several
times slower than uninstrumented. Uninstrumented those steps take well under a
second; 45s is deliberately far above that so the ceiling never clips a healthy run
and only ever fires on a genuine hang. -}
loopHardTimeout :: Int
loopHardTimeout = 45_000_000

{- Run the supervised mirror worker ('runWorker') under the given re-evaluation
policies for a fixed wall-clock window, then cancel it -- for the cases that assert a
/negative/ (nothing published, an idle heartbeat) where there is no positive
condition to wait on. -}
runLoopFor :: WorkerPolicies -> Env -> Int -> IO ()
runLoopFor policies env micros = void (timeout micros (runWorker policies env))

-- Poll a condition until it holds, bounded so a failing test does not hang. The
-- bound (~40s of 200ms ticks) sits just under 'loopHardTimeout' so that ceiling, not
-- this poller, is what fires on a genuine hang -- while still leaving the slowest
-- healthy positive condition (the -fhpc redelivery wait) ample room to land.
waitFor :: IO Bool -> IO ()
waitFor done = go (200 :: Int)
  where
    go :: Int -> IO ()
    go 0 = pure ()
    go n =
        done >>= \case
            True -> pure ()
            False -> threadDelay 200_000 >> go (n - 1)

publishedAtLeast :: IORef [a] -> Int -> IO Bool
publishedAtLeast logRef n = (>= n) . length <$> readIORef logRef

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
body. The worker's mirror-presence probe (a metadata @GET@) receives the same fixed
answer, and @{}@ never parses as a version list, so every job here runs the full
pipeline rather than the dedup short-circuit. -}
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
