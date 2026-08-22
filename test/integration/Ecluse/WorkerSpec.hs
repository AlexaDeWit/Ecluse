-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.WorkerSpec (spec) where

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
import Ecluse.Core.Package (Hash, HashAlg (SRI), mkPackageName)
import Ecluse.Core.Queue (
    MirrorJob (..),
    MirrorQueue (enqueue, receive),
 )
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec)
import Ecluse.Core.Registry.Publish (MirrorTransport (MirrorTransport, ptLimits, ptManager, ptMintToken), newMirrorPublish)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Core.Worker (WorkerPolicies)
import Ecluse.Integration.Ministack (
    QueueOptions (qoWaitSeconds),
    defaultQueueOptions,
    freshQueue,
    unwrapQ,
    withMinistack,
 )
import Ecluse.Runtime.Env (Env, envWorkerHeartbeat, lastPoll)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Runtime.Test.Support (newTestEnvWith)
import Ecluse.Test.Package (sriSha512Of, unsafeHash)
import Ecluse.Test.Worker (admitAllPolicies, admitAllPoliciesCapped)

{- | The mirror worker end to end against real SQS (a @ministack@ container) and WAI
stubs. It covers the queue semantics the in-memory double cannot reproduce: visibility
timeouts, redelivery, and held messages. Needs a Docker daemon and no real AWS.
-}
spec :: Spec
spec =
    aroundAll withMinistack $
        describe "mirror worker (ministack + WAI stubs)" $ do
            it "fetches, verifies, publishes, and acks a faithful job (via the loop)" $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-success" defaultQueueOptions
                        env <- envFor queue
                        policies <- faithfulPolicies mirrorUrl
                        unwrapQ (enqueue queue (job upstreamUrl))
                        -- Run the supervised loop against the real queue until it
                        -- publishes, then cancel it.
                        runLoopUntil policies env (publishedAtLeast publishLog 1)
                        published <- readIORef publishLog
                        length published `shouldBe` 1
                        -- The worker acked the job, so it does not redeliver.
                        leftover <- unwrapQ (receive queue)
                        leftover `shouldBe` []

            it "publishes nothing when the artifact fails its integrity digest" $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-tamper" defaultQueueOptions
                        env <- envFor queue
                        -- The re-admitted digest is well-formed but does not match the served
                        -- bytes: a tampered artifact. The worker must refuse to publish.
                        tamperPolicies <- policiesFor mirrorUrl (unsafeHash SRI mismatchSri :| [])
                        unwrapQ (enqueue queue (job upstreamUrl))
                        runLoopFor tamperPolicies env 4_000_000
                        published <- readIORef publishLog
                        published `shouldBe` []

            it "treats a 409 (version already present) as idempotent success and acks" $ \container ->
                withUpstream $ \upstreamUrl ->
                    -- The mirror target answers 409 (the version is already present). 409-is-
                    -- success, so the worker acks and the job does not redeliver.
                    withMirrorTarget status409 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-idempotent" defaultQueueOptions
                        env <- envFor queue
                        policies <- faithfulPolicies mirrorUrl
                        unwrapQ (enqueue queue (job upstreamUrl))
                        runLoopUntil policies env (publishedAtLeast publishLog 1)
                        leftover <- unwrapQ (receive queue)
                        leftover `shouldBe` []

            it "leaves a transiently-rejected job un-acked, so it redelivers" $ \container ->
                withUpstream $ \upstreamUrl ->
                    -- The mirror target answers 503 (a retryable rejection). The worker
                    -- must not ack, so the message redelivers: a real second delivery.
                    withMirrorTarget status503 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-retry" defaultQueueOptions
                        env <- envFor queue
                        policies <- faithfulPolicies mirrorUrl
                        unwrapQ (enqueue queue (job upstreamUrl))
                        -- Wait for a second publish PUT: it exists only because the un-acked 503
                        -- message redelivered through 'releaseForRetry', which makes it visible at
                        -- once (the success path's 'holdForLongPublish' holds it 300s). Stopping at
                        -- the first PUT races that release.
                        runLoopUntil policies env (publishedAtLeast publishLog 2)
                        published <- readIORef publishLog
                        -- At least twice, not an exact count: the loop keeps redriving until
                        -- teardown, so the tally depends on timing while "redelivered at least
                        -- once" is the invariant.
                        length published `shouldSatisfy` (>= 2)
                        published `shouldSatisfy` all (== npmPublishPath)

            it "never mirrors an over-cap artifact; it dead-letters the job to ride the redrive policy (issue #846)" $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-overcap" defaultQueueOptions
                        env <- envFor queue
                        -- A fetch cap below the artifact's size. The bounded fetch aborts fail-
                        -- closed, so the worker dead-letters the job to the redrive policy and
                        -- never acks, deletes, or mirrors it.
                        policies <- cappedPolicies mirrorUrl 8
                        unwrapQ (enqueue queue (job upstreamUrl))
                        runLoopFor policies env 4_000_000
                        published <- readIORef publishLog
                        published `shouldBe` []

            it "advances the heartbeat as the loop polls a real queue" $ \container ->
                withUpstream $ \_upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl _publishLog -> do
                        queue <- freshQueue container "worker-heartbeat" defaultQueueOptions{qoWaitSeconds = 1}
                        env <- envFor queue
                        policies <- faithfulPolicies mirrorUrl
                        pollBefore <- lastPoll (envWorkerHeartbeat env)
                        pollBefore `shouldBe` Nothing
                        -- No job enqueued: an idle loop still completes real polls, so
                        -- the heartbeat must advance from Nothing.
                        runLoopFor policies env 3_000_000
                        pollAfter <- lastPoll (envWorkerHeartbeat env)
                        pollAfter `shouldSatisfy` isJust

-- The artifact bytes the upstream stub serves.
tarballBytes :: LByteString
tarballBytes = "left-pad-artifact-bytes"

-- The true SRI (@sha512-<base64>@) of the served bytes: the digest the worker's
-- re-evaluation re-admits from current metadata and verifies the fetched bytes against.
trueSri :: Text
trueSri = sriSha512Of (toStrict tarballBytes)

{- | A well-formed sha512 SRI of OTHER bytes, the tamper fixture: current metadata
whose digest the served bytes cannot satisfy, distinct from a malformed digest.
-}
mismatchSri :: Text
mismatchSri = sriSha512Of "completely-different-bytes"

{- | Policies whose re-admitted artifact carries the served bytes' true digest, so
verification passes and the pipeline publishes at that mirror target.
-}
faithfulPolicies :: Text -> IO WorkerPolicies
faithfulPolicies mirrorUrl = policiesFor mirrorUrl (unsafeHash SRI trueSri :| [])

{- | Admit-everything policies publishing through the production marriage (npm's codec
over the shared transport) at the given mirror target, as the composition root builds it.
-}
policiesFor :: Text -> NonEmpty Hash -> IO WorkerPolicies
policiesFor mirrorUrl digests = do
    manager <- newManager defaultManagerSettings
    let transport =
            MirrorTransport
                { ptManager = manager
                , ptMintToken = pure (Just (mkSecret "test-token"))
                , -- The mount's plan-resolved response bound on the probe (production
                  -- threads 'pdLimits'). The default here, since no override is set.
                  ptLimits = defaultLimits
                }
    pure (admitAllPolicies (newMirrorPublish transport mirrorUrl npmPublishCodec) digests)

{- | 'policiesFor' with an explicit artifact fetch byte cap, for the over-cap drop
case. An artifact larger than the cap is a terminal drop, never a retry.
-}
cappedPolicies :: Text -> Int -> IO WorkerPolicies
cappedPolicies mirrorUrl cap = do
    manager <- newManager defaultManagerSettings
    let transport =
            MirrorTransport
                { ptManager = manager
                , ptMintToken = pure (Just (mkSecret "test-token"))
                , -- The mount's plan-resolved response bound on the probe (production
                  -- threads 'pdLimits'). The default here, since no override is set.
                  ptLimits = defaultLimits
                }
    pure (admitAllPoliciesCapped cap (newMirrorPublish transport mirrorUrl npmPublishCodec) (unsafeHash SRI trueSri :| []))

-- The path the job's artifact URL appends to the upstream stub base.
artifactPath :: Text
artifactPath = "/left-pad/-/left-pad-1.3.0.tgz"

-- The path an npm publish PUTs to: @\/{package}@. The mirror-target stub records it, so
-- the redelivery case asserts each delivery's PUT landed here.
npmPublishPath :: ByteString
npmPublishPath = "/left-pad"

-- A mirror job pointing at the upstream stub. The payload names the artifact by
-- filename only: the digests the worker verifies against live on the policies.
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

envFor :: MirrorQueue -> IO Env
envFor queue = do
    manager <- newManager defaultManagerSettings
    newTestEnvWith queue (manager, manager) telemetryDisabled

{- Run the supervised worker against the real queue until a condition holds, then cancel
it with 'race_'. A hard timeout bounds the run, so a failing test cannot hang. -}
runLoopUntil :: WorkerPolicies -> Env -> IO Bool -> IO ()
runLoopUntil policies env done =
    void $ timeout loopHardTimeout $ race_ (runWorker policies env) (waitFor done)

{- The ceiling on a 'runLoopUntil' run. 45s clears the slowest healthy case (the
redelivery wait, several times slower under @-fhpc@), so it fires only on a real hang. -}
loopHardTimeout :: Int
loopHardTimeout = 45_000_000

{- Run the supervised worker for a fixed wall-clock window, then cancel it. For the
cases asserting a negative, where no positive condition exists to wait on. -}
runLoopFor :: WorkerPolicies -> Env -> Int -> IO ()
runLoopFor policies env micros = void (timeout micros (runWorker policies env))

-- Poll until the condition holds, bounded at ~40s of 200ms ticks. That sits just under
-- 'loopHardTimeout', so the ceiling fires on a genuine hang rather than this poller.
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

{- A WAI mirror-target stub answering the given status and recording each publish PUT's
path. The presence probe gets the same @{}@ body, which never parses as a version list,
so every job runs the full pipeline rather than the dedup short-circuit. -}
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
