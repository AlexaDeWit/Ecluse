-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.WorkerSpec (spec) where

import Network.HTTP.Types (status200, status201, status409, status503)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (HashAlg (SRI), mkPackageName)
import Ecluse.Core.Queue (
    MirrorJob (..),
    MirrorQueue (enqueue, receive),
 )
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
import Ecluse.Integration.WorkerLoop (
    mirrorPoliciesAt,
    newQueueEnv,
    publishedAtLeast,
    runLoopFor,
    runLoopUntil,
    withMirrorTarget,
 )
import Ecluse.Runtime.Env (envWorkerHeartbeat, lastPoll)
import Ecluse.Test.Package (sriSha512Of, unsafeFilename, unsafeHash)
import Ecluse.Test.Stub (stubBaseUrl, withStub)

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
                        env <- newQueueEnv queue
                        policies <- faithfulPolicies mirrorUrl
                        unwrapQ (enqueue queue (job upstreamUrl))
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
                        env <- newQueueEnv queue
                        -- The re-admitted digest is well-formed but does not match the served
                        -- bytes: a tampered artifact the worker must refuse to publish.
                        tamperPolicies <- mirrorPoliciesAt Nothing mirrorUrl (unsafeHash SRI mismatchSri :| [])
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
                        env <- newQueueEnv queue
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
                        env <- newQueueEnv queue
                        policies <- faithfulPolicies mirrorUrl
                        unwrapQ (enqueue queue (job upstreamUrl))
                        -- A second PUT exists only because the un-acked 503 message redelivered
                        -- through 'releaseForRetry'. Stopping at the first PUT races that release.
                        runLoopUntil policies env (publishedAtLeast publishLog 2)
                        published <- readIORef publishLog
                        -- At least twice, not an exact count: the loop keeps redriving until
                        -- teardown, so "redelivered at least once" is the invariant.
                        length published `shouldSatisfy` (>= 2)
                        published `shouldSatisfy` all (== npmPublishPath)

            it "never mirrors an over-cap artifact; it dead-letters the job to ride the redrive policy (issue #846)" $ \container ->
                withUpstream $ \upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl publishLog -> do
                        queue <- freshQueue container "worker-overcap" defaultQueueOptions
                        env <- newQueueEnv queue
                        -- A fetch cap below the artifact's size. The bounded fetch aborts fail-
                        -- closed, so the worker dead-letters the job and never mirrors it.
                        policies <- mirrorPoliciesAt (Just 8) mirrorUrl (unsafeHash SRI trueSri :| [])
                        unwrapQ (enqueue queue (job upstreamUrl))
                        runLoopFor policies env 4_000_000
                        published <- readIORef publishLog
                        published `shouldBe` []

            it "advances the heartbeat as the loop polls a real queue" $ \container ->
                withUpstream $ \_upstreamUrl ->
                    withMirrorTarget status201 $ \mirrorUrl _publishLog -> do
                        queue <- freshQueue container "worker-heartbeat" defaultQueueOptions{qoWaitSeconds = 1}
                        env <- newQueueEnv queue
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

-- The true SRI of the served bytes: the digest the worker re-admits from current
-- metadata and verifies the fetched bytes against.
trueSri :: Text
trueSri = sriSha512Of (toStrict tarballBytes)

-- A well-formed sha512 SRI of OTHER bytes, the tamper fixture: current metadata whose
-- digest the served bytes cannot satisfy, distinct from a malformed digest.
mismatchSri :: Text
mismatchSri = sriSha512Of "completely-different-bytes"

-- Policies whose re-admitted artifact carries the served bytes' true digest, so
-- verification passes and the pipeline publishes at that mirror target.
faithfulPolicies :: Text -> IO WorkerPolicies
faithfulPolicies mirrorUrl = mirrorPoliciesAt Nothing mirrorUrl (unsafeHash SRI trueSri :| [])

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
        , jobArtifactFilename = unsafeFilename "left-pad-1.3.0.tgz"
        , jobTraceContext = Nothing
        }

-- A WAI upstream serving the artifact bytes at any path, yielding its base URL.
withUpstream :: (Text -> IO a) -> IO a
withUpstream body = withStub status200 tarballBytes (body . stubBaseUrl)
