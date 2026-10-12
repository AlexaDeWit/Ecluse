-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The mirror-worker loop harness the integration suite shares.

The worker spec and the AWS end-to-end spec both drive 'Ecluse.runWorker' against a real
queue. Both need a supervised loop under a hard timeout, a mirror-target stub recording
each publish @PUT@, worker policies over the production npm publish marriage, and an 'Env'
carrying only the queue. This lives in the suite rather than @ecluse-test-support@ because
'Ecluse.runWorker' comes from the app library.
-}
module Ecluse.Integration.WorkerLoop (
    -- * Driving the supervised loop
    runLoopUntil,
    runLoopFor,
    publishedAtLeast,

    -- * Fixtures the loop runs against
    withMirrorTarget,
    observeAcknowledgement,
    mirrorPoliciesAt,
    mirrorPoliciesUnderAt,
    newQueueEnv,
) where

import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status, status200, status404, statusCode)
import Network.Wai (Application, rawPathInfo, requestMethod, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import UnliftIO (timeout)

import Ecluse.Core.Credential (bareCredential, mkSecret)
import Ecluse.Core.Package (Hash)
import Ecluse.Core.Queue (MirrorQueue (ack))
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec)
import Ecluse.Core.Registry.Publish (MirrorPublish, MirrorTransport (MirrorTransport, ptLimits, ptManager, ptMintCredential, ptTracing), newMirrorPublish)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Worker (WorkerPolicies)
import Ecluse.Runtime.Env (Env)
import Ecluse.Server.Pipeline.TestSupport (newTestEnvWithQueue)
import Ecluse.Service (runWorker)
import Ecluse.Test.Poll (runUntil)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Wai (localhost)
import Ecluse.Test.Worker (admitAllPolicies, admitAllPoliciesCapped, mirrorPoliciesUnder)

-- | Run until progress, failing on timeout or early exit, then cancel the worker.
runLoopUntil :: WorkerPolicies -> Env -> IO Bool -> IO ()
runLoopUntil policies env = runUntil loopHardTimeout 200_000 (runWorker policies env)

{- | Run the supervised worker for a fixed wall-clock window, then cancel it. For a case
asserting a negative, where no positive condition exists to wait on.
-}
runLoopFor :: WorkerPolicies -> Env -> Int -> IO ()
runLoopFor policies env micros = void (timeout micros (runWorker policies env))

-- | Whether the mirror-target stub has recorded at least @n@ publish @PUT@s.
publishedAtLeast :: IORef [a] -> Int -> IO Bool
publishedAtLeast logRef n = (>= n) . length <$> readIORef logRef

-- 45s clears the slowest healthy case (the redelivery wait, several times slower under
-- @-fhpc@), so the ceiling fires only on a genuine hang.
loopHardTimeout :: Int
loopHardTimeout = 45_000_000

-- | Observe successful acknowledgement without changing the queue's receipt or failure behaviour.
observeAcknowledgement :: MirrorQueue -> IO (MirrorQueue, IO Bool)
observeAcknowledgement queue = do
    acknowledged <- newIORef False
    let observed =
            queue
                { ack = \receipt -> do
                    result <- ack queue receipt
                    when (isRight result) (writeIORef acknowledged True)
                    pure result
                }
    pure (observed, readIORef acknowledged)

-- | Accepted native publication documents become visible to subsequent inventory probes.
withMirrorTarget :: Status -> (Text -> IORef [ByteString] -> IO a) -> IO a
withMirrorTarget replyStatus body = do
    logRef <- newIORef []
    stored <- newIORef Nothing
    testWithApplication (pure (app logRef stored)) $ \port -> body (localhost port) logRef
  where
    app :: IORef [ByteString] -> IORef (Maybe LBS.ByteString) -> Application
    app logRef stored request respond
        | requestMethod request == "PUT" = do
            publication <- strictRequestBody request
            atomicModifyIORef' logRef (\xs -> (rawPathInfo request : xs, ()))
            when (statusCode replyStatus `elem` [200, 201, 409]) (writeIORef stored (Just publication))
            respond (responseLBS replyStatus [] "{}")
        | otherwise =
            readIORef stored >>= \case
                Nothing -> respond (responseLBS status404 [] "{}")
                Just publication -> respond (responseLBS status200 [("Content-Type", "application/json")] publication)

{- | Admit-everything policies publishing through the production marriage (npm's codec over
the shared transport) at @mirrorUrl@. A 'Just' caps the fetch: an artifact past it is dropped.
-}
mirrorPoliciesAt :: Maybe Int -> Text -> NonEmpty Hash -> IO WorkerPolicies
mirrorPoliciesAt cap mirrorUrl digests = do
    publish <- newPublishAt mirrorUrl
    pure (maybe admitAllPolicies admitAllPoliciesCapped cap publish digests)

{- | 'mirrorPoliciesAt' under the caller's own rules and no fetch cap, for a case whose workers boot
different policies over one queue.
-}
mirrorPoliciesUnderAt :: [PreparedRule] -> Text -> NonEmpty Hash -> IO WorkerPolicies
mirrorPoliciesUnderAt rules mirrorUrl digests = do
    publish <- newPublishAt mirrorUrl
    pure (mirrorPoliciesUnder rules publish digests)

-- The production publish marriage every fixture policy writes through, aimed at a stub target.
newPublishAt :: Text -> IO MirrorPublish
newPublishAt mirrorUrl = do
    manager <- newManager defaultManagerSettings
    let transport =
            MirrorTransport
                { ptManager = manager
                , ptMintCredential = pure (Just (bareCredential (mkSecret "test-token")))
                , -- The mount's plan-resolved response bound on the probe (production
                  -- threads 'pdLimits'). The default here, since no override is set.
                  ptLimits = defaultLimits
                , ptTracing = passthroughTracingPort
                }
    pure (newMirrorPublish transport (loopbackRegistryUrl mirrorUrl) (loopbackRegistryUrl mirrorUrl) npmPublishCodec)

-- | An 'Env' over handle doubles and a real (no-TLS) manager, carrying only the given queue.
newQueueEnv :: MirrorQueue -> IO Env
newQueueEnv queue = newManager defaultManagerSettings >>= newTestEnvWithQueue queue
