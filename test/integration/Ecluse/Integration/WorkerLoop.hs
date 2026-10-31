-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Supervised worker loops, native mirror targets, and queue fixtures for integration tests.
module Ecluse.Integration.WorkerLoop (
    -- * Driving the supervised loop
    runLoopUntil,
    runLoopFor,
    publishedAtLeast,

    -- * Fixtures the loop runs against
    withMirrorTarget,
    observeAcknowledgement,
    observeDeadLetter,
    mirrorPoliciesAt,
    mirrorPoliciesUnderAt,
    newQueueEnv,
) where

import Data.Aeson (Value (Object), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status, status200, status404, statusCode)
import Network.Wai (Application, rawPathInfo, requestMethod, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import UnliftIO (timeout)

import Ecluse.Core.Credential (bareCredential, mkSecret)
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Package (Hash)
import Ecluse.Core.Queue (MirrorQueue (ack, deadLetter))
import Ecluse.Core.Registry.Npm.Filter (rewriteVersion)
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec)
import Ecluse.Core.Registry.Publish (MirrorPublish, MirrorTransport (MirrorTransport, ptClient, ptLimits, ptMintCredential, ptTracing), newMirrorPublish)
import Ecluse.Core.Registry.Request (nativePresentation)
import Ecluse.Core.Registry.ServedDocument (documentObject, objectField)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Worker (WorkerPolicies)
import Ecluse.Runtime.Env (Env)
import Ecluse.Server.Pipeline.TestSupport (newTestEnvWithQueue)
import Ecluse.Service (runWorker)
import Ecluse.Test.Deadline (untracedClient)
import Ecluse.Test.Poll (runUntil)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Support (decodeJsonOrFail, expectRightText)
import Ecluse.Test.Wai (localhost, selfBaseUrl)
import Ecluse.Test.Worker (admitAllPolicies, admitAllPoliciesCapped, mirrorPoliciesUnder)

-- | Run until progress, failing on timeout or early exit, then cancel the worker.
runLoopUntil :: WorkerPolicies -> Env -> IO Bool -> IO ()
runLoopUntil policies env = runUntil loopHardTimeout 200_000 (runWorker policies env)

-- | Use a fixed cancellation window only when no positive completion signal exists.
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

-- | Observe successful terminal disposition after the real queue operation completes.
observeDeadLetter :: MirrorQueue -> IO (MirrorQueue, IO Bool)
observeDeadLetter queue = do
    (observed, disposed) <- observeAcknowledgement queue{ack = deadLetter queue}
    pure (queue{deadLetter = ack observed}, disposed)

-- | Accepted native publication documents become visible to subsequent inventory probes.
withMirrorTarget :: Status -> (Text -> IORef [ByteString] -> IO a) -> IO a
withMirrorTarget replyStatus body = do
    logRef <- newIORef []
    stored <- newIORef Nothing
    testWithApplication (pure (app logRef stored)) $ \port -> body (localhost port) logRef
  where
    app :: IORef [ByteString] -> IORef (Maybe Value) -> Application
    app logRef stored request respond
        | requestMethod request == "PUT" = do
            publication <- decodeJsonOrFail . toStrict =<< strictRequestBody request
            atomicModifyIORef' logRef (\xs -> (rawPathInfo request : xs, ()))
            when (statusCode replyStatus `elem` [200, 201, 409]) $ do
                let fields = documentObject publication
                    artifactUrl filename = Just (selfBaseUrl request <> decodeUtf8 (rawPathInfo request) <> "/-/" <> filename)
                nativeVersions <- expectRightText (maybeToRight "publication has no versions object" (objectField "versions" fields))
                let versions = Object (fmap (rewriteVersion artifactUrl) nativeVersions)
                writeIORef stored (Just (Object (KeyMap.insert "versions" versions fields)))
            respond (responseLBS replyStatus [] "{}")
        | otherwise =
            readIORef stored >>= \case
                Nothing -> respond (responseLBS status404 [] "{}")
                Just publication -> respond (responseLBS status200 [("Content-Type", "application/json")] (encode publication))

-- | Admit every version through the npm writer, with an optional artifact fetch cap.
mirrorPoliciesAt :: Deadlines -> Maybe Int -> Text -> NonEmpty Hash -> IO WorkerPolicies
mirrorPoliciesAt deadlines cap mirrorUrl digests = do
    publish <- newPublishAt deadlines mirrorUrl
    pure (maybe admitAllPolicies admitAllPoliciesCapped cap publish digests)

-- | Apply the consuming worker's rules while retaining the default artifact fetch cap.
mirrorPoliciesUnderAt :: Deadlines -> [PreparedRule] -> Text -> NonEmpty Hash -> IO WorkerPolicies
mirrorPoliciesUnderAt deadlines rules mirrorUrl digests = do
    publish <- newPublishAt deadlines mirrorUrl
    pure (mirrorPoliciesUnder rules publish digests)

newPublishAt :: Deadlines -> Text -> IO MirrorPublish
newPublishAt deadlines mirrorUrl = do
    client <- untracedClient <$> newManager defaultManagerSettings <*> pure deadlines
    let transport =
            MirrorTransport
                { ptClient = client
                , ptMintCredential = pure (Just (bareCredential (mkSecret "test-token")))
                , ptLimits = defaultLimits
                , ptTracing = passthroughTracingPort
                }
    pure (newMirrorPublish transport (loopbackRegistryUrl mirrorUrl) (loopbackRegistryUrl mirrorUrl) (npmPublishCodec nativePresentation))

-- | An 'Env' over handle doubles, a real (no-TLS) manager and swept deadlines, carrying only the given queue.
newQueueEnv :: Deadlines -> MirrorQueue -> IO Env
newQueueEnv deadlines queue = newManager defaultManagerSettings >>= newTestEnvWithQueue deadlines queue
