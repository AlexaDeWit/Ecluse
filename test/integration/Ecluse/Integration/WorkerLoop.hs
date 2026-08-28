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
    mirrorPoliciesAt,
    newQueueEnv,
) where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status)
import Network.Wai (Application, rawPathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import UnliftIO (race_, timeout)

import Ecluse (runWorker)
import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Package (Hash)
import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec)
import Ecluse.Core.Registry.Publish (MirrorTransport (MirrorTransport, ptLimits, ptManager, ptMintToken), newMirrorPublish)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Worker (WorkerPolicies)
import Ecluse.Runtime.Env (Env)
import Ecluse.Server.Pipeline.TestSupport (newTestEnvWithQueue)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Wai (localhost)
import Ecluse.Test.Worker (admitAllPolicies, admitAllPoliciesCapped)

{- | Run the supervised worker against the queue until @done@ holds, then cancel it with
'race_'. A hard timeout bounds the run, so a failing test cannot hang.
-}
runLoopUntil :: WorkerPolicies -> Env -> IO Bool -> IO ()
runLoopUntil policies env done =
    void $ timeout loopHardTimeout $ race_ (runWorker policies env) (waitFor done)

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

-- ~40s of 200ms ticks sits just under 'loopHardTimeout', so the ceiling reports the hang
-- rather than this poller conceding first.
waitFor :: IO Bool -> IO ()
waitFor done = void (pollUntil 200 200_000 id done)

{- | A WAI mirror-target stub answering @replyStatus@ and recording each publish @PUT@'s path.
Its @{}@ body never parses as a version list, so no job takes the dedup short-circuit.
-}
withMirrorTarget :: Status -> (Text -> IORef [ByteString] -> IO a) -> IO a
withMirrorTarget replyStatus body = do
    logRef <- newIORef []
    testWithApplication (pure (app logRef)) $ \port -> body (localhost port) logRef
  where
    app :: IORef [ByteString] -> Application
    app logRef request respond = do
        when (requestMethod request == "PUT") $
            atomicModifyIORef' logRef (\xs -> (rawPathInfo request : xs, ()))
        respond (responseLBS replyStatus [] "{}")

{- | Admit-everything policies publishing through the production marriage (npm's codec over
the shared transport) at @mirrorUrl@. A 'Just' caps the fetch: an artifact past it is dropped.
-}
mirrorPoliciesAt :: Maybe Int -> Text -> NonEmpty Hash -> IO WorkerPolicies
mirrorPoliciesAt cap mirrorUrl digests = do
    manager <- newManager defaultManagerSettings
    let transport =
            MirrorTransport
                { ptManager = manager
                , ptMintToken = pure (Just (mkSecret "test-token"))
                , -- The mount's plan-resolved response bound on the probe (production
                  -- threads 'pdLimits'). The default here, since no override is set.
                  ptLimits = defaultLimits
                }
    pure (maybe admitAllPolicies admitAllPoliciesCapped cap (newMirrorPublish transport mirrorUrl npmPublishCodec) digests)

-- | An 'Env' over handle doubles and a real (no-TLS) manager, carrying only the given queue.
newQueueEnv :: MirrorQueue -> IO Env
newQueueEnv queue = newManager defaultManagerSettings >>= newTestEnvWithQueue queue
