-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.MirrorSpec (spec) where

import Prelude hiding (get)

import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import Ecluse.Composition.Support (expectAppConfig)
import Ecluse.Core.Worker (Liveness (Liveness, liveHealthy, liveLastPoll))
import Ecluse.Mirror (mirrorServerConfig)
import Ecluse.Runtime.Server (ServerConfig (scMounts, scPort), probeOnlyApplication)
import Ecluse.Test.Wai (bodyContainsAll)

-- | A fixed poll instant, so the rendered probe body is deterministic.
polledAt :: UTCTime
polledAt = UTCTime (fromGregorian 2026 6 23) 0

{- | The dedicated worker's front door with the given liveness verdict and readiness gate
injected, which is how the composition root wires the consume-loop heartbeat behind it.
-}
mirrorApp :: Liveness -> Bool -> IO Application
mirrorApp liveness ready = do
    appCfg <- expectAppConfig [] Nothing
    probeOnlyApplication (mirrorServerConfig appCfg (pure ready) (pure liveness))

spec :: Spec
spec = do
    describe "mirrorServerConfig -- the dedicated worker's health surface" $ do
        it "listens on the shared server.port, so every role reads one configuration key" $ do
            appCfg <- expectAppConfig [("ECLUSE_SERVER__PORT", "9231")] Nothing
            scPort (mirrorServerConfig appCfg (pure True) (pure alive)) `shouldBe` 9231

        it "serves no mount: a worker pod exposes probes and no request surface" $ do
            appCfg <- expectAppConfig [] Nothing
            scMounts (mirrorServerConfig appCfg (pure True) (pure alive)) `shouldSatisfy` null

    describe "the dedicated worker's probes -- a healthy consume loop" $
        with (mirrorApp alive True) $ do
            it "answers /livez with 200 and the last successful poll an orchestrator can judge" $
                get "/livez"
                    `shouldRespondWith` 200{matchBody = bodyContainsAll ["\"lastPoll\"", "2026-06-23T00:00:00"]}

            it "answers /readyz with 200 once the advisory sync has landed" $
                get "/readyz" `shouldRespondWith` 200

            it "404s a package path, because the worker role mounts no registry" $
                get "/npm/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

    describe "the dedicated worker's probes -- a stalled consume loop" $
        with (mirrorApp stalled True) $
            it "fails /livez with 503, so the orchestrator restarts the pod" $
                get "/livez"
                    `shouldRespondWith` 503{matchBody = bodyContainsAll ["liveness check failed", "2026-06-23T00:00:00"]}

    describe "the dedicated worker's probes -- awaiting startup readiness" $
        with (mirrorApp alive False) $ do
            it "fails /readyz with 503 until the advisory sync lands" $
                get "/readyz" `shouldRespondWith` 503

            it "keeps /livez at 200 (a worker still syncing is alive, not stalled)" $
                get "/livez" `shouldRespondWith` 200
  where
    alive = Liveness{liveHealthy = True, liveLastPoll = Just polledAt}
    stalled = Liveness{liveHealthy = False, liveLastPoll = Just polledAt}
