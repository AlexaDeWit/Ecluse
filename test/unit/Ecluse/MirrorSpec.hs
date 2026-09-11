-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.MirrorSpec (spec) where

import Prelude hiding (get)

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import Ecluse.Composition.Support (expectAppConfig)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Server.Readiness (
    MountReadiness (MountAwaitingFirstSync, MountReady),
    Readiness,
    alwaysReady,
    mountReadiness,
 )
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
mirrorApp :: Liveness -> Readiness -> IO Application
mirrorApp liveness ready = do
    appCfg <- expectAppConfig [] Nothing
    probeOnlyApplication (mirrorServerConfig appCfg (pure ready) (pure liveness))

spec :: Spec
spec = do
    describe "mirrorServerConfig -- the dedicated worker's health surface" $ do
        it "listens on the shared server.port, so every role reads one configuration key" $ do
            appCfg <- expectAppConfig [("ECLUSE_SERVER__PORT", "9231")] Nothing
            scPort (mirrorServerConfig appCfg (pure alwaysReady) (pure alive)) `shouldBe` 9231

        it "serves no mount: a worker pod exposes probes and no request surface" $ do
            appCfg <- expectAppConfig [] Nothing
            null (scMounts (mirrorServerConfig appCfg (pure alwaysReady) (pure alive))) `shouldBe` True

    describe "the dedicated worker's probes -- a healthy consume loop" $
        with (mirrorApp alive bothSynced) $ do
            it "answers /livez with 200 and the last successful poll an orchestrator can judge" $
                get "/livez"
                    `shouldRespondWith` 200{matchBody = bodyContainsAll ["\"lastPoll\"", "2026-06-23T00:00:00"]}

            it "answers /readyz with 200 once the advisory sync has landed" $
                get "/readyz"
                    `shouldRespondWith` 200{matchBody = bodyContainsAll ["\"status\":\"ready\"", "\"npm\":\"ready\"", "\"pypi\":\"ready\""]}

            it "404s a package path, because the worker role mounts no registry" $
                get "/npm/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

    describe "the dedicated worker's probes -- a stalled consume loop" $
        with (mirrorApp stalled bothSynced) $
            it "fails /livez with 503, so the orchestrator restarts the pod" $
                get "/livez"
                    `shouldRespondWith` 503{matchBody = bodyContainsAll ["liveness check failed", "2026-06-23T00:00:00"]}

    describe "the dedicated worker's probes -- awaiting startup readiness" $
        with (mirrorApp alive neitherSynced) $ do
            it "fails /readyz with 503 until an advisory sync lands, naming both mounts" $
                get "/readyz"
                    `shouldRespondWith` 503{matchBody = bodyContainsAll ["\"npm\":\"awaiting startup readiness\"", "\"pypi\":\"awaiting startup readiness\""]}

            it "keeps /livez at 200 (a worker still syncing is alive, not stalled)" $
                get "/livez" `shouldRespondWith` 200

    describe "the dedicated worker's probes -- one ecosystem's artifact missing" $
        with (mirrorApp alive npmOnly) $
            it "answers /readyz with 200 and names the mount still awaiting its database" $
                -- A missing PyPI database must not pull the pod, and its healthy npm mount,
                -- out of rotation.
                get "/readyz"
                    `shouldRespondWith` 200{matchBody = bodyContainsAll ["\"status\":\"ready\"", "\"npm\":\"ready\"", "\"pypi\":\"awaiting startup readiness\""]}
  where
    alive = Liveness{liveHealthy = True, liveLastPoll = Just polledAt}
    stalled = Liveness{liveHealthy = False, liveLastPoll = Just polledAt}
    bothSynced = mountReadiness (Map.fromList [(Npm, MountReady), (PyPI, MountReady)])
    npmOnly = mountReadiness (Map.fromList [(Npm, MountReady), (PyPI, MountAwaitingFirstSync)])
    neitherSynced = mountReadiness (Map.fromList [(Npm, MountAwaitingFirstSync), (PyPI, MountAwaitingFirstSync)])
