-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.ServiceSpec (spec) where

import Data.Time (addUTCTime, getCurrentTime)
import Test.Hspec

import Ecluse.Composition.MirrorRole (MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly))
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Worker (
    Liveness (liveHealthy, liveLastPoll),
    WorkerHeartbeat,
    newWorkerHeartbeat,
    recordPoll,
    workerHeartbeatStaleAfter,
 )
import Ecluse.Runtime.Server (MountBinding (bindingPrefix))
import Ecluse.Service (mountBindingFor, roleLiveness)
import Ecluse.Test.Server.Mount (inertPackumentDeps)

{- | A heartbeat whose last poll is older than 'workerHeartbeatStaleAfter': a consume loop
that stopped advancing, which is what the worker arm of @\/livez@ exists to catch.
-}
stalledHeartbeat :: IO WorkerHeartbeat
stalledHeartbeat = do
    heartbeat <- newWorkerHeartbeat
    now <- getCurrentTime
    recordPoll heartbeat (addUTCTime (negate (workerHeartbeatStaleAfter + 60)) now)
    pure heartbeat

spec :: Spec
spec = do
    describe "roleLiveness -- which roles are judged on the worker heartbeat" $ do
        it "reports a stalled consume loop as not live in the single-process role" $ do
            liveness <- stalledHeartbeat >>= roleLiveness ServeAndMirror
            liveHealthy liveness `shouldBe` False

        it "reports a stalled consume loop as not live in the dedicated worker role" $ do
            -- The whole point of the dedicated pod: an orchestrator restarts it on a stall.
            liveness <- stalledHeartbeat >>= roleLiveness MirrorOnly
            liveHealthy liveness `shouldBe` False

        it "stays live under --no-worker, which runs no consume loop to stall" $ do
            liveness <- stalledHeartbeat >>= roleLiveness ServeOnly
            liveHealthy liveness `shouldBe` True
            liveLastPoll liveness `shouldBe` Nothing

        it "carries the last poll instant so an orchestrator can judge staleness itself" $ do
            heartbeat <- newWorkerHeartbeat
            now <- getCurrentTime
            recordPoll heartbeat now
            liveness <- roleLiveness ServeAndMirror heartbeat
            liveHealthy liveness `shouldBe` True
            liveLastPoll liveness `shouldBe` Just now

        it "is live before the first poll, because a starting worker is not a stalled one" $ do
            liveness <- newWorkerHeartbeat >>= roleLiveness ServeAndMirror
            liveHealthy liveness `shouldBe` True
            liveLastPoll liveness `shouldBe` Nothing

    describe "mountBindingFor -- ecosystem drives the binding" $ do
        it "resolves npm to a binding whose prefix is derived from the ecosystem (/npm)" $
            (bindingPrefix <$> mountBindingFor Npm inertPackumentDeps Nothing) `shouldBe` Just ("npm" :| [])

        it "has no binding for an ecosystem with no adapter wired (loud Nothing, not a stub)" $ do
            (bindingPrefix <$> mountBindingFor PyPI inertPackumentDeps Nothing) `shouldBe` Nothing
            (bindingPrefix <$> mountBindingFor RubyGems inertPackumentDeps Nothing) `shouldBe` Nothing
