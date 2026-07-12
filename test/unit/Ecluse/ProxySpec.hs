-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.ProxySpec (spec) where

import Prelude hiding (get)

import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Server.Cache (newMetadataCache)
import Ecluse.Proxy (mountBindingFor)
import Ecluse.Runtime.Env (Env, newEnvWithAdmission, newWorkerHeartbeat)
import Ecluse.Runtime.Server (MountBinding (..), application, mkServerConfig)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Support (testServeAdmission)

newTestManager :: IO Manager
newTestManager = newManager defaultManagerSettings

newTestEnv :: IO Env
newTestEnv = do
    queue <- newTestMemoryQueue
    manager <- newTestManager
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    heartbeat <- newWorkerHeartbeat
    admission <- testServeAdmission
    newEnvWithAdmission admission queue manager manager metadataCache logEnv telemetryDisabled heartbeat

{- | The composed npm front door: a single npm mount with no packument-serve or
publish dependencies, assembled through the public binding resolver exactly as
the composition root would ('mountBindingFor' over npm).
-}
npmApp :: IO Application
npmApp = application (mkServerConfig (maybeToList (mountBindingFor Npm Nothing Nothing))) <$> newTestEnv

spec :: Spec
spec = do
    describe "the composed npm front door (a bare npm mount over mkServerConfig)" $
        with npmApp $ do
            it "mounts npm at /npm (answers /npm/-/ping locally with 200 {})" $
                get "/npm/-/ping" `shouldRespondWith` "{}"{matchStatus = 200}

            it "recognises an npm packument route under the mount (501; serve deps unwired)" $
                get "/npm/is-odd" `shouldRespondWith` 501

            it "does NOT mount npm at the root -- /-/ping there is the neutral 404" $
                get "/-/ping" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

            it "renders an unmounted prefix as a neutral text/plain 404" $
                get "/pypi/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

    describe "mountBindingFor -- ecosystem drives the binding" $ do
        it "resolves npm to a binding whose prefix is derived from the ecosystem (/npm)" $
            (bindingPrefix <$> mountBindingFor Npm Nothing Nothing) `shouldBe` Just ("npm" :| [])

        it "has no binding for an ecosystem with no adapter wired (loud Nothing, not a stub)" $ do
            (bindingPrefix <$> mountBindingFor PyPI Nothing Nothing) `shouldBe` Nothing
            (bindingPrefix <$> mountBindingFor RubyGems Nothing Nothing) `shouldBe` Nothing
