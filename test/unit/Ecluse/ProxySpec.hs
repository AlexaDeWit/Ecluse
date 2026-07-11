module Ecluse.ProxySpec (spec) where

import Prelude hiding (get)

import Katip (Environment (Environment), Namespace (Namespace), initLogEnv)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Queue.Memory (newInMemoryQueue)
import Ecluse.Core.Server.Cache (defaultCacheConfig, newMetadataCache)
import Ecluse.Proxy (mountBindingFor, npmServerConfig, unconfiguredRegistry)
import Ecluse.Runtime.Env (Env, newEnv, newWorkerHeartbeat)
import Ecluse.Runtime.Server (MountBinding (..), application)
import Ecluse.Runtime.Telemetry (telemetryDisabled)

newTestManager :: IO Manager
newTestManager = newManager defaultManagerSettings

newTestEnv :: IO Env
newTestEnv = do
    queue <- newInMemoryQueue
    manager <- newTestManager
    metadataCache <- newMetadataCache defaultCacheConfig
    logEnv <- initLogEnv (Namespace ["ecluse"]) (Environment "test")
    heartbeat <- newWorkerHeartbeat
    newEnv unconfiguredRegistry queue manager manager metadataCache logEnv telemetryDisabled heartbeat

npmApp :: IO Application
npmApp = application npmServerConfig <$> newTestEnv

spec :: Spec
spec = do
    describe "npmServerConfig -- the composed npm front door" $
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
