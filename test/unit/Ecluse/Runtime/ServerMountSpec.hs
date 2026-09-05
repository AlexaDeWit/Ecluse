-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.ServerMountSpec (spec) where

import Prelude hiding (get)

import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Runtime.Server (application, mkServerConfig)
import Ecluse.Runtime.Test.Support (newTestEnv)
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Server.Mount (inertPackumentDeps)

{- | A single npm mount with __inert__ packument-serve dependencies (every upstream a
closed port) and no publish target, resolved as the composition root resolves it.
-}
npmApp :: IO Application
npmApp = application (mkServerConfig (maybeToList (mountBindingFor Npm inertPackumentDeps Nothing))) <$> newTestEnv

spec :: Spec
spec = do
    describe "the composed npm front door (a bare npm mount over mkServerConfig)" $
        with npmApp $ do
            it "mounts npm at /npm (answers /npm/-/ping locally with 200 {})" $
                get "/npm/-/ping" `shouldRespondWith` "{}"{matchStatus = 200}

            it "routes an npm packument under the mount into the data plane (503; upstreams closed)" $
                -- Both upstreams are bound to a closed port, so no version survives and the serve
                -- path answers 503. A 404 would mean the mount's router never claimed the path.
                get "/npm/is-odd" `shouldRespondWith` 503

            it "does NOT mount npm at the root -- /-/ping there is the neutral 404" $
                get "/-/ping" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

            it "renders an unmounted prefix as a neutral text/plain 404" $
                get "/pypi/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}
