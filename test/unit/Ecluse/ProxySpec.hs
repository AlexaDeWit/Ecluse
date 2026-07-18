-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.ProxySpec (spec) where

import Prelude hiding (get)

import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Proxy (mountBindingFor)
import Ecluse.Runtime.Server (MountBinding (..), application, mkServerConfig)
import Ecluse.Runtime.Test.Support (newTestEnv)
import Ecluse.Test.Server.Mount (inertPackumentDeps)

{- | The composed npm front door: a single npm mount with __inert__ packument-serve
dependencies (every upstream a closed port) and no publish target, assembled through the
public binding resolver exactly as the composition root would ('mountBindingFor' over npm).
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
                -- The route is recognised AND reaches the pipeline: with both upstreams
                -- bound to a closed port, no version survives and the most recoverable
                -- cause is transient, so the serve path answers 503. A 404 here would mean
                -- the mount's router never claimed the path at all.
                get "/npm/is-odd" `shouldRespondWith` 503

            it "does NOT mount npm at the root -- /-/ping there is the neutral 404" $
                get "/-/ping" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

            it "renders an unmounted prefix as a neutral text/plain 404" $
                get "/pypi/is-odd" `shouldRespondWith` "Not Found\n"{matchStatus = 404}

    describe "mountBindingFor -- ecosystem drives the binding" $ do
        it "resolves npm to a binding whose prefix is derived from the ecosystem (/npm)" $
            (bindingPrefix <$> mountBindingFor Npm inertPackumentDeps Nothing) `shouldBe` Just ("npm" :| [])

        it "has no binding for an ecosystem with no adapter wired (loud Nothing, not a stub)" $ do
            (bindingPrefix <$> mountBindingFor PyPI inertPackumentDeps Nothing) `shouldBe` Nothing
            (bindingPrefix <$> mountBindingFor RubyGems inertPackumentDeps Nothing) `shouldBe` Nothing
