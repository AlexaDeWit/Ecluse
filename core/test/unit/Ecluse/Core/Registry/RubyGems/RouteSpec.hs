-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.RubyGems.RouteSpec (spec) where

import Network.HTTP.Types (methodDelete, methodGet, methodHead, methodPost)
import Test.Hspec

import Ecluse.Core.Registry.RubyGems.Route (rubyGemsRoutes)
import Ecluse.Core.Server.Route (Route (routeName), RouteName (RouteName), matchRoute)

spec :: Spec
spec = describe "Ruby native route captures" $ do
    for_ [methodGet, methodHead] $ \method -> do
        it "claims discovery, admitted info, specifications and archives" $ do
            claimed method ["versions"] `shouldBe` Just (RouteName "versions")
            claimed method ["info", "name"] `shouldBe` Just (RouteName "info")
            claimed method ["api", "v1", "dependencies"] `shouldBe` Nothing
            claimed method ["quick", "Marshal.4.8", "name-1.0-x86_64-linux-musl.gemspec.rz"] `shouldBe` Just (RouteName "compactSpec")
            claimed method ["gems", "name-1.0-x86_64-linux-musl.gem"] `shouldBe` Just (RouteName "artifact")
        it "exposes no names, full-index or client-facing yank fallback" $ do
            for_ [["info", "bad/name"], ["names"], ["specs.4.8.gz"], ["api", "v1", "gems", "yank"]] $ \path -> claimed method path `shouldBe` Nothing
    it "claims only the native publication method" $ do
        claimed methodPost ["api", "v1", "gems"] `shouldBe` Just (RouteName "publish")
        claimed methodDelete ["api", "v1", "gems", "yank"] `shouldBe` Nothing
  where
    claimed method path = routeName . fst <$> matchRoute rubyGemsRoutes method [] path
