-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.BenchLoad.NpmArtifactSpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Types (methodGet)
import Test.Hspec

import Ecluse.BenchLoad.NpmArtifact (SelectedArtifact (..), selectedNpmArtifact)
import Ecluse.Core.Registry.Npm.Route.Internal (npmRoutes)
import Ecluse.Core.Server.Route (Route (routeName), RouteName (RouteName), matchRoute)
import Ecluse.Test.Package (unscopedNpm)

spec :: Spec
spec = describe "captured npm artifact follow-up" $ do
    it "selects a real captured coordinate claimed by the production tarball route" $ do
        bytes <- readFileBS "bench/corpus/npm/request.full.json"
        case selectedNpmArtifact (unscopedNpm "request") "2.88.2" bytes of
            Left reason -> expectationFailure (toString reason)
            Right selected -> do
                saProxyPath selected `shouldBe` "request/-/request-2.88.2.tgz"
                saUpstreamUrl selected `shouldBe` "https://registry.npmjs.org/request/-/request-2.88.2.tgz"
                (routeName . fst <$> matchRoute npmRoutes methodGet [] (T.splitOn "/" (saProxyPath selected)))
                    `shouldBe` Just (RouteName "tarball")
                (routeName . fst <$> matchRoute npmRoutes methodGet [] ["request", "2.88.2"])
                    `shouldBe` Nothing
    it "fails for a missing pin instead of substituting a guessed endpoint" $ do
        bytes <- readFileBS "bench/corpus/npm/request.full.json"
        selectedNpmArtifact (unscopedNpm "request") "9999.0.0" bytes `shouldSatisfy` isLeft
