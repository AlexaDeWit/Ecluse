-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native sparse path spelling stays distinct from ownership aliases.
module Ecluse.Core.Registry.Cargo.RequestSpec (spec) where

import Network.HTTP.Client (path, redirectCount)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Registry.Cargo.Request (cargoConfigRequest, cargoIndexPath, cargoIndexRequest)
import Ecluse.Core.Registry.Request (mkBaseRequest)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "sparse index paths" $ do
    for_ [("A", "1/a"), ("AB", "2/ab"), ("ABC", "3/a/abc"), ("Ab_C-d", "ab/_c/ab_c-d")] $ \(name, expected) ->
        it ("keeps the native path of " <> toString name) $
            cargoIndexPath (mkPackageName Cargo Nothing name) `shouldBe` expected
    it "appends discovery and index paths under a configured base path" $ do
        base <- mkBaseRequest <$> expectRight (mkRegistryUrl "https://registry.test/tenant/index")
        discovery <- expectRight (cargoConfigRequest base Nothing)
        index <- expectRight (cargoIndexRequest base Nothing (mkPackageName Cargo Nothing "serde"))
        path discovery `shouldBe` "/tenant/index/config.json"
        path index `shouldBe` "/tenant/index/se/rd/serde"
        redirectCount index `shouldBe` 0
        cargoIndexRequest base Nothing (mkPackageName Npm Nothing "serde") `shouldSatisfy` isLeft
