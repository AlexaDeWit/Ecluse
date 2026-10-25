-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Cargo names retain wire spelling and distinct index paths.
module Ecluse.Core.Registry.Cargo.ProjectSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm))
import Ecluse.Core.Package (mkPackageName, mkScope, renderPackageName)
import Ecluse.Core.Registry.Cargo.Project (cargoCollisionKey, cargoNameKey, projectName)

-- | Native Cargo name grammar and identity examples.
spec :: Spec
spec = do
    describe "projectName" $ do
        for_ ["a", "Ab", "Ab_", "foo--bar", "foo__bar", "a0", T.replicate 64 "a"] $ \raw ->
            it ("retains " <> show raw) $
                renderPackageName <$> projectName raw `shouldBe` Right raw
        for_ ["", "1crate", "_crate", "-crate", "crate.name", "@scope/crate", "a/b", "a\\b", "a%2fb", "a b", "café", T.replicate 65 "a"] $ \raw ->
            it ("rejects " <> show raw) $
                projectName raw `shouldSatisfy` isLeft
        it "shares case lookup without merging native separator paths" $ do
            projectName "Acme_Tools" `shouldBe` projectName "acme_tools"
            projectName "acme_tools" `shouldNotBe` projectName "acme-tools"
            cargoNameKey "Acme_Tools" `shouldBe` "acme_tools"
    describe "cargoCollisionKey" $ do
        it "covers case and separator aliases while preserving separator counts" $
            map (cargoCollisionKey . mkPackageName Cargo Nothing) ["Acme_Tools", "acme-tools", "acme__tools", "acme.tools"]
                `shouldBe` [Just "acme-tools", Just "acme-tools", Just "acme--tools", Just "acme.tools"]
        it "excludes foreign ecosystems and npm scopes" $
            map cargoCollisionKey [mkPackageName Npm Nothing "acme", mkPackageName Cargo (Just (mkScope "acme")) "tools"]
                `shouldBe` [Nothing, Nothing]
