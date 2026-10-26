-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Validating fixture paths preserve native spelling and reject incompatible identities.
module Ecluse.Test.Registry.Cargo.MetadataSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Registry.Cargo.Metadata (cargoArtifactPath)

-- | Preserve raw native captures and refuse identities the runtime coordinate gate cannot admit.
spec :: Spec
spec = describe "cargoArtifactPath" $ do
    it "keeps all native archive claims in explicit captures" $
        cargoArtifactPath (mkPackageName Cargo Nothing "My-crate") (mkVersion Cargo "1.2.3-rc.1+build.4")
            `shouldBe` Just "crates/My-crate/1.2.3-rc.1+build.4/My-crate-1.2.3-rc.1+build.4.crate"
    it "refuses an invalid native version" $
        cargoArtifactPath (mkPackageName Cargo Nothing "crate") (mkVersion Cargo "1.2") `shouldBe` Nothing
    it "refuses another ecosystem's package or version" $ do
        cargoArtifactPath (mkPackageName Npm Nothing "crate") (mkVersion Cargo "1.2.3") `shouldBe` Nothing
        cargoArtifactPath (mkPackageName Cargo Nothing "crate") (mkVersion Npm "1.2.3") `shouldBe` Nothing
    it "refuses an invalid native package name" $
        cargoArtifactPath (mkPackageName Cargo Nothing "not/a-crate") (mkVersion Cargo "1.2.3") `shouldBe` Nothing
