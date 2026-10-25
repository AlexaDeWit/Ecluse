-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Discovery templates cannot grant an archive authority or change its native logical filename.
module Ecluse.Core.Registry.Cargo.DiscoverySpec (spec) where

import Data.Aeson (Value, eitherDecodeStrict)
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Package (HashAlg (SHA256), mkHash)
import Ecluse.Core.Registry.Cargo.Discovery (bindCargoDownload, cargoArtifactCoordinate, coordinateFilename, discoveryAuthRequired, discoveryDl, parseCargoDiscovery, renderCargoDiscovery)
import Ecluse.Core.Registry.Cargo.Request (cargoArtifactHosts)
import Ecluse.Core.Security (ecosystemArtifactAuthorities)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestVersion)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "Cargo discovery" $ do
    it "supports native markers, checksum terminals and logical archive names" $ do
        discovery <- expectRight (parse "https://static.crates.io/{lowerprefix}/{crate}/{sha256-checksum}")
        checksum <- expectRight (mkHash SHA256 (T.replicate 64 "a"))
        coordinate <- expectRight (cargoArtifactCoordinate "Ab_C" (cargoTestVersion "1.0.0-rc.1+build") checksum)
        unFilename (coordinateFilename coordinate) `shouldBe` "Ab_C-1.0.0-rc.1+build.crate"
        bindCargoDownload discovery coordinate `shouldBe` Right ("https://static.crates.io/ab/_c/Ab_C/" <> T.replicate 64 "a")
    it "adds the native download suffix when the template has no markers" $ do
        discovery <- expectRight (parse "https://index.crates.io/dl")
        checksum <- expectRight (mkHash SHA256 (T.replicate 64 "a"))
        coordinate <- expectRight (cargoArtifactCoordinate "fixture" (cargoTestVersion "1.0.0") checksum)
        bindCargoDownload discovery coordinate `shouldBe` Right "https://index.crates.io/dl/fixture/1.0.0/download"
    it "accepts a coordinate-bearing query without relaxing the filename gate of other protocols" $ do
        discovery <- expectRight (parse "https://index.crates.io/?name={crate}&version={version}")
        checksum <- expectRight (mkHash SHA256 (T.replicate 64 "a"))
        coordinate <- expectRight (cargoArtifactCoordinate "fixture" (cargoTestVersion "1.0.0") checksum)
        bindCargoDownload discovery coordinate `shouldBe` Right "https://index.crates.io/?name=fixture&version=1.0.0"
    it "refuses undeclared hosts, unknown markers, userinfo and fragments" $
        for_ ["https://hostile.test/{crate}", "https://static.crates.io/{unknown}", "https://user:token@index.crates.io/{crate}", "https://index.crates.io/{crate}#fragment"] $ \template ->
            fmap discoveryDl (parse template) `shouldSatisfy` isLeft
    it "retains trusted authentication and rejects a malformed declaration" $ do
        let trusted = renderCargoDiscovery "https://index.crates.io/{crate}" Nothing True
        discovery <- expectRight (parseCargoDiscovery hosts origin trusted)
        discoveryAuthRequired discovery `shouldBe` True
        fmap discoveryDl (parseCargoDiscovery hosts origin "{\"dl\":\"https://index.crates.io\",\"auth-required\":null}") `shouldSatisfy` isLeft
    it "renders only the supplied local routes" $
        (eitherDecodeStrict (renderCargoDiscovery "https://proxy.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate" (Just "https://proxy.test/cargo/private") True) :: Either String Value)
            `shouldSatisfy` isRight
  where
    hosts = ecosystemArtifactAuthorities cargoArtifactHosts
    origin = "https://index.crates.io"
    parse template = parseCargoDiscovery hosts origin (renderCargoDiscovery template Nothing False)
