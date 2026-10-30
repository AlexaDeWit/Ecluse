-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Captured discovery keeps public archive routes separate from local view routes.
module Ecluse.Test.Corpus.CargoSpec (spec) where

import Data.Text qualified as T
import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Core.Package (HashAlg (SHA256), mkHash)
import Ecluse.Core.Registry.Cargo.Discovery (bindCargoDownload, cargoArtifactCoordinate)
import Ecluse.Test.Corpus (syntheticProxyBase)
import Ecluse.Test.Corpus.Cargo (cargoCaptureDiscovery, cargoCaptureDiscoveryBytes, cargoServedDiscovery)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestVersion)
import Ecluse.Test.Registry.Cargo.Reference (referenceDiscovery)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "Cargo corpus discovery" $ do
    it "advertises the complete public archive template exactly once" $ do
        (download, api, authenticated, _) <- expectRight (referenceDiscovery cargoCaptureDiscoveryBytes)
        (download, api, authenticated)
            `shouldBe` ("https://static.crates.io/crates/{crate}/{crate}-{version}.crate", Nothing, False)
    it "binds public and local archives to their distinct route shapes" $ do
        captured <- expectRight cargoCaptureDiscovery
        served <- expectRight cargoServedDiscovery
        checksum <- expectRight (mkHash SHA256 (T.replicate 64 "a"))
        coordinate <- expectRight (cargoArtifactCoordinate "tokio" (cargoTestVersion "1.48.0") checksum)
        bindCargoDownload captured coordinate `shouldBe` Right "https://static.crates.io/crates/tokio/tokio-1.48.0.crate"
        bindCargoDownload served coordinate `shouldBe` Right (syntheticProxyBase <> "/crates/tokio/1.48.0/tokio-1.48.0.crate")
