-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Owned view fixtures advertise the exact archive route they serve.
module Ecluse.Test.Registry.Cargo.FixturesSpec (spec) where

import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldBe)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Test.Registry.Cargo.Fixtures (cargoViewDiscovery, minimalCargoRelease, writeCargoRegistry)
import Ecluse.Test.Registry.Cargo.Reference (referenceDiscovery)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "Cargo view discovery" $ do
    it "keeps the registry prefix, publication API and authentication declaration"
        $ for_
            [ ("https://proxy.test/cargo", Nothing, False, "https://proxy.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate")
            , ("https://proxy.test/cargo/", Nothing, False, "https://proxy.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate")
            , ("https://proxy.test/cargo/", Just "https://proxy.test/cargo", True, "https://proxy.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate")
            ]
        $ \(base, api, authenticated, expectedDownload) -> do
            (download, advertisedApi, advertisedAuth, _) <- expectRight (referenceDiscovery (cargoViewDiscovery base api authenticated))
            (download, advertisedApi, advertisedAuth) `shouldBe` (expectedDownload, api, authenticated)
    it "writes the view route used by the native registry fixture" $
        withSystemTempDirectory "cargo-view-discovery" $ \directory -> do
            writeCargoRegistry directory "https://registry.test/root/" (Just "https://registry.test/root") True [minimalCargoRelease "example" "1.0.0"]
            bytes <- readFileBS (directory </> "config.json")
            (download, api, authenticated, _) <- expectRight (referenceDiscovery bytes)
            (download, api, authenticated)
                `shouldBe` ("https://registry.test/root/crates/{crate}/{version}/{crate}-{version}.crate", Just "https://registry.test/root", True)
