-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Discovery templates cannot grant an archive authority or change its native logical filename.
module Ecluse.Core.Registry.Cargo.DiscoverySpec (spec) where

import Data.Aeson (Value, eitherDecodeStrict, object, (.=))
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (status200)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (HashAlg (SHA1, SHA256), mkHash)
import Ecluse.Core.Registry.Cargo.Discovery (bindCargoDownload, cargoArtifactCoordinate, cargoDiscovery, coordinateChecksum, coordinateFilename, coordinateName, coordinateVersion, discoveryApi, discoveryAuthRequired, discoveryDl, parseDiscoveryWithDepth, renderCargoDiscovery)
import Ecluse.Core.Registry.Cargo.Request (cargoArtifactHosts)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataUndecodable))
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead), originClient)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge, TooDeeplyNested), defaultLimits, ecosystemArtifactAuthorities)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestVersion, parseCargoDiscovery)
import Ecluse.Test.Stub (stubBaseUrl, withStub)
import Ecluse.Test.Support (expectRight, expectRightIO)

spec :: Spec
spec = describe "Cargo discovery" $ do
    it "supports native markers, checksum terminals and logical archive names" $ do
        discovery <- expectRight (parse "https://static.crates.io/{lowerprefix}/{crate}/{sha256-checksum}")
        checksum <- expectRight (mkHash SHA256 (T.replicate 64 "a"))
        coordinate <- expectRight (cargoArtifactCoordinate "Ab_C" (cargoTestVersion "1.0.0-rc.1+build") checksum)
        unFilename (coordinateFilename coordinate) `shouldBe` "Ab_C-1.0.0-rc.1+build.crate"
        coordinateName coordinate `shouldBe` "Ab_C"
        coordinateVersion coordinate `shouldBe` cargoTestVersion "1.0.0-rc.1+build"
        coordinateChecksum coordinate `shouldBe` checksum
        bindCargoDownload discovery coordinate `shouldBe` Right ("https://static.crates.io/ab/_c/Ab_C/" <> T.replicate 64 "a")
    it "adds the native download suffix when the template has no markers" $ do
        checksum <- expectRight (mkHash SHA256 (T.replicate 64 "a"))
        coordinate <- expectRight (cargoArtifactCoordinate "fixture" (cargoTestVersion "1.0.0") checksum)
        for_ ["https://index.crates.io/dl", "https://index.crates.io/dl///"] $ \template -> do
            discovery <- expectRight (parse template)
            bindCargoDownload discovery coordinate `shouldBe` Right "https://index.crates.io/dl/fixture/1.0.0/download"
    it "accepts a coordinate-bearing query without relaxing the filename gate of other protocols" $ do
        discovery <- expectRight (parse "https://index.crates.io/?name={crate}&version={version}")
        checksum <- expectRight (mkHash SHA256 (T.replicate 64 "a"))
        coordinate <- expectRight (cargoArtifactCoordinate "fixture" (cargoTestVersion "1.0.0") checksum)
        bindCargoDownload discovery coordinate `shouldBe` Right "https://index.crates.io/?name=fixture&version=1.0.0"
    it "refuses undeclared hosts, unknown markers, userinfo and fragments" $
        for_ ["https://hostile.test/{crate}", "https://static.crates.io/{unknown}", "https://user:token@index.crates.io/{crate}", "https://index.crates.io/{crate}#fragment", "https:///{crate}", "https://index.crates.io/a b/{crate}", ""] $ \template ->
            fmap discoveryDl (parse template) `shouldBe` Left MetadataUndecodable
    it "distinguishes malformed JSON from the nesting and byte bounds" $ do
        fmap discoveryDl (parseCargoDiscovery hosts origin "{") `shouldBe` Left MetadataUndecodable
        fmap discoveryDl (parseDiscoveryWithDepth 1 hosts origin "{\"dl\":\"https://index.crates.io\",\"extra\":{}}")
            `shouldBe` Left (MetadataBoundExceeded (TooDeeplyNested 1))
        let valid = renderCargoDiscovery "https://index.crates.io/{crate}" Nothing False
            padded = valid <> BS.replicate (65536 - BS.length valid) 32
        fmap discoveryDl (parseDiscoveryWithDepth 2 hosts origin valid) `shouldBe` Right "https://index.crates.io/{crate}"
        fmap discoveryDl (parseDiscoveryWithDepth 2 hosts origin "{\"dl\":\"https://index.crates.io\",\"extra\":{\"nested\":{}}}")
            `shouldBe` Left (MetadataBoundExceeded (TooDeeplyNested 2))
        fmap discoveryDl (parseCargoDiscovery hosts origin padded) `shouldBe` Right "https://index.crates.io/{crate}"
        fmap discoveryDl (parseCargoDiscovery hosts origin (padded <> " "))
            `shouldBe` Left (MetadataBoundExceeded (BodyTooLarge (MetadataBodyLimit 65536)))
    it "checks discovery field types and defaults without granting API authority" $ do
        for_ ["[]", "{}", "{\"dl\":false}", "{\"dl\":\"https://index.crates.io\",\"api\":42}", "{\"dl\":\"https://index.crates.io\",\"auth-required\":\"true\"}"] $ \bytes ->
            fmap discoveryDl (parseCargoDiscovery hosts origin bytes) `shouldBe` Left MetadataUndecodable
        for_ ["{\"dl\":\"https://index.crates.io\"}", "{\"dl\":\"https://index.crates.io\",\"api\":null}"] $ \bytes -> do
            discovery <- expectRight (parseCargoDiscovery hosts origin bytes)
            discoveryApi discovery `shouldBe` Nothing
            discoveryAuthRequired discovery `shouldBe` False
        for_ ["/relative", "https:///missing", "https://user:token@api.test", "https://api.test/#fragment", "https://api.test/a b", "https://{crate}.test"] $ \api ->
            fmap discoveryApi (parseCargoDiscovery hosts origin (renderCargoDiscovery "https://index.crates.io" (Just api) False)) `shouldBe` Left MetadataUndecodable
        fmap discoveryApi (parseCargoDiscovery hosts origin (renderCargoDiscovery "https://index.crates.io" (Just "https://foreign.test/api") False))
            `shouldBe` Right (Just "https://foreign.test/api")
    it "refuses invalid coordinate claims before binding a download" $ do
        checksum <- expectRight (mkHash SHA256 (T.replicate 64 "a"))
        sha1 <- expectRight (mkHash SHA1 (T.replicate 40 "a"))
        cargoArtifactCoordinate "invalid/name" (cargoTestVersion "1.0.0") checksum `shouldBe` Left "invalid Cargo package name"
        cargoArtifactCoordinate "fixture" (mkVersion Cargo "not-a-version") checksum `shouldBe` Left "invalid Cargo version"
        cargoArtifactCoordinate "fixture" (cargoTestVersion "1.0.0") sha1 `shouldBe` Left "Cargo requires SHA-256"
    it "retains trusted authentication and rejects a malformed declaration" $ do
        let trusted = renderCargoDiscovery "https://index.crates.io/{crate}" Nothing True
        discovery <- expectRight (parseCargoDiscovery hosts origin trusted)
        discoveryAuthRequired discovery `shouldBe` True
        fmap discoveryDl (parseCargoDiscovery hosts origin "{\"dl\":\"https://index.crates.io\",\"auth-required\":null}") `shouldSatisfy` isLeft
    it "pays each discovery chunk's own charge, never a running total" $ do
        let body = renderCargoDiscovery "https://static.crates.io/{crate}" Nothing False <> BS.replicate 51200 32
        recorded <- newIORef []
        withStub status200 (toLazy body) $ \stub ->
            withExchangeClient (newManager defaultManagerSettings) $ \client -> do
                let index = originClient defaultLimits client (loopbackRegistryUrl (stubBaseUrl stub <> "/index")) Nothing
                discovery <- expectRightIO (cargoDiscovery index{ocChargeFullRead = \cost -> modifyIORef' recorded (cost :)})
                discoveryDl discovery `shouldBe` "https://static.crates.io/{crate}"
        charges <- readIORef recorded
        length charges `shouldSatisfy` (>= 2)
        sum charges `shouldBe` 4 * BS.length body + 128 * length charges
    it "renders only the supplied local routes" $
        (eitherDecodeStrict (renderCargoDiscovery "https://proxy.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate" (Just "https://proxy.test/cargo/private") True) :: Either String Value)
            `shouldBe` Right
                ( object
                    [ "dl" .= ("https://proxy.test/cargo/crates/{crate}/{version}/{crate}-{version}.crate" :: Text)
                    , "api" .= ("https://proxy.test/cargo/private" :: Text)
                    , "auth-required" .= True
                    ]
                )
  where
    hosts = ecosystemArtifactAuthorities cargoArtifactHosts
    origin = "https://index.crates.io"
    parse template = parseCargoDiscovery hosts origin (renderCargoDiscovery template Nothing False)
