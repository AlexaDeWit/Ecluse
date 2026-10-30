-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Live Cargo drift checks bind crates.io discovery and one sparse index to a pinned archive's SHA-256.
module Ecluse.Core.Registry.CargoSmokeSpec (spec) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec

import Ecluse.Core.Package (Artifact (..), HashAlg (SHA256), PackageDetails (pkgArtifacts, pkgName, pkgVersion), PackageInfo (infoInvalidEntries, infoVersions), hashAlg)
import Ecluse.Core.Package.Integrity (IntegrityResult (IntegrityVerified), verifyIntegrity)
import Ecluse.Core.Registry (RegistryResponse (responseBody))
import Ecluse.Core.Registry.Cargo.Metadata (prepareCargoRead)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.Request (cargoArtifactRequest)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (prepareManifest)
import Ecluse.Core.Registry.Origin (originClient)
import Ecluse.Core.Security (BodyLimit (MirrorArtifactBodyLimit), Limits (progressFloor), defaultLimits)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.RegistrySmoke (liveMetadata, liveRegistryResponse)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Metadata.Fetch (fetchTestBody)
import Ecluse.Test.Support (expectRight, expectRightText)

-- | Protocol, bounds and integrity failures remain failures after the transport availability check.
spec :: Spec
spec = describe "live Cargo registry protocol" $
    it (caseLabel "live/cargo/serde" "reads crates.io discovery and one sparse index and verifies the pinned serde 1.0.200 archive") $
        withExchangeClient (newManager tlsManagerSettings) $ \client -> do
            base <- expectRightText (mkRegistryUrl "https://index.crates.io")
            name <- expectRight (projectName "serde")
            let origin = originClient defaultLimits client base Nothing
            info <- liveMetadata "serde" (prepareManifest prepareCargoRead passthroughTracingPort origin name)
            infoInvalidEntries info `shouldBe` []
            release <- maybe (fail "live sparse index omitted serde 1.0.200") pure (Map.lookup "1.0.200" (infoVersions info))
            pkgName release `shouldBe` name
            renderVersion (pkgVersion release) `shouldBe` "1.0.200"
            artifact <- case filter ((== "serde-1.0.200.crate") . artFilename) (toList (pkgArtifacts release)) of
                [file] -> pure file
                _ -> fail "live sparse index has no unique serde 1.0.200 archive"
            hashes <- maybe (fail "live crate omitted its advertised checksum") pure (nonEmpty (artHashes artifact))
            case filter ((== SHA256) . hashAlg) (toList hashes) of
                [_] -> pass
                _ -> fail "live crate has no unique advertised SHA-256"
            request <- expectRight (cargoArtifactRequest Nothing (artUrl artifact))
            response <- liveRegistryResponse "Cargo archive host" (fetchTestBody client (progressFloor defaultLimits) (MirrorArtifactBodyLimit crateByteLimit) request)
            responseBody response `shouldNotBe` ""
            verifyIntegrity hashes (responseBody response) `shouldBe` IntegrityVerified

-- serde 1.0.200 is 77,611 bytes. The bound keeps drift detection from fetching an unbounded body.
crateByteLimit :: Int
crateByteLimit = 1024 * 1024
