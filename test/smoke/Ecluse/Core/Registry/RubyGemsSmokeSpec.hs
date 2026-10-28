-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Live RubyGems drift checks bind one named index to a pinned archive's native identity and SHA-256.
module Ecluse.Core.Registry.RubyGemsSmokeSpec (spec) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec

import Ecluse.Core.Package (Artifact (..), HashAlg (SHA256), PackageDetails (pkgArtifacts, pkgName, pkgVersion), PackageInfo (infoInvalidEntries, infoVersions), hashAlg, hashValue)
import Ecluse.Core.Package.Integrity (IntegrityResult (IntegrityVerified), verifyIntegrity)
import Ecluse.Core.Registry (RegistryResponse (responseBody))
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (prepareManifest)
import Ecluse.Core.Registry.Origin (originClient)
import Ecluse.Core.Registry.Request (artifactRequestByUrl)
import Ecluse.Core.Registry.RubyGems.Archive (checkGemArchive, inspectGemArchive)
import Ecluse.Core.Registry.RubyGems.Credential (rubyReadCredential)
import Ecluse.Core.Registry.RubyGems.Metadata (prepareGemRead)
import Ecluse.Core.Registry.RubyGems.Project (mkCoordinate, mkGemPlatform, projectName)
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
spec = describe "live RubyGems registry protocol" $
    it (caseLabel "live/rubygems/rake" "reads one named index and verifies the pinned rake 13.2.1 archive") $
        withExchangeClient (newManager tlsManagerSettings) $ \client -> do
            base <- expectRightText (mkRegistryUrl "https://rubygems.org")
            name <- expectRight (projectName "rake")
            let origin = originClient defaultLimits client base Nothing
            info <- liveMetadata "rake" (prepareManifest prepareGemRead passthroughTracingPort origin name)
            infoInvalidEntries info `shouldBe` []
            release <- maybe (fail "live named index omitted rake 13.2.1") pure (Map.lookup "13.2.1" (infoVersions info))
            pkgName release `shouldBe` name
            renderVersion (pkgVersion release) `shouldBe` "13.2.1"
            artifact <- case filter ((== "rake-13.2.1.gem") . artFilename) (toList (pkgArtifacts release)) of
                [file] -> pure file
                _ -> fail "live named index has no unique rake 13.2.1 ruby artifact"
            hashes <- maybe (fail "live gem omitted advertised integrity") pure (nonEmpty (artHashes artifact))
            checksum <- case filter ((== SHA256) . hashAlg) (toList hashes) of
                [digest] -> pure digest
                _ -> fail "live gem has no unique advertised SHA-256"
            request <- expectRight (artifactRequestByUrl rubyReadCredential Nothing (artUrl artifact))
            response <- liveRegistryResponse "RubyGems archive host" (fetchTestBody client (progressFloor defaultLimits) (MirrorArtifactBodyLimit gemByteLimit) request)
            responseBody response `shouldNotBe` ""
            verifyIntegrity hashes (responseBody response) `shouldBe` IntegrityVerified
            platform <- expectRight (mkGemPlatform "ruby")
            coordinate <- expectRight (mkCoordinate name (pkgVersion release) platform)
            archive <- expectRight =<< inspectGemArchive defaultLimits (const pass) (responseBody response)
            void (expectRight (checkGemArchive coordinate (hashValue checksum) archive))

gemByteLimit :: Int
gemByteLimit = 1024 * 1024
