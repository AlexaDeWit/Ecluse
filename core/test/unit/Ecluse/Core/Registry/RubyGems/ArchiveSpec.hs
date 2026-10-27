-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Genuine gem envelopes and independent Ruby observations bind metadata to complete payloads.
module Ecluse.Core.Registry.RubyGems.ArchiveSpec (spec) where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Data.Aeson (Value (..), eitherDecodeStrict', object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Set qualified as Set
import Test.Hspec

import Ecluse.Core.Package (renderPackageName)
import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.RubyGems.Archive
import Ecluse.Core.Registry.RubyGems.CompactSpec (specificationCoordinate)
import Ecluse.Core.Registry.RubyGems.Project (unGemPlatform)
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Registry.Archive (tarBytes)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "native gem validation" $ do
    it "matches independent Ruby identity and digest observations for every genuine gem" $ do
        manifest <- readFileBS "test/fixtures/rubygems/manifest.json" >>= expectRight . eitherDecodeStrict'
        releases <- case manifest of
            Object fields | Just (Array rows) <- KeyMap.lookup "releases" fields -> pure (toList rows)
            _ -> fail "native manifest has no releases"
        for_ releases $ \case
            Object fields | Just (String filename) <- KeyMap.lookup "filename" fields -> do
                bytes <- readFileBS ("test/fixtures/rubygems/gems/" <> toString filename)
                archive <- inspect defaultLimits bytes >>= expectRight
                gemArchiveBytes archive `shouldBe` bytes
                let observed = object ["name" .= renderPackageName (gemArchiveName archive), "version" .= renderVersion (gemArchiveVersion archive), "platform" .= unGemPlatform (gemArchivePlatform archive), "sha256" .= gemArchiveSha256 archive]
                    expected = Object (KeyMap.filterWithKey (\key _ -> key `elem` ["name", "version", "platform", "sha256"]) fields)
                observed `shouldBe` expected
                gemArchiveInventory archive `shouldSatisfy` (not . Set.null)
            _ -> fail "native manifest contains an invalid release"
    it "refuses duplicate, missing and unknown envelope members and nonzero trailing data" $ do
        bytes <- readFileBS fixture
        members <- nativeMembers bytes
        for_ [members <> take 1 members, drop 1 members, members <> [("unexpected", "data")]] $ \entries -> do
            changed <- BSL.toStrict <$> tarBytes entries
            inspect defaultLimits changed >>= (`shouldSatisfy` isLeft)
        inspect defaultLimits (bytes <> "junk") >>= (`shouldSatisfy` isLeft)
    it "checks member checksums and traverses the complete nested compressed stream" $ do
        bytes <- readFileBS fixture
        members <- nativeMembers bytes
        let mutate key f = map (\(name, body) -> (name, if name == key then f body else body)) members
        for_ [mutate "metadata.gz" (<> "junk"), mutate "data.tar.gz" (<> "junk"), mutate "checksums.yaml.gz" (<> "junk"), mutate "data.tar.gz" (\raw -> BS.take (BS.length raw - 1) raw)] $ \entries -> do
            changed <- BSL.toStrict <$> tarBytes entries
            inspect defaultLimits changed >>= (`shouldSatisfy` isLeft)
    it "rejects malformed tagged metadata even when the outer tar is valid" $ do
        bytes <- readFileBS fixture
        members <- nativeMembers bytes
        let malicious = BSL.toStrict (GZip.compress "--- !ruby/object:Kernel {}")
        changed <- BSL.toStrict <$> tarBytes (map (\(name, body) -> (name, if name == "metadata.gz" then malicious else body)) members)
        inspect defaultLimits changed >>= (`shouldSatisfy` isLeft)
    it "reaches artifact, inflated, metadata and inventory bounds" $ do
        bytes <- readFileBS fixture
        inspect defaultLimits{maxMirrorArtifactBytes = BS.length bytes - 1} bytes >>= (`shouldSatisfy` isLeft)
        inspect defaultLimits{maxMetadataBytes = 32} bytes >>= (`shouldSatisfy` isLeft)
        inspect defaultLimits{maxArtifactCount = 2} bytes >>= (`shouldSatisfy` isLeft)
    it "binds the checked archive to an exact admitted coordinate and digest" $ do
        bytes <- readFileBS fixture
        archive <- inspect defaultLimits bytes >>= expectRight
        let wrongDigest = "0000000000000000000000000000000000000000000000000000000000000000"
        coordinate <- expectRight (specificationCoordinate (gemArchiveSpecification archive))
        checkGemArchive coordinate (gemArchiveSha256 archive) archive `shouldBe` Right archive
        checkGemArchive coordinate wrongDigest archive `shouldSatisfy` isLeft

fixture :: FilePath
fixture = "test/fixtures/rubygems/gems/ecluse-fixture-root-1.0.0.gem"

inspect :: Limits -> ByteString -> IO (Either ParseError GemArchive)
inspect limits = inspectGemArchive limits (const (pure ()))

nativeMembers :: ByteString -> IO [(Text, ByteString)]
nativeMembers = walk . Tar.read . BSL.fromStrict
  where
    walk Tar.Done = pure []
    walk (Tar.Fail fault) = fail (show fault)
    walk (Tar.Next entry rest) = case Tar.entryContent entry of
        Tar.NormalFile body _ -> ((toText (Tar.entryPath entry), BSL.toStrict body) :) <$> walk rest
        _ -> fail "native fixture contains a non-file envelope entry"
