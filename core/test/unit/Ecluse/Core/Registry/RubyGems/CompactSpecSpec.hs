-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Genuine Ruby-generated specifications preserve native bytes and independent identity observations.
module Ecluse.Core.Registry.RubyGems.CompactSpecSpec (spec) where

import Codec.Compression.Zlib qualified as Zlib
import Data.Aeson (Value (..), eitherDecodeStrict', object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Package (renderPackageName)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Package.Hash (HashAlg (SHA256), mkHash, sha256Hex)
import Ecluse.Core.Registry.RubyGems.CompactSpec
import Ecluse.Core.Registry.RubyGems.Document
import Ecluse.Core.Registry.RubyGems.Project (unGemPlatform)
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "native compact specifications" $ do
    it "matches Ruby's independent manifest for every genuine specification" $ do
        manifest <- readFileBS "test/fixtures/rubygems/manifest.json" >>= expectRight . eitherDecodeStrict'
        releases <- case manifest of
            Object fields | Just (Array rows) <- KeyMap.lookup "releases" fields -> pure (toList rows)
            _ -> fail "native manifest has no releases"
        for_ releases $ \case
            Object fields | Just (String path) <- KeyMap.lookup "compact" fields -> do
                bytes <- readFileBS ("test/fixtures/rubygems/" <> toString path)
                compact <- decodeCompactSpec defaultLimits (const (pure ())) bytes >>= expectRight
                compactSpecBytes compact `shouldBe` bytes
                let specification = compactSpecification compact
                    observed = object ["name" .= renderPackageName (specificationName specification), "version" .= renderVersion (specificationVersion specification), "platform" .= unGemPlatform (specificationPlatform specification)]
                    expected = Object (KeyMap.filterWithKey (\key _ -> key `elem` ["name", "version", "platform"]) fields)
                observed `shouldBe` expected
                Just (String (T.intercalate ", " (toList (specificationRequiredRuby specification)))) `shouldBe` KeyMap.lookup "required_ruby_version" fields
                Just (String (T.intercalate ", " (toList (specificationRequiredRubyGems specification)))) `shouldBe` KeyMap.lookup "required_rubygems_version" fields
                specificationMetadata specification `shouldSatisfy` Map.member "fixture-kind"
                when (renderPackageName (specificationName specification) == "ecluse-fixture-root") $
                    map (renderPackageName . gdName) (specificationDependencies specification) `shouldBe` ["ecluse-fixture-leaf"]
            _ -> fail "native manifest contains an invalid release"
    it "binds runtime requirements and exact identity to the selected native source row" $ do
        bytes <- readFileBS "test/fixtures/rubygems/quick/Marshal.4.8/ecluse-fixture-root-1.0.0.gemspec.rz"
        artifact <- readFileBS "test/fixtures/rubygems/gems/ecluse-fixture-root-1.0.0.gem"
        compact <- decodeCompactSpec defaultLimits (const (pure ())) bytes >>= expectRight
        checksum <- expectRight (mkHash SHA256 (sha256Hex artifact))
        let specification = compactSpecification compact
        coordinate <- expectRight (specificationCoordinate specification)
        let row = GemRow (RecordEntry 0) coordinate (specificationDependencies specification) (">=3.2" :| []) (">=3.4" :| []) checksum Nothing Map.empty ""
        checkGemSpecification coordinate row specification `shouldBe` Right specification
        for_ [row{grDependencies = []}, row{grRequiredRuby = ">= 999" :| []}, row{grRequiredRubyGems = ">= 999" :| []}] $ \changed -> do
            result <- validateCompactSpec defaultLimits (const (pure ())) coordinate changed bytes
            result `shouldSatisfy` isLeft
    it "accepts historical native layouts observed through Ruby's reference loader" $ do
        manifest <- readFileBS "test/fixtures/rubygems/legacy.json" >>= expectRight . eitherDecodeStrict'
        layouts <- case manifest of
            Object fields | Just (Array rows) <- KeyMap.lookup "layouts" fields -> pure (toList rows)
            _ -> fail "legacy manifest has no layouts"
        for_ layouts $ \case
            Object fields | Just (String path) <- KeyMap.lookup "path" fields -> do
                bytes <- readFileBS ("test/fixtures/rubygems/" <> toString path)
                compact <- decodeCompactSpec defaultLimits (const (pure ())) bytes >>= expectRight
                compactSpecBytes compact `shouldBe` bytes
                let specification = compactSpecification compact
                Just (String (renderPackageName (specificationName specification))) `shouldBe` KeyMap.lookup "decoded_name" fields
                Just (String (renderVersion (specificationVersion specification))) `shouldBe` KeyMap.lookup "decoded_version" fields
                Just (String (unGemPlatform (specificationPlatform specification))) `shouldBe` KeyMap.lookup "decoded_platform" fields
            _ -> fail "legacy manifest contains an invalid layout"
    it "rejects trailing compressed or Marshal bytes and checks body/depth bounds" $ do
        bytes <- readFileBS "test/fixtures/rubygems/quick/Marshal.4.8/ecluse-fixture-root-1.0.0.gemspec.rz"
        for_ [bytes <> "junk", BS.take (BS.length bytes - 1) bytes, BSL.toStrict (Zlib.compress "\x04\x08\x30")] $ \malformed -> do
            result <- decodeCompactSpec defaultLimits (const (pure ())) malformed
            result `shouldSatisfy` isLeft
        decodeGemSpecification defaultLimits (BSL.toStrict (Zlib.decompress (BSL.fromStrict bytes)) <> "junk") `shouldSatisfy` isLeft
        result <- decodeCompactSpec defaultLimits{maxMetadataBytes = BS.length bytes - 1} (const (pure ())) bytes
        result `shouldSatisfy` isLeft
        deep <- decodeCompactSpec defaultLimits{maxNestingDepth = 1} (const (pure ())) bytes
        deep `shouldSatisfy` isLeft
