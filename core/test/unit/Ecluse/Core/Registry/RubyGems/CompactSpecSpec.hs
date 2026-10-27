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
