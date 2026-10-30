-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Genuine Ruby-generated specifications preserve native bytes and independent identity observations.
module Ecluse.Core.Registry.RubyGems.CompactSpecSpec (spec) where

import Codec.Compression.Zlib qualified as Zlib
import Data.Aeson (Value (..), eitherDecodeStrict', object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Package (renderPackageName)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Package.Hash (HashAlg (SHA256), mkHash, sha256Hex)
import Ecluse.Core.Registry (ParseError (..))
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
                Just (toJSON (specificationMetadata specification)) `shouldBe` KeyMap.lookup "metadata" fields
                manifestRuntime <$> KeyMap.lookup "dependencies" fields `shouldBe` Just (runtimeOf specification)
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
                -- Ruby reports a layout without a metadata field as null, which the reader keeps as no entries.
                Just (toJSON (specificationMetadata specification)) `shouldBe` (KeyMap.lookup "decoded_metadata" fields <&> \case Null -> Object mempty; other -> other)
                legacyRuntime <$> KeyMap.lookup "dependencies" fields `shouldBe` Just [(name, T.intercalate ", " clauses) | (name, clauses) <- runtimeOf specification]
            _ -> fail "legacy manifest contains an invalid layout"
    it "refuses each Marshal class and instance variable outside the specification allowlist" $
        for_
            [ (marshal (userDefined "Evil" "x"), "unsupported Marshal user-defined class")
            , (specificationOf ("[\x06U" <> symbol "Evil" <> "0"), "unsupported Marshal user-marshal class")
            , (specificationOf ("[\x06o" <> symbol "Evil" <> "\x00"), "unsupported Marshal object class")
            , (specificationOf ("[\x06I\x22\x06x\x06" <> symbol "@evil" <> "\x22\x06y"), "unsupported Marshal instance variable")
            , (specificationOf ("[\x06I\x22\x06x\x06" <> symbol "E" <> "o" <> symbol "Evil" <> "\x00"), "unsupported Marshal object class")
            ]
            $ \(bytes, reason) -> decodeGemSpecification defaultLimits bytes `shouldBe` Left (ParseError reason)
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

-- Hand-built Marshal 4.8: a version header, then lengths under 123 as one byte offset by five.
marshal :: ByteString -> ByteString
marshal body = "\x04\x08" <> body

specificationOf :: ByteString -> ByteString
specificationOf inner = marshal (userDefined "Gem::Specification" (marshal inner))

userDefined :: ByteString -> ByteString -> ByteString
userDefined name payload = "u" <> symbol name <> counted payload

symbol :: ByteString -> ByteString
symbol name = ":" <> counted name

counted :: ByteString -> ByteString
counted bytes = BS.singleton (fromIntegral (BS.length bytes + 5)) <> bytes

-- Each runtime dependency's name and native requirement clauses, as the reader keeps them.
runtimeOf :: GemSpecification -> [(Text, [Text])]
runtimeOf specification = [(renderPackageName (gdName dependency), toList (gdRequirement dependency)) | dependency <- specificationDependencies specification]

-- Ruby's runtime dependencies in manifest.json: objects with a type and operator-version pairs.
manifestRuntime :: Value -> [(Text, [Text])]
manifestRuntime = \case
    Array rows ->
        [ (name, [operator <> " " <> version | Array clause <- toList requirements, [String operator, String version] <- [toList clause]])
        | Object row <- toList rows
        , Just (String "runtime") <- [KeyMap.lookup "type" row]
        , Just (String name) <- [KeyMap.lookup "name" row]
        , Just (Array requirements) <- [KeyMap.lookup "requirements" row]
        ]
    _ -> []

-- Ruby's runtime dependencies in legacy.json: name, type and the requirement Ruby renders.
legacyRuntime :: Value -> [(Text, Text)]
legacyRuntime = \case
    Array rows -> [(name, requirement) | Array row <- toList rows, [String name, String "runtime", String requirement] <- [toList row]]
    _ -> []
