-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native metadata fields preserve identity and reject conflicting dependency declarations.
module Ecluse.Core.Registry.RubyGems.Archive.MetadataSpec (spec) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Test.Hspec

import Ecluse.Core.Package (renderPackageName)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.RubyGems.Archive.Metadata
import Ecluse.Core.Registry.RubyGems.Archive.Yaml (decodeYaml)
import Ecluse.Core.Registry.RubyGems.CompactSpec (GemSpecification (..))
import Ecluse.Core.Registry.RubyGems.Project (unGemPlatform)
import Ecluse.Core.Security.Limits (defaultLimits)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "native gem metadata schema" $ do
    it "retains checked identity, native requirements and the complete inert document" $ do
        raw <- decodeYaml defaultLimits (const (pure ())) metadata >>= expectRight
        parsed <- expectRight (parseGemMetadata raw)
        renderPackageName (specificationName (metadataSpecification parsed)) `shouldBe` "owned"
        metadataFiles parsed `shouldBe` ["lib/owned.rb", "ext/extconf.rb"]
        metadataExtensions parsed `shouldBe` ["ext/extconf.rb"]
        metadataDocument parsed `shouldBe` raw
    it "preserves file order and permits repeated extension declarations in their original order" $ do
        let bytes = replace "- ext/extconf.rb\nextensions: [ext/extconf.rb]" "- ext/z.rb\n- ext/a.rb\nextensions: [ext/a.rb, ext/z.rb, ext/a.rb]" metadata
        raw <- decodeYaml defaultLimits (const (pure ())) bytes >>= expectRight
        parsed <- expectRight (parseGemMetadata raw)
        metadataFiles parsed `shouldBe` ["lib/owned.rb", "ext/z.rb", "ext/a.rb"]
        metadataExtensions parsed `shouldBe` ["ext/a.rb", "ext/z.rb", "ext/a.rb"]
    it "rejects duplicate files before checking missing extension membership" $ do
        let bytes = replace "- ext/extconf.rb" "- lib/owned.rb" metadata
        raw <- decodeYaml defaultLimits (const (pure ())) bytes >>= expectRight
        parseGemMetadata raw `shouldBe` Left (ParseError "duplicate gem metadata file")
    it "rejects an extension absent from otherwise distinct files" $ do
        let bytes = replace "extensions: [ext/extconf.rb]" "extensions: [ext/missing.rb]" metadata
        raw <- decodeYaml defaultLimits (const (pure ())) bytes >>= expectRight
        parseGemMetadata raw `shouldBe` Left (ParseError "gem extension is absent from metadata file inventory")
    it "rejects missing identity, non-canonical files and unsupported schema versions" $ do
        for_ ["--- {}", metadata <> "name: other\n", replace "lib/owned.rb" "../owned.rb" metadata, replace "specification_version: 4" "specification_version: 999" metadata, replace "ext/extconf.rb" "ext/missing.rb" (replace "- ext/extconf.rb" "- lib/owned.rb" metadata)] $ \bytes -> do
            decoded <- decodeYaml defaultLimits (const (pure ())) bytes
            (decoded >>= parseGemMetadata) `shouldSatisfy` isLeft
    it "reads identity scalars as Psych loads them" $ do
        for_ [("{cpu: null, os: linux, version: ~}", "linux"), ("{cpu: 'x86_64', os: linux, version: }", "x86_64-linux"), ("{cpu: !!null arm64, os: darwin, version: '8'}", "arm64-darwin-8")] $ \(mapping, expected) -> do
            raw <- decodeYaml defaultLimits (const (pure ())) (replace "platform: ruby" ("platform: !ruby/object:Gem::Platform " <> mapping) metadata) >>= expectRight
            parsed <- expectRight (parseGemMetadata raw)
            unGemPlatform (specificationPlatform (metadataSpecification parsed)) `shouldBe` expected
        raw <- decodeYaml defaultLimits (const (pure ())) (replace "name: owned" "name: '010'" metadata) >>= expectRight
        parsed <- expectRight (parseGemMetadata raw)
        renderPackageName (specificationName (metadataSpecification parsed)) `shouldBe` "010"
    it "refuses identity scalars Psych would load as nil, a Boolean or a number" $
        for_ [replace "{version: '1.0'}" "{version: 1.0}" metadata, replace "name: owned" "name: 010" metadata, replace "name: owned" "name: Yes" metadata, replace "name: owned" "name: ~" metadata, replace "name: owned" "name: !!float 1" metadata] $ \bytes -> do
            raw <- decodeYaml defaultLimits (const (pure ())) bytes >>= expectRight
            parseGemMetadata raw `shouldBe` Left (ParseError "YAML scalar does not load as a native string")
  where
    replace needle replacement = TE.encodeUtf8 . T.replace (TE.decodeUtf8 needle) (TE.decodeUtf8 replacement) . TE.decodeUtf8

metadata :: ByteString
metadata = "--- !ruby/object:Gem::Specification\nname: owned\nversion: !ruby/object:Gem::Version {version: '1.0'}\nplatform: ruby\nspecification_version: 4\ndependencies: []\nrequired_ruby_version: !ruby/object:Gem::Requirement {requirements: [['>=', !ruby/object:Gem::Version {version: '3.2'}]]}\nrequired_rubygems_version: !ruby/object:Gem::Requirement {requirements: [['>=', !ruby/object:Gem::Version {version: '3.4'}]]}\nfiles:\n- lib/owned.rb\n- ext/extconf.rb\nextensions: [ext/extconf.rb]\n"
