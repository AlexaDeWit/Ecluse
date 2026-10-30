-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The YAML event reader refuses object hooks, ambiguity and reference amplification.
module Ecluse.Core.Registry.RubyGems.Archive.YamlSpec (spec) where

import Data.Aeson (Value (String), eitherDecodeStrict', toJSON)
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.RubyGems.Archive.Yaml
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "inert bounded gem YAML" $ do
    it "resolves completed native aliases without object construction" $ do
        value <- decode defaultLimits "---\na: &version !ruby/object:Gem::Version {version: '1.0'}\nb: *version\n" >>= expectRight
        fields <- expectRight (yamlMapping value)
        Map.lookup "a" fields `shouldBe` Map.lookup "b" fields
    it "reads aliases produced by Ruby's Psych emitter" $ do
        bytes <- readFileBS "test/fixtures/rubygems/aliases.yaml"
        value <- decode defaultLimits bytes >>= expectRight
        fields <- expectRight (yamlMapping value)
        Map.lookup "first" fields `shouldBe` Map.lookup "second" fields
        decoded <- readFileBS "test/fixtures/rubygems/aliases.json" >>= expectRight . eitherDecodeStrict'
        plainValue value `shouldBe` decoded
    it "refuses cycles, forward aliases, duplicate keys, merge keys and unknown tags" $
        for_ ["--- &a [*a]", "--- [*missing]", "--- {name: one, name: two}", "--- {a: &a {}, b: {<<: *a}}", "--- !ruby/object:Kernel {}", "--- one\n--- two", "--- [unterminated"] $ \bytes -> do
            result <- decode defaultLimits bytes
            result `shouldSatisfy` isLeft
    it "refuses the reached depth, node, byte and alias-work bounds" $ do
        decode defaultLimits{maxNestingDepth = 1} "--- [[[one]]]" >>= (`shouldSatisfy` isLeft)
        decode defaultLimits{inertNodeCeiling = 4} "--- [one, two, three, four]" >>= (`shouldSatisfy` isLeft)
        decode defaultLimits{maxMetadataBytes = 4} "--- one" >>= (`shouldSatisfy` isLeft)
        decode defaultLimits{inertNodeCeiling = 30} "--- [ &a [one, two, three], &b [*a, *a, *a], *b, *b, *b]" >>= (`shouldSatisfy` isLeft)

decode :: Limits -> ByteString -> IO (Either ParseError YamlValue)
decode limits = decodeYaml limits (const (pure ()))

-- The JSON form of an untagged document of strings, as Ruby's reference wrote aliases.json.
plainValue :: YamlValue -> Value
plainValue = \case
    YamlScalar _ _ text -> String text
    YamlSequence _ values -> toJSON (map plainValue values)
    YamlMapping _ fields -> toJSON (Map.map plainValue fields)
