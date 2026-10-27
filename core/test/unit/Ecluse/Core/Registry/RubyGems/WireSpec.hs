-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native compact-index grammar preserves identity and complete dependency requirements.
module Ecluse.Core.Registry.RubyGems.WireSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.RubyGems.Document (GemDependency (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Project (coordinatePlatform, projectName, unGemPlatform)
import Ecluse.Core.Registry.RubyGems.Wire (decodeInfoRow, parseGemRequirement)
import Ecluse.Core.Text.Iso8601 (readIso8601Utc)
import Ecluse.Test.Support (expectRight)

-- | Pin timestamp separators, conjunctions and future-format refusal independently of the encoder.
spec :: Spec
spec = describe "RubyGems compact info rows" $ do
    it "retains timestamps, runtime conjunctions, platform suffixes and additive metadata" $ do
        name <- expectRight (projectName "native-gem")
        parsed <- expectRight (decodeInfoRow name (RecordEntry 7) ("1.2.3-x86_64-linux-musl child:~> 2.0&!= 2.1|checksum:" <> digest <> ",ruby:>= 3.1,rubygems:>= 3.3,created_at:2026-01-02T03:04:05Z,opaque:value"))
        grEntryKey parsed `shouldBe` RecordEntry 7
        unGemPlatform (coordinatePlatform (grCoordinate parsed)) `shouldBe` "x86_64-linux-musl"
        map gdRequirement (grDependencies parsed) `shouldBe` ["~> 2.0" :| ["!= 2.1"]]
        grPublishedAt parsed `shouldBe` readIso8601Utc "2026-01-02T03:04:05Z"
        Map.lookup "opaque" (grExtras parsed) `shouldBe` Just "value"
    it "does not invent publication times for older sources" $ do
        name <- expectRight (projectName "rack")
        parsed <- expectRight (decodeInfoRow name (RecordEntry 1) ("1.0 |checksum:" <> digest))
        grPublishedAt parsed `shouldBe` Nothing
        grRequiredRuby parsed `shouldBe` (">= 0" :| [])
    for_ ["1.0 |checksum:bad", "1.0 |checksum:" <> digest <> ",checksum:" <> digest, "1.0 child:>= 1,child:< 2|checksum:" <> digest, "1.0 |checksum:" <> digest <> ",platform:x86_64-linux", "1.0 child:bogus|checksum:" <> digest] $ \bytes ->
        it ("refuses malformed or unsupported data " <> show bytes) $ do
            name <- expectRight (projectName "rack")
            decodeInfoRow name (RecordEntry 1) bytes `shouldSatisfy` isLeft
    it "requires every conjunction to carry a native operator and valid version" $ do
        parseGemRequirement ">= 1&< 2" `shouldBe` Right (">= 1" :| ["< 2"])
        for_ ["", ">=", ">= 1&", "^ 1", ">= 💎", ">= arbitrary"] $ \raw -> parseGemRequirement raw `shouldSatisfy` isLeft
  where
    digest = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
