-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native compact-index grammar preserves identity and complete dependency requirements.
module Ecluse.Core.Registry.RubyGems.WireSpec (spec) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Ecluse.Core.Version (renderVersion)
import Test.Hspec

import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.RubyGems.Document (GemDependency (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Project (coordinatePlatform, projectName, unGemPlatform)
import Ecluse.Core.Registry.RubyGems.Wire (decodeInfoRow, infoRowIdentity, infoRowVersion, parseGemRequirement)
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
    it "recovers an ASCII identity independently of invalid UTF-8 in its metadata" $ do
        name <- expectRight (projectName "rack")
        let bytes = "1.0 |checksum:" <> BS.singleton 255
        infoRowIdentity name bytes `shouldSatisfy` isJust
        fmap renderVersion (infoRowVersion bytes) `shouldBe` Just "1.0"
        decodeInfoRow name (RecordEntry 1) bytes `shouldSatisfy` isLeft
    it "retains a raw version claim when the platform cannot decode or is unsupported" $ do
        name <- expectRight (projectName "rack")
        for_ ["1.0-unknown-platform |checksum:" <> digest, "1.0-" <> BS.singleton 255 <> " |checksum:" <> digest] $ \bytes -> do
            infoRowIdentity name bytes `shouldBe` Nothing
            fmap renderVersion (infoRowVersion bytes) `shouldBe` Just "1.0"
            decodeInfoRow name (RecordEntry 1) bytes `shouldSatisfy` isLeft
    it "does not invent publication times for older sources" $ do
        name <- expectRight (projectName "rack")
        parsed <- expectRight (decodeInfoRow name (RecordEntry 1) ("1.0 |checksum:" <> digest))
        grPublishedAt parsed `shouldBe` Nothing
        grRequiredRuby parsed `shouldBe` (">= 0" :| [])
    it "gives no publication time for a created_at it cannot read, and keeps the row" $ do
        name <- expectRight (projectName "rack")
        parsed <- expectRight (decodeInfoRow name (RecordEntry 1) ("1.0 |checksum:" <> digest <> ",created_at:not-a-time"))
        grPublishedAt parsed `shouldBe` Nothing
        Map.lookup "created_at" (grExtras parsed) `shouldBe` Just "not-a-time"
    for_ ["1.0 |checksum:bad", "1.0 |checksum:" <> digest <> ",checksum:" <> digest, "1.0 child:>= 1,child:< 2|checksum:" <> digest, "1.0 |checksum:" <> digest <> ",platform:x86_64-linux", "1.0 child:bogus|checksum:" <> digest] $ \bytes ->
        it ("refuses malformed or unsupported data " <> show bytes) $ do
            name <- expectRight (projectName "rack")
            decodeInfoRow name (RecordEntry 1) bytes `shouldSatisfy` isLeft
    it "requires every conjunction to carry a native operator and valid version" $ do
        parseGemRequirement ">= 1&< 2" `shouldBe` Right (">= 1" :| ["< 2"])
        for_ ["", ">=", ">= 1&", "^ 1", ">= 💎", ">= arbitrary"] $ \raw -> parseGemRequirement raw `shouldSatisfy` isLeft
  where
    digest = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
