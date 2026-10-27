-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Retained RubyGems documents preserve authorised physical artifacts and raw versions.
module Ecluse.Core.Registry.RubyGems.DocumentSpec (spec) where

import Data.Set qualified as Set
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (npmCached, pypiSimpleCached, rubyGemsCached, weighCachedDoc)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (..), restrictGemDocument, selectGemVersion)
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Registry.RubyGems.Wire (decodeInfoRow)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Support (expectRight)

-- | Check selected multi-platform retention and opaque-document accounting.
spec :: Spec
spec = describe "RubyGems retained document" $ do
    it "keeps every selected platform without collapsing raw version spellings" $ do
        document <- fixture
        fmap (map grEntryKey . gemRows) (selectGemVersion (mkVersion RubyGems "1.0") document) `shouldBe` Just [RecordEntry 1, RecordEntry 2]
        fmap (map grEntryKey . gemRows) (selectGemVersion (mkVersion RubyGems "1.0.0") document) `shouldBe` Just [RecordEntry 3]
        selectGemVersion (mkVersion RubyGems "2.0") document `shouldBe` Nothing
    it "restricts artifacts by original source coordinates" $ do
        document <- fixture
        map grEntryKey (gemRows (restrictGemDocument (Set.singleton (RecordEntry 2)) document)) `shouldBe` [RecordEntry 2]
    it "injects native rows without projecting them as another ecosystem" $ do
        document <- fixture
        let retained = fst rubyGemsCached document
        snd rubyGemsCached retained `shouldBe` Just document
        snd npmCached retained `shouldBe` Nothing
        snd pypiSimpleCached retained `shouldBe` Nothing
        weighCachedDoc retained `shouldSatisfy` (> 0)
  where
    fixture = do
        name <- expectRight (projectName "native-gem")
        rows <- traverse (\(at, version) -> expectRight (decodeInfoRow name (RecordEntry at) (version <> " |checksum:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"))) [(1, "1.0"), (2, "1.0-java"), (3, "1.0.0")]
        pure (GemDocument name "---\n" rows)
