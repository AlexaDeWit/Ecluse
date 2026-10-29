-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

module Ecluse.Core.Registry.RubyGems.ServingSpec (spec) where

import Data.Aeson (Value (Null))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package.Merge (Provenance (..), mergePackuments)
import Ecluse.Core.Registry.CachedDocument (npmCached, rubyGemsCached)
import Ecluse.Core.Registry.Metadata (Manifest (..))
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Metadata (gemReadWith)
import Ecluse.Core.Registry.RubyGems.Project (coordinatePlatform, coordinateVersion, projectName, unGemPlatform)
import Ecluse.Core.Registry.RubyGems.Serving (assembleRubyGemsDocument, serialiseRubyGemsDocument)
import Ecluse.Core.Registry.ServedDocument (RenderRefused (RenderRefused))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Registry.Metadata.Fetch (heldManifest)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "Ruby native winning-source assembly" $ do
    it "retains physical order and platform rows from the winning source only" $ do
        name <- expectRight (projectName "ecluse-fixture-platform")
        bytes <- BS.readFile "test/fixtures/rubygems/info/ecluse-fixture-platform"
        trusted <- expectRight =<< heldManifest (gemReadWith (const pass) (const pass) "https://private.test") defaultLimits "https://private.test" name [bytes]
        public <- expectRight =<< heldManifest (gemReadWith (const pass) (const pass) "https://public.test") defaultLimits "https://public.test" name [bytes]
        plan <- expectRight (maybeToRight ("missing merge plan" :: Text) (mergePackuments [(TrustedSource, info trusted), (GatedSource, info public)]))
        let sources = Map.fromList [(0, raw trusted), (1, raw public)]
            served = assembleRubyGemsDocument "https://proxy.test" sources plan Nothing
        document <- expectRight (maybeToRight ("missing Ruby document" :: Text) (snd rubyGemsCached served))
        map (unGemPlatform . coordinatePlatform . grCoordinate) (gemRows document) `shouldBe` ["ruby", "java", "x86_64-linux-musl", "arm64-darwin"]
        map (renderVersion . coordinateVersion . grCoordinate) (gemRows document) `shouldBe` replicate 4 "1.0.0"
        let mismatched = Map.insert 0 (Snapshot (digestOf "different snapshot") (manifestRaw trusted)) sources
        emptyDocument <- expectRight (maybeToRight ("missing Ruby document" :: Text) (snd rubyGemsCached (assembleRubyGemsDocument "https://proxy.test" mismatched plan Nothing)))
        gemRows emptyDocument `shouldBe` []
    it "serialises the exact admitted native Compact Index rows" $ do
        name <- expectRight (projectName "ecluse-fixture-root")
        bytes <- BS.readFile "test/fixtures/rubygems/info/ecluse-fixture-root"
        manifest <- expectRight =<< heldManifest (gemReadWith (const pass) (const pass) "https://public.test") defaultLimits "https://public.test" name [bytes]
        plan <- expectRight (maybeToRight ("missing merge plan" :: Text) (mergePackuments [(GatedSource, info manifest)]))
        let served = assembleRubyGemsDocument "https://proxy.test" (Map.singleton 0 (raw manifest)) plan Nothing
        encoded <- toStrict <$> expectRight (serialiseRubyGemsDocument served)
        encoded `shouldBe` bytes
    it "refuses a foreign source instead of serialising JSON" $
        serialiseRubyGemsDocument (fst npmCached Null) `shouldBe` Left RenderRefused
  where
    info manifest = Snapshot (manifestDigest manifest) (manifestInfo manifest)
    raw manifest = Snapshot (manifestDigest manifest) (manifestRaw manifest)
