-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.RubyGems.ServingSpec (spec) where

import Data.Aeson (Value (Null))
import Data.ByteString qualified as BS
import Data.List (lookup)
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
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo)
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
        rows <- expectRight (referenceInfo encoded)
        map rowVersion rows `shouldBe` ["1.0.0", "2.0.0", "3.0.0"]
        map rowPlatform rows `shouldBe` replicate 3 "ruby"
        map (lookup "checksum" . rowFields) rows
            `shouldBe` [ Just "646c0b16ba8b286beb737cc2e18ceb32b9cd42f7c7903ca3adc9b2896f489885"
                       , Just "1a64355f223fb775b48257611ad5822fcb421d61174b8d2fdebce7a86847908f"
                       , Just "5bc7e3193eec05c1d1d649d7ff829d78798c9ca71a0836c03010815d71532cb8"
                       ]
    it "refuses a foreign source instead of serialising JSON" $
        serialiseRubyGemsDocument (fst npmCached Null) `shouldBe` Left RenderRefused
  where
    info manifest = Snapshot (manifestDigest manifest) (manifestInfo manifest)
    raw manifest = Snapshot (manifestDigest manifest) (manifestRaw manifest)
