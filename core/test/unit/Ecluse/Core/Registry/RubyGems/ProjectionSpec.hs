-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native projection binds anonymous source artifacts and leaves execution evidence unknown.
module Ecluse.Core.Registry.RubyGems.ProjectionSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Unique (hashUnique, newUnique)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import System.Mem (performMajorGC)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)
import Test.Hspec
import UnliftIO.Exception (bracket, evaluate, impureThrow)

import Ecluse.Core.Package (Artifact (..), CodeExecSignal (CodeExecUnknown), PackageDetails (..), PackageInfo (..))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.RubyGems.Document (GemRow (..))
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Registry.RubyGems.Projection (projectGemRows)
import Ecluse.Core.Registry.RubyGems.Wire (decodeInfoRow)
import Ecluse.Test.Support (expectRight)

-- | Keep exact source URLs, physical positions and the blocked rule's unknown signal.
spec :: Spec
spec = describe "projectGemRows" $ do
    it "binds the artifact path beneath the selected source without inventing installation evidence" $ do
        name <- expectRight (projectName "Native_Gem")
        row <- expectRight (decodeInfoRow name (RecordEntry 4) "1.0-java |checksum:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
        info <- expectRight (projectGemRows name "https://private.test/ruby" [row])
        details <- expectRight (maybeToRight ("version missing" :: Text) (Map.lookup "1.0" (infoVersions info)))
        pkgInstallCode details `shouldBe` CodeExecUnknown
        pkgPublishedAt details `shouldBe` Nothing
        map artEntryKey (toList (pkgArtifacts details)) `shouldBe` [RecordEntry 4]
        map artUrl (toList (pkgArtifacts details)) `shouldBe` ["https://private.test/ruby/gems/Native_Gem-1.0-java.gem"]
    it "preserves source artifact order across interleaved raw versions" $ do
        name <- expectRight (projectName "native-gem")
        rows <- forM [(2, "1.0-java"), (3, "2.0"), (4, "1.0"), (5, "1.0-x86_64-linux-musl")] $ \(position, coordinate) ->
            expectRight (decodeInfoRow name (RecordEntry position) (coordinate <> " |checksum:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"))
        info <- expectRight (projectGemRows name "https://rubygems.org" rows)
        details <- expectRight (maybeToRight ("version missing" :: Text) (Map.lookup "1.0" (infoVersions info)))
        map artEntryKey (toList (pkgArtifacts details)) `shouldBe` [RecordEntry 2, RecordEntry 4, RecordEntry 5]
        map artFilename (toList (pkgArtifacts details)) `shouldBe` ["native-gem-1.0-java.gem", "native-gem-1.0.gem", "native-gem-1.0-x86_64-linux-musl.gem"]
    it "settles optional publication payloads before returning a successful projection" $ do
        name <- expectRight (projectName "rack")
        row <- expectRight (decodeInfoRow name (RecordEntry 1) "1.0 |checksum:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
        let deferred = row{grPublishedAt = Just (impureThrow DeferredTimestamp)}
        evaluate (projectGemRows name "https://rubygems.org" [deferred]) `shouldThrow` (== DeferredTimestamp)
    it "releases native source rows while only unforced policy facts remain held" $ do
        nonce <- hashUnique <$> newUnique
        bracket (holdProjection nonce) (freeStablePtr . fst) $ \(root, inputs) -> do
            performMajorGC
            traverse deRefWeak inputs `shouldReturn` replicate 3 Nothing
            info <- deRefStablePtr root
            Map.size (infoVersions info) `shouldBe` 2
    it "refuses a foreign requested identity" $ do
        name <- expectRight (projectName "rack")
        other <- expectRight (projectName "other")
        row <- expectRight (decodeInfoRow other (RecordEntry 1) "1.0 |checksum:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
        projectGemRows name "https://rubygems.org" [row] `shouldSatisfy` isLeft

data DeferredTimestamp = DeferredTimestamp
    deriving stock (Eq, Show)

instance Exception DeferredTimestamp

-- Decoder inputs and native rows leave scope before collection examines these roots.
{-# NOINLINE holdProjection #-}
holdProjection :: Int -> IO (StablePtr PackageInfo, [Weak (Map Text Text)])
holdProjection nonce = do
    name <- expectRight (projectName "native-gem")
    rows <- forM (zip [1 ..] ["1.0", "1.0-java", "2.0"]) $ \(position, version) ->
        expectRight (decodeInfoRow name (RecordEntry position) (encodeUtf8 (version <> " |checksum:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef,created_at:2020-01-01T00:00:00Z,opaque:" <> T.replicate 16384 "x" <> show nonce)))
    inputs <- forM rows $ \row -> do
        fields <- evaluate (grExtras row)
        mkWeakPtr fields Nothing
    info <- expectRight (projectGemRows name "https://rubygems.org" rows) >>= evaluate
    root <- newStablePtr info
    pure (root, inputs)
