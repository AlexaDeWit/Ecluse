-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Complete reads preserve platform grouping and reject late conflicts and resource breaches.
module Ecluse.Core.Registry.RubyGems.ReaderSpec (spec) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Test.Registry.Source (assertCancelledRead)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (PackageDetails (..), PackageInfo (..))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Registry.RubyGems.Reader (GemRead (..), GemReadResult (..), readGemInfoCharged)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (..), Limits (..), defaultLimits)
import Ecluse.Core.Text.Iso8601 (readIso8601Utc)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Registry.JsonStream (heldChunks, testTable)
import Ecluse.Test.Support (expectRight)

-- | Exercise framing at every split and scan selected versions through the final source record.
spec :: Spec
spec = describe "RubyGems complete named reads" $ do
    it "keeps all platform rows and the newest original publication time at every byte split" $ do
        let bytes = "opaque header\n---\n" <> row "1.0" "2026-01-01T00:00:00Z" <> "\n\n" <> row "1.0-java" "2026-01-03T00:00:00Z"
        reference <- parse defaultLimits FullRead [bytes]
        for_ [1 .. BS.length bytes - 1] $ \cut -> parse defaultLimits FullRead [BS.take cut bytes, BS.drop cut bytes] `shouldReturn` reference
        result <- expectRight . streamValue =<< expectRight reference
        map grEntryKey (gemRows (gemReadDocument result)) `shouldBe` [RecordEntry 2, RecordEntry 4]
        details <- maybe (expectationFailure "version absent" >> fail "version absent") pure (Map.lookup "1.0" (infoVersions (gemReadInfo result)))
        length (pkgArtifacts details) `shouldBe` 2
        pkgPublishedAt details `shouldBe` readIso8601Utc "2026-01-03T00:00:00Z"
    it "keeps grouped age unknown when one platform timestamp is missing" $ do
        result <- readResult FullRead ("---\n" <> row "1.0" "2026-01-01T00:00:00Z" <> "\n1.0-java |checksum:" <> digest)
        fmap pkgPublishedAt (Map.lookup "1.0" (infoVersions (gemReadInfo result))) `shouldBe` Just Nothing
    it "quarantines a selected version when a malformed duplicate appears after another version" $ do
        let bytes = "---\n" <> row "1.0" "2026-01-01T00:00:00Z" <> "\n" <> row "2.0" "2026-01-01T00:00:00Z" <> "\n1.0 |checksum:invalid"
        result <- readResult (SelectedRead (mkVersion RubyGems "1.0")) bytes
        infoVersions (gemReadInfo result) `shouldBe` Map.empty
        infoInvalidEntries (gemReadInfo result) `shouldSatisfy` (not . null)
    it "counts trailing unselected artifacts and complete source bytes" $ do
        let bytes = "---\n" <> row "1.0" "2026-01-01T00:00:00Z" <> "\n" <> row "2.0" "2026-01-01T00:00:00Z"
        parse defaultLimits{maxArtifactCount = 1} (SelectedRead (mkVersion RubyGems "1.0")) [bytes] `shouldReturn` Left (TooManyArtifacts 2 1)
        parse defaultLimits{maxVersionCount = 1} FullRead [bytes] `shouldReturn` Left (TooManyVersions 2 1)
        parse defaultLimits{maxMetadataBytes = BS.length bytes - 1} FullRead [bytes] `shouldReturn` Left (BodyTooLarge (MetadataBodyLimit (BS.length bytes - 1)))
    it "keeps selected workspace independent of eightfold distinct dropped history" $ do
        let target = row "1.0" "2026-01-01T00:00:00Z"
            body count = "---\n" <> target <> BS.concat ["\n" <> row (encodeUtf8 ("2." <> T.justifyRight 6 '0' (show at))) "2026-01-01T00:00:00Z" | at <- [1 .. count :: Int]]
            selected = SelectedRead (mkVersion RubyGems "1.0")
        (small, smallPeak) <- charged selected (body 250)
        (large, largePeak) <- charged selected (body 2000)
        gemReadDocument large `shouldBe` gemReadDocument small
        largePeak `shouldSatisfy` (< 2 * smallPeak)
    it "propagates asynchronous cancellation from a blocked source" $ do
        name <- expectRight (projectName "native-gem")
        assertCancelledRead (readGemInfoCharged (const pass) defaultLimits name "https://rubygems.org" FullRead (MetadataBodyLimit (maxMetadataBytes defaultLimits)) (testTable []))
    it "requires an actual header delimiter" $ do
        parsed <- expectRight =<< parse defaultLimits FullRead [row "1.0" "2026-01-01T00:00:00Z"]
        streamValue parsed `shouldSatisfy` isLeft
  where
    row coordinate timestamp = coordinate <> " |checksum:" <> digest <> ",created_at:" <> timestamp
    digest = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
    parse limits mode chunks = do
        name <- expectRight (projectName "native-gem")
        next <- heldChunks chunks
        readGemInfoCharged (const pass) limits name "https://rubygems.org" mode (MetadataBodyLimit (maxMetadataBytes limits)) (testTable []) next
    readResult mode bytes = expectRight . streamValue =<< expectRight =<< parse defaultLimits mode [bytes]

    charged mode body = do
        name <- expectRight (projectName "native-gem")
        peak <- newIORef (0 :: Int)
        next <- heldChunks [body]
        streamed <- expectRight =<< readGemInfoCharged (modifyIORef' peak . max) defaultLimits name "https://rubygems.org" mode (MetadataBodyLimit (maxMetadataBytes defaultLimits)) (testTable []) next
        result <- expectRight (streamValue streamed)
        (result,) <$> readIORef peak
