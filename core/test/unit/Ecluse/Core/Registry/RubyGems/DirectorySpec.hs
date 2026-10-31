-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Directory fixtures check native updates independently from per-gem admission.
module Ecluse.Core.Registry.RubyGems.DirectorySpec (spec) where

import Data.Aeson (Value (Null))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Test.Hspec

import Ecluse.Core.Package (InvalidEntry (invalidKey, invalidKind, invalidValue), InvalidEntryKind (InvalidVersionListing))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.JsonStream (StreamResult (..), readStepsWithEnd)
import Ecluse.Core.Registry.Metadata.RecordStream (finishRecordSteps)
import Ecluse.Core.Registry.RubyGems.Directory
import Ecluse.Core.Registry.RubyGems.Directory.Internal (directoryNameCeiling, directoryStepsWithin)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (..), Limits (..), defaultLimits)
import Ecluse.Core.Server.MemoryModel (decodedDocumentCharge)
import Ecluse.Test.Registry.JsonStream (heldChunks)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "RubyGems packed discovery" $ do
    it "reconciles repeated additions and yanks at every byte split" $ do
        let body = "created_at: 2026-01-01T00:00:00Z\n---\nb 1.0,2.0 0123456789abcdef0123456789abcdef\na 1.0 \nb -1.0,3.0 \na -1.0 \n"
            expected = GemDirectory "b 2.0,3.0 \n"
        for_ [1 .. BS.length body] $ \cut -> do
            result <- expectRight =<< parse [BS.take cut body, BS.drop cut body]
            readDirectory <$> streamValue result `shouldBe` Right expected
    it "merges public and private collisions with explicit blank hints" $ do
        let public = GemDirectory "a 1.0 \nc 2.0 \n"
            private = GemDirectory "b 1.0 \nc 1.0,3.0 \n"
        mergeDirectories public private `shouldBe` GemDirectory "a 1.0 \nb 1.0 \nc 1.0,2.0,3.0 \n"
    it "drops a malformed row and keeps the rest of the directory" $ do
        parsed <- expectRight . streamValue =<< expectRight =<< parse ["---\na 1.0 wrong\nb 1.0 \n---\n"]
        readDirectory parsed `shouldBe` GemDirectory "b 1.0 \n"
        map dropped (readDropped parsed) `shouldBe` [(InvalidVersionListing, show (RecordEntry 1), Null), (InvalidVersionListing, show (RecordEntry 3), Null)]
    it "keeps a gem's earlier state when a later row for it is malformed" $ do
        parsed <- expectRight . streamValue =<< expectRight =<< parse ["---\na 1.0 \na -1.0 junk\n"]
        readDirectory parsed `shouldBe` GemDirectory "a 1.0 \n"
        map dropped (readDropped parsed) `shouldBe` [(InvalidVersionListing, show (RecordEntry 2), Null)]
    it "bounds distinct gem names at the directory ceiling" $ do
        parseWithin 1 ["---\na 1.0 \nb 1.0 \n"] `shouldReturn` Left (TooManyVersions 2 1)
        result <- expectRight =<< parseWithin 1 ["---\na 1.0 \na 2.0 \n"]
        readDirectory <$> streamValue result `shouldBe` Right (GemDirectory "a 1.0,2.0 \n")
    it "fixes the documented ceiling at 2,000,000 gem names" $
        directoryNameCeiling `shouldBe` 2000000
    it "keeps the complete source byte bound" $ do
        let body = "---\na 1.0 \nb 1.0 \n"
        parseCapped (BS.length body - 1) [body] `shouldReturn` Left (BodyTooLarge (MetadataBodyLimit (BS.length body - 1)))
    it "rejects a directory without its header delimiter" $ do
        result <- expectRight =<< parse ["a 1.0 \n"]
        streamValue result `shouldSatisfy` isLeft
    it "drops rows with invalid names, hints or version tokens" $
        for_ ["---\n../a 1.0 \n", "---\na 1.0 0123\n", "---\na ,1.0 \n"] $ \body -> do
            parsed <- expectRight . streamValue =<< expectRight =<< parse [body]
            readDirectory parsed `shouldBe` GemDirectory ""
            map dropped (readDropped parsed) `shouldBe` [(InvalidVersionListing, show (RecordEntry 1), Null)]
    it "charges a long public history colliding with one private token" $ do
        let row = "a " <> B8.intercalate "," [show n | n <- [1 .. 10000 :: Int]] <> " "
            public = GemDirectory (row <> "\n")
            private = GemDirectory "a 10001 \n"
        directoryMergeWorkspace public private `shouldBe` decodedDocumentCharge row + decodedDocumentCharge "a 10001 "
        directoryMergeWorkspace public (GemDirectory "b 1 \n") `shouldBe` 0
        BS.length (directoryBytes (mergeDirectories public private)) `shouldSatisfy` (> BS.length row)
    it "retains only packed bytes after parsing" $ do
        result <- expectRight . streamValue =<< expectRight =<< parse ["created_at: 2026-01-01T00:00:00Z\n", "---\na 1.0,2.0 ", "0123456789abcdef0123456789abcdef\n"]
        directoryBytes (readDirectory result) `shouldBe` "a 1.0,2.0 \n"
  where
    dropped entry = (invalidKind entry, invalidKey entry, invalidValue entry)
    parse = parseCapped (maxMetadataBytes defaultLimits)
    parseCapped cap = readWith cap (directorySteps (const pass))
    parseWithin nameCeiling = readWith (maxMetadataBytes defaultLimits) (directoryStepsWithin nameCeiling (const pass))
    readWith cap steps chunks = do
        next <- heldChunks chunks
        readStepsWithEnd id finishRecordSteps (MetadataBodyLimit cap) steps next
