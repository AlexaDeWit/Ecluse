-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native record framing, complete selected scans and source-wide resource refusals.
module Ecluse.Core.Registry.Cargo.ReaderSpec (spec) where

import Data.Aeson (Value (Number, Object), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package (PackageInfo (infoInvalidEntries, infoVersions))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.Cargo.Document (indexRecords)
import Ecluse.Core.Registry.Cargo.Reader (CargoIndexResult (..), CargoRead (..), readCargoIndex)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge, TooManyArtifacts, TooManyVersions), Limits (maxArtifactCount, maxMetadataBytes, maxVersionCount), defaultLimits)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestDiscovery, cargoTestName, cargoTestRecord, cargoTestVersion, readCargoChunks)
import Ecluse.Test.Registry.JsonStream (testTable)
import Ecluse.Test.Registry.Source (assertCancelledRead)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "readCargoIndex" $ do
    it "accepts CRLF, blank lines and a final row without LF at every chunk boundary" $ do
        let body = "\n" <> row "2.0.0" <> "\r\n \t\r\n" <> row "1.0.0"
        expected <- readCargoChunks defaultLimits FullRead [body]
        for_ [1 .. BS.length body - 1] $ \cut -> do
            actual <- readCargoChunks defaultLimits FullRead [BS.take cut body, BS.drop cut body]
            actual `shouldBe` expected
        parsed <- expectRight =<< (streamValue <$> expectRight expected)
        map fst (indexRecords (cargoIndexDocument parsed)) `shouldBe` [RecordEntry 1, RecordEntry 3]
    it "keeps healthy siblings beside malformed, truncated and unsupported schema rows" $ do
        let unsupported = case cargoTestRecord "3.0.0" of
                Object fields -> Object (KeyMap.insert "v" (Number 3) fields)
                value -> value
            body = "{broken}\n" <> row "1.0.0" <> "\n" <> toStrict (encode unsupported) <> "\n{"
        parsed <- readResult FullRead body
        Map.keys (infoVersions (cargoIndexInfo parsed)) `shouldBe` ["1.0.0"]
        length (infoInvalidEntries (cargoIndexInfo parsed)) `shouldBe` 3
    it "quarantines every duplicate build-metadata identity after scanning the full source" $ do
        let body = row "1.0.0+first" <> "\n" <> row "2.0.0" <> "\n" <> row "1.0.0+second"
        full <- readResult FullRead body
        Map.keys (infoVersions (cargoIndexInfo full)) `shouldBe` ["2.0.0"]
        length (infoInvalidEntries (cargoIndexInfo full)) `shouldBe` 2
        selected <- readResult (SelectedRead (cargoTestVersion "1.0.0+first")) body
        Map.keys (infoVersions (cargoIndexInfo selected)) `shouldBe` []
        indexRecords (cargoIndexDocument selected) `shouldBe` []
    it "lets an unsupported row's valid native version quarantine a selected sibling" $ do
        let unsupported = case cargoTestRecord "1.0.0+other" of
                Object fields -> Object (KeyMap.insert "v" (Number 3) fields)
                value -> value
        selected <- readResult (SelectedRead (cargoTestVersion "1.0.0")) (row "1.0.0" <> "\n" <> toStrict (encode unsupported))
        Map.keys (infoVersions (cargoIndexInfo selected)) `shouldBe` []
    it "counts malformed and unselected records before returning a selected result" $ do
        let limits = defaultLimits{maxVersionCount = 1}
        result <- readCargoChunks limits (SelectedRead (cargoTestVersion "1.0.0")) [row "1.0.0" <> "\n{broken}"]
        result `shouldBe` Left (TooManyVersions 2 1)
    it "enforces the artifact and complete source byte limits" $ do
        let body = row "1.0.0" <> "\n" <> row "2.0.0"
        artifacts <- readCargoChunks defaultLimits{maxArtifactCount = 1} FullRead [body]
        artifacts `shouldBe` Left (TooManyArtifacts 2 1)
        bytes <- readCargoChunks defaultLimits{maxMetadataBytes = BS.length body - 1} FullRead [body]
        bytes `shouldBe` Left (BodyTooLarge (MetadataBodyLimit (BS.length body - 1)))
    it "does not intercept asynchronous cancellation while waiting for the source" $ do
        discovery <- expectRight cargoTestDiscovery
        assertCancelledRead (readCargoIndex discovery defaultLimits cargoTestName FullRead (MetadataBodyLimit 65536) (testTable []))

row :: Text -> ByteString
row = toStrict . encode . cargoTestRecord

readResult :: CargoRead -> ByteString -> IO CargoIndexResult
readResult mode body = do
    streamed <- expectRight =<< readCargoChunks defaultLimits mode [body]
    expectRight (streamValue streamed)
