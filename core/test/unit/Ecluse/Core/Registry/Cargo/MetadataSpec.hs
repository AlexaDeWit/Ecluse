-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native full and selected reads share exact source coordinates and preserve original digests.
module Ecluse.Core.Registry.Cargo.MetadataSpec (spec) where

import Data.Aeson (encode)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package (PackageInfo (infoDistTags))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (cargoCached)
import Ecluse.Core.Registry.Cargo.Document (cargoRecords)
import Ecluse.Core.Registry.Cargo.Metadata (cargoRead, selectCargoVersionDoc)
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError (MetadataUndecodable), VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.Fetch (readVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Types (ReadTerms (..))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestDiscovery, cargoTestName, cargoTestRecord, cargoTestVersion)
import Ecluse.Test.Registry.Metadata.Fetch (heldBody, heldManifest)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "prepared Cargo metadata" $ do
    it "hashes the original source bytes including CRLF and blank lines" $ do
        discovery <- expectRight cargoTestDiscovery
        let body = "\n" <> row "2.0.0" <> "\r\n" <> row "1.0.0"
        manifest <- expectRight =<< heldManifest (cargoRead discovery) defaultLimits origin cargoTestName [body]
        manifestDigest manifest `shouldBe` digestOf body
        manifestBodyBytes manifest `shouldBe` BS.length body
        infoDistTags (manifestInfo manifest) `shouldBe` Map.empty
        let selected = selectCargoVersionDoc (cargoTestVersion "1.0.0") (manifestRaw manifest)
        fmap (map fst . cargoRecords) (selected >>= snd cargoCached) `shouldBe` Just [RecordEntry 2]
    it "retains the selected native row and returns no invented latest tag" $ do
        discovery <- expectRight cargoTestDiscovery
        let body = row "2.0.0" <> "\n" <> row "1.0.0"
            version = cargoTestVersion "1.0.0"
            terms = ReadTerms defaultLimits origin (const pass)
        selected <- expectRight =<< readVersion (cargoRead discovery) passthroughTracingPort terms cargoTestName version (heldBody [body])
        vrBodyBytes selected `shouldBe` BS.length body
        vrUpstreamLatest selected `shouldBe` Nothing
        let rows = vrVersion selected >>= vdRaw >>= snd cargoCached
        fmap cargoRecords rows `shouldBe` Just [(RecordEntry 1, cargoTestRecord "1.0.0")]
    it "refuses a prepared reader used against another index root" $ do
        discovery <- expectRight cargoTestDiscovery
        result <- heldManifest (cargoRead discovery) defaultLimits "https://other.test/index" cargoTestName [row "1.0.0"]
        fmap manifestBodyBytes result `shouldBe` Left MetadataUndecodable
  where
    origin = "https://index.crates.io"

row :: Text -> ByteString
row = toStrict . encode . cargoTestRecord
