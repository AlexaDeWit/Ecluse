-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native full and selected reads share exact source coordinates and preserve original digests.
module Ecluse.Core.Registry.Cargo.MetadataSpec (spec) where

import Data.Aeson (Value (..), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (PackageInfo (..))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (cargoCached)
import Ecluse.Core.Registry.Cargo.Document (cargoRecords)
import Ecluse.Core.Registry.Cargo.Metadata (selectCargoVersionDoc)
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError (MetadataUndecodable), VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.Fetch (readVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Types (ReadTerms (..))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Metadata.Projection (MetadataFraming (RecordStream), ProjectionDriver (..), metadataProjectionContract, projectionFailureLaw)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Cargo.Metadata (cargoRead, cargoTestDiscovery, cargoTestName, cargoTestRecord, cargoTestVersion)
import Ecluse.Test.Registry.Metadata.Fetch (heldBody, heldManifest)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    metadataProjectionContract "cargo" projectionDriver
    it (caseLabel "core.metadata.projection.cargo.record-framing" "requires native records with independent refusal evidence") $
        projectionFraming projectionDriver `shouldBe` RecordStream
    it (caseLabel "core.metadata.projection.cargo.invalid-evidence-control" "rejects a permissive reader that silently loses malformed records") $ do
        let permissive = projectionDriver{projectFull = \_ _ -> pure (Right (PackageInfo cargoTestName Map.empty Map.empty []))}
        projectionFailureLaw permissive MetadataUndecodable "{broken" `shouldThrow` anyException
    nativeSpec

nativeSpec :: Spec
nativeSpec = describe "prepared Cargo metadata" $ do
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

projectionDriver :: ProjectionDriver
projectionDriver =
    ProjectionDriver
        { projectFull = \limits bytes -> do
            discovery <- expectRight cargoTestDiscovery
            fmap manifestInfo <$> heldManifest (cargoRead discovery) limits origin cargoTestName [bytes]
        , projectSelected = \limits version bytes -> do
            discovery <- expectRight cargoTestDiscovery
            result <- readVersion (cargoRead discovery) passthroughTracingPort (ReadTerms limits origin (const pass)) cargoTestName (mkVersion Cargo version) (heldBody [bytes])
            pure (fmap (fmap vdDetails . vrVersion) result)
        , projectionFraming = RecordStream
        , projectionDocument = \name versions -> BS.intercalate "\n" [toStrict (encode (Object (KeyMap.insert "name" (String name) (fields version)))) | version <- versions]
        , projectionPackageName = "fixture"
        , projectionOtherName = "foreign"
        , projectionVersions = ["1.0.0", "2.0.0"]
        , projectionMissingName = toStrict (encode (Object (KeyMap.delete "name" (fields "4.0.0"))))
        }
  where
    origin = "https://index.crates.io"
    fields version = case cargoTestRecord version of Object values -> values; _ -> KeyMap.empty
