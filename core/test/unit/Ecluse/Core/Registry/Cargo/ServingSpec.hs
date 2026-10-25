-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Admitted native rows come only from their winning source and exact content digest.
module Ecluse.Core.Registry.Cargo.ServingSpec (spec) where

import Data.Aeson (Value (Object), encode)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package.Merge (Provenance (GatedSource, TrustedSource), mergePackuments)
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.Cargo.Metadata (cargoRead)
import Ecluse.Core.Registry.Cargo.Serving (assembleCargoDocument, serialiseCargoDocument)
import Ecluse.Core.Registry.Metadata (Manifest (..))
import Ecluse.Core.Registry.ServedDocument (RenderRefused (RenderRefused))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Snapshot (Snapshot (..))
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestDiscovery, cargoTestName, cargoTestRecord)
import Ecluse.Test.Registry.Cargo.Reference (IndexLine (indexValue), referenceIndex)
import Ecluse.Test.Registry.Metadata.Fetch (heldManifest)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "native Cargo assembly" $ do
    it "keeps winning source order rather than numeric or lexical version order" $ do
        first <- source ["2.0.0", "1.0.0"]
        second <- source ["3.0.0", "2.0.0"]
        plan <- expectRight (maybeToRight ("missing merge plan" :: Text) (mergePackuments [(TrustedSource, info first), (GatedSource, info second)]))
        let sources = Map.fromList [(0, raw first), (1, raw second)]
        bytes <- expectRight (serialiseCargoDocument (assembleCargoDocument "https://proxy.test/cargo" sources plan Nothing))
        map (fmap Object . indexValue) (referenceIndex (toStrict bytes)) `shouldBe` map (Right . cargoTestRecord) ["2.0.0", "1.0.0", "3.0.0"]
        let wrongSnapshot = Map.insert 0 (Snapshot (digestOf "different source") (manifestRaw first)) sources
        refused <- expectRight (serialiseCargoDocument (assembleCargoDocument "https://proxy.test/cargo" wrongSnapshot plan Nothing))
        map (fmap Object . indexValue) (referenceIndex (toStrict refused)) `shouldBe` [Right (cargoTestRecord "3.0.0")]
    it "refuses a foreign ecosystem document" $
        serialiseCargoDocument (fst npmCached (Object mempty)) `shouldBe` Left RenderRefused
  where
    source versions = do
        discovery <- expectRight cargoTestDiscovery
        expectRight =<< heldManifest (cargoRead discovery) defaultLimits "https://index.crates.io" cargoTestName [BS.concat [toStrict (encode (cargoTestRecord version)) <> "\n" | version <- versions]]
    info manifest = Snapshot (manifestDigest manifest) (manifestInfo manifest)
    raw manifest = Snapshot (manifestDigest manifest) (manifestRaw manifest)
