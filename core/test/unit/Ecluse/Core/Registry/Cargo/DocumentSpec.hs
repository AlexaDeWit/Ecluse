-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native packed records preserve source order, optional values and original coordinates.
module Ecluse.Core.Registry.Cargo.DocumentSpec (spec) where

import Data.Aeson (Value (Object), encode)
import Data.ByteString qualified as BS
import Test.Hspec

import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.Cargo.Document (cargoDocument, packCargoDocument, packedIndexDocument, packedIndexPlan, packedIndexResident)
import Ecluse.Core.Registry.Json.Packed (renderPackedLines)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestRecord)
import Ecluse.Test.Registry.Cargo.Reference (IndexLine (..), referenceIndex)

spec :: Spec
spec = describe "Cargo packed documents" $ do
    it "round trips source coordinates and all retained native values" $ do
        let document = cargoDocument [(RecordEntry 9, cargoTestRecord "2.0.0"), (RecordEntry 11, cargoTestRecord "1.0.0")]
            packed = packCargoDocument document
        packedIndexDocument packed `shouldBe` Just document
        packedIndexResident packed `shouldSatisfy` (> 0)
    it "renders one LF after each record in source order" $ do
        let records = [cargoTestRecord "2.0.0", cargoTestRecord "1.0.0"]
            document = cargoDocument (zipWith (\position value -> (RecordEntry position, value)) [0 ..] records)
            rendered = renderPackedLines (packedIndexPlan (packCargoDocument document))
        rendered `shouldBe` Just (BS.concat [toStrict (encode record) <> "\n" | record <- records])
        fmap (map (fmap Object . indexValue) . referenceIndex) rendered `shouldBe` Just (map Right records)
