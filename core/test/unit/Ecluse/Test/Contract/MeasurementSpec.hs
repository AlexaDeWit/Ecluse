-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Discovery must retain independently declared live and retention obligations.
module Ecluse.Test.Contract.MeasurementSpec (spec) where

import Test.Hspec

import Ecluse.Test.Contract.Catalogue (validateCatalogue)
import Ecluse.Test.Contract.Measurement (measurementCatalogue)
import Ecluse.Test.Contract.Types (Catalogue (catalogueGroups), Group (groupCases))

-- | Removing any registered case fails before acquiring a live service or a heap probe.
spec :: Spec
spec = describe "measurement catalogue" $ do
    it "covers the production adapters and independent case obligations" $
        validateCatalogue measurementCatalogue `shouldBe` Right measurementCatalogue
    it "rejects every individually omitted group" $
        for_ (catalogueGroups measurementCatalogue) $ \omitted ->
            validateCatalogue measurementCatalogue{catalogueGroups = filter (/= omitted) (catalogueGroups measurementCatalogue)} `shouldSatisfy` isLeft
    it "rejects every individually omitted case" $
        for_ (catalogueGroups measurementCatalogue) $ \group ->
            for_ (groupCases group) $ \omitted -> do
                let replace original = if original == group then original{groupCases = filter (/= omitted) (groupCases original)} else original
                validateCatalogue measurementCatalogue{catalogueGroups = map replace (catalogueGroups measurementCatalogue)} `shouldSatisfy` isLeft
