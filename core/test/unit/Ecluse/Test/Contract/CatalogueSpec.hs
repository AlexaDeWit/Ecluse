-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Removal controls prevent coverage declarations from erasing their own requirements.
module Ecluse.Test.Contract.CatalogueSpec (spec) where

import Test.Hspec

import Ecluse.Test.Contract.Catalogue
import Ecluse.Test.Contract.Fixtures
import Ecluse.Test.Contract.Types

spec :: Spec
spec = do
    describe "validateCatalogue" $ do
        it "accepts the complete independently declared registration" $
            validateCatalogue catalogue `shouldBe` Right catalogue
        forM_ omissions $ \(label, altered) ->
            it label $ validateCatalogue altered `shouldSatisfy` isLeft
        it "permits planned and unsupported obligations without runnable no-op cases" $ do
            let plans = [obligation{obligationId = "future", obligationSupport = Planned "delivery-issue"}, obligation{obligationId = "refused", obligationSupport = Unsupported "no protocol"}]
                altered = catalogue{catalogueObligations = obligation : plans}
            validateCatalogue altered `shouldBe` Right altered
        it "rejects a concrete case for a planned operation" $
            validateCatalogue catalogue{catalogueObligations = [obligation{obligationSupport = Planned "delivery-issue"}]} `shouldSatisfy` isLeft
    describe "selectGroups" $ do
        it "selects whole lifecycle groups by stable ID" $
            selectGroups emptySelection{selectionGroup = Just "example/read"} catalogue `shouldBe` Right [group]
        it "intersects ecosystem, backend, capability and partition filters" $
            selectGroups emptySelection{selectionEcosystem = Just "example", selectionBackend = Just "http", selectionCapability = Just "read", selectionPartition = Just "primary"} catalogue `shouldBe` Right [group]
        it "rejects unknown IDs without evaluating any provider" $
            selectGroups emptySelection{selectionGroup = Just "absent"} catalogue `shouldSatisfy` isLeft
        it "rejects empty filter intersections" $
            selectGroups emptySelection{selectionBackend = Just "absent"} catalogue `shouldSatisfy` isLeft
    describe "case identity" $ do
        it "preserves the ID while human text changes" $
            labelCaseId (caseLabel "example/read" "A renamed explanation") `shouldBe` Just "example/read"
        it "refuses to infer an ID from human descriptions" $
            labelCaseId "example/read" `shouldBe` Nothing
        it "rejects incomplete or invalid markers" $ do
            labelCaseId "[example/read]" `shouldBe` Nothing
            labelCaseId "[Human description] example" `shouldBe` Nothing

omissions :: [(String, Catalogue)]
omissions =
    [ ("rejects a removed group", catalogue{catalogueGroups = []})
    , ("rejects a removed required profile", catalogue{catalogueProfiles = []})
    , ("rejects a removed capability", catalogue{catalogueProfiles = [profile{profileCapabilities = []}]})
    , ("rejects an empty required case list", catalogue{catalogueObligations = [obligation{obligationCases = []}]})
    , ("rejects an omitted case", catalogue{catalogueGroups = [group{groupCases = []}]})
    , ("rejects a duplicate group", catalogue{catalogueGroups = [group, group]})
    , ("rejects a duplicate case", catalogue{catalogueGroups = [group{groupCases = [item, item]}]})
    , ("rejects a ghost case", catalogue{catalogueGroups = [group{groupCases = [item{caseId = "ghost"}]}]})
    , ("rejects an empty added profile", catalogue{catalogueProfiles = [profile, profile{profileId = "empty"}]})
    , ("rejects a group omitting required capabilities", catalogue{catalogueGroups = [group{groupCapabilities = []}]})
    , ("rejects a removed requirement", catalogue{catalogueObligations = []})
    , ("rejects a required new production ecosystem without a profile", catalogue{catalogueProductionEcosystems = ["example", "new"]})
    , ("rejects a profile for an unregistered production ecosystem", catalogue{catalogueProductionEcosystems = []})
    ]
