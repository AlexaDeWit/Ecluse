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
            for_ [Planned "delivery-issue", Unsupported "no protocol"] $ \support -> do
                let deferred = obligation{obligationId = "read/deferred", obligationSupport = support, obligationCases = ["example/read/deferred"]}
                    altered = withSecondObligation deferred (Case "example/read/deferred" ["read/deferred"])
                validateCatalogue altered `shouldBe` Left ["case does not satisfy its declared obligation: example/read/deferred: read/deferred"]
        it "rejects an empty required case list" $ do
            let other = obligation{obligationId = "read/other"}
                altered = catalogue{catalogueObligations = [obligation{obligationCases = []}, other], catalogueGroups = [scenarioGroup{groupCases = [item{caseRequirements = ["read/other"]}]}]}
            validateCatalogue altered `shouldBe` Left ["required obligation has no cases: read/identity"]
        it "rejects a removed requirement" $ do
            let other = obligation{obligationId = "read/other", obligationCases = ["example/read/other"]}
                altered = withSecondObligation other (Case "example/read/other" ["read/other"])
            validateCatalogue altered `shouldBe` Right altered
            validateCatalogue altered{catalogueObligations = [other]} `shouldBe` Left ["unknown case requirement: read/identity"]
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
    , ("rejects an omitted case", catalogue{catalogueGroups = [scenarioGroup{groupCases = []}]})
    , ("rejects a duplicate group", catalogue{catalogueGroups = [scenarioGroup, scenarioGroup]})
    , ("rejects a duplicate case", catalogue{catalogueGroups = [scenarioGroup{groupCases = [item, item]}]})
    , ("rejects a ghost case", catalogue{catalogueGroups = [scenarioGroup{groupCases = [item{caseId = "ghost"}]}]})
    , ("rejects an empty added profile", catalogue{catalogueProfiles = [profile, profile{profileId = "empty"}]})
    , ("rejects a group omitting required capabilities", catalogue{catalogueGroups = [scenarioGroup{groupCapabilities = []}]})
    , ("rejects a required new production ecosystem without a profile", catalogue{catalogueProductionEcosystems = ["example", "new"]})
    , ("rejects a profile for an unregistered production ecosystem", catalogue{catalogueProductionEcosystems = []})
    ]

{- | The fixture with a second supported or deferred obligation and its own case, so removing or
changing one obligation leaves the profile's other checks satisfied.
-}
withSecondObligation :: Obligation -> Case -> Catalogue
withSecondObligation extra extraCase =
    catalogue{catalogueObligations = [obligation, extra], catalogueGroups = [scenarioGroup{groupCases = [item, extraCase]}]}
