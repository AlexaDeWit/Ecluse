-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent integration requirements reject removed cases and capability drift.
module Ecluse.Test.Contract.Integration.CatalogueSpec (spec) where

import Test.Hspec

import Ecluse.Test.Contract.Catalogue (validateCatalogue)
import Ecluse.Test.Contract.Integration.Catalogue (integrationCatalogue)
import Ecluse.Test.Contract.Types

spec :: Spec
spec = do
    it "validates integration coverage without acquiring its providers" $
        validateCatalogue integrationCatalogue `shouldBe` Right integrationCatalogue
    it "rejects each missing native or canonical case independently" $
        for_ (catalogueGroups integrationCatalogue) $ \scenarioGroup ->
            for_ (groupCases scenarioGroup) $ \item -> do
                let changed = integrationCatalogue{catalogueGroups = map (removeCase scenarioGroup item) (catalogueGroups integrationCatalogue)}
                validateCatalogue changed `shouldSatisfy` isLeft
    it "rejects every removed supported profile" $
        for_ (catalogueProfiles integrationCatalogue) $ \profile ->
            validateCatalogue integrationCatalogue{catalogueProfiles = filter ((/= profileId profile) . profileId) (catalogueProfiles integrationCatalogue)} `shouldSatisfy` isLeft
    it "rejects capability removal without erasing its obligation" $
        for_ (catalogueProfiles integrationCatalogue) $ \profile ->
            for_ (profileCapabilities profile) $ \capability -> do
                let removeCapability candidate = if profileId candidate == profileId profile then candidate{profileCapabilities = filter (/= capability) (profileCapabilities candidate)} else candidate
                validateCatalogue integrationCatalogue{catalogueProfiles = map removeCapability (catalogueProfiles integrationCatalogue)} `shouldSatisfy` isLeft
    it "rejects a newly registered ecosystem without coverage" $
        validateCatalogue integrationCatalogue{catalogueProductionEcosystems = "new-ecosystem" : catalogueProductionEcosystems integrationCatalogue} `shouldSatisfy` isLeft
    it "declares all integration groups in the fixed integration component" $ do
        length (catalogueGroups integrationCatalogue) `shouldBe` 22
        map groupPartition (catalogueGroups integrationCatalogue) `shouldSatisfy` all (== "integration")
    it "retains every unique integration case" $ do
        let cases = concatMap groupCases (catalogueGroups integrationCatalogue)
        length cases `shouldBe` 471
        length (ordNub (map caseId cases)) `shouldBe` 471
        for_ ["integration/origin/native-018", "integration/cargo/source-layout", "integration.serving.rubygems.artifact-integrity"] $ \identifier ->
            map caseId cases `shouldContain` [identifier]

removeCase :: Group -> Case -> Group -> Group
removeCase selected item candidate
    | groupId candidate == groupId selected = candidate{groupCases = filter ((/= caseId item) . caseId) (groupCases candidate)}
    | otherwise = candidate
