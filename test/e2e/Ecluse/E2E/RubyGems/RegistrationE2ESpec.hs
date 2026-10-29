-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent native obligations reject omitted providers before their fixture hooks run.
module Ecluse.E2E.RubyGems.RegistrationE2ESpec (rubyRegistrationContract) where

import Data.Text qualified as T
import Test.Hspec (it, shouldBe, shouldSatisfy)

import Ecluse.E2E.Requirements (obligations)
import Ecluse.E2E.RubyGems.Profiles (rubyProfiles)
import Ecluse.Test.Contract.Catalogue (caseLabel, validateCatalogue)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Types

-- | These controls inspect declarations and an independent requirement snapshot without acquiring resources.
rubyRegistrationContract :: [DeclaredGroup] -> DeclaredGroup
rubyRegistrationContract declarations = DeclaredGroup metadata $ const $ do
    it (label "provider-omission" "rejects every removed native family or provider") $ do
        validateCatalogue snapshot `shouldBe` Right snapshot
        for_ groups $ \selected ->
            validateCatalogue snapshot{catalogueGroups = filter ((/= groupId selected) . groupId) groups} `shouldSatisfy` isLeft
    it (label "case-omission" "rejects each removed native case") $ do
        validateCatalogue snapshot `shouldBe` Right snapshot
        for_ groups $ \selected ->
            for_ (groupCases selected) $ \missing ->
                let remove candidate = if groupId candidate == groupId selected then candidate{groupCases = filter ((/= caseId missing) . caseId) (groupCases candidate)} else candidate
                 in validateCatalogue snapshot{catalogueGroups = map remove groups} `shouldSatisfy` isLeft
    it (label "capability-omission" "rejects each removed profile capability while its obligation stays required") $ do
        validateCatalogue snapshot `shouldBe` Right snapshot
        for_ profiles $ \selected ->
            for_ (profileCapabilities selected) $ \missing ->
                let remove candidate = if profileId candidate == profileId selected then candidate{profileCapabilities = filter (/= missing) (profileCapabilities candidate)} else candidate
                 in validateCatalogue snapshot{catalogueProfiles = map remove profiles} `shouldSatisfy` isLeft
        for_ groups $ \selected ->
            for_ (groupCapabilities selected) $ \missing ->
                let remove candidate = if groupId candidate == groupId selected then candidate{groupCapabilities = filter (/= missing) (groupCapabilities candidate)} else candidate
                 in validateCatalogue snapshot{catalogueGroups = map remove groups} `shouldSatisfy` isLeft
  where
    groups = map declaredGroup declarations
    profiles = filter ((/= "rubygems/contracts") . profileId) rubyProfiles
    required = filter (\obligation -> "rubygems/" `T.isPrefixOf` obligationId obligation && obligationId obligation /= "rubygems/registration") obligations
    snapshot = Catalogue ["rubygems"] profiles required groups
    metadata =
        Group
            "rubygems/registration"
            "rubygems/registration"
            "rubygems/contracts"
            E2E
            "rubygems"
            [Case ("rubygems/registration/" <> name) ["rubygems/registration"] | name <- ["provider-omission", "case-omission", "capability-omission"]]
            ["contract-registration"]
            []
            []
            []
    label name = caseLabel ("rubygems/registration/" <> name)
