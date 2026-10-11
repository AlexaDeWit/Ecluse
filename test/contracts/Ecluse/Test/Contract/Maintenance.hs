-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Store laws retain each backend's declared capabilities and exact inventory.
module Ecluse.Test.Contract.Maintenance (
    MaintenanceProfile (..),
    maintenanceFactsContract,
    maintenanceConsentContract,
    maintenanceInventoryContract,
    maintenanceDeletionContract,
) where

import Ecluse.Test.Contract.Catalogue (caseLabel)
import Test.Hspec

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance (CompletionNotion, ConsentVerdict, DeleteCeiling, RefillPosture, StoreClass, StoreFacts (..), StoreMaintenance (..), StoredVersion, VersionOutcome (VersionRemoved), collectPages)
import Ecluse.Core.Registry.Maintenance.NameSpace (NameAlphabet, wholeNameSpace)
import Ecluse.Core.Version (Version)
import Ecluse.Test.Maintenance (testDeleteGuard)

-- | Expectations come from the supported backend contract, never the handle under test.
data MaintenanceProfile = MaintenanceProfile
    { profileBackend :: Text
    , profileCeiling :: DeleteCeiling
    , profileRefill :: RefillPosture
    , profileCompletion :: CompletionNotion
    , profileAlphabet :: NameAlphabet
    , profileCursor :: Bool
    }

-- | Destructive callers must see the provider's actual limits before issuing work.
maintenanceFactsContract :: Text -> MaintenanceProfile -> SpecWith StoreMaintenance
maintenanceFactsContract group profile = describe "maintenance facts contract" $ do
    it (caseLabel (group <> "/identity-ceiling") "reports its backend identity and deletion ceiling") $ \store -> do
        factBackend (storeFacts store) `shouldBe` profileBackend profile
        factDeleteCeiling (storeFacts store) `shouldBe` profileCeiling profile
    it (caseLabel (group <> "/completion-refill") "reports completion and refill semantics") $ \store -> do
        factCompletion (storeFacts store) `shouldBe` profileCompletion profile
        factRefill (storeFacts store) `shouldBe` profileRefill profile
    it (caseLabel (group <> "/inventory-capability") "reports the inventory alphabet and cursor capability") $ \store -> do
        factNameAlphabet (storeFacts store) `shouldBe` profileAlphabet profile
        isJust (storeCursor store) `shouldBe` profileCursor profile

-- | Consent and store classification are separate evidence, with provider-specific reasons.
maintenanceConsentContract :: Text -> ConsentVerdict -> StoreClass -> SpecWith StoreMaintenance
maintenanceConsentContract group consent classification = describe "maintenance consent contract" $
    it (caseLabel (group <> "/consent") "preserves the declared consent and classification") $ \store -> do
        verifyConsent store `shouldReturn` Right consent
        classifyStore store `shouldReturn` Right classification

-- | Compare every inventory entry, including presence and revision, against independent fixtures.
maintenanceInventoryContract :: Text -> [PackageName] -> [(PackageName, [StoredVersion])] -> SpecWith StoreMaintenance
maintenanceInventoryContract group names versions = describe "maintenance inventory contract" $ do
    it (caseLabel (group <> "/packages") "enumerates exactly the expected packages") $ \store ->
        collectPages (listPackagesIn store wholeNameSpace) `shouldReturn` Right names
    it (caseLabel (group <> "/versions") "preserves each version's presence and revision") $ \store ->
        for_ versions $ \(package, expected) -> enumerateVersions store package `shouldReturn` Right expected

-- | Successful deletion reports exactly one outcome per requested version, in request order.
maintenanceDeletionContract :: Text -> PackageName -> [Version] -> SpecWith StoreMaintenance
maintenanceDeletionContract group package versions = describe "maintenance deletion contract" $
    it (caseLabel (group <> "/removed") "reports removal for every exact submitted version") $ \store ->
        deleteVersions store testDeleteGuard package versions
            `shouldReturn` [(version, VersionRemoved) | version <- versions]
