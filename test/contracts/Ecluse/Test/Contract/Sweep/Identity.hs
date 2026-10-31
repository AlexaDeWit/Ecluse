-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RecordWildCards #-}

-- | Consent, inventory and identity-denial laws for a pair of package stores.
module Ecluse.Test.Contract.Sweep.Identity (identityScenarios) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Install (shouldSucceed)
import Ecluse.Test.Contract.Sweep (listed)
import Ecluse.Test.Contract.Sweep.Types
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

-- | Run in declaration order against the same seeded pair of stores.
identityScenarios :: IdentityFixture -> StoreOps store -> ClientOps store project -> SweepOps plane -> SpecWith (plane, store, store)
identityScenarios IdentityFixture{..} stores@StoreOps{..} ClientOps{..} SweepOps{..} = describe "identity denies with no advisory database" $ do
    it (caseLabel (identityCasePrefix <> "inventory") "walks the seeded store through listPackagesIn, including scoped base-name buckets") $ \(_, e2e, _) -> do
        storeSnapshot e2e `shouldReturn` identitySeeded
        names <- storeListing e2e
        names `shouldMatchList` Map.keys identitySeeded
        storeNamesUnder e2e "" `shouldReturn` sort names
        storeNamesUnder e2e identityListingPrefix `shouldReturn` sort names
        storeNamesUnder e2e identityAbsentPrefix `shouldReturn` []

    it (caseLabel (identityCasePrefix <> "admission-only") "keeps every version an identity deny limited to admission names, and states the phase at boot") $ \(plane, e2e, cache) -> do
        initial <- storeSnapshot e2e
        privateInitial <- storeSnapshot cache
        run <- runSweep plane Apply (AdmissionOnly (packageName identityDenied))
        (sweepExit run, sweepOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
        sweepAudit run `shouldSatisfy` elem "rule 1: DenyByIdentity (precedence 400, applies at admission only)"

        let guardCount = length (Map.findWithDefault [] identityFirstPartyName initial)
            examined = sum (map length (Map.elems initial)) - guardCount
        sweepEvents run
            `shouldBe` [cycleLine ["examined " <> show examined, "deleted 0", "kept " <> show examined, "guard-skipped " <> show guardCount]]
        storeSnapshot e2e `shouldReturn` initial
        storeSnapshot cache `shouldReturn` privateInitial

    it (caseLabel (identityCasePrefix <> "denied-versions") "deletes every denied version and preserves all other versions, including first-party versions") $ \(plane, e2e, cache) -> do
        initial <- storeSnapshot e2e
        privateInitial <- storeSnapshot cache
        run <- runSweep plane Apply (FullIdentity (packageName identityDenied))
        assertFullSweep identityFirstPartyName identityDenied initial run
        storeVersions e2e (packageName identityDenied) `shouldReturn` []
        finalStore <- storeSnapshot e2e
        finalStore `shouldBe` Map.delete (packageName identityDenied) initial
        storeNamesUnder e2e "" `shouldReturn` Map.keys finalStore
        storeSnapshot cache `shouldReturn` privateInitial

    it (caseLabel (identityCasePrefix <> "dry-run") "previews distinct real inventories without writes and preserves first-party versions") $ \(plane, e2e, cache) -> do
        for_ [packageVersion identityPreview, identityPreviewVersion] $ \version ->
            void $ clientPublish cache (packageName identityPreview) version >>= shouldSucceed
        void $ clientPublish cache identityFirstPartyName identityPublishVersion >>= shouldSucceed
        listed stores cache [packageName identityPreview, identityFirstPartyName]
        initial <- storeSnapshot e2e
        privateInitial <- storeSnapshot cache
        run <- runSweep plane Preview (FullIdentity (packageName identityPreview))
        sweepExit run `shouldBe` ExitSuccess
        let output = sweepOutput run
            preview = packageName identityPreview
            held = Map.findWithDefault [] preview
            firstParty = length (Map.findWithDefault [] identityFirstPartyName initial) + length (Map.findWithDefault [] identityFirstPartyName privateInitial)
            examined = sum (map length (Map.elems initial <> Map.elems privateInitial)) - firstParty
            condemned = length (held initial) + length (held privateInitial)
        output `shouldSatisfy` T.isInfixOf identityMirrorDiagnostic
        output `shouldSatisfy` T.isInfixOf identityCacheDiagnostic
        -- Each store previews its own copies, so the mirror's copy is named apart from the cache's.
        sort (mapMaybe (previewedIn preview) (sweepAudit run))
            `shouldBe` sort ([("mirrorTarget", version) | version <- held initial] <> [("privateUpstream", version) | version <- held privateInitial])
        held initial `shouldSatisfy` (not . null)
        held privateInitial `shouldSatisfy` elem identityPreviewVersion
        filter ("mirror sweep cycle " `T.isPrefixOf`) (sweepEvents run)
            `shouldBe` [ cycleLine
                            [ "examined " <> show examined
                            , "deleted " <> show (length (ordNub (held initial <> held privateInitial)))
                            , "kept " <> show (examined - condemned)
                            , "guard-skipped " <> show firstParty
                            ]
                       ]
        output `shouldSatisfy` (not . T.isInfixOf "counted from partial evidence")
        output `shouldSatisfy` T.isInfixOf "previewing only: this run holds nothing that could delete"
        output `shouldSatisfy` T.isInfixOf "This preview deleted nothing, so it proves no authority to delete"
        output `shouldSatisfy` (not . T.isInfixOf ("would delete " <> identityFirstPartyName))
        storeSnapshot e2e `shouldReturn` initial
        storeSnapshot cache `shouldReturn` privateInitial

    it (caseLabel (identityCasePrefix <> "consent-refusal") "refuses missing consent, names the key, and leaves every version intact") $ \(plane, e2e, cache) -> do
        initial <- storeSnapshot e2e
        privateInitial <- storeSnapshot cache
        run <- runSweep plane WithoutConsent (FullIdentity (packageName identityDenied))
        (sweepExit run, sweepOutput run) `shouldSatisfy` ((/= ExitSuccess) . fst)
        sweepOutput run `shouldSatisfy` T.isInfixOf identityConsentDiagnostic
        sweepEvents run `shouldBe` identityConsentEvents
        storeSnapshot cache `shouldReturn` privateInitial
        storeSnapshot e2e `shouldReturn` initial

    it (caseLabel (identityCasePrefix <> "first-party") "protects every first-party version even when an identity deny names one") $ \(plane, e2e, cache) -> do
        initial <- storeSnapshot e2e
        privateInitial <- storeSnapshot cache
        let guardCount = length (Map.findWithDefault [] identityFirstPartyName initial) + length (Map.findWithDefault [] identityFirstPartyName privateInitial)
        run <- runSweep plane Apply (IdentityPolicy [identityFirstPartyName <> "@" <> identityPublishVersion])
        (sweepExit run, sweepOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
        sweepEvents run `shouldBe` [cycleLine ["examined 0", "deleted 0", "kept 0", "guard-skipped " <> show guardCount]]
        storeVersions e2e identityFirstPartyName `shouldReturn` identityFirstPartyVersions
        storeSnapshot e2e `shouldReturn` initial

    it (caseLabel (identityCasePrefix <> "two-stores") "deletes shared and cache-only versions while preserving each store's other contents") $ \(plane, mirror, cache) -> do
        void $ clientPublish cache (packageName identityPreview) identityCacheOnlyVersion >>= shouldSucceed
        storeHasVersion cache (packageName identityPreview) identityCacheOnlyVersion `shouldReturn` True
        storeHasVersion mirror (packageName identityPreview) identityCacheOnlyVersion `shouldReturn` False
        mirrorBefore <- storeSnapshot mirror
        cacheBefore <- storeSnapshot cache
        run <- runSweep plane Apply (FullIdentity (packageName identityPreview))
        sweepExit run `shouldBe` ExitSuccess
        storeSnapshot mirror `shouldReturn` Map.delete (packageName identityPreview) mirrorBefore
        storeSnapshot cache `shouldReturn` Map.delete (packageName identityPreview) cacheBefore
        storeVersions mirror (packageName identityPreview) `shouldReturn` []
        storeVersions cache (packageName identityPreview) `shouldReturn` []

assertFullSweep :: Text -> PackageFixture -> Map Text [Text] -> SweepRun -> Expectation
assertFullSweep firstPartyName pkg initial run = do
    (sweepExit run, sweepOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
    let versions = Map.findWithDefault [] (packageName pkg) initial
        guardCount = length (Map.findWithDefault [] firstPartyName initial)
        examined = sum (map length (Map.elems initial)) - guardCount
        fields =
            [ "examined " <> show examined
            , "deleted " <> show (length versions)
            , "kept " <> show (examined - length versions)
            , "guard-skipped " <> show guardCount
            ]
    versions `shouldBe` [packageVersion pkg]
    sweepEvents run `shouldMatchList` (map auditLine versions <> [cycleLine fields])
  where
    auditLine version =
        "deleting "
            <> packageName pkg
            <> "@"
            <> version
            <> ": blocked by DenyByIdentity (identity "
            <> packageName pkg
            <> " is revoked by operator); advisory generation none"

cycleLine :: [Text] -> Text
cycleLine fields = "mirror sweep cycle complete: " <> T.intercalate ", " fields

-- The store role and version of a labelled preview line for the package, such as
-- @mirrorTarget <url>: dry run, would delete <package>\@<version>: <reason>@.
previewedIn :: Text -> Text -> Maybe (Text, Text)
previewedIn package message = do
    let (label, rest) = T.breakOn ": " message
    named <- T.stripPrefix (": dry run, would delete " <> package <> "@") rest
    role <- find (`T.isPrefixOf` label) ["mirrorTarget ", "privateUpstream "]
    pure (T.strip role, fst (T.breakOn ": " named))
