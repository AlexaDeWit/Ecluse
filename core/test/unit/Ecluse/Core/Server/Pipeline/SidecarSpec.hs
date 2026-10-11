-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Companion selection retains the eligible parent's source identity and declaration.
module Ecluse.Core.Server.Pipeline.SidecarSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package (Artifact (..), CoreMetadata (..), HashAlg (SHA256), PackageDetails (pkgArtifacts))
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Package.Integrity (mkMinIntegrity)
import Ecluse.Core.Server.Pipeline.Sidecar (selectSidecar, sidecarUrl)
import Ecluse.Test.Package (sampleArtifact, sampleDetails, thingName, unsafeHash, v1_0_0, validSha256)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "sidecarUrl" $
        for_ ["", "?signature=secret", "#sha256=digest", "?signature=secret#sha256=digest"] $ \suffix ->
            it ("appends to the path before " <> toString suffix) $
                sidecarUrl ("https://files.test/file.whl" <> suffix) `shouldBe` "https://files.test/file.whl.metadata" <> suffix

    describe "selectSidecar" $ do
        for_ [Nothing, Just (MetadataAvailable False), Just (MetadataDigests mempty)] $ \declaration ->
            it ("refuses an unusable declaration " <> show declaration) $ do
                floorSpec <- expectRight (mkMinIntegrity SHA256)
                selectSidecar floorSpec (const True) (artFilename parent) (details [parent{artCoreMetadata = declaration}]) `shouldBe` Nothing

        for_ [MetadataAvailable True, MetadataDigests (Map.singleton "unknown" "verbatim")] $ \declaration ->
            it ("accepts a usable declaration " <> show declaration) $ do
                floorSpec <- expectRight (mkMinIntegrity SHA256)
                let candidate = parent{artCoreMetadata = Just declaration}
                selectSidecar floorSpec (const True) (artFilename parent) (details [candidate]) `shouldBe` Just candidate

        it "skips a weak duplicate without borrowing its declaration or location" $ do
            floorSpec <- expectRight (mkMinIntegrity SHA256)
            let weak = parent{artHashes = [], artEntryKey = ArrayEntry 0, artUrl = "https://files.test/weak"}
            selectSidecar floorSpec (const True) (artFilename parent) (details [weak, parent]) `shouldBe` Just parent

        it "never borrows a later duplicate's declaration" $ do
            floorSpec <- expectRight (mkMinIntegrity SHA256)
            selectSidecar floorSpec (const True) (artFilename parent) (details [parent{artCoreMetadata = Nothing}, parent]) `shouldBe` Nothing

        it "refuses a foreign parent before selecting its declaration" $ do
            floorSpec <- expectRight (mkMinIntegrity SHA256)
            selectSidecar floorSpec (const False) (artFilename parent) (details [parent]) `shouldBe` Nothing

parent :: Artifact
parent = sampleArtifact{artEntryKey = ArrayEntry 1, artHashes = [unsafeHash SHA256 validSha256], artCoreMetadata = Just (MetadataAvailable True)}

details :: [Artifact] -> PackageDetails
details artifacts = (sampleDetails thingName v1_0_0){pkgArtifacts = fromMaybe (parent :| []) (nonEmpty artifacts)}
