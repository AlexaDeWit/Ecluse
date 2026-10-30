-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native source records survive restart without borrowing target publication facts.
module Ecluse.Core.Registry.RubyGems.SourceStateSpec (spec) where

import Control.Monad (foldM)
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord)
import Ecluse.Core.Mirror.State.Internal (VerifiedCompanionBytes (..))
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (rubyGemsCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.Publish (MirrorCompanion (mcValidate))
import Ecluse.Core.Registry.RubyGems.Document
import Ecluse.Core.Registry.RubyGems.Mirror (mirrorRow, rubyGemsMirrorDocuments, rubyMirrorCompanion)
import Ecluse.Core.Registry.RubyGems.Project
import Ecluse.Core.Registry.RubyGems.Projection (projectGemRows)
import Ecluse.Core.Registry.RubyGems.SourceState
import Ecluse.Core.Registry.RubyGems.Wire (decodeInfoRow)
import Ecluse.Core.Registry.WireSupport (newestUpload)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.MirrorView (mirrorVersionView)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Registry.RubyGems.Fixtures (loadFixtureInfo)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "RubyGems durable source" $ do
    it "retains all original platform rows and requires each compact companion after reopen" $ do
        (document, details, inventory) <- fixture
        let target = stateTarget "native target"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        version <- maybe (fail "missing native version") pure (recordVersion nativeVersion record)
        Map.size (versionCompanions version) `shouldBe` 4
        checkMirrorCompletion version (toList (pkgArtifacts details)) `shouldSatisfy` incomplete
        bytes <- expectRight (encodeProjectRecord defaultLimits record)
        reopened <- chargedDecodeProjectRecord (const pass) defaultLimits target (pkgName details) bytes >>= expectRight
        reopened `shouldBe` record
        let fragment :| _ = versionFragments version
        inventoryReadData inventory `shouldBe` fragmentReadData fragment
        parseEither parseGemSource (gemSourceValue document) `shouldBe` Right document

    it "requires retained compact bytes for every platform before it restores source metadata" $ do
        (document, details, inventory) <- fixture
        initial <- expectRight (extendProjectRecord defaultLimits (stateTarget "native target") inventory Nothing)
        retained <- foldM (retain inventory) initial (toList (inventoryFiles inventory))
        let targetDetails = details{pkgPublishedAt = Nothing, pkgArtifacts = fmap (\artifact -> artifact{artUrl = "https://private.test/gems/" <> artFilename artifact}) (pkgArtifacts details)}
        targetVersion <- maybe (fail "missing expected version") pure (recordVersion nativeVersion retained)
        checkMirrorCompletion targetVersion (toList (pkgArtifacts targetDetails)) `shouldBe` MirrorComplete
        restored <- expectRight (mirrorVersionView rubyGemsMirrorDocuments nativeVersion retained (Just (VersionDoc targetDetails Nothing)))
        fmap (pkgPublishedAt . vdDetails) restored `shouldBe` Just (pkgPublishedAt details)
        fmap (fmap gemRows . (snd rubyGemsCached <=< vdRaw)) restored `shouldBe` Just (Just [row{grEntryKey = RecordEntry position} | (position, row) <- zip [0 ..] (gemRows document)])
        mirrorVersionView rubyGemsMirrorDocuments nativeVersion retained Nothing `shouldSatisfy` isLeft
        let missing = targetDetails{pkgArtifacts = takeOne (pkgArtifacts targetDetails)}
        mirrorVersionView rubyGemsMirrorDocuments nativeVersion retained (Just (VersionDoc missing Nothing)) `shouldSatisfy` isLeft

    it "rekeys overlapping physical positions after a later platform upload" $ do
        (document, _, inventory) <- fixture
        firstRow :| _ <- maybe (fail "missing source rows") pure (nonEmpty (gemRows document))
        let firstSource = document{gemRows = [firstRow]}
            target = stateTarget "native target"
        original <- inventoryFor firstSource
        initial <- expectRight (extendProjectRecord defaultLimits target original Nothing)
        firstRetained <- foldM (retain original) initial (toList (inventoryFiles original))
        let changed = document{gemRows = [row{grEntryKey = RecordEntry position} | (position, row) <- zip [1 ..] (reverse (gemRows document))]}
        incoming <- inventoryFor changed
        extended <- expectRight (extendProjectRecord defaultLimits target incoming (Just firstRetained))
        complete <- foldM (retain incoming) extended (filter ((/= grCoordinate firstRow) . grCoordinate) (gemRows changed) >>= \row -> filter ((== grEntryKey row) . artEntryKey . expectedArtifact) (toList (inventoryFiles incoming)))
        let details = inventoryDetails inventory
        restored <- expectRight (mirrorVersionView rubyGemsMirrorDocuments nativeVersion complete (Just (VersionDoc details Nothing)))
        let source = restored >>= vdRaw >>= snd rubyGemsCached
        fmap (map grEntryKey . gemRows) source `shouldBe` Just (map RecordEntry [0 .. 3])
        fmap (pkgPublishedAt . vdDetails) restored `shouldBe` Just (pkgPublishedAt details)

    it "preserves unknown whole-version age when it readmits a known-time source fragment" $ do
        (document, _, _) <- fixture
        firstRow :| remaining <- maybe (fail "missing source rows") pure (nonEmpty (gemRows document))
        known <- maybe (fail "missing known-time platform") pure (listToMaybe remaining)
        unknown <- expectRight (decodeInfoRow (gemName document) (grEntryKey firstRow) (encodeUtf8 (T.replace "created_at:" "missing_created_at:" (decodeUtf8 (grSource firstRow)))))
        inventory <- inventoryFor document{gemRows = unknown : remaining}
        let filename = unFilename (coordinateFilename (grCoordinate known))
        (details, source) <- maybe (fail "missing known-time fragment") pure (splitMirrorInventory (Set.singleton filename) inventory)
        pkgPublishedAt details `shouldBe` Nothing
        let doc = VersionDoc details (Just (mirrorReadDocument source))
        checkedMirrorInventory (pkgName details) nativeVersion doc source `shouldSatisfy` isRight

    it "rejects native row changes, missing platforms and inconsistent artifact digests" $ do
        (document, details, _) <- fixture
        row :| remaining <- maybe (fail "missing source rows") pure (nonEmpty (gemRows document))
        let artifact :| artifacts = pkgArtifacts details
        validateGemSource details document{gemRows = remaining} `shouldSatisfy` isLeft
        validateGemSource details document{gemRows = row{grRequiredRuby = ">= 99" :| []} : remaining} `shouldSatisfy` isLeft
        validateGemSource details{pkgArtifacts = artifact{artHashes = []} :| artifacts} document `shouldSatisfy` isLeft
  where
    incomplete MirrorIncomplete{} = True
    incomplete MirrorComplete = False
    retain inventory record file = do
        row <- expectRight (mirrorRow inventory file)
        bytes <- readFileBS ("test/fixtures/rubygems/quick/Marshal.4.8/" <> toString (coordinateStem (grCoordinate row)) <> ".gemspec.rz")
        original <- withSweeper (\deadlines -> mcValidate rubyMirrorCompanion deadlines defaultLimits (const pass) inventory file bytes) >>= expectRight
        original `shouldBe` bytes
        checked@(VerifiedCompanionBytes _ reference _) <- expectRight (verifySourceCompanionBytes defaultLimits (inventoryReadData inventory) file original)
        expectRight (retainCompanionReference nativeVersion checked reference record)
    takeOne (value :| _) = value :| []

nativeVersion :: Version
nativeVersion = mkVersion RubyGems "1.0.0"

fixture :: IO (GemDocument, PackageDetails, MirrorInventory)
fixture = do
    (name, rows) <- loadFixtureInfo "ecluse-fixture-platform"
    members <- maybe (fail "fixture has no native rows") pure (nonEmpty rows)
    let artifacts = fmap (\row -> let filename = unFilename (coordinateFilename (grCoordinate row)) in Artifact (grEntryKey row) filename ("https://public.test/gems/" <> filename) [grChecksum row] Nothing Nothing) members
        firstRow :| otherRows = members
        published = foldl' (\instant row -> newestUpload instant (grPublishedAt row)) (grPublishedAt firstRow) otherRows
        document = GemDocument name "---\n" rows
        details = PackageDetails name nativeVersion published CodeExecUnknown DependencySourcesUnknown Available artifacts
        doc = VersionDoc details (Just (fst rubyGemsCached document))
    inventory <- expectRight (checkedMirrorInventory name nativeVersion doc (RubyGemsMirrorRead document))
    pure (document, details, inventory)

inventoryFor :: GemDocument -> IO MirrorInventory
inventoryFor document = do
    info <- expectRight (projectGemRows defaultLimits (gemName document) "https://public.test" (gemRows document))
    details <- maybe (fail "missing projected native version") pure (Map.lookup "1.0.0" (infoVersions info))
    let doc = VersionDoc details (Just (fst rubyGemsCached document))
    expectRight (checkedMirrorInventory (gemName document) nativeVersion doc (RubyGemsMirrorRead document))
