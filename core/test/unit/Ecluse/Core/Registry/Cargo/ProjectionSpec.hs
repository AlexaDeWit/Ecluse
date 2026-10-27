-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native facts retain registry time, yank and exact archive identity while execution stays unknown.
module Ecluse.Core.Registry.Cargo.ProjectionSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Test.Hspec

import Ecluse.Core.Package (Artifact (artEntryKey, artFilename, artUrl), Availability (Yanked), CodeExecSignal (CodeExecUnknown), PackageDetails (..))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.Cargo.Projection (projectCargoRecord)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestDiscovery, cargoTestName, cargoTestRecord)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "projectCargoRecord" $ do
    it "binds archive identity and source position without declaring an execution result" $ do
        discovery <- expectRight cargoTestDiscovery
        details <- expectRight (projectCargoRecord cargoTestName (RecordEntry 7) discovery (cargoTestRecord "1.0.0+build"))
        pkgInstallCode details `shouldBe` CodeExecUnknown
        pkgPublishedAt details `shouldSatisfy` isJust
        map artEntryKey (toList (pkgArtifacts details)) `shouldBe` [RecordEntry 7]
        map artFilename (toList (pkgArtifacts details)) `shouldBe` ["fixture-1.0.0+build.crate"]
        map artUrl (toList (pkgArtifacts details)) `shouldBe` ["https://static.crates.io/crates/fixture/fixture-1.0.0%2Bbuild.crate"]
    it "keeps invalid or absent native publication times unknown and retains yank" $ do
        discovery <- expectRight cargoTestDiscovery
        let changed = case cargoTestRecord "1.0.0" of
                Object fields -> Object (KeyMap.insert "yanked" (Bool True) (KeyMap.insert "pubtime" (String "not-a-time") fields))
                value -> value
        details <- expectRight (projectCargoRecord cargoTestName (RecordEntry 0) discovery changed)
        pkgPublishedAt details `shouldBe` Nothing
        pkgAvailability details `shouldBe` Yanked
    it "rejects a checksum or native name that disagrees with the requested package" $ do
        discovery <- expectRight cargoTestDiscovery
        for_ [("name", String "foreign"), ("cksum", String "bad")] $ \(key, replacement) -> do
            let changed = case cargoTestRecord "1.0.0" of Object fields -> Object (KeyMap.insert key replacement fields); value -> value
            projectCargoRecord cargoTestName (RecordEntry 0) discovery changed `shouldSatisfy` isLeft
