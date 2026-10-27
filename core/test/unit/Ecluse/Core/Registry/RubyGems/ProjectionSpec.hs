-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native projection binds anonymous source artifacts and leaves execution evidence unknown.
module Ecluse.Core.Registry.RubyGems.ProjectionSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package (Artifact (..), CodeExecSignal (CodeExecUnknown), PackageDetails (..), PackageInfo (..))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Registry.RubyGems.Projection (projectGemRows)
import Ecluse.Core.Registry.RubyGems.Wire (decodeInfoRow)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Support (expectRight)

-- | Keep exact source URLs, physical positions and the blocked rule's unknown signal.
spec :: Spec
spec = describe "projectGemRows" $ do
    it "binds the artifact path beneath the selected source without inventing installation evidence" $ do
        name <- expectRight (projectName "Native_Gem")
        row <- expectRight (decodeInfoRow name (RecordEntry 4) "1.0-java |checksum:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
        info <- expectRight (projectGemRows defaultLimits name "https://private.test/ruby" [row])
        details <- expectRight (maybeToRight ("version missing" :: Text) (Map.lookup "1.0" (infoVersions info)))
        pkgInstallCode details `shouldBe` CodeExecUnknown
        pkgPublishedAt details `shouldBe` Nothing
        map artEntryKey (toList (pkgArtifacts details)) `shouldBe` [RecordEntry 4]
        map artUrl (toList (pkgArtifacts details)) `shouldBe` ["https://private.test/ruby/gems/Native_Gem-1.0-java.gem"]
    it "refuses a foreign requested identity" $ do
        name <- expectRight (projectName "rack")
        other <- expectRight (projectName "other")
        row <- expectRight (decodeInfoRow other (RecordEntry 1) "1.0 |checksum:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
        projectGemRows defaultLimits name "https://rubygems.org" [row] `shouldSatisfy` isLeft
