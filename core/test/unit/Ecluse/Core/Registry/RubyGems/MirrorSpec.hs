-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Companion locations preserve the source root and checked native artifact identity.
module Ecluse.Core.Registry.RubyGems.MirrorSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package (PackageDetails (..), PackageInfo (infoVersions))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (rubyGemsCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.Publish (MirrorCompanion (mcLocation))
import Ecluse.Core.Registry.RubyGems.Document
import Ecluse.Core.Registry.RubyGems.Mirror
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Registry.RubyGems.Projection (projectGemRows)
import Ecluse.Core.Registry.RubyGems.Wire (decodeInfoRow)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "RubyGems mirror companions" $ do
    it "keeps a configured source prefix and preserves the exact platform suffix" $ do
        name <- expectRight (projectName "fixture")
        row <- expectRight (decodeInfoRow name (RecordEntry 7) "1.0.0-x86_64-linux-musl |checksum:0000000000000000000000000000000000000000000000000000000000000000")
        info <- expectRight (projectGemRows defaultLimits name "https://source.test/prefix" [row])
        details <- maybe (fail "missing fixture version") pure (Map.lookup "1.0.0" (infoVersions info))
        let source = GemDocument name "---\n" [row]
        inventory <- expectRight (checkedMirrorInventory name (pkgVersion details) (VersionDoc details (Just (fst rubyGemsCached source))) (RubyGemsMirrorRead source))
        let file :| _ = inventoryFiles inventory
            location = mcLocation rubyMirrorCompanion file
        location "https://source.test/prefix/gems/fixture-1.0.0-x86_64-linux-musl.gem"
            `shouldBe` Right "https://source.test/prefix/quick/Marshal.4.8/fixture-1.0.0-x86_64-linux-musl.gemspec.rz"
        location "https://source.test/prefix/gems/other-1.0.0.gem" `shouldSatisfy` isLeft
        location "https://source.test/prefix/gems/fixture-1.0.0-x86_64-linux-musl.gem?redirect=1" `shouldSatisfy` isLeft
