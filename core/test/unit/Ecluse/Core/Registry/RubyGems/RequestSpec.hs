-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | RubyGems named requests retain source prefixes, checked coordinates and credential posture.
module Ecluse.Core.Registry.RubyGems.RequestSpec (spec) where

import Data.List (lookup)
import Network.HTTP.Client (path, redirectCount, requestHeaders)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, RubyGems))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Registry.Request (mkBaseRequest)
import Ecluse.Core.Registry.RubyGems.Project (mkCoordinate, mkGemPlatform, projectName)
import Ecluse.Core.Registry.RubyGems.Request (gemArtifactUrl, gemCompactSpecUrl, gemInfoRequestUnder)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Registry.PyPI (alicePair)
import Ecluse.Test.Support (expectRight)

-- | Verify native URL formation and private Basic reads without forwarding credentials publicly.
spec :: Spec
spec = describe "RubyGems requests" $ do
    it "keeps package case and a configured source path on anonymous metadata reads" $ do
        base <- mkBaseRequest <$> expectRight (mkRegistryUrl "https://registry.test/tenant/ruby")
        name <- expectRight (projectName "Native_Gem")
        request <- expectRight (gemInfoRequestUnder base Nothing name)
        path request `shouldBe` "/tenant/ruby/info/Native_Gem"
        lookup "Authorization" (requestHeaders request) `shouldBe` Nothing
        lookup "Accept" (requestHeaders request) `shouldBe` Just "text/plain"
        redirectCount request `shouldBe` 0
        gemInfoRequestUnder base Nothing (mkPackageName Npm Nothing "Native_Gem") `shouldSatisfy` isLeft
    it "presents private caller credentials as Basic and refuses redirects" $ do
        base <- mkBaseRequest <$> expectRight (mkRegistryUrl "https://registry.test/ruby")
        name <- expectRight (projectName "native-gem")
        request <- expectRight (gemInfoRequestUnder base alicePair name)
        lookup "Authorization" (requestHeaders request) `shouldBe` Just "Basic YWxpY2U6aHVudGVyMg=="
        redirectCount request `shouldBe` 0
    it "uses the checked platform coordinate in artifact and compact-spec paths" $ do
        name <- expectRight (projectName "native-gem")
        platform <- expectRight (mkGemPlatform "x86_64-linux-musl")
        coordinate <- expectRight (mkCoordinate name (mkVersion RubyGems "1.2.3") platform)
        gemArtifactUrl "https://registry.test/ruby" coordinate `shouldBe` Right "https://registry.test/ruby/gems/native-gem-1.2.3-x86_64-linux-musl.gem"
        gemCompactSpecUrl "https://registry.test/ruby" coordinate `shouldBe` Right "https://registry.test/ruby/quick/Marshal.4.8/native-gem-1.2.3-x86_64-linux-musl.gemspec.rz"
