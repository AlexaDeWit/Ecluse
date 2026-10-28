-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native identity captures must agree with checked artifact coordinates.
module Ecluse.Core.Registry.RubyGems.ProjectSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, RubyGems))
import Ecluse.Core.Package (renderPackageName)
import Ecluse.Core.Registry.RubyGems.Project
import Ecluse.Core.Server.Path (mkFilename, unFilename)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Support (expectRight)

-- | Preserve native identity and bound filename interpretations.
spec :: Spec
spec = do
    describe "projectName" $ do
        for_ ["rack", "Ruby_Name", "a.b-c_d", ".cat", "-"] $ \raw ->
            it ("retains " <> show raw) $
                fmap renderPackageName (projectName raw) `shouldBe` Right raw
        for_ ["", "../rack", "a/b", "a%2fb", "a b", "café"] $ \raw ->
            it ("refuses " <> show raw) $
                projectName raw `shouldSatisfy` isLeft
    describe "mkGemPlatform" $ do
        for_ ["ruby", "java", "arm64-darwin", "x86_64-linux-musl", "x64-mingw-ucrt", "x86-mswin32-60"] $ \platform ->
            it ("accepts native platform " <> toString platform) $ mkGemPlatform platform `shouldSatisfy` isRight
        for_ ["1234abcd", "futureformat", "x64-newos", "a/b"] $ \platform ->
            it ("refuses unsupported platform " <> toString platform) $ mkGemPlatform platform `shouldSatisfy` isLeft
    describe "candidateCoordinates" $ do
        it "retains a checked platform capture among bounded interpretations" $ do
            file <- expectRight (maybeToRight ("fixture filename invalid" :: Text) (mkFilename "my-gem-1.0.0-x86_64-linux-musl.gem"))
            candidates <- expectRight (candidateCoordinates 65536 file)
            expected <- expectRight (coordinate "x86_64-linux-musl")
            candidates `shouldContain` [expected]
        it "refuses separator amplification and an exhausted byte budget" $ do
            file <- expectRight (maybeToRight ("fixture filename invalid" :: Text) (mkFilename ("gem-" <> T.replicate 101 "a-" <> "1.0.gem")))
            candidateCoordinates 65536 file `shouldSatisfy` isLeft
            normal <- expectRight (maybeToRight ("fixture filename invalid" :: Text) (mkFilename "gem-1.0.gem"))
            candidateCoordinates 1 normal `shouldSatisfy` isLeft
    describe "mkCoordinate" $ do
        it "rebuilds the native comparison key while preserving a caller's raw version" $ do
            name <- expectRight (projectName "native-gem")
            platform <- expectRight (mkGemPlatform "ruby")
            value <- expectRight (mkCoordinate name (mkVersion Npm "1.0") platform)
            coordinateVersion value `shouldBe` mkVersion RubyGems "1.0"

        it "retains all native platform suffix components" $
            fmap (unFilename . coordinateFilename) (coordinate "x86_64-linux-musl") `shouldBe` Right "my-gem-1.0.0-x86_64-linux-musl.gem"
        it "omits the generic platform from the filename" $
            fmap coordinateStem (coordinate "ruby") `shouldBe` Right "my-gem-1.0.0"
  where
    coordinate platform = do
        name <- projectName "my-gem"
        parsed <- mkGemPlatform platform
        mkCoordinate name (mkVersion RubyGems "1.0.0") parsed
