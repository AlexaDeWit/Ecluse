-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native identity captures must agree with checked artifact coordinates.
module Ecluse.Core.Registry.RubyGems.ProjectSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (renderPackageName)
import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Registry.RubyGems.Project
import Ecluse.Core.Server.Path (mkFilename, unFilename)
import Ecluse.Core.Version (mkVersion)

-- | Preserve native identity and refuse filename ambiguity.
spec :: Spec
spec = do
    describe "projectName" $ do
        for_ ["rack", "Ruby_Name", "a.b-c_d", ".cat", "-"] $ \raw ->
            it ("retains " <> show raw) $
                fmap renderPackageName (projectName raw) `shouldBe` Right raw
        for_ ["", "../rack", "a/b", "a%2fb", "a b", "café"] $ \raw ->
            it ("refuses " <> show raw) $
                projectName raw `shouldSatisfy` isLeft
    describe "mkCoordinate" $ do
        it "retains all native platform suffix components" $
            fmap (unFilename . coordinateFilename) (coordinate "x86_64-linux-musl") `shouldBe` Right "my-gem-1.0.0-x86_64-linux-musl.gem"
        it "omits the generic platform from the filename" $
            fmap coordinateStem (coordinate "ruby") `shouldBe` Right "my-gem-1.0.0"
        it "matches only an exact unambiguous coordinate" $ do
            let result = do
                    value <- coordinate "java"
                    file <- maybeToRight (ParseError "invalid fixture filename") (mkFilename "my-gem-1.0.0-java.gem")
                    pure (value, file)
            case result of
                Left _ -> expectationFailure "fixture coordinate failed"
                Right (value, file) -> do
                    matchCoordinate (coordinateName value) [value] file `shouldBe` Right value
                    matchCoordinate (coordinateName value) [value, value] file `shouldSatisfy` isLeft
  where
    coordinate platform = do
        name <- projectName "my-gem"
        parsed <- mkGemPlatform platform
        mkCoordinate name (mkVersion RubyGems "1.0.0") parsed
