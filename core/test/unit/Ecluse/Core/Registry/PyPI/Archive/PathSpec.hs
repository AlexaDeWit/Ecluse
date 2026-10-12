-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Archive path and metadata-directory identity checks.
module Ecluse.Core.Registry.PyPI.Archive.PathSpec (spec) where

import Ecluse.Core.Registry.PyPI.Archive.Path (checkPath, metadataPath)
import Ecluse.Core.Registry.PyPI.Project (DistributionKind (..))
import Ecluse.Test.Package (pypiVersion, unscopedPyPI)
import Test.Hspec

spec :: Spec
spec = do
    describe "checkPath" $ do
        it "accepts ordinary relative files and directories" $ do
            checkPath "example-1/PKG-INFO" `shouldBe` Right ["example-1", "PKG-INFO"]
            checkPath "example-1/" `shouldBe` Right ["example-1"]
        it "rejects ambiguous and platform-dependent paths" $
            for_ ["../PKG-INFO", "/PKG-INFO", "a/./b", "a//b", "C:/b", "a\\b"] $
                \path -> checkPath path `shouldSatisfy` isLeft
    describe "metadataPath" $
        it "checks the metadata directory against the admitted version" $ do
            metadataPath (unscopedPyPI "example") (pypiVersion "1") Sdist ["example-1", "PKG-INFO"] `shouldBe` Right True
            metadataPath (unscopedPyPI "example") (pypiVersion "1") Wheel ["other-1.dist-info", "METADATA"] `shouldSatisfy` isLeft
