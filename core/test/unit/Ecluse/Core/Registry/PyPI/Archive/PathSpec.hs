-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Distribution metadata-directory identity checks.
module Ecluse.Core.Registry.PyPI.Archive.PathSpec (spec) where

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.PyPI.Archive.Path (metadataPath)
import Ecluse.Core.Registry.PyPI.Project (DistributionKind (..))
import Ecluse.Test.Package (pypiVersion, unscopedPyPI)
import Test.Hspec

spec :: Spec
spec =
    describe "metadataPath" $
        it "checks the metadata directory against the admitted version" $ do
            metadataPath (unscopedPyPI "example") (pypiVersion "1") Sdist ["example-1", "PKG-INFO"] `shouldBe` Right True
            metadataPath (unscopedPyPI "example") (pypiVersion "1") Wheel ["other-1.dist-info", "METADATA"] `shouldBe` Left (ParseError "archive metadata directory disagrees with admitted identity")
