-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The pypi mount on the base topology, driven by a real @pip@ client in hash-checking mode.
module Ecluse.E2E.PyPI.InstallE2ESpec (spec) where

import Test.Hspec

import Ecluse.E2E.Fixtures.PyPI (pypiProject)
import Ecluse.E2E.Harness

-- | Drive the product image with a real pip client and local stores.
spec :: Spec
spec = whenE2EAvailable (aroundAll withGlobalDataPlane scenarios)

scenarios :: SpecWith GlobalDataPlane
scenarios =
    aroundAllWith withE2E $
        describe "pypi surface -- a real pip install" $
            it "installs the compatible non-yanked wheel with its advertised hash and no metadata sidecar" $ \e2e -> do
                advertised <- advertisedFiles e2e pypiProject
                length advertised `shouldBe` 3
                pipInstallsWheel e2e advertised
