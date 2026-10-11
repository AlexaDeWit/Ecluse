-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Both mounts behind one proxy, driven by a real @npm@ client and a real @pip@ client in turn. The
modules under an ecosystem directory each send their proxies one ecosystem's traffic.
-}
module Ecluse.E2E.MixedEcosystemE2ESpec (mixedMountContract) where

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Phase (PhaseRecorder, timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Test.Hspec

import Ecluse.E2E.Fixtures.Npm (allowPkg, psName, psVersion)
import Ecluse.E2E.Fixtures.PyPI (pypiProject)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.Test.Contract.Install (InstallDriver (..), installsObserved)

-- | Drive one product image with both real clients and local stores.
mixedMountContract :: ConcreteFixture -> DeclaredGroup
mixedMountContract fixture = DeclaredGroup (fixtureGroup fixture) $ \recorder -> aroundAll (timedFixture recorder (withFixture fixture)) (scenarios recorder)

scenarios :: PhaseRecorder -> SpecWith GlobalDataPlane
scenarios recorder =
    aroundAllWith (timedFixtureWith recorder withE2E) $
        describe "one proxy, both mounts" $
            it (caseLabel "mixed/alternating-clients" "serves an npm install, a pip install, and a second npm install in turn") $ \e2e -> do
                npmInstallsAllowed e2e
                advertisedFiles e2e pypiProject >>= pipInstallsWheel e2e
                -- Mirror publication does not assert which leg serves the next install.
                verdaccioHasVersion e2e (psName allowPkg) (psVersion allowPkg) `shouldReturn` True
                npmInstallsAllowed e2e

-- A fresh project has an empty npm cache, so each call requests the metadata and the artifact.
npmInstallsAllowed :: E2E -> Expectation
npmInstallsAllowed e2e =
    withNpmProject e2e $ \proj -> do
        installsObserved
            InstallDriver
                { installRun = (,) <$> npmInstallIn proj (psName allowPkg) <*> installedVersion proj (psName allowPkg)
                , installExpected = Just (psVersion allowPkg)
                }
