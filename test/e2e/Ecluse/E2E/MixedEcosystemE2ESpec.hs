-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Three native clients use one proxy and preserve their separate registry mounts.
module Ecluse.E2E.MixedEcosystemE2ESpec (mixedMountContract) where

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Phase (PhaseRecorder, timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Types (Group)
import Test.Hspec

import Ecluse.E2E.Cargo.InstallE2ESpec (cargoConsumer)
import Ecluse.E2E.Fixtures.Npm (allowPkg, psName, psVersion)
import Ecluse.E2E.Fixtures.PyPI (pypiProject)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Cargo (currentCargo, runCargo)
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.Test.Contract.Install (InstallDriver (..), installsObserved)

-- | Drive one product image with npm, pip and Cargo clients and their declared stores.
mixedMountContract :: ConcreteFixture Group -> DeclaredGroup
mixedMountContract fixture = DeclaredGroup (fixtureMetadata fixture) $ \recorder -> aroundAll (timedFixture recorder (withFixture fixture)) (scenarios recorder)

scenarios :: PhaseRecorder -> SpecWith GlobalDataPlane
scenarios recorder =
    aroundAllWith (timedFixtureWith recorder withE2E) $
        describe "one proxy, three mounts" $
            it (caseLabel "mixed/alternating-clients" "serves npm, pip and Cargo installs before a second npm install") $ \e2e -> do
                npmInstallsAllowed e2e
                advertisedFiles e2e pypiProject >>= pipInstallsWheel e2e
                cargoConsumer e2e currentCargo $ \project -> void (runCargo project ["build"] >>= shouldSucceed)
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
