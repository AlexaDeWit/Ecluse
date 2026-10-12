-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI's canonicalised fixed-version spelling is a native advisory extension.
module Ecluse.E2E.PyPI.AdvisoriesE2ESpec (nativeAdvisoryContract) where

import Data.Aeson (object, (.=))
import Data.List (lookup)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.E2E.AdvisoriesE2ESpec
import Ecluse.E2E.Fixtures.Advisories (pypiAdvisoryExportBase)
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfoFor, pypiWheelFor)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Install (InstallDriver (..), installsObserved)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Types (Group)

-- | Retain the native fixed-version control alongside the common advisory scenarios.
nativeAdvisoryContract :: ConcreteFixture Group -> DeclaredGroup
nativeAdvisoryContract fixture = DeclaredGroup (fixtureMetadata fixture) $ \recorder ->
    aroundAll (timedFixture recorder (withFixture fixture)) $
        aroundAllWith (timedFixtureWith recorder (withPublishedAdvisories PyPI pypiAdvisoryExportBase)) $
            it (caseLabel "pypi/advisories/canonical-fix" "admits a canonicalised advisory fix and preserves refusal controls") $ \plane -> do
                withPolicy PyPI ["advisory-deny" .= object ["type" .= ("DenyIfCve" :: Text), "minCvss" .= (7.0 :: Double)]] plane $ \e2e ->
                    refusesPyPIRelease e2e "e2e-raw-fix" "1.18" "DenyIfCve" (Just "GHSA-e2e-raw-fix")
                withPolicy PyPI [] plane $ \e2e -> do
                    files <- advertisedFiles e2e "e2e-raw-fix"
                    case lookup (pypiWheelFor "e2e-raw-fix" "1.19") files of
                        Nothing -> expectationFailure "the canonicalised advisory fix was absent from the served index"
                        Just digest ->
                            withPipProject e2e "e2e-raw-fix" "1.19" digest $ \project ->
                                installsObserved (InstallDriver ((,) <$> pipInstallIn project <*> pipInstalled project (pypiDistInfoFor "e2e-raw-fix" "1.19")) True)
                withPolicy PyPI ["remediation-fast-track" .= object ["enabled" .= False]] plane $ \e2e -> do
                    files <- advertisedFiles e2e "e2e-raw-fix"
                    map fst files `shouldBe` [pypiWheelFor "e2e-raw-fix" "1.18"]
                    refusesPyPIRelease e2e "e2e-raw-fix" "1.19" "BlockedByDefault" Nothing
