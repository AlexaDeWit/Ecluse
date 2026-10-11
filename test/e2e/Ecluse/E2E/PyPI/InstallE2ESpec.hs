-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The pypi mount on the base topology, driven by a real @pip@ client in hash-checking mode. The mount has
no private store, so the same install fails through a second proxy while public is down.
-}
module Ecluse.E2E.PyPI.InstallE2ESpec (nativeInstallationContract) where

import Data.List (lookup)
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Phase (PhaseRecorder, timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Test.Hspec

import Ecluse.E2E.Fixtures.PyPI (pypiProject, pypiSdistFile, pypiVersion, pypiWheelFile, pypiYankReason, pypiYankedDistInfo, pypiYankedVersion, pypiYankedWheelFile)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.Test.Contract.Install.Outage
import Ecluse.Test.Contract.Serving (ServingResponse (..), artifactDigestCase)
import Ecluse.Test.Package (hexSha256Of)

-- | Drive the product image with a real pip client and local stores.
nativeInstallationContract :: ConcreteFixture -> DeclaredGroup
nativeInstallationContract fixture = DeclaredGroup (fixtureGroup fixture) $ \recorder -> aroundAll (timedFixture recorder (withFixture fixture)) (scenarios recorder)

scenarios :: PhaseRecorder -> SpecWith GlobalDataPlane
scenarios recorder =
    aroundAllWith (timedFixtureWith recorder withE2E) $
        describe "pypi surface -- a real pip install" $ do
            it (caseLabel "pypi/install/wheel" "installs the compatible non-yanked wheel with its advertised hash") $ \e2e -> do
                advertised <- advertisedFiles e2e pypiProject
                length advertised `shouldBe` 5
                pipInstallsWheel e2e advertised

            it (caseLabel "pypi/install/metadata-companion" "resolves through the advertised metadata companion before the hash-checked install") $ \e2e -> do
                advertised <- advertisedFiles e2e pypiProject
                case lookup pypiWheelFile advertised of
                    Nothing -> expectationFailure "the served index advertised no digest for the wheel"
                    Just digest -> do
                        let plane = e2ePlane e2e
                            companion readEvent = srRoute readEvent == PyPIPublic && srPath readEvent == "/simple/" <> pypiProject <> "/" <> pypiWheelFile <> ".metadata" && answeredByPublic readEvent
                        seen <- length <$> stubReadsNow plane
                        withPipProject e2e pypiProject pypiVersion digest $ \proj ->
                            void $ pipResolveIn proj pypiProject pypiVersion >>= shouldSucceedThroughProxy e2e
                        observedReads <- awaitStubReads plane seen (any companion)
                        observedReads `shouldSatisfy` any companion
                        pipInstallsWheel e2e advertised

            it (caseLabel "pypi/install/yanked-pin" "relays the yank reason when pip installs an exact pin") $ \e2e -> do
                advertised <- advertisedFiles e2e pypiProject
                case lookup pypiYankedWheelFile advertised of
                    Just digest ->
                        withPipProject e2e pypiProject pypiYankedVersion digest $ \proj -> do
                            void $ pipInstallIn proj >>= shouldSucceedThroughProxy e2e >>= shouldWarn ("Reason for being yanked: " <> pypiYankReason)
                            pipInstalled proj pypiYankedDistInfo `shouldReturn` True
                    Nothing -> expectationFailure "the served index advertised no digest for the yanked wheel"

            it (caseLabel "pypi/install/yank-negative-control" "rejects the yank warning assertion for a release without the member") $ \e2e -> do
                advertised <- advertisedFiles e2e pypiProject
                case lookup pypiWheelFile advertised of
                    Just digest ->
                        withPipProject e2e pypiProject pypiVersion digest $ \proj -> do
                            installed <- pipInstallIn proj >>= shouldSucceedThroughProxy e2e
                            shouldWarn ("Reason for being yanked: " <> pypiYankReason) installed `shouldThrow` anyException
                    Nothing -> expectationFailure "the served index advertised no digest for the control wheel"

            it (caseLabel "pypi/install/sdist" "serves the source distribution beside the wheel under the shipped policy") $ \e2e -> do
                advertised <- advertisedFiles e2e pypiProject
                case lookup pypiSdistFile advertised of
                    Nothing -> expectationFailure "the served index advertised no digest for the source distribution"
                    Just digest -> do
                        (status, body) <- proxyGet e2e ("/pypi/simple/" <> pypiProject <> "/" <> pypiSdistFile)
                        artifactDigestCase hexSha256Of digest (ServingResponse status body [] [] mempty)

            it (caseLabel "pypi/install/outage" "fails the same install through a second proxy while both public upstreams are down") $ \e2e -> do
                advertised <- advertisedFiles e2e pypiProject
                case lookup pypiWheelFile advertised of
                    Nothing -> expectationFailure "the served index advertised no digest for the wheel"
                    Just digest -> do
                        let plane = e2ePlane e2e
                        outageRefuses
                            OutageDriver
                                { outageExpectedClosed = "/simple/" <> pypiProject <> "/"
                                , outageRun = withPublicUpstreamsDown plane . flip withE2E plane $ \offline -> do
                                    seen <- length <$> stubReadsNow plane
                                    result <- withPipProject offline pypiProject pypiVersion digest pipInstallIn
                                    answered <- awaitStubReads plane seen (any indexLeftUnanswered)
                                    pure (result, map srPath (filter indexLeftUnanswered answered), map srPath (filter answeredByPublic answered))
                                }

-- The proxy's read of the project's index, which the public route took and closed with no answer.
indexLeftUnanswered :: StubRead -> Bool
indexLeftUnanswered r =
    srRoute r == PyPIPublic && srPath r == "/simple/" <> pypiProject <> "/" && not (answeredByPublic r)
