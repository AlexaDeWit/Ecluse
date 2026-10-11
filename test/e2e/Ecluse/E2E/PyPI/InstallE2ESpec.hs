-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The pypi mount on the base topology, driven by a real @pip@ client in hash-checking mode. The
mount has no private store, so the same install fails through a second proxy while public is down.
-}
module Ecluse.E2E.PyPI.InstallE2ESpec (spec) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Test.Hspec

import Ecluse.E2E.Fixtures.PyPI (pypiProject, pypiSdistFile, pypiVersion, pypiWheelFile, pypiYankReason, pypiYankedDistInfo, pypiYankedVersion, pypiYankedWheelFile)
import Ecluse.E2E.Harness
import Ecluse.Test.Package (hexSha256Of)

-- | Drive the product image with a real pip client and local stores.
spec :: Spec
spec = whenE2EAvailable (aroundAll withGlobalDataPlane scenarios)

scenarios :: SpecWith GlobalDataPlane
scenarios =
    aroundAllWith withE2E $
        describe "pypi surface -- a real pip install" $ do
            it "installs the compatible non-yanked wheel with its advertised hash and no metadata sidecar" $ \e2e -> do
                advertised <- advertisedFiles e2e pypiProject
                length advertised `shouldBe` 5
                pipInstallsWheel e2e advertised

            it "relays the yank reason when pip installs an exact pin" $ \e2e -> do
                advertised <- advertisedFiles e2e pypiProject
                case lookup pypiYankedWheelFile advertised of
                    Just digest ->
                        withPipProject e2e pypiProject pypiYankedVersion digest $ \proj -> do
                            void $ pipInstallIn proj >>= shouldSucceedThroughProxy e2e >>= shouldWarn ("Reason for being yanked: " <> pypiYankReason)
                            pipInstalled proj pypiYankedDistInfo `shouldReturn` True
                    Nothing -> expectationFailure "the served index advertised no digest for the yanked wheel"

            it "rejects the yank warning assertion for a release without the member" $ \e2e -> do
                advertised <- advertisedFiles e2e pypiProject
                case lookup pypiWheelFile advertised of
                    Just digest ->
                        withPipProject e2e pypiProject pypiVersion digest $ \proj -> do
                            installed <- pipInstallIn proj >>= shouldSucceedThroughProxy e2e
                            shouldWarn ("Reason for being yanked: " <> pypiYankReason) installed `shouldThrow` anyException
                    Nothing -> expectationFailure "the served index advertised no digest for the control wheel"

            it "serves the source distribution beside the wheel under the shipped policy" $ \e2e -> do
                advertised <- advertisedFiles e2e pypiProject
                case lookup pypiSdistFile advertised of
                    Nothing -> expectationFailure "the served index advertised no digest for the source distribution"
                    Just digest -> do
                        (status, body) <- proxyGet e2e ("/pypi/simple/" <> pypiProject <> "/" <> pypiSdistFile)
                        status `shouldBe` 200
                        hexSha256Of (LBS.toStrict body) `shouldBe` digest

            it "fails the same install through a second proxy while both public upstreams are down" $ \e2e -> do
                advertised <- advertisedFiles e2e pypiProject
                case lookup pypiWheelFile advertised of
                    Nothing -> expectationFailure "the served index advertised no digest for the wheel"
                    Just digest -> do
                        let plane = e2ePlane e2e
                        withPublicUpstreamsDown plane . flip withE2E plane $ \offline -> do
                            seen <- length <$> stubReadsNow plane
                            void $ withPipProject offline pypiProject pypiVersion digest pipInstallIn >>= shouldFail
                            answered <- awaitStubReads plane seen (any indexLeftUnanswered)
                            answered `shouldSatisfy` any indexLeftUnanswered
                            filter answeredByPublic answered `shouldBe` []

-- The proxy's read of the project's index, which the public route took and closed with no answer.
indexLeftUnanswered :: StubRead -> Bool
indexLeftUnanswered r =
    srRoute r == PyPIPublic && srPath r == "/simple/" <> pypiProject <> "/" && not (answeredByPublic r)
