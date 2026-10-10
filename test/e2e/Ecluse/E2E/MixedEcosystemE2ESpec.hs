-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Both mounts behind one proxy, driven by a real @npm@ client and a real @pip@ client in turn.
The modules under an ecosystem directory each send their proxies one ecosystem's traffic.
-}
module Ecluse.E2E.MixedEcosystemE2ESpec (spec) where

import Data.List (lookup)

import Test.Hspec

import Ecluse.E2E.Fixtures.Npm (allowPkg, psName, psVersion)
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfo, pypiProject, pypiVersion, pypiWheelFile)
import Ecluse.E2E.Harness

-- | Drive one product image with both real clients and local stores.
spec :: Spec
spec = whenE2EAvailable (aroundAll withGlobalDataPlane scenarios)

scenarios :: SpecWith GlobalDataPlane
scenarios =
    aroundAllWith withE2E $
        describe "one proxy, both mounts" $
            it "serves an npm install, a pip install, and a second npm install in turn" $ \e2e -> do
                npmInstallsAllowed e2e
                pipInstallsWheel e2e
                -- The worker mirrors the first install. Waiting for it fixes the leg the second
                -- npm install reads, which a race with the worker would otherwise decide.
                verdaccioHasVersion e2e (psName allowPkg) (psVersion allowPkg) `shouldReturn` True
                npmInstallsAllowed e2e

-- A fresh project has an empty npm cache, so each call requests the metadata and the artifact.
npmInstallsAllowed :: E2E -> Expectation
npmInstallsAllowed e2e =
    withNpmProject e2e $ \proj -> do
        void $ npmInstallIn proj (psName allowPkg) >>= shouldSucceedThroughProxy e2e
        installedVersion proj (psName allowPkg) `shouldReturn` Just (psVersion allowPkg)

-- pip accepts the wheel only under the digest the mount's own index advertised for it.
pipInstallsWheel :: E2E -> Expectation
pipInstallsWheel e2e = do
    advertised <- advertisedFiles e2e pypiProject
    case lookup pypiWheelFile advertised of
        Nothing -> expectationFailure ("the served index advertised " <> show (map fst advertised) <> ", not the wheel")
        Just digest ->
            withPipProject e2e pypiProject pypiVersion digest $ \proj -> do
                void $ pipInstallIn proj >>= shouldSucceedThroughProxy e2e
                pipInstalled proj pypiDistInfo `shouldReturn` True
