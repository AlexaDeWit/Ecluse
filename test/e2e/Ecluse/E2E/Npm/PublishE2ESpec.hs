-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | First-party publication through the npm mount with a publication target configured. The
refusal with no target configured is a base-topology case in "Ecluse.E2E.Npm.InstallE2ESpec".
-}
module Ecluse.E2E.Npm.PublishE2ESpec (spec) where

import Test.Hspec

import Ecluse.E2E.Harness

-- | Publish with a real npm client and read the publication target back.
spec :: Spec
spec = whenE2EAvailable (aroundAll withGlobalDataPlane scenarios)

scenarios :: SpecWith GlobalDataPlane
scenarios = do
    describe "first-party publish -- publication target enabled" $
        aroundAllWith (withE2EWith defaultE2EConfig{ecExtraEnv = publishTargetEnv}) $ do
            it "publishes an in-scope package, then installs it back through the private leg" $ \e2e -> do
                let name = publishInScopeName
                    ver = publishVersion
                -- The guard admits an in-scope publish and the relay forwards it to the
                -- publication target (Verdaccio)...
                void $ withPublishProject e2e name ver npmPublishIn >>= shouldSucceed
                onTarget <- verdaccioHasVersion e2e name ver
                onTarget `shouldBe` True
                -- ...and readable back: the proxy serves it over the private leg, so a fresh
                -- install through the proxy succeeds.
                void $ npmInstall e2e name >>= shouldSucceed

            it "refuses an out-of-scope publish before any upstream write (anti-shadowing guard)" $ \e2e -> do
                let name = publishOutOfScopeName
                    ver = publishVersion
                -- Precondition: no other case publishes this out-of-scope name, so the absence
                -- below is attributable to the refusal, not to stale state.
                absentBefore <- verdaccioHasVersionNow e2e name ver
                absentBefore `shouldBe` False
                withPublishProject e2e name ver $ \proj -> do
                    void $ npmPublishIn proj >>= shouldFail
                    reached <- verdaccioMirroredWithinWindow e2e name ver
                    reached `shouldBe` False
