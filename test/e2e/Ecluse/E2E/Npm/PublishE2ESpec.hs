-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | First-party publication through the npm mount with a publication target configured. The refusal
with no target configured is a base-topology case in "Ecluse.E2E.Npm.InstallE2ESpec".
-}
module Ecluse.E2E.Npm.PublishE2ESpec (specWith) where

import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Phase (PhaseRecorder, timedFixture)
import Test.Hspec

import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.Test.Contract.Publication

-- | Publish with a real npm client and read the publication target back.
specWith :: PhaseRecorder -> Spec
specWith recorder = aroundAll (timedFixture recorder withGlobalDataPlane) (scenarios recorder)

scenarios :: PhaseRecorder -> SpecWith GlobalDataPlane
scenarios recorder = do
    describe "first-party publish -- publication target enabled" $
        aroundAllWith (timedFixtureWith recorder (withE2EWith defaultE2EConfig{ecExtraEnv = publishTargetEnv})) $ do
            it (caseLabel "npm/publish/publish-install" "publishes an in-scope package, then installs it back through the private leg") $ \e2e -> do
                publishesAndInstalls publishVersion (publicationDriver e2e publishInScopeName)

            it (caseLabel "npm/publish/scope-refusal" "refuses an out-of-scope publish before any upstream write (anti-shadowing guard)") $ \e2e -> do
                refusesWithoutPublication publishVersion (publicationDriver e2e publishOutOfScopeName)

publicationDriver :: E2E -> Text -> PublicationDriver
publicationDriver e2e name =
    PublicationDriver
        { publicationRun = withPublishProject e2e name publishVersion npmPublishIn
        , publicationInstall = npmInstall e2e name
        , publicationVersions = verdaccioVersions e2e name
        , publicationAwait = verdaccioAwaitVersions e2e name [publishVersion]
        , publicationSettle = awaitMirrorWindow
        }
