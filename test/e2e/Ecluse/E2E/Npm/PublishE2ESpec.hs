-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | First-party publication through the npm mount with a publication target configured. The refusal
with no target configured is a base-topology case in "Ecluse.E2E.Npm.InstallE2ESpec".
-}
module Ecluse.E2E.Npm.PublishE2ESpec (npmPublicationFamily) where

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture, concreteFamily)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture, FamilyMetadata)

import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.Test.Contract.Publication

-- | Supply accepted and refused native publication operations to the common scenarios.
npmPublicationFamily :: ConcreteFixture FamilyMetadata -> FamilyFixture PublicationFixture
npmPublicationFamily fixture = concreteFamily fixture $ \recorder action ->
    timedFixtureWith recorder (withE2EWith defaultE2EConfig{ecExtraEnv = publishTargetEnv}) $ \e2e ->
        action (PublicationFixture publishVersion (publicationDriver e2e publishInScopeName) (publicationDriver e2e publishOutOfScopeName))

publicationDriver :: E2E -> Text -> PublicationDriver
publicationDriver e2e name =
    PublicationDriver
        { publicationRun = withPublishProject e2e name publishVersion npmPublishIn
        , publicationInstall = npmInstall e2e name
        , publicationVersions = verdaccioVersions e2e name
        , publicationAwait = verdaccioAwaitVersions e2e name [publishVersion]
        , publicationSettle = awaitMirrorWindow
        }
