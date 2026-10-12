-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native clients and wire readers for the shared installation scenarios.
module Ecluse.E2E.InstallationE2ESpec (installationFamily, npmInstallationDriver, pypiInstallationDriver) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.List (lookup)
import Data.Text qualified as T

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture, concreteFamily)
import Ecluse.E2E.Fixtures.Npm (allowPkg, psName, psVersion)
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfo, pypiProject, pypiVersion, pypiWheelFile)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture, FamilyMetadata)
import Ecluse.Test.Contract.Install (InstallDriver (..), InstallationDriver (..))
import Ecluse.Test.Contract.Serving (ServingResponse (..))
import Ecluse.Test.Json (textAtPath)
import Ecluse.Test.Package (hexSha256Of, sriSha512Of)

-- | Keep resource acquisition outside the ecosystem-neutral scenario body.
installationFamily :: ConcreteFixture FamilyMetadata -> (E2E -> InstallationDriver observation) -> FamilyFixture (InstallationDriver observation)
installationFamily fixture driver = concreteFamily fixture $ \recorder action ->
    timedFixtureWith recorder withE2E (action . driver)

-- | Observe npm's installed version and independently read its advertised SRI.
npmInstallationDriver :: E2E -> InstallationDriver (Maybe Text)
npmInstallationDriver e2e =
    InstallationDriver
        { installationAccepted = InstallDriver (install e2e (`installedVersion` name)) (Just version)
        , installationDenied =
            InstallDriver
                (withE2EWith (deniedConfig "npm") (`install` observeRefusal) (e2ePlane e2e))
                (Nothing, mempty)
        , installationArtifact = do
            (_, body) <- proxyGet e2e ("/npm/" <> name)
            value <- maybe (fail "npm returned no metadata JSON") pure (Aeson.decode body)
            digest <- maybe (fail "npm advertised no artifact integrity") pure (textAtPath ["versions", Key.fromText version, "dist", "integrity"] value)
            response <- artifactGet e2e path
            pure (digest, response)
        , installationDigest = sriSha512Of
        , installationHead = proxyHead e2e path
        }
  where
    name = psName allowPkg
    version = psVersion allowPkg
    path = npmTarballPath name version
    observeRefusal project =
        (,) <$> installedVersion project name <*> snapshotTree (npDir project) ["node_modules", "package-lock.json"]
    install proxy observe = withNpmProject proxy $ \project ->
        (,) <$> npmInstallIn project (name <> "@" <> version) <*> observe project

-- | Observe pip's installed distribution and independently read its advertised SHA-256.
pypiInstallationDriver :: E2E -> InstallationDriver Bool
pypiInstallationDriver e2e =
    InstallationDriver
        { installationAccepted = InstallDriver (wheelDigest >>= \digest -> install e2e digest (`pipInstalled` pypiDistInfo)) True
        , installationDenied =
            InstallDriver
                ( do
                    digest <- wheelDigest
                    withE2EWith (deniedConfig "pypi") (\proxy -> install proxy digest observeRefusal) (e2ePlane e2e)
                )
                (False, mempty)
        , installationArtifact = (,) <$> wheelDigest <*> artifactGet e2e path
        , installationDigest = hexSha256Of
        , installationHead = proxyHead e2e path
        }
  where
    path = "/pypi/simple/" <> pypiProject <> "/" <> pypiWheelFile
    observeRefusal project = (,) <$> pipInstalled project pypiDistInfo <*> pipInstalledTree project
    wheelDigest = do
        files <- advertisedFiles e2e pypiProject
        maybe (fail "PyPI advertised no wheel digest") pure (lookup pypiWheelFile files)
    install proxy digest observe = withPipProject proxy pypiProject pypiVersion digest $ \project ->
        (,) <$> pipInstallIn project <*> observe project

artifactGet :: E2E -> Text -> IO ServingResponse
artifactGet e2e path = do
    (status, body) <- proxyGet e2e path
    pure (ServingResponse status body [] [] mempty)

deniedConfig :: Text -> E2EConfig
deniedConfig ecosystem =
    defaultE2EConfig
        { ecExtraEnv =
            [
                ( "ECLUSE_MOUNTS__" <> T.toUpper ecosystem <> "__RULES"
                , "{\"min-age\":{\"enabled\":false},\"remediation-fast-track\":{\"enabled\":false}}"
                )
            ]
        }
