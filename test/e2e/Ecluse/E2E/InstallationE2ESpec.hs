-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native clients and wire readers for the shared installation scenarios.
module Ecluse.E2E.InstallationE2ESpec (installationFamily, npmInstallationDriver, pypiInstallationDriver, codedArtifact, probedArtifact) where

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
import Ecluse.Test.Contract.Install (ArtifactProbe (..), CodedArtifact (..), InstallDriver (..), InstallationDriver (..), UpstreamRead (..))
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
        , installationArtifact = (,) <$> advertised <*> proxyGetCoded e2e path
        , installationDigest = sriSha512Of
        , installationHead = probedArtifact e2e NpmPublic (npmArtifactPath name version) path
        , installationCodedArtifact = codedArtifact e2e NpmPublic (npmArtifactPath name version) path advertised
        }
  where
    name = psName allowPkg
    version = psVersion allowPkg
    path = npmTarballPath name version
    advertised = do
        (_, body) <- proxyGet e2e ("/npm/" <> name)
        value <- maybe (fail "npm returned no metadata JSON") pure (Aeson.decode body)
        maybe (fail "npm advertised no artifact integrity") pure (textAtPath ["versions", Key.fromText version, "dist", "integrity"] value)
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
        , installationArtifact = (,) <$> wheelDigest <*> proxyGetCoded e2e path
        , installationDigest = hexSha256Of
        , installationHead = probedArtifact e2e PyPIPublic upstreamPath path
        , installationCodedArtifact = codedArtifact e2e PyPIPublic upstreamPath path wheelDigest
        }
  where
    upstreamPath = "/simple/" <> pypiProject <> "/" <> pypiWheelFile
    path = "/pypi" <> upstreamPath
    observeRefusal project = (,) <$> pipInstalled project pypiDistInfo <*> pipInstalledTree project
    wheelDigest = do
        files <- advertisedFiles e2e pypiProject
        maybe (fail "PyPI advertised no wheel digest") pure (lookup pypiWheelFile files)
    install proxy digest observe = withPipProject proxy pypiProject pypiVersion digest $ \project ->
        (,) <$> pipInstallIn project <*> observe project

{- Read the artifact at @proxyPath@ and the public upstream's own log of that fetch, where the
artifact sits at @upstreamPath@. The route's earlier reads show whether it gzips on request.
-}
codedArtifact :: E2E -> StubRoute -> Text -> Text -> IO Text -> IO CodedArtifact
codedArtifact e2e route upstreamPath proxyPath advertised = do
    digest <- advertised
    (response, during) <- upstreamReadsDuring e2e route upstreamPath (proxyGetCoded e2e proxyPath)
    answered <- filter ((== route) . srRoute) <$> stubReadsNow (e2ePlane e2e)
    pure
        CodedArtifact
            { codedDigest = digest
            , codedResponse = response
            , codedArtifactReads = map upstreamRead during
            , codedRouteReads = map upstreamRead answered
            }
  where
    upstreamRead answer = UpstreamRead (srAcceptEncoding answer) (srContentEncoding answer)

-- | HEAD the artifact at @proxyPath@, keeping the methods of the upstream's reads of @upstreamPath@ meanwhile.
probedArtifact :: E2E -> StubRoute -> Text -> Text -> IO ArtifactProbe
probedArtifact e2e route upstreamPath proxyPath = do
    (response, during) <- upstreamReadsDuring e2e route upstreamPath (proxyHead e2e proxyPath)
    pure (ArtifactProbe response (map srMethod during))

-- Run a proxy request and collect the upstream's logged reads of one path that it caused.
upstreamReadsDuring :: E2E -> StubRoute -> Text -> IO a -> IO (a, [StubRead])
upstreamReadsDuring e2e route upstreamPath request = do
    seen <- length <$> stubReadsNow plane
    response <- request
    during <- awaitStubReads plane seen (any fetched)
    pure (response, filter fetched during)
  where
    plane = e2ePlane e2e
    fetched answer = srRoute answer == route && srPath answer == upstreamPath

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
