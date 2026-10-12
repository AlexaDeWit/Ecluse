-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | First-party multipart publication through the product image and its native target.
module Ecluse.E2E.PyPI.PublishE2ESpec (pypiPublicationFamily, nativePublicationContract) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Client (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyBS), applyBasicAuth, httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types (statusCode)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((</>))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..), concreteFamily)
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfoFor)
import Ecluse.E2E.Fixtures.PyPI.Mirror
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.PyPIMirror
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Publication
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))

-- | Native boundary and disabled-route controls supplement the shared publication laws.
nativePublicationContract :: ConcreteFixture -> DeclaredGroup
nativePublicationContract fixture = DeclaredGroup (fixtureGroup fixture) $ \recorder ->
    around (timedFixture recorder (withFixture fixture)) $ do
        it (caseLabel "pypi/publish-native/conflicting-names" "refuses conflicting multipart names before contacting the target") $ \plane ->
            withPublicationFixture plane $ \target distributions ->
                withE2EWith
                    publicationConfig
                    ( \proxy -> do
                        distribution <- requiredDistribution "e2e-published" distributions
                        sendUpload proxy distribution [("name", "unowned-project")] `shouldReturn` 403
                        sendUpload proxy distribution [("name", "e2e-published"), ("name", "e2e-published-other")] `shouldReturn` 400
                        targetFiles target `shouldReturn` Map.empty
                        readTVarIO (ntRequests target) `shouldReturn` 0
                    )
                    plane
        it (caseLabel "pypi/publish-native/unconfigured" "refuses POST without a publication target") $ \plane ->
            withPublicationFixture plane $ \target distributions ->
                withE2E
                    ( \proxy -> do
                        distribution <- requiredDistribution "e2e-published" distributions
                        sendUpload proxy distribution [] `shouldReturn` 405
                        targetFiles target `shouldReturn` Map.empty
                        readTVarIO (ntRequests target) `shouldReturn` 0
                    )
                    plane

-- | Bind real multipart publication and a fresh native pip installation to the common contract.
pypiPublicationFamily :: ConcreteFixture -> FamilyFixture PublicationFixture
pypiPublicationFamily fixture = concreteFamily fixture $ \recorder action plane ->
    timedFixture recorder (withPublicationFixture plane . curry) $ \(target, distributions) ->
        timedFixture recorder (\use -> withE2EWith publicationConfig use plane) $ \proxy ->
            action
                ( PublicationFixture
                    mirrorVersion
                    (publicationDriver proxy target distributions "e2e-published")
                    (publicationDriver proxy target distributions "e2e-unowned")
                )

publicationDriver :: E2E -> NativeTarget -> [Distribution] -> Text -> PublicationDriver
publicationDriver proxy target distributions project =
    PublicationDriver
        { publicationRun = publishProject proxy [distribution | distribution <- distributions, distributionProject distribution == project]
        , publicationInstall = do
            let client = proxy{e2eClientEndpoints = Map.adjust (pipCredentialUrl "client-user" "client-password") PyPI (e2eClientEndpoints proxy)}
            (result, tree) <- pipInstallAndInspect client project mirrorVersion
            when (crExit result == ExitSuccess) $
                Map.member (toString (pypiDistInfoFor project mirrorVersion) <> "/METADATA") tree `shouldBe` True
            pure result
        , publicationVersions = nativeVersions target distributions project
        , publicationAwait = do
            awaitProjectFiles target distributions project `shouldReturn` True
            nativeVersions target distributions project
        , publicationSettle = awaitNativeIdle target
        }

requiredDistribution :: Text -> [Distribution] -> IO Distribution
requiredDistribution project = maybe (fail "publication fixture is empty") pure . find ((== project) . distributionProject)

publishProject :: E2E -> [Distribution] -> IO ClientResult
publishProject _ [] = fail "publication fixture is empty"
publishProject proxy (distribution : rest) = do
    result <- uploadClient proxy distribution []
    case (crExit result, rest) of
        (ExitSuccess, _ : _) -> publishProject proxy rest
        _ -> pure result

publicationConfig :: E2EConfig
publicationConfig =
    defaultE2EConfig
        { ecExtraEnv =
            [ ("ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl PyPIMirror)
            , ("ECLUSE_MOUNTS__PYPI__FIRST_PARTY", "e2e-published")
            , ("ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__URL", stubUrl PyPIMirror <> "legacy/")
            , ("ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__TOKEN", "fixture-password")
            , ("ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__USERNAME", "fixture-user")
            , ("ECLUSE_SERVER__AUTH_TOKEN", "client-password")
            ]
        }

withPublicationFixture :: GlobalDataPlane -> (NativeTarget -> [Distribution] -> IO a) -> IO a
withPublicationFixture plane action = do
    distributions <- buildNativeProjects (gdpWorkDir plane </> "pypi") ["e2e-published", "e2e-unowned"]
    withNativeTarget PreserveUploadFields distributions $ \target ->
        withStubFault plane (PyPIMirrorBackend (ntPort target) False) (action target distributions)

sendUpload :: E2E -> Distribution -> [(Text, Text)] -> IO Int
sendUpload proxy distribution extra = do
    result <- uploadClient proxy distribution extra
    maybe (fail "upload response omitted its status") pure (readMaybe (toString (crStdout result)))

uploadClient :: E2E -> Distribution -> [(Text, Text)] -> IO ClientResult
uploadClient proxy distribution extra = do
    base <- parseRequest (toString (e2eBaseUrl proxy <> "/pypi/legacy/"))
    let request =
            applyBasicAuth
                "client-user"
                "client-password"
                base
                    { method = "POST"
                    , requestHeaders = [("Content-Type", nativeUploadContentType)]
                    , requestBody = RequestBodyBS (distributionUploadBody distribution extra)
                    }
    response <- httpLbs request (e2eManager proxy)
    let status = statusCode (responseStatus response)
    pure
        ClientResult
            { crCommand = "POST " <> e2eBaseUrl proxy <> "/pypi/legacy/ " <> distributionFilename distribution
            , crExit = if status >= 200 && status < 300 then ExitSuccess else ExitFailure 1
            , crStdout = show status
            , crStderr = decodeUtf8 (responseBody response)
            }
