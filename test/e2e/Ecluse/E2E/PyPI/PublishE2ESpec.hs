-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | First-party multipart publication through the product image and its native target.
module Ecluse.E2E.PyPI.PublishE2ESpec (pypiPublicationFamily, nativePublicationContract) where

import Data.Map.Strict qualified as Map
import Network.HTTP.Client (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyBS), applyBasicAuth, httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types (statusCode)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((</>))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Registry.PyPI.Publish.Multipart (pypiFileUploadOnly)
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..), concreteFamily)
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfoFor)
import Ecluse.E2E.Fixtures.PyPI.Mirror
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.PyPIMirror
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture, FamilyMetadata)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Publication
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Types (Group)
import Ecluse.Test.Package (hexSha256Of)

-- | Native boundary and disabled-route controls supplement the shared publication laws.
nativePublicationContract :: ConcreteFixture Group -> DeclaredGroup
nativePublicationContract fixture = DeclaredGroup (fixtureMetadata fixture) $ \recorder ->
    around (timedFixture recorder (withFixture fixture)) $ do
        it (caseLabel "pypi/publish-native/conflicting-names" "refuses conflicting multipart names before contacting the target") $ \plane ->
            withPublicationFixture plane $ \target distributions ->
                withE2EWith
                    publicationConfig
                    ( \proxy -> do
                        distribution <- requiredDistribution "e2e-published" distributions
                        unowned <- requiredDistribution "e2e-unowned" distributions
                        sendUpload proxy unowned [] `shouldReturn` 403
                        sendUpload proxy unowned [("name", "e2e-published")] `shouldReturn` 400
                        sendUpload proxy distribution [("name", "e2e-published"), ("name", "e2e-published-other")] `shouldReturn` 400
                        targetFiles target `shouldReturn` Map.empty
                        readTVarIO (ntRequests target) `shouldReturn` 0
                    )
                    plane
        it (caseLabel "pypi/publish-native/hidden-parts" "refuses a content part hidden after a bare LF, a bare CR or a padded delimiter before contacting the target") $ \plane ->
            withPublicationFixture plane $ \target distributions ->
                withE2EWith
                    publicationConfig
                    ( \proxy -> do
                        carrier <- requiredDistribution "e2e-published" distributions
                        hidden <- requiredDistribution "e2e-unowned" distributions
                        for_ [("\n", ""), ("\r", ""), ("\r\n", "  ")] $ \(lineEnd, padding) -> do
                            result <- postUpload proxy ("hidden " <> distributionFilename hidden) (hiddenPartUploadBody carrier hidden lineEnd padding)
                            crStdout result `shouldBe` "400"
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
pypiPublicationFamily :: ConcreteFixture FamilyMetadata -> FamilyFixture PublicationFixture
pypiPublicationFamily fixture = concreteFamily fixture $ \recorder action plane ->
    timedFixture recorder (withPublicationFixture plane . curry) $ \(target, distributions) ->
        timedFixture recorder (\use -> withE2EWith publicationConfig use plane) $ \proxy ->
            action
                ( PublicationFixture
                    mirrorVersion
                    (publicationDriver proxy target distributions "e2e-published")
                    (publicationDriver proxy target distributions "e2e-unowned")
                    (removalDriver proxy target distributions)
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

-- No PyPI client sends a removal, but pypiserver deletes the named release on a raw remove_pkg.
removalDriver :: E2E -> NativeTarget -> [Distribution] -> NonUploadDriver
removalDriver proxy target distributions =
    NonUploadDriver
        { nonUploadRelease = do
            published <- publishProject proxy [distribution | distribution <- distributions, distributionProject distribution == retainedProject]
            when (crExit published == ExitSuccess) (awaitProjectFiles target distributions retainedProject `shouldReturn` True)
            pure published
        , nonUploadWrite = postUpload proxy ("remove_pkg " <> retainedProject) (releaseRemovalBody retainedProject mirrorVersion)
        , nonUploadRefusal = ["501", pypiFileUploadOnly]
        , nonUploadDestination = do
            files <- targetFiles target
            uploads <- readTVarIO (ntRequests target)
            pure (["file " <> name <> " " <> hexSha256Of bytes | (name, bytes) <- Map.toList files] <> ["uploads received " <> show uploads])
        , nonUploadSettle = awaitNativeIdle target
        }

-- A first-party project whose release a removal through the proxy must leave in place.
retainedProject :: Text
retainedProject = "e2e-retained"

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
            , ("ECLUSE_MOUNTS__PYPI__FIRST_PARTY", "e2e-published," <> retainedProject)
            , ("ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__URL", stubUrl PyPIMirror)
            , ("ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__UPLOAD_URL", stubUrl PyPIMirror <> "legacy/")
            , ("ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__TOKEN", "fixture-password")
            , ("ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__USERNAME", "fixture-user")
            , ("ECLUSE_SERVER__AUTH_TOKEN", "client-password")
            ]
        }

withPublicationFixture :: GlobalDataPlane -> (NativeTarget -> [Distribution] -> IO a) -> IO a
withPublicationFixture plane action = do
    distributions <- filter builtByCurrentTool <$> buildNativeProjects (gdpWorkDir plane </> "pypi") ["e2e-published", "e2e-unowned", retainedProject]
    withNativeTarget PreserveUploadFields distributions $ \target ->
        withStubFault plane (PyPIMirrorBackend (ntPort target) False) (action target distributions)

sendUpload :: E2E -> Distribution -> [(Text, Text)] -> IO Int
sendUpload proxy distribution extra = do
    result <- uploadClient proxy distribution extra
    maybe (fail "upload response omitted its status") pure (readMaybe (toString (crStdout result)))

uploadClient :: E2E -> Distribution -> [(Text, Text)] -> IO ClientResult
uploadClient proxy distribution extra = postUpload proxy (distributionFilename distribution) (distributionUploadBody distribution extra)

-- POST a multipart form to the upload route with the edge credential, as a raw client.
postUpload :: E2E -> Text -> ByteString -> IO ClientResult
postUpload proxy label body = do
    base <- parseRequest (toString (e2eBaseUrl proxy <> "/pypi/legacy/"))
    let request =
            applyBasicAuth
                "client-user"
                "client-password"
                base
                    { method = "POST"
                    , requestHeaders = [("Content-Type", nativeUploadContentType)]
                    , requestBody = RequestBodyBS body
                    }
    response <- httpLbs request (e2eManager proxy)
    let status = statusCode (responseStatus response)
    pure
        ClientResult
            { crCommand = "POST " <> e2eBaseUrl proxy <> "/pypi/legacy/ " <> label
            , crExit = if status >= 200 && status < 300 then ExitSuccess else ExitFailure 1
            , crStdout = show status
            , crStderr = decodeUtf8 (responseBody response)
            }
