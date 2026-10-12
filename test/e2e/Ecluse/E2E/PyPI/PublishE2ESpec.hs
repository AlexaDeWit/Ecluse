-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | First-party multipart publication through the product image and its native target.
module Ecluse.E2E.PyPI.PublishE2ESpec (spec) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyBS), applyBasicAuth, httpLbs, parseRequest, responseStatus)
import Network.HTTP.Types (statusCode)
import System.FilePath ((</>))
import Test.Hspec

import Ecluse.E2E.Fixtures.PyPI.Mirror
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.PyPIMirror
import Ecluse.Test.Package (hexSha256Of)

-- | Exercise the live pathless route, ownership guard, credential pair, and disabled control.
spec :: Spec
spec = whenE2EAvailable . around withGlobalDataPlane $
    describe "PyPI first-party publication" $ do
        it "relays multipart bytes to the exact endpoint and installs the publication privately" $ \plane ->
            withPublicationFixture plane $ \target distributions ->
                withE2EWith
                    publicationConfig
                    ( \proxy -> do
                        for_ distributions $ \distribution -> sendUpload proxy distribution [] `shouldReturn` 200
                        targetFiles target `shouldReturn` Map.fromList [(distributionFilename d, distributionBytes d) | d <- distributions]
                        tree <- pipInstallGraph proxy "e2e-published" mirrorVersion
                        Map.member "e2e_published-1.0.0.dist-info/METADATA" tree `shouldBe` True
                    )
                    plane
        it "refuses an unowned or conflicting project before the target receives a body" $ \plane ->
            withPublicationFixture plane $ \target distributions ->
                withE2EWith
                    publicationConfig
                    ( \proxy -> do
                        distribution <- maybe (fail "publication fixture is empty") pure (listToMaybe distributions)
                        sendUpload proxy distribution [("name", "unowned-project")] `shouldReturn` 403
                        sendUpload proxy distribution [("name", "e2e-published"), ("name", "e2e-published-other")] `shouldReturn` 400
                        targetFiles target `shouldReturn` Map.empty
                        readTVarIO (ntRequests target) `shouldReturn` 0
                    )
                    plane
        it "refuses POST when no publication target is configured" $ \plane ->
            withPublicationFixture plane $ \target distributions ->
                withE2E
                    ( \proxy -> do
                        distribution <- maybe (fail "publication fixture is empty") pure (listToMaybe distributions)
                        sendUpload proxy distribution [] `shouldReturn` 405
                        targetFiles target `shouldReturn` Map.empty
                        readTVarIO (ntRequests target) `shouldReturn` 0
                    )
                    plane

publicationConfig :: E2EConfig
publicationConfig =
    defaultE2EConfig
        { ecExtraEnv =
            [ ("ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl PyPIMirror)
            , ("ECLUSE_MOUNTS__PYPI__FIRST_PARTY", "e2e-published")
            , ("ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__URL", stubUrl PyPIMirror <> "legacy/")
            , ("ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__TOKEN", "fixture-password")
            , ("ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__REGISTRY__USERNAME", "fixture-user")
            ]
        }

withPublicationFixture :: GlobalDataPlane -> (NativeTarget -> [Distribution] -> IO a) -> IO a
withPublicationFixture plane action = do
    distributions <- buildPublication (gdpWorkDir plane </> "pypi")
    withNativeTarget PreserveUploadFields distributions $ \target ->
        withStubFault plane (PyPIMirrorBackend (ntPort target) False) (action target distributions)

sendUpload :: E2E -> Distribution -> [(Text, Text)] -> IO Int
sendUpload proxy distribution extra = do
    base <- parseRequest (toString (e2eBaseUrl proxy <> "/pypi/"))
    let request =
            applyBasicAuth
                "client-user"
                "client-password"
                base
                    { method = "POST"
                    , requestHeaders = [("Content-Type", "multipart/form-data; boundary=fixture-upload")]
                    , requestBody = RequestBodyBS (uploadBody distribution extra)
                    }
    statusCode . responseStatus <$> httpLbs request (e2eManager proxy)

uploadBody :: Distribution -> [(Text, Text)] -> ByteString
uploadBody distribution extra =
    BS.concat (map fieldPart (fields <> if null extra then [("name", distributionProject distribution)] else extra))
        <> "--fixture-upload\r\nContent-Disposition: form-data; name=\"content\"; filename=\""
        <> encodeUtf8 (distributionFilename distribution)
        <> "\"\r\nContent-Type: application/octet-stream\r\n\r\n"
        <> distributionBytes distribution
        <> "\r\n--fixture-upload--\r\n"
  where
    fields =
        [ (":action", "file_upload")
        , ("protocol_version", "1")
        , ("version", mirrorVersion)
        , ("metadata_version", "2.2")
        , ("provides_extra", "test")
        , ("filetype", if ".whl" `T.isSuffixOf` distributionFilename distribution then "bdist_wheel" else "sdist")
        , ("pyversion", if ".whl" `T.isSuffixOf` distributionFilename distribution then "py3" else "source")
        , ("sha256_digest", hexSha256Of (distributionBytes distribution))
        , ("requires_python", ">=3.8")
        , ("classifiers", "Programming Language :: Python :: 3")
        , ("classifiers", "License :: OSI Approved :: MIT License")
        , ("description", "Fixture description with a second line.\nRetain this body verbatim.\n")
        ]
            <> [("summary", value) | line <- T.lines (decodeUtf8 (distributionMetadata distribution)), Just value <- [T.stripPrefix "Summary: " line]]
    fieldPart (name, value) = "--fixture-upload\r\nContent-Disposition: form-data; name=\"" <> encodeUtf8 name <> "\"\r\n\r\n" <> encodeUtf8 value <> "\r\n"
