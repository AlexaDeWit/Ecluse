-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | A native multipart target whose stored bytes and faults remain observable across product restarts.
module Ecluse.E2E.Harness.PyPIMirror (
    NativeTarget (..),
    UploadFault (..),
    TargetFields (..),
    withNativeTarget,
    targetFiles,
    setUploadFault,
    awaitTargetFiles,
    nativeMirrorConfig,
    withMirrorFixture,
) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Types (status200, status400, status404, status503)
import Network.Wai (Application, pathInfo, requestHeaders, requestMethod, responseLBS, responseRaw)
import Network.Wai.Handler.Warp (defaultSettings, setHost, testWithApplicationSettings)
import Network.Wai.Parse (FileInfo (fileContent, fileName), lbsBackEnd, parseRequestBody)

import Ecluse.E2E.Fixtures.PyPI.Mirror
import Ecluse.E2E.Harness.Advisories (advisoryBucket, createAdvisoryBucket)
import Ecluse.E2E.Harness.Docker (withStubFault)
import Ecluse.E2E.Harness.Stub (StubFault (PyPIMirrorBackend), StubRoute (PyPIMirror, PyPIPublic), stubUrl)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Poll (pollUntil)
import System.FilePath ((</>))

-- | Native targets can preserve upload fields or lose the richer public source fields.
data TargetFields = PreserveUploadFields | ChangeSourceFields
    deriving stock (Eq, Show)

-- | Fail before storing a sibling or lose a reply after the target commits it.
data UploadFault = AcceptUploads | RejectAfter Int | LoseNextReply
    deriving stock (Eq, Show)

-- | Observable physical files and parsed fields, independent of the product's state record.
data NativeTarget = NativeTarget
    { ntPort :: Int
    , ntFiles :: TVar (Map Text ByteString)
    , ntUploads :: TVar [(Text, [(ByteString, ByteString)])]
    , ntFault :: TVar UploadFault
    , ntRequests :: TVar Int
    }

-- | Bind on the bridge gateway so nginx can forward its TLS route to this application.
withNativeTarget :: TargetFields -> [Distribution] -> (NativeTarget -> IO a) -> IO a
withNativeTarget fieldsMode expected action = do
    files <- newTVarIO Map.empty
    uploads <- newTVarIO []
    fault <- newTVarIO AcceptUploads
    requests <- newTVarIO 0
    let target = NativeTarget 0 files uploads fault requests
    testWithApplicationSettings (setHost "*" defaultSettings) (pure (nativeApplication fieldsMode expected target)) $ \port ->
        action target{ntPort = port}

-- | Read the exact filenames and bytes accepted by the target.
targetFiles :: NativeTarget -> IO (Map Text ByteString)
targetFiles = readTVarIO . ntFiles

-- | Replace the target's deterministic upload failure mode.
setUploadFault :: NativeTarget -> UploadFault -> IO ()
setUploadFault target = atomically . writeTVar (ntFault target)

-- | Wait for exactly the expected whole graph, including all source archives.
awaitTargetFiles :: NativeTarget -> [Distribution] -> IO Bool
awaitTargetFiles target expected = (== wanted) <$> pollUntil 120 500000 (== wanted) (targetFiles target)
  where
    wanted = Map.fromList [(distributionFilename d, distributionBytes d) | d <- expected]

nativeApplication :: TargetFields -> [Distribution] -> NativeTarget -> Application
nativeApplication fieldsMode expected target request respond
    | requestMethod request == "POST"
    , filter (not . T.null) (pathInfo request) == ["legacy"] = do
        atomically (modifyTVar' (ntRequests target) (+ 1))
        (fields, files) <- parseRequestBody lbsBackEnd request
        case files of
            [("content", file)] -> do
                let filename = decodeUtf8 (fileName file)
                    bytes = LBS.toStrict (fileContent file)
                case find ((== filename) . distributionFilename) expected of
                    Just distribution
                        | validUpload distribution fields bytes
                        , lookup "Authorization" (requestHeaders request) == Just "Basic Zml4dHVyZS11c2VyOmZpeHR1cmUtcGFzc3dvcmQ=" -> do
                            result <- atomically (acceptUpload target filename fields bytes)
                            respond $ case result of
                                AcceptUploads -> responseLBS status200 [] "stored"
                                RejectAfter _ -> responseLBS status503 [] "injected partial copy"
                                LoseNextReply -> responseRaw (\_ _ -> pass) (responseLBS status503 [] "lost reply")
                    _ -> respond (responseLBS status400 [] "upload metadata or exact bytes differ")
            _ -> respond (responseLBS status400 [] "expected one content part")
    | requestMethod request == "GET" = do
        files <- targetFiles target
        respond $ case filter (not . T.null) (pathInfo request) of
            ["simple", project] -> responseLBS status200 [("Content-Type", "application/vnd.pypi.simple.v1+json")] (Aeson.encode (targetIndex fieldsMode project expected files))
            ["simple", _, filename] -> maybe (responseLBS status404 [] "missing") (responseLBS status200 [] . LBS.fromStrict) (Map.lookup filename files)
            _ -> responseLBS status404 [] "missing"
    | otherwise = respond (responseLBS status404 [] "missing")

acceptUpload :: NativeTarget -> Text -> [(ByteString, ByteString)] -> ByteString -> STM UploadFault
acceptUpload target filename fields bytes = do
    fault <- readTVar (ntFault target)
    files <- readTVar (ntFiles target)
    case fault of
        RejectAfter limit | Map.size files >= limit -> pure fault
        _ -> do
            writeTVar (ntFiles target) (Map.insert filename bytes files)
            modifyTVar' (ntUploads target) (<> [(filename, fields)])
            when (fault == LoseNextReply) (writeTVar (ntFault target) AcceptUploads)
            pure (if fault == LoseNextReply then LoseNextReply else AcceptUploads)

validUpload :: Distribution -> [(ByteString, ByteString)] -> ByteString -> Bool
validUpload distribution fields bytes =
    bytes == distributionBytes distribution
        && lookup ":action" fields == Just "file_upload"
        && lookup "protocol_version" fields == Just "1"
        && lookup "metadata_version" fields == Just "2.2"
        && lookup "provides_extra" fields == Just "test"
        && lookup "filetype" fields == Just (if ".whl" `T.isSuffixOf` distributionFilename distribution then "bdist_wheel" else "sdist")
        && lookup "name" fields == Just (encodeUtf8 (distributionProject distribution))
        && lookup "version" fields == Just (encodeUtf8 mirrorVersion)
        && lookup "sha256_digest" fields == Just (encodeUtf8 (hexSha256Of bytes))
        && lookup "requires_python" fields == Just ">=3.8"
        && lookup "summary" fields == (encodeUtf8 <$> listToMaybe [value | line <- T.lines (decodeUtf8 (distributionMetadata distribution)), Just value <- [T.stripPrefix "Summary: " line]])
        && lookup "description" fields == Just "Fixture description with a second line.\nRetain this body verbatim.\n"
        && length (filter ((== "classifiers") . fst) fields) == 2
        && (distributionProject distribution /= mirrorRoot || lookup "requires_dist" fields == Just (encodeUtf8 (mirrorDependency <> "==" <> mirrorVersion)))

-- Native reads deliberately lose source fields and companions to exercise durable reconstruction.
targetIndex :: TargetFields -> Text -> [Distribution] -> Map Text ByteString -> Aeson.Value
targetIndex fieldsMode project expected files =
    object
        [ "name" .= project
        , "meta" .= object ["api-version" .= ("1.1" :: Text)]
        , "files"
            .= [ object
                    [ "filename" .= distributionFilename distribution
                    , "url" .= (stubUrl PyPIMirror <> "simple/" <> project <> "/" <> distributionFilename distribution)
                    , "hashes" .= object ["sha256" .= hexSha256Of bytes]
                    , "requires-python" .= (if fieldsMode == ChangeSourceFields then ">=99" else ">=3.8" :: Text)
                    , "upload-time" .= (if fieldsMode == ChangeSourceFields then "2099-01-01T00:00:00Z" else "2020-01-01T00:00:00Z" :: Text)
                    , "yanked" .= (if fieldsMode == ChangeSourceFields then Aeson.String "target changed the reason" else Aeson.Bool False)
                    ]
               | distribution <- expected
               , distributionProject distribution == project
               , Just bytes <- [Map.lookup (distributionFilename distribution) files]
               ]
        ]

-- | Use the ordinary S3 emulator and an explicit native upload endpoint.
nativeMirrorConfig :: E2EConfig
nativeMirrorConfig =
    defaultE2EConfig
        { ecExtraEnv =
            [ ("ECLUSE_MIRROR_STATE__URL", "s3://" <> advisoryBucket <> "/pypi-mirror")
            , ("ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl PyPIMirror)
            , ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__URL", stubUrl PyPIMirror)
            , ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__UPLOAD_URL", stubUrl PyPIMirror <> "legacy/")
            , ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__TOKEN", "fixture-password")
            , ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__USERNAME", "fixture-user")
            ]
        }

-- | Build the complete source graph and hold its native target behind the existing TLS route.
withMirrorFixture :: GlobalDataPlane -> (NativeTarget -> [Distribution] -> IO a) -> IO a
withMirrorFixture plane action = do
    distributions <- buildMirrorGraph (gdpWorkDir plane </> "pypi")
    for_ [mirrorRoot, mirrorDependency] $ \project ->
        writeFileLBS
            (gdpWorkDir plane </> "pypi" </> "simple" </> toString project </> "index.json")
            (Aeson.encode (distributionIndex (stubUrl PyPIPublic) project distributions))
    createAdvisoryBucket plane
    withNativeTarget ChangeSourceFields distributions $ \target ->
        withStubFault plane (PyPIMirrorBackend (ntPort target) False) (action target distributions)
