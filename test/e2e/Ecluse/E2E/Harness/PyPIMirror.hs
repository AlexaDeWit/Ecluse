-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | A native multipart target whose stored bytes and faults remain observable across product restarts.
module Ecluse.E2E.Harness.PyPIMirror (
    NativeTarget (..),
    NativeTargetConfig (..),
    withConfiguredNativeTarget,
    nativeApplication,
    UploadFault (..),
    TargetFields (..),
    withNativeTarget,
    targetFiles,
    setUploadFault,
    awaitBlockedUpload,
    releaseBlockedUpload,
    awaitTargetFiles,
    nativeMirrorConfig,
    withMirrorFixture,
    PublicWorkCounters (..),
    publicWorkCounters,
) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (status200, status400, status404, status413, status503, statusCode)
import Network.Wai (Application, Request, Response, getRequestBodyChunk, pathInfo, requestHeaders, requestMethod, responseLBS, responseRaw)
import Network.Wai.Handler.Warp (defaultSettings, setHost, testWithApplicationSettings)
import Network.Wai.Parse (FileInfo (fileContent, fileName), RequestBodyType (Multipart), lbsBackEnd, parseContentType, sinkRequestBody)
import System.Exit (ExitCode (ExitSuccess))
import System.Process.Typed (proc, readProcess)

import UnliftIO (bracket_, timeout)
import UnliftIO.STM (check)

import Ecluse.Core.Security.Limits (BodyLimit (PublishRequestBodyLimit), boundedRead)
import Ecluse.E2E.Fixtures.PyPI.Mirror
import Ecluse.E2E.Harness.Advisories (advisoryBucket, createAdvisoryBucket)
import Ecluse.E2E.Harness.Docker (withStubFault)
import Ecluse.E2E.Harness.Stub (StubFault (PyPIMirrorBackend), StubRoute (PyPIMirror, PyPIPublic), nativeUploadLimit, stubUrl)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Poll (pollUntil)
import System.FilePath ((</>))

-- | Native targets can preserve upload fields or lose the richer public source fields.
data TargetFields = PreserveUploadFields | ChangeSourceFields
    deriving stock (Eq, Show)

-- | Fail before storing a sibling or lose a reply after the target commits it.
data UploadFault = AcceptUploads | HoldAfter Int | LoseNextReply
    deriving stock (Eq, Show)

-- | Observable physical files and parsed fields, independent of the product's state record.
data NativeTarget = NativeTarget
    { ntPort :: Int
    , ntFiles :: TVar (Map Text ByteString)
    , ntAttempts :: TVar [Text]
    , ntFault :: TVar UploadFault
    , ntRequests :: TVar Int
    , ntBlocked :: TVar (Maybe Text)
    , ntReleased :: TVar Bool
    , ntInFlight :: TVar Int
    }

-- | Native endpoint facts, independent of the product credential and URL constructors.
data NativeTargetConfig = NativeTargetConfig
    { ntcBaseUrl :: Text
    , ntcAuthorization :: ByteString
    , ntcFields :: TargetFields
    }

-- | Bind the fixed mirror graph's target with its declared fixture credential.
withNativeTarget :: TargetFields -> [Distribution] -> (NativeTarget -> IO a) -> IO a
withNativeTarget fieldsMode =
    withConfiguredNativeTarget
        (NativeTargetConfig (stubUrl PyPIMirror) "Basic Zml4dHVyZS11c2VyOmZpeHR1cmUtcGFzc3dvcmQ=" fieldsMode)

-- | Bind a configured native target on the bridge gateway behind its TLS route.
withConfiguredNativeTarget :: NativeTargetConfig -> [Distribution] -> (NativeTarget -> IO a) -> IO a
withConfiguredNativeTarget config expected action = do
    files <- newTVarIO Map.empty
    attempts <- newTVarIO []
    fault <- newTVarIO AcceptUploads
    requests <- newTVarIO 0
    blocked <- newTVarIO Nothing
    released <- newTVarIO False
    unless (all ((< nativeUploadLimit `div` 2) . BS.length . distributionBytes) expected) (fail "fixture artifact exceeds half the multipart bound")
    active <- newTVarIO 0
    let target = NativeTarget 0 files attempts fault requests blocked released active
    testWithApplicationSettings (setHost "*" defaultSettings) (pure (nativeApplication config expected target)) $ \port ->
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

-- | Expose native upload and Simple routes for a composed backend transport fixture.
nativeApplication :: NativeTargetConfig -> [Distribution] -> NativeTarget -> Application
nativeApplication config expected target request respond
    | requestMethod request == "POST"
    , filter (not . T.null) (pathInfo request) == ["legacy"] =
        bracket_ beginUpload endUpload $
            boundedRead (PublishRequestBodyLimit nativeUploadLimit) (getRequestBodyChunk request) >>= \case
                Left _ -> respond (responseLBS status413 [] "multipart request exceeds fixture bound")
                Right (_, body) -> parseUpload config expected target request body >>= respond
    | requestMethod request `elem` ["GET", "HEAD"] = do
        files <- targetFiles target
        respond (nativeRead config expected request files)
    | otherwise = respond (responseLBS status404 [] "missing")
  where
    beginUpload = atomically $ do
        modifyTVar' (ntRequests target) (+ 1)
        modifyTVar' (ntInFlight target) (+ 1)
    endUpload = atomically (modifyTVar' (ntInFlight target) (subtract 1))

parseUpload :: NativeTargetConfig -> [Distribution] -> NativeTarget -> Request -> ByteString -> IO Response
parseUpload config expected target request body =
    case parseContentType <$> lookup "Content-Type" (requestHeaders request) of
        Just ("multipart/form-data", attributes)
            | Just boundary <- lookup "boundary" attributes
            , not (BS.null boundary)
            , BS.length boundary <= 256 -> do
                chunks <- newIORef body
                (fields, files) <- sinkRequestBody lbsBackEnd (Multipart boundary) (atomicModifyIORef' chunks (BS.empty,))
                case files of
                    [("content", file)] -> receiveUpload config expected target request fields file
                    _ -> pure (responseLBS status400 [] "expected one content part")
        _ -> pure (responseLBS status400 [] "expected bounded multipart boundary")

receiveUpload :: NativeTargetConfig -> [Distribution] -> NativeTarget -> Request -> [(ByteString, ByteString)] -> FileInfo LByteString -> IO Response
receiveUpload config expected target request fields file =
    case find ((== filename) . distributionFilename) expected of
        Just distribution
            | validUpload distribution fields bytes
            , lookup "Authorization" (requestHeaders request) == Just (ntcAuthorization config) -> do
                atomically (modifyTVar' (ntAttempts target) (<> [filename]))
                result <- atomically (acceptUpload target filename bytes)
                case result of
                    UploadStored -> pure (responseLBS status200 [] "stored")
                    UploadDuplicate -> pure (responseLBS status400 [] "File already exists.")
                    UploadHeld -> do
                        released <- timeout 120000000 (atomically (readTVar (ntReleased target) >>= check))
                        pure (responseLBS status503 [] (if isJust released then "interrupted upload released" else "fixture upload barrier timed out"))
                    UploadReplyLost -> pure (responseRaw (\_ _ -> pass) (responseLBS status503 [] "lost reply"))
        _ -> pure (responseLBS status400 [] "upload metadata or exact bytes differ")
  where
    filename = decodeUtf8 (fileName file)
    bytes = LBS.toStrict (fileContent file)

nativeRead :: NativeTargetConfig -> [Distribution] -> Request -> Map Text ByteString -> Response
nativeRead config expected request files = case filter (not . T.null) (pathInfo request) of
    ["simple", project]
        | any (\distribution -> distributionProject distribution == project && Map.member (distributionFilename distribution) files) expected -> answer status200 [("Content-Type", "application/vnd.pypi.simple.v1+json")] (Aeson.encode (targetIndex config project expected files))
    ["simple", _, filename] -> maybe (answer status404 [] "missing") (answer status200 [] . LBS.fromStrict) (Map.lookup filename files)
    _ -> answer status404 [] "missing"
  where
    answer status headers bytes =
        responseLBS
            status
            (("Content-Length", encodeUtf8 (show (LBS.length bytes) :: Text)) : headers)
            (if requestMethod request == "HEAD" then LBS.empty else bytes)

data UploadReply = UploadStored | UploadDuplicate | UploadHeld | UploadReplyLost

acceptUpload :: NativeTarget -> Text -> ByteString -> STM UploadReply
acceptUpload target filename bytes = do
    fault <- readTVar (ntFault target)
    files <- readTVar (ntFiles target)
    case fault of
        _ | Map.member filename files -> pure UploadDuplicate
        HoldAfter limit | Map.size files >= limit -> do
            modifyTVar' (ntBlocked target) (<|> Just filename)
            pure UploadHeld
        _ -> do
            writeTVar (ntFiles target) (Map.insert filename bytes files)
            when (fault == LoseNextReply) (writeTVar (ntFault target) AcceptUploads)
            pure (if fault == LoseNextReply then UploadReplyLost else UploadStored)

-- | Wait until a complete, validated second upload reaches the blocked target handler.
awaitBlockedUpload :: NativeTarget -> IO Text
awaitBlockedUpload target =
    pollUntil 120 500000 isJust (readTVarIO (ntBlocked target))
        >>= maybe (fail "no upload reached the interruption barrier") pure

-- | Release interrupted connections without committing their files and await handler settlement.
releaseBlockedUpload :: NativeTarget -> IO ()
releaseBlockedUpload target = do
    atomically (writeTVar (ntReleased target) True)
    active <- pollUntil 120 50000 (== 0) (readTVarIO (ntInFlight target))
    unless (active == 0) (fail "interrupted upload handlers did not settle")

validUpload :: Distribution -> [(ByteString, ByteString)] -> ByteString -> Bool
validUpload distribution fields bytes =
    bytes == distributionBytes distribution
        && lookup ":action" fields == Just "file_upload"
        && lookup "protocol_version" fields == Just "1"
        && lookup "filetype" fields == Just (if ".whl" `T.isSuffixOf` distributionFilename distribution then "bdist_wheel" else "sdist")
        && lookup "name" fields == Just (encodeUtf8 (distributionProject distribution))
        && lookup "version" fields == Just (encodeUtf8 (distributionVersion distribution))
        && lookup "sha256_digest" fields == Just (encodeUtf8 (hexSha256Of bytes))
        && all (\name -> values name fields == values name expected) (ordNub (map fst expected))
  where
    expected = distributionFields distribution
    values name = map snd . filter ((== name) . fst)

-- Native reads deliberately lose source fields and companions to exercise durable reconstruction.
targetIndex :: NativeTargetConfig -> Text -> [Distribution] -> Map Text ByteString -> Aeson.Value
targetIndex config project expected files =
    object
        [ "name" .= project
        , "meta" .= object ["api-version" .= ("1.1" :: Text)]
        , "files"
            .= [ object
                    [ "filename" .= distributionFilename distribution
                    , "url" .= (ntcBaseUrl config <> "simple/" <> project <> "/" <> distributionFilename distribution)
                    , "hashes" .= object ["sha256" .= hexSha256Of bytes]
                    , "requires-python" .= (if ntcFields config == ChangeSourceFields then ">=99" else ">=3.8" :: Text)
                    , "upload-time" .= (if ntcFields config == ChangeSourceFields then "2099-01-01T00:00:00Z" else "2020-01-01T00:00:00Z" :: Text)
                    , "yanked" .= (if ntcFields config == ChangeSourceFields then Aeson.String "target changed the reason" else Aeson.Bool False)
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
            [ ("ECLUSE_OBSERVABILITY__TELEMETRY", "on")
            , ("OTEL_METRICS_EXPORTER", "prometheus")
            , ("OTEL_TRACES_EXPORTER", "none")
            , ("OTEL_LOGS_EXPORTER", "none")
            , ("OTEL_EXPORTER_PROMETHEUS_HOST", "0.0.0.0")
            , ("OTEL_EXPORTER_PROMETHEUS_PORT", "9464")
            , ("ECLUSE_MIRROR_STATE__URL", "s3://" <> advisoryBucket <> "/pypi-mirror")
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

-- | Synchronously collected rule evaluations and mirror enqueues within one product process.
data PublicWorkCounters = PublicWorkCounters
    { pwcRuleEvaluations :: Double
    , pwcMirrorEnqueues :: Double
    }
    deriving stock (Eq, Show)

-- | Scrape the product listener directly on its isolated bridge address.
publicWorkCounters :: E2E -> IO PublicWorkCounters
publicWorkCounters proxy = do
    (code, address, diagnostics) <- readProcess (proc "docker" ["inspect", "--format", "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}", e2eProxyContainer proxy])
    unless (code == ExitSuccess) (fail ("could not locate proxy scrape listener: " <> toString (decodeUtf8 diagnostics :: Text)))
    request <- Http.parseRequest ("http://" <> toString (T.strip (decodeUtf8 address :: Text)) <> ":9464/metrics")
    response <- Http.httpLbs request (e2eManager proxy)
    unless (statusCode (Http.responseStatus response) == 200) (fail "product metrics scrape failed")
    samples <- traverse sample (filter (not . T.isPrefixOf "#") (T.lines (decodeUtf8 (Http.responseBody response))))
    pure
        PublicWorkCounters
            { pwcRuleEvaluations = sum [value | Just (name, value) <- samples, "ecluse_rule_eval_duration" `T.isPrefixOf` name, "_count" `T.isSuffixOf` name]
            , pwcMirrorEnqueues = sum [value | Just (name, value) <- samples, name `elem` ["ecluse_mirror_enqueued", "ecluse_mirror_enqueued_total"]]
            }
  where
    sample line
        | T.null (T.strip line) = pure Nothing
        | otherwise = case T.words line of
            [labelledName, value] -> case readMaybe (toString value) of
                Just number -> pure (Just (T.takeWhile (/= '{') labelledName, number))
                Nothing -> fail "metrics scrape contains a non-numeric sample"
            _ -> fail "metrics scrape contains an unsupported sample shape"
