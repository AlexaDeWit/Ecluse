-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Native distributions, multipart uploads, and conditional S3 objects for the PyPI worker load row.
module Ecluse.BenchLoad.PyPI.WorkerFixture (
    WorkerFixture (..),
    newWorkerFixture,
    resetWorkerFixture,
    sourceApp,
    targetApp,
    stateApp,
    fixturePackage,
    fixtureVersion,
    fixtureTrigger,
) where

import Codec.Compression.GZip qualified as GZip
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Types (hContentType, status200, status400, status404)
import Network.Wai (Application, Request, pathInfo, requestHeaders, requestMethod, responseLBS, strictRequestBody)
import Network.Wai.Parse (FileInfo (..), RequestBodyType (Multipart), lbsBackEnd, parseContentType, sinkRequestBody)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (artifactBytes, delayUpstream)
import Ecluse.BenchLoad.WorkerState (WorkerTarget (..), acceptUpload, countRequest, newWorkerTarget, resetWorkerTarget, targetStateApp)
import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry.PyPI.Wire (simpleIndexMediaType)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.Archive (tarBytes)
import Ecluse.Test.Registry.PyPI.Archive (metadata, zipBytes)
import Ecluse.Test.Wai (selfBaseUrl)

-- | Each reset removes both target files and durable objects before another complete copy.
data WorkerFixture = WorkerFixture
    { wfFiles :: Map Text ByteString
    , wfState :: WorkerTarget
    }

-- | Native archive framing encloses the configured uncompressed payload in each distribution.
newWorkerFixture :: Int -> IO WorkerFixture
newWorkerFixture payloadBytes = do
    let payload = LBS.toStrict (artifactBytes payloadBytes)
    wheel <- zipBytes [("example-1.dist-info/METADATA", metadata), ("example/data", payload)]
    sourceZip <- zipBytes [("example-1/PKG-INFO", metadata), ("example-1/data", payload)]
    sourceTar <- LBS.toStrict . GZip.compress <$> tarBytes [("example-1/PKG-INFO", metadata), ("example-1/data", payload)]
    checkUploadOracle fixtureTrigger wheel
    let files = Map.fromList [(fixtureTrigger, wheel), ("example-1-py2-none-any.whl", wheel), ("example-1.zip", sourceZip), ("example-1.tar.gz", sourceTar)]
    WorkerFixture files <$> newWorkerTarget

-- | Resets do not change the immutable source archives or injected failure switch.
resetWorkerFixture :: WorkerFixture -> IO ()
resetWorkerFixture = resetWorkerTarget . wfState

-- | The demand names one wheel while the selected source version contains all four distributions.
fixtureTrigger :: Text
fixtureTrigger = "example-1-py3-none-any.whl"

-- | Identity shared by the archives and PEP 691 document.
fixturePackage :: PackageName
fixturePackage = mkPackageName PyPI Nothing "example"

-- | Every archive belongs to the same release.
fixtureVersion :: Version
fixtureVersion = mkVersion PyPI "1"

-- | Only declared distributions exist, so an incorrect worker URL fails before publication.
sourceApp :: Int -> WorkerFixture -> Application
sourceApp latency fixture request respond = do
    delayUpstream latency
    case route request of
        ["simple", "example"] -> do
            countRequest (wfState fixture) "source-index"
            respond (responseLBS status200 [(hContentType, simpleIndexMediaType)] (encode (indexValue (selfBaseUrl request) (wfFiles fixture))))
        ["files", filename] | Just bytes <- Map.lookup filename (wfFiles fixture) -> do
            countRequest (wfState fixture) "source-file"
            respond (responseLBS status200 [(hContentType, "application/octet-stream")] (LBS.fromStrict bytes))
        _ -> respond (responseLBS status404 [] "missing")

-- | The target advertises only exact bytes accepted by its independent multipart decoder.
targetApp :: Int -> WorkerFixture -> Application
targetApp latency fixture request respond = do
    delayUpstream latency
    case (requestMethod request, route request) of
        ("GET", ["simple", "example"]) -> do
            countRequest (wfState fixture) "target-index"
            files <- readIORef (wtFiles (wfState fixture))
            respond (responseLBS status200 [(hContentType, simpleIndexMediaType)] (encode (indexValue (selfBaseUrl request) files)))
        ("POST", ["upload"]) -> do
            body <- LBS.toStrict <$> strictRequestBody request
            accepted <- decodeUpload fixture request body
            case accepted of
                Just (filename, bytes) -> acceptUpload (wfState fixture) filename bytes (responseLBS status200 [] "stored") >>= respond
                Nothing -> respond (responseLBS status400 [] "upload fields or bytes differ")
        _ -> respond (responseLBS status404 [] "missing")

decodeUpload :: WorkerFixture -> Request -> ByteString -> IO (Maybe (Text, ByteString))
decodeUpload fixture request bytes = case parseContentType <$> lookup hContentType (requestHeaders request) of
    Just ("multipart/form-data", attributes)
        | Just boundary <- lookup "boundary" attributes -> do
            chunks <- newIORef bytes
            (fields, files) <- sinkRequestBody lbsBackEnd (Multipart boundary) (atomicModifyIORef' chunks (BS.empty,))
            pure $ case files of
                [("content", file)]
                    | let filename = decodeUtf8 (fileName file)
                    , let content = LBS.toStrict (fileContent file)
                    , Map.lookup filename (wfFiles fixture) == Just content
                    , matchesUploadFields filename content fields ->
                        Just (filename, content)
                _ -> Nothing
    _ -> pure Nothing

matchesUploadFields :: Text -> ByteString -> [(ByteString, ByteString)] -> Bool
matchesUploadFields filename bytes fields = sort fields == sort (expectedUploadFields filename bytes)

checkUploadOracle :: Text -> ByteString -> IO ()
checkUploadOracle filename bytes = do
    let fields = expectedUploadFields filename bytes
        accepted = matchesUploadFields filename bytes
        altered =
            [("unexpected", "value") : fields]
                <> concat
                    [ [ filter ((/= name) . fst) fields
                      , map (\(key, value) -> (key, if key == name then "changed" else value)) fields
                      , field : fields
                      ]
                    | field@(name, _) <- fields
                    ]
    unless (accepted (reverse fields) && not (any accepted altered)) $
        benchFail "PyPI upload oracle accepted missing, changed, duplicated or unexpected metadata fields"

expectedUploadFields :: Text -> ByteString -> [(ByteString, ByteString)]
expectedUploadFields filename bytes =
    [ (":action", "file_upload")
    , ("protocol_version", "1")
    , ("name", "example")
    , ("version", "1")
    , ("metadata_version", "2.4")
    , ("description", "Description.\n")
    , ("sha256_digest", encodeUtf8 (hexSha256Of bytes))
    , ("filetype", if wheel then "bdist_wheel" else "sdist")
    , ("pyversion", if filename == fixtureTrigger then "py3" else if wheel then "py2" else "source")
    ]
  where
    wheel = ".whl" `T.isSuffixOf` filename

-- | Reuse conditional object semantics with this fixture's owned stores and counters.
stateApp :: Int -> WorkerFixture -> Application
stateApp latency = targetStateApp latency . wfState

indexValue :: Text -> Map Text ByteString -> Value
indexValue base files =
    object
        [ "name" .= ("example" :: Text)
        , "meta" .= object ["api-version" .= ("1.4" :: Text)]
        , "files" .= [object ["filename" .= filename, "url" .= (base <> "/files/" <> filename), "hashes" .= object ["sha256" .= hexSha256Of bytes], "size" .= BS.length bytes, "upload-time" .= ("2020-01-01T00:00:00Z" :: Text)] | (filename, bytes) <- Map.toList files]
        ]

route :: Request -> [Text]
route = filter (not . T.null) . pathInfo
