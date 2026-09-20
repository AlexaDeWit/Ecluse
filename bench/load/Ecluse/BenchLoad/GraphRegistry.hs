-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

{- | Capture complete anonymous npm responses and serve a closed frozen registry.
Artifacts retain their original bytes. Metadata changes only its registry authority.
-}
module Ecluse.BenchLoad.GraphRegistry (
    Capture (..),
    graphRegistry,
    graphRegistryWithFetch,
    loadCapture,
    capturePath,
    packageKey,
    traceHeaders,
) where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Header, hContentLength, hContentType, mkStatus, status200, status401, status404, status405, statusCode)
import Network.Wai (Application, Middleware, pathInfo, rawQueryString, requestHeaders, requestMethod, responseFile, responseHeaders, responseLBS, responseStatus)
import System.Directory (doesFileExist, renameFile)
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)
import UnliftIO.Exception (bracketOnError)
import UnliftIO.MVar (modifyMVar, withMVar)

import Ecluse.BenchLoad.CacheWeight (accountedFullBytes)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), boundedRead)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Wai (rebaseAuthority, selfBaseUrl)

-- | Provenance names the unmodified, decompressed upstream bytes.
data Capture = Capture
    { capKey :: Text
    , capSource :: Text
    , capDate :: Text
    , capSha256 :: Text
    , capBytes :: Int
    , capStatus :: Int
    , capContentType :: Text
    , capHeaders :: [(Text, Text)]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | A package key ignores artifact filenames but preserves the scoped package identity.
packageKey :: Text -> Text
packageKey = fst . T.breakOn "/-/"

-- | Filenames cannot inherit path traversal from HTTP input.
capturePath :: FilePath -> Text -> FilePath
capturePath root key = root </> toString (hexSha256Of (encodeUtf8 key))

-- | Check both the frozen index and its original body before serving it.
loadCapture :: FilePath -> Text -> IO (Maybe (Capture, ByteString))
loadCapture root key = do
    let base = capturePath root key
    exists <- doesFileExist (base <> ".json")
    if not exists
        then pure Nothing
        else do
            raw <- readFileLBS (base <> ".json")
            provenance <- either (benchFail . toText) pure (eitherDecode raw)
            body <- BS.readFile (base <> ".body")
            unless (capKey provenance == key && capBytes provenance == BS.length body && capSha256 provenance == hexSha256Of body) $
                benchFail ("capture integrity failure: " <> key)
            pure (Just (provenance, body))

-- | Capture mode alone has network access. Frozen misses remain visible HTTP failures.
graphRegistry :: Bool -> FilePath -> Int -> IO Application
graphRegistry capture root latency =
    if capture
        then do
            manager <- HTTP.newManager tlsManagerSettings
            graphRegistryWithFetch (Just (fetchCapture manager root)) root latency
        else graphRegistryWithFetch Nothing root latency

-- | A supplied capture fetch is the only path to new bodies. 'Nothing' closes the registry.
graphRegistryWithFetch :: Maybe (Text -> IO (Capture, ByteString)) -> FilePath -> Int -> IO Application
graphRegistryWithFetch fetch root latency = do
    locks <- newMVar mempty
    counters <- newIORef Map.empty
    pure $ countRegistry counters $ \request respond -> do
        let key = T.intercalate "/" (pathInfo request)
            sensitive = any (\(name, _) -> name `elem` ["authorization", "proxy-authorization", "cookie"]) (requestHeaders request)
            refusal
                | sensitive = Just (status401, "credentials are forbidden in graph experiments")
                | "private-miss/" `T.isPrefixOf` key = Just (status404, "{}")
                | requestMethod request /= "GET" || rawQueryString request /= "" = Just (status405, "only anonymous GET without query is supported")
                | otherwise = Nothing
        case refusal of
            Just (status, message) -> respond (responseLBS status [] message)
            Nothing -> do
                lock <- modifyMVar locks $ \entries -> case Map.lookup key entries of
                    Just existing -> pure (entries, existing)
                    Nothing -> do
                        fresh <- newMVar ()
                        pure (Map.insert key fresh entries, fresh)
                result <- withMVar lock $ \() -> do
                    existing <- loadCapture root key
                    case existing of
                        Nothing -> traverse (\capture -> capture key) fetch
                        _ -> pure existing
                when (latency > 0) (threadDelay latency)
                maybe (respond (responseLBS status404 [] "uncaptured request")) (\captured -> serveCapture root key captured request respond) result

serveCapture :: FilePath -> Text -> (Capture, ByteString) -> Application
serveCapture root key (provenance, bytes) request respond = do
    let status = mkStatus (capStatus provenance) "frozen"
        headers = [(hContentType, encodeUtf8 (capContentType provenance))]
    if "/-/" `T.isInfixOf` key
        then respond (responseFile status ((hContentLength, encodeUtf8 (show (capBytes provenance) :: Text)) : headers) (capturePath root key <> ".body") Nothing)
        else do
            let rewritten = rebaseAuthority "https://registry.npmjs.org" (selfBaseUrl request) (LBS.fromStrict bytes)
            recordWeight root (selfBaseUrl request) key rewritten
            respond (responseLBS status ((hContentLength, encodeUtf8 (show (LBS.length rewritten) :: Text)) : headers) rewritten)

countRegistry :: IORef (Map Text Integer) -> Middleware
countRegistry counters application request respond
    | pathInfo request == ["_bench", "counters"] = do
        values <- readIORef counters
        respond (responseLBS status200 [(hContentType, "application/json")] (encode values))
    | otherwise = application request $ \response -> do
        let key = T.intercalate "/" (pathInfo request)
            kind
                | "private-miss/" `T.isPrefixOf` key = "private"
                | "/-/" `T.isInfixOf` key = "artifact"
                | otherwise = "metadata"
            status = kind <> "." <> show (statusCode (responseStatus response))
            bytes = fromMaybe 0 (List.lookup hContentLength (responseHeaders response) >>= readMaybe . toString . (decodeUtf8 :: ByteString -> Text))
        atomicModifyIORef' counters (\values -> (Map.insertWith (+) status 1 (Map.insertWith (+) (kind <> ".bodyBytes") bytes values), ()))
        respond response

fetchCapture :: HTTP.Manager -> FilePath -> Text -> IO (Capture, ByteString)
fetchCapture manager root key = do
    let source = "https://registry.npmjs.org/" <> key
    request <- HTTP.parseRequest (toString source)
    HTTP.withResponse request{HTTP.requestHeaders = [("Accept", "application/json"), ("Accept-Encoding", "identity")], HTTP.redirectCount = 0, HTTP.responseTimeout = HTTP.responseTimeoutMicro 120_000_000} manager $ \response -> do
        result <- boundedRead (MetadataBodyLimit (512 * 1024 * 1024)) (HTTP.brRead (HTTP.responseBody response))
        (_, bytes) <- either (benchFail . show) pure result
        date <- toText . iso8601Show <$> getCurrentTime
        let code = statusCode (HTTP.responseStatus response)
            contentType = maybe "application/octet-stream" decodeUtf8 (List.lookup hContentType (HTTP.responseHeaders response))
            provenance = Capture key source date (hexSha256Of bytes) (BS.length bytes) code contentType (traceHeaders (HTTP.responseHeaders response))
        when (code < 200 || code >= 300) $
            appendFileText (root </> "failures.log") (source <> " status " <> show code <> "\n")
        atomicWrite root (capturePath root key <> ".body") (LBS.fromStrict bytes)
        atomicWrite root (capturePath root key <> ".json") (encode provenance)
        pure (provenance, bytes)

recordWeight :: FilePath -> Text -> Text -> LByteString -> IO ()
recordWeight root source key body = do
    let destination = capturePath root key <> ".weight"
    exists <- doesFileExist destination
    unless exists $ do
        name <- either (benchFail . show) pure (projectName key)
        case accountedFullBytes Npm source name body of
            Left err -> appendFileText (root </> "projection-failures.log") (key <> ": " <> show err <> "\n")
            Right weight -> atomicWrite root destination (encode weight)

-- | Record only protocol headers with no credential payload. Unknown values are redacted.
traceHeaders :: [Header] -> [(Text, Text)]
traceHeaders = map (\(name, value) -> (show name, if name `elem` visible then decodeUtf8 value else "[redacted]"))
  where
    visible = ["host", "user-agent", "accept", "accept-encoding", "content-type", "content-length", "etag", "if-none-match", "if-modified-since", "last-modified", "date", "cache-control", "content-encoding", "vary"]

atomicWrite :: FilePath -> FilePath -> LByteString -> IO ()
atomicWrite root destination bytes =
    bracketOnError (openBinaryTempFile root "capture.tmp") (\(_, handle) -> hClose handle) $ \(temporary, handle) -> do
        LBS.hPut handle bytes
        hClose handle
        renameFile temporary destination
