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
    loadCapture,
    capturePath,
    packageKey,
) where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hContentType, mkStatus, status404, status405, statusCode)
import Network.Wai (Application, pathInfo, rawQueryString, requestMethod, responseFile, responseLBS)
import System.Directory (doesFileExist, renameFile)
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)
import UnliftIO.Exception (bracketOnError)
import UnliftIO.MVar (modifyMVar, withMVar)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.Metadata (digestOf)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), boundedRead, defaultLimits)
import Ecluse.Core.Server.Cache (CacheEntry (..), weighCacheEntry)
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
graphRegistry capture root latency = do
    manager <- HTTP.newManager tlsManagerSettings
    locks <- newMVar mempty
    pure $ \request respond -> do
        let key = T.intercalate "/" (pathInfo request)
        if "private-miss/" `T.isPrefixOf` key
            then respond (responseLBS status404 [] "{}")
            else
                if requestMethod request /= "GET" || rawQueryString request /= ""
                    then respond (responseLBS status405 [] "only anonymous GET without query is supported")
                    else do
                        lock <- modifyMVar locks $ \entries ->
                            case Map.lookup key entries of
                                Just existing -> pure (entries, existing)
                                Nothing -> do
                                    fresh <- newMVar ()
                                    pure (Map.insert key fresh entries, fresh)
                        result <- withMVar lock $ \() -> do
                            existing <- loadCapture root key
                            case existing of
                                Nothing | capture -> Just <$> fetchCapture manager root key
                                _ -> pure existing
                        when (latency > 0) (threadDelay latency)
                        case result of
                            Nothing -> respond (responseLBS status404 [] "uncaptured request")
                            Just (provenance, bytes) -> do
                                let status = mkStatus (capStatus provenance) "frozen"
                                    headers = [(hContentType, encodeUtf8 (capContentType provenance))]
                                if "/-/" `T.isInfixOf` key
                                    then respond (responseFile status headers (capturePath root key <> ".body") Nothing)
                                    else do
                                        let rewritten = rebaseAuthority "https://registry.npmjs.org" (selfBaseUrl request) (LBS.fromStrict bytes)
                                        recordWeight root key rewritten
                                        respond (responseLBS status headers rewritten)

fetchCapture :: HTTP.Manager -> FilePath -> Text -> IO (Capture, ByteString)
fetchCapture manager root key = do
    let source = "https://registry.npmjs.org/" <> key
    request <- HTTP.parseRequest (toString source)
    HTTP.withResponse request{HTTP.requestHeaders = [("Accept", "application/json"), ("Accept-Encoding", "identity")], HTTP.redirectCount = 0, HTTP.responseTimeout = HTTP.responseTimeoutMicro 120_000_000} manager $ \response -> do
        result <- boundedRead (MetadataBodyLimit (512 * 1024 * 1024)) (HTTP.brRead (HTTP.responseBody response))
        (_, bytes) <- either (benchFail . show) pure result
        date <- toText . iso8601Show <$> getCurrentTime
        let code = statusCode (HTTP.responseStatus response)
            contentType = maybe "application/octet-stream" decodeUtf8 (lookup hContentType (HTTP.responseHeaders response))
            provenance = Capture key source date (hexSha256Of bytes) (BS.length bytes) code contentType [(show name, decodeUtf8 value) | (name, value) <- HTTP.responseHeaders response]
        when (code < 200 || code >= 300) $
            appendFileText (root </> "failures.log") (source <> " status " <> show code <> "\n")
        atomicWrite root (capturePath root key <> ".body") (LBS.fromStrict bytes)
        atomicWrite root (capturePath root key <> ".json") (encode provenance)
        pure (provenance, bytes)

recordWeight :: FilePath -> Text -> LByteString -> IO ()
recordWeight root key body = do
    let destination = capturePath root key <> ".weight"
    exists <- doesFileExist destination
    unless exists $ do
        name <- either (benchFail . show) pure (projectName key)
        let bytes = LBS.toStrict body
        case projectNpmManifest defaultLimits name bytes of
            Left err -> appendFileText (root </> "projection-failures.log") (key <> ": " <> show err <> "\n")
            Right (info, document) ->
                atomicWrite root destination (encode (weighCacheEntry (CacheEntry info (fst npmCached document) (BS.length bytes) (digestOf bytes))))

atomicWrite :: FilePath -> FilePath -> LByteString -> IO ()
atomicWrite root destination bytes =
    bracketOnError (openBinaryTempFile root "capture.tmp") (\(_, handle) -> hClose handle) $ \(temporary, handle) -> do
        LBS.hPut handle bytes
        hClose handle
        renameFile temporary destination
