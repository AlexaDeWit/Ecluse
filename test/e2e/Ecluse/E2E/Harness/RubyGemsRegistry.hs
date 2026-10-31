-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native targets retain physical archive inventories and observable writes across product restarts.
module Ecluse.E2E.Harness.RubyGemsRegistry (
    RubyRegistry (..),
    RubyRegistryFault (..),
    RubyArtifactResponse (..),
    setRubyArtifactResponse,
    withRubyRegistry,
    rubyRegistryApplication,
    allowRubyRegistryUploads,
    removeRubyRegistryVersion,
    seedRubyRegistry,
    rubyRegistryVersions,
    rubyRegistryFiles,
    rubyRegistryDirectory,
    awaitRubyRegistry,
    awaitBlockedRubyUpload,
    releaseBlockedRubyUpload,
    setRubyRegistryFault,
    rubyRegistryToken,
) where

import Data.Aeson (encode, withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Types (RequestHeaders, ResponseHeaders, Status, mkStatus, status200, status201, status400, status401, status404, status405, status409, status413, status503, statusCode)
import Network.Wai qualified as Wai
import UnliftIO (timeout)
import UnliftIO.STM (checkSTM)

import Ecluse.Core.Security.Limits (BodyLimit (PublishRequestBodyLimit), boundedRead)
import Ecluse.E2E.Harness.Container (withBridgeApplication)
import Ecluse.E2E.Harness.NativeEdge (NativeRequest (..))
import Ecluse.Test.Package (hexMd5Of)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Registry.RubyGems.Fixtures
import Ecluse.Test.Registry.RubyGems.Reference (runRubyReference)

-- | Faults affect actual target exchanges and never declare completion on behalf of the product.
data RubyRegistryFault
    = RubyRegistryHealthy
    | RubyRegistryUnavailable
    | RubyRegistryDirectoryStatus Int
    | RubyRegistryInfoStatus Text Int
    | RubyRegistryLoseReply
    | RubyRegistryRequireRead
    | RubyRegistryMissingArchive Text
    | RubyRegistryCompactBytes FilePath ByteString
    | RubyRegistryRejectUpload Text
    | RubyRegistryStopAfterOne
    | RubyRegistryInfoHints
    deriving stock (Eq, Show)

-- | Programmable native responses retain real status, framing and chunk boundaries.
data RubyArtifactResponse = RubyArtifactResponse
    { artifactReplyStatus :: Int
    , artifactReplyHeaders :: ResponseHeaders
    , artifactReplyChunks :: [ByteString]
    , artifactReplyChunked :: Bool
    }
    deriving stock (Eq, Show)

-- | Hostnames identify separate public, private and mirror inventories behind one fixture listener.
data RubyRegistry = RubyRegistry
    { rubyRegistryPort :: Int
    , rubyRegistryInventory :: TVar (Map (Text, Text) RubyRelease)
    , rubyRegistryRequests :: TVar [NativeRequest]
    , rubyRegistryUploads :: TVar [(Text, ByteString)]
    , rubyRegistryLostReplies :: TVar [(Text, Text)]
    , rubyRegistryFaults :: TVar (Map Text RubyRegistryFault)
    , rubyRegistryCredentials :: TVar [(Text, ByteString, Maybe ByteString)]
    , rubyRegistryReleases :: TVar [RubyRelease]
    , rubyRegistryArtifactReplies :: TVar (Map (Text, Text) RubyArtifactResponse)
    , rubyRegistryBlockedUpload :: TVar (Maybe Text)
    , rubyRegistryUploadReleased :: TVar Bool
    , rubyRegistryHeaders :: TVar [(Text, ByteString, Text, RequestHeaders)]
    }

-- | Writes use the native API-key value without a bearer prefix.
rubyRegistryToken :: Text
rubyRegistryToken = "rubygems-fixture-token"

-- | The public registry starts with genuine owned source artifacts. Private stores start empty.
withRubyRegistry :: (RubyRegistry -> IO a) -> IO a
withRubyRegistry action = do
    releases <- loadRubyReleases
    inventory <- newTVarIO Map.empty
    requests <- newTVarIO []
    uploads <- newTVarIO []
    lostReplies <- newTVarIO []
    faults <- newTVarIO Map.empty
    known <- newTVarIO releases
    credentials <- newTVarIO []
    replies <- newTVarIO Map.empty
    headers <- newTVarIO []
    blocked <- newTVarIO Nothing
    released <- newTVarIO False
    let registry = RubyRegistry 0 inventory requests uploads lostReplies faults credentials known replies blocked released headers
    seedRubyRegistry registry "rubygems-upstream" releases
    withBridgeApplication (rubyRegistryApplication registry) $ \port -> action registry{rubyRegistryPort = port}

-- | Tests seed an ordinary private source without using the product publication path.
seedRubyRegistry :: RubyRegistry -> Text -> [RubyRelease] -> IO ()
seedRubyRegistry registry host releases = do
    allowRubyRegistryUploads registry releases
    atomically $ modifyTVar' (rubyRegistryInventory registry) (\inventory -> foldl' (\stored release -> Map.insert (host, releaseFilename release) release stored) inventory releases)

-- | Register genuine upload bytes without exposing a first-party package through a read source.
allowRubyRegistryUploads :: RubyRegistry -> [RubyRelease] -> IO ()
allowRubyRegistryUploads registry releases = atomically (modifyTVar' (rubyRegistryReleases registry) (ordNub . (<> releases)))

-- | Backend deletion removes every physical platform artifact for one exact raw version.
removeRubyRegistryVersion :: RubyRegistry -> Text -> Text -> Text -> IO ()
removeRubyRegistryVersion registry host name version =
    atomically $
        modifyTVar' (rubyRegistryInventory registry) $
            Map.filterWithKey (\(authority, _) release -> authority /= host || releaseName release /= name || releaseVersion release /= version)

-- | Only physically retained archives count towards native inventory.
rubyRegistryFiles :: RubyRegistry -> Text -> IO [RubyRelease]
rubyRegistryFiles registry host = map snd . filter ((== host) . fst . fst) . Map.toList <$> readTVarIO (rubyRegistryInventory registry)

-- | The discovery body one authority serves now, hinted rows included.
rubyRegistryDirectory :: RubyRegistry -> Text -> IO Text
rubyRegistryDirectory registry host = do
    fault <- Map.findWithDefault RubyRegistryHealthy host <$> readTVarIO (rubyRegistryFaults registry)
    directoryBody host fault <$> rubyRegistryFiles registry host

-- | Whole raw versions remain distinct from individual platform artifacts.
rubyRegistryVersions :: RubyRegistry -> Text -> Text -> IO [Text]
rubyRegistryVersions registry host name = sort . ordNub . map releaseVersion . filter ((== name) . releaseName) <$> rubyRegistryFiles registry host

-- | The expected coordinate set and exact archive bytes must both reach the physical target.
awaitRubyRegistry :: RubyRegistry -> Text -> [RubyRelease] -> IO Bool
awaitRubyRegistry registry host expected = pollUntil 120 500000 complete (rubyRegistryFiles registry host) <&> complete
  where
    complete actual = all (\release -> any (\stored -> releaseFilename stored == releaseFilename release && releaseArchive stored == releaseArchive release) actual) expected

-- | Wait for a validated upload that cannot commit before the product stops.
awaitBlockedRubyUpload :: RubyRegistry -> IO Text
awaitBlockedRubyUpload registry =
    pollUntil 120 500000 isJust (readTVarIO (rubyRegistryBlockedUpload registry))
        >>= maybe (fail "no RubyGems upload reached the interruption barrier") pure

-- | Release the held connection without committing its archive.
releaseBlockedRubyUpload :: RubyRegistry -> IO ()
releaseBlockedRubyUpload registry = atomically (writeTVar (rubyRegistryUploadReleased registry) True)

-- | A fault belongs to one authority and cannot affect another source implicitly.
setRubyRegistryFault :: RubyRegistry -> Text -> RubyRegistryFault -> IO ()
setRubyRegistryFault registry host fault = atomically (modifyTVar' (rubyRegistryFaults registry) (Map.insert host fault))

-- | Overrides belong to one actual authority and native artifact filename.
setRubyArtifactResponse :: RubyRegistry -> Text -> Text -> Maybe RubyArtifactResponse -> IO ()
setRubyArtifactResponse registry host filename response =
    atomically $
        modifyTVar' (rubyRegistryArtifactReplies registry) $
            maybe (Map.delete (host, filename)) (Map.insert (host, filename)) response

-- | Native data-plane requests retain the requested host as their independent repository identity.
rubyRegistryApplication :: RubyRegistry -> Wai.Application
rubyRegistryApplication registry request respond = do
    let host = T.takeWhile (/= ':') (maybe "" decodeUtf8 (Wai.requestHeaderHost request))
        method = Wai.requestMethod request
        path = decodeUtf8 (Wai.rawPathInfo request <> Wai.rawQueryString request)
        send status headers body = do
            atomically (modifyTVar' (rubyRegistryRequests registry) (<> [NativeRequest "https" host (decodeUtf8 method) path (statusCode status)]))
            respond (Wai.responseLBS status headers (if method == "HEAD" then "" else body))
    atomically (modifyTVar' (rubyRegistryHeaders registry) (<> [(host, method, path, Wai.requestHeaders request)]))
    atomically (modifyTVar' (rubyRegistryCredentials registry) (<> [(host, method, lookup "Authorization" (Wai.requestHeaders request))]))
    fault <- Map.findWithDefault RubyRegistryHealthy host <$> readTVarIO (rubyRegistryFaults registry)
    stored <- rubyRegistryFiles registry host
    if fault == RubyRegistryUnavailable
        then send status503 [] "native registry unavailable"
        else
            if fault == RubyRegistryRequireRead && method `elem` ["GET", "HEAD"] && lookup "Authorization" (Wai.requestHeaders request) /= Just "Basic cnVieTpyZWFk"
                then send status401 [("WWW-Authenticate", "Basic realm=\"RubyGems fixture\"")] "native read authorisation required"
                else case (method, Wai.pathInfo request) of
                    ("POST", ["api", "v1", "gems"]) -> publish registry host fault request send respond
                    (readMethod, ["versions"]) | readMethod `elem` ["GET", "HEAD"] ->
                        case fault of
                            RubyRegistryDirectoryStatus status -> send (mkStatus status "native directory failure") [] "native directory unavailable"
                            _ -> send status200 [("Content-Type", "text/plain")] (LBS.fromStrict (encodeUtf8 (directoryBody host fault stored)))
                    (readMethod, ["api", "v1", "dependencies"]) | readMethod `elem` ["GET", "HEAD"] -> do
                        let names = maybe [] (T.splitOn "," . decodeUtf8) (join (lookup "gems" (Wai.queryString request)))
                            rows = [releaseManifest release | release <- stored, releaseName release `elem` names]
                        native <- runRubyReference "encode-dependencies" (LBS.toStrict (encode rows))
                        bytes <- either fail pure (parseEither (withObject "native bytes" (.: "bytes")) native)
                        send status200 [("Content-Type", "application/octet-stream")] (LBS.fromStrict (BS.pack bytes))
                    (readMethod, ["info", name])
                        | readMethod `elem` ["GET", "HEAD"]
                        , RubyRegistryInfoStatus selected status <- fault
                        , selected == name ->
                            send (mkStatus status "native info failure") [] "native info unavailable"
                    (readMethod, ["info", name]) | readMethod `elem` ["GET", "HEAD"] -> do
                        let releases = filter ((== name) . releaseName) stored
                        if null releases
                            then send status404 [] ""
                            else send status200 [("Content-Type", "text/plain")] (LBS.fromStrict (encodeUtf8 (infoBody host releases)))
                    (readMethod, ["gems", filename]) | readMethod `elem` ["GET", "HEAD"] ->
                        case find ((== filename) . releaseFilename) stored of
                            Just release | fault /= RubyRegistryMissingArchive filename -> serveArchive registry host request release send respond
                            _ -> send status404 [] ""
                    (readMethod, ["quick", "Marshal.4.8", filename]) | readMethod `elem` ["GET", "HEAD"] ->
                        case find ((== "quick/Marshal.4.8/" <> toString filename) . releaseCompactPath) stored of
                            Just release ->
                                let bytes = case fault of
                                        RubyRegistryCompactBytes selected changed | selected == releaseCompactPath release -> changed
                                        _ -> releaseCompact release
                                 in send status200 [] (LBS.fromStrict bytes)
                            _ -> send status404 [] ""
                    ("DELETE", _) -> send status405 [] "native yank is not physical deletion"
                    _ -> send status404 [] ""

-- A hinted row carries the MD5 of the info body this authority serves for the name, as rubygems.org's rows do.
directoryBody :: Text -> RubyRegistryFault -> [RubyRelease] -> Text
directoryBody host fault stored = case fault of
    RubyRegistryInfoHints -> rubyVersionsWith (\name -> hexMd5Of (encodeUtf8 (infoBody host (filter ((== name) . releaseName) stored)))) stored
    _ -> rubyVersions stored

-- Only the public authority keeps the fixture's publication times. Every other authority dates its rows 2030.
infoBody :: Text -> [RubyRelease] -> Text
infoBody host releases = "---\n" <> T.unlines (map (assigned . releaseInfoRow) releases)
  where
    assigned = if host == "rubygems-upstream" then id else T.replace "2020-01-01T00:00:00Z" "2030-01-01T00:00:00Z" . T.replace "2020-01-03T00:00:00Z" "2030-01-03T00:00:00Z"

serveArchive :: RubyRegistry -> Text -> Wai.Request -> RubyRelease -> (Status -> ResponseHeaders -> LBS.ByteString -> IO Wai.ResponseReceived) -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
serveArchive registry host request release send respond = do
    selected <- Map.lookup (host, releaseFilename release) <$> readTVarIO (rubyRegistryArtifactReplies registry)
    case selected of
        Nothing -> send status200 [("Content-Length", show (BS.length (releaseArchive release)))] (LBS.fromStrict (releaseArchive release))
        Just reply -> do
            let status = mkStatus (artifactReplyStatus reply) "native fixture"
                body = LBS.fromChunks (artifactReplyChunks reply)
                supplied = artifactReplyHeaders reply
                headers =
                    if artifactReplyChunked reply || isJust (lookup "Content-Length" supplied)
                        then supplied
                        else ("Content-Length", show (LBS.length body)) : supplied
            if artifactReplyChunked reply && Wai.requestMethod request /= "HEAD"
                then do
                    let path = decodeUtf8 (Wai.rawPathInfo request <> Wai.rawQueryString request)
                    atomically (modifyTVar' (rubyRegistryRequests registry) (<> [NativeRequest "https" host (decodeUtf8 (Wai.requestMethod request)) path (statusCode status)]))
                    respond (Wai.responseStream status headers (\write flush -> for_ (artifactReplyChunks reply) (\chunk -> write (byteString chunk) >> flush)))
                else send status headers body

publish :: RubyRegistry -> Text -> RubyRegistryFault -> Wai.Request -> (Status -> ResponseHeaders -> LBS.ByteString -> IO Wai.ResponseReceived) -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
publish registry host fault request send respond =
    boundedRead (PublishRequestBodyLimit (16 * 1024 * 1024)) (Wai.getRequestBodyChunk request) >>= \case
        Left _ -> send status413 [] "native archive exceeds fixture bound"
        Right (_, bytes) -> do
            atomically (modifyTVar' (rubyRegistryUploads registry) (<> [(host, bytes)]))
            known <- readTVarIO (rubyRegistryReleases registry)
            if lookup "Authorization" (Wai.requestHeaders request) /= Just (encodeUtf8 rubyRegistryToken)
                then send status401 [] "native API key required"
                else case find ((== bytes) . releaseArchive) known of
                    Nothing -> send status400 [] "archive is not an owned native fixture"
                    Just release | fault == RubyRegistryRejectUpload (releaseFilename release) -> send status503 [] "native upload refused"
                    Just release -> do
                        inventory <- readTVarIO (rubyRegistryInventory registry)
                        if Map.member (host, releaseFilename release) inventory
                            then send status409 [] "native coordinate already exists"
                            else
                                if fault == RubyRegistryStopAfterOne && any ((== host) . fst . fst) (Map.toList inventory)
                                    then do
                                        atomically (modifyTVar' (rubyRegistryBlockedUpload registry) (<|> Just (releaseFilename release)))
                                        released <- timeout 120000000 (atomically (readTVar (rubyRegistryUploadReleased registry) >>= checkSTM))
                                        send status503 [] (if isJust released then "interrupted native upload released" else "native upload barrier timed out")
                                    else do
                                        seedRubyRegistry registry host [release]
                                        if fault == RubyRegistryLoseReply
                                            then do
                                                atomically $ do
                                                    modifyTVar' (rubyRegistryLostReplies registry) (<> [(host, releaseFilename release)])
                                                    modifyTVar' (rubyRegistryFaults registry) (Map.insert host RubyRegistryHealthy)
                                                respond (Wai.responseRaw (\_ _ -> pure ()) (Wai.responseLBS status503 [] "upload reply lost"))
                                            else send status201 [] "native archive retained"
